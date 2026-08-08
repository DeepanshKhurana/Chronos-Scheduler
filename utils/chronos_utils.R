box::use(
  dplyr[
    arrange,
    filter,
    bind_rows,
    case_when,
    distinct,
    group_by,
    mutate,
    select,
    slice_min,
    ungroup
  ],
  ical[
    ical_parse_df
  ],
  tidyr[
    separate_rows
  ],
  glue[
    glue
  ],
  supabaseR[
    get_table_data
  ],
  almanac[
    alma_in,
    daily,
    monthly,
    recur_for_count,
    recur_on_day_of_week,
    recur_on_interval,
    recur_on_month_of_year,
    weekly,
    yearly
  ],
  purrr[
    pmap
  ],
)

#' RRULE BYDAY two-letter weekday codes, keyed to full weekday names.
weekday_code_to_name <- c(
  MO = "Monday",
  TU = "Tuesday",
  WE = "Wednesday",
  TH = "Thursday",
  FR = "Friday",
  SA = "Saturday",
  SU = "Sunday"
)

#' Build a recurrence schedule for a single (already BYDAY-split) event row.
#'
#' Delegates all RFC 5545 recurrence semantics (nth-weekday-of-month,
#' nth-weekday-of-month-of-year, UNTIL/COUNT bounds, INTERVAL, defaulting
#' BYDAY-less WEEKLY/MONTHLY/YEARLY rules to dtstart's weekday/day-of-month/
#' month-and-day) to almanac, rather than reimplementing that date math
#' by hand.
#'
#' @param dtstart The series' start date.
#' @param rrule_freq One of DAILY, WEEKLY, MONTHLY, YEARLY.
#' @param rrule_byday A single RRULE BYDAY token (e.g. "WE", "2WE", "-1FR"),
#'   or NA/"" if absent.
#' @param rrule_until The series' end date, or NA if unbounded.
#' @param rrule_count The series' occurrence count, or NA if unbounded.
#' @param rrule_interval The series' recurrence interval, or NA for 1.
#' @param rrule_bymonth The RRULE BYMONTH value (1-12), or NA if absent.
#'   Only used for YEARLY + BYDAY (e.g. "last Monday of May"); when absent,
#'   defaults to dtstart's month.
#' @return An almanac rschedule object.
build_rschedule <- function(
  dtstart,
  rrule_freq,
  rrule_byday = NA_character_,
  rrule_until = as.Date(NA),
  rrule_count = NA_integer_,
  rrule_interval = NA_integer_,
  rrule_bymonth = NA_integer_
) {
  freq_fn <- switch(
    rrule_freq,
    DAILY = daily,
    WEEKLY = weekly,
    MONTHLY = monthly,
    YEARLY = yearly
  )

  rschedule <- freq_fn(
    since = dtstart,
    until = if (is.na(rrule_until)) NULL else rrule_until
  )

  if (!is.na(rrule_interval) && rrule_interval > 1) {
    rschedule <- rschedule |> recur_on_interval(rrule_interval)
  }

  if (!is.na(rrule_count)) {
    rschedule <- rschedule |> recur_for_count(rrule_count)
  }

  if (rrule_freq == "YEARLY" && !is.na(rrule_byday) && rrule_byday != "") {
    month_of_year <- if (is.na(rrule_bymonth)) {
      as.integer(format(dtstart, "%m"))
    } else {
      rrule_bymonth
    }
    rschedule <- rschedule |> recur_on_month_of_year(month_of_year)
  }

  if (!is.na(rrule_byday) && rrule_byday != "") {
    code <- toupper(substr(rrule_byday, nchar(rrule_byday) - 1, nchar(rrule_byday)))
    ordinal <- substr(rrule_byday, 1, nchar(rrule_byday) - 2)
    weekday_name <- weekday_code_to_name[[code]]

    rschedule <- if (rrule_freq %in% c("MONTHLY", "YEARLY") && ordinal != "") {
      rschedule |> recur_on_day_of_week(weekday_name, nth = as.integer(ordinal))
    } else {
      rschedule |> recur_on_day_of_week(weekday_name)
    }
  }

  rschedule
}

#' Process calendar data frame.
#'
#' @param calendar_df A data frame with calendar events.
#' @param to_ignore A vector of event types to ignore.
#' @return A processed data frame with status labels.
process_calendar_df <- function(
  calendar_df,
  to_ignore = c(
    "stay",
    "vacation",
    "holidays",
    "optional",
    "coffee together"
  )
) {
  to_ignore <- paste(to_ignore, collapse = "|")
  calendar_df |>
    mutate(
      start = as.Date(dtstart),
      is_ignored = grepl(to_ignore, tolower(summary)),
      status = case_when(
        is_ignored ~ NA_character_,
        start == Sys.Date() ~ "TODAY",
        start == (Sys.Date() + 1) ~ "TOMORROW",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(status))
}

#' Process repeating events in calendar data.
#'
#' Handles DAILY, WEEKLY, MONTHLY, and YEARLY recurrence rules, including
#' UNTIL/COUNT bounds, INTERVAL, nth-weekday-of-month BYDAY tokens
#' (e.g. "2WE", "-1FR"), and nth-weekday-of-month-of-year YEARLY BYDAY
#' tokens (e.g. "-1MO" with BYMONTH=5, for "last Monday of May"), via
#' almanac's recurrence rule engine.
#'
#' @param calendar_df A data frame with calendar events.
#' @return A data frame of repeating events occurring today or tomorrow.
process_recurring_events <- function(
  calendar_df
) {
  today <- Sys.Date()
  tomorrow <- Sys.Date() + 1

  for (col in c("rrule_byday", "rrule_until", "rrule_count", "rrule_interval", "rrule_bymonth")) {
    if (!(col %in% names(calendar_df))) {
      calendar_df[[col]] <- NA_character_
    }
  }

  recurring_df <- calendar_df |>
    filter(
      rrule_freq %in% c("DAILY", "WEEKLY", "MONTHLY", "YEARLY")
    ) |>
    mutate(
      dtstart = as.Date(dtstart),
      rrule_until = as.Date(rrule_until),
      rrule_count = suppressWarnings(as.integer(rrule_count)),
      rrule_interval = suppressWarnings(as.integer(rrule_interval)),
      rrule_bymonth = suppressWarnings(as.integer(rrule_bymonth))
    ) |>
    separate_rows(rrule_byday, sep = ",") |>
    mutate(
      rrule_byday = trimws(rrule_byday)
    )

  if (nrow(recurring_df) == 0) {
    recurring_df$start <- as.Date(character())
    recurring_df$status <- character()
    return(
      recurring_df |>
        select(
          -dtstart,
          -rrule_freq,
          -rrule_byday,
          -rrule_until,
          -rrule_count,
          -rrule_interval,
          -rrule_bymonth
        )
    )
  }

  rschedules <- pmap(
    list(
      dtstart = recurring_df$dtstart,
      rrule_freq = recurring_df$rrule_freq,
      rrule_byday = recurring_df$rrule_byday,
      rrule_until = recurring_df$rrule_until,
      rrule_count = recurring_df$rrule_count,
      rrule_interval = recurring_df$rrule_interval,
      rrule_bymonth = recurring_df$rrule_bymonth
    ),
    build_rschedule
  )

  recurring_df$status <- vapply(
    rschedules,
    function(rschedule) {
      if (alma_in(today, rschedule)) {
        "TODAY"
      } else if (alma_in(tomorrow, rschedule)) {
        "TOMORROW"
      } else {
        NA_character_
      }
    },
    character(1)
  )

  recurring_df |>
    mutate(start = today) |>
    filter(!is.na(status)) |>
    select(
      -dtstart,
      -rrule_freq,
      -rrule_byday,
      -rrule_until,
      -rrule_count,
      -rrule_interval,
      -rrule_bymonth
    )
}

#' Combine calendar data frames, including recurring events.
#'
#' @param calendar_df A data frame with calendar events.
#' @return A combined and filtered data frame of calendar events.
combine_calendar_df <- function(
    calendar_df
) {
  processed_df <- process_calendar_df(calendar_df)

  if ("rrule_freq" %in% names(calendar_df)) {
    processed_df <- bind_rows(
      processed_df,
      process_recurring_events(calendar_df)
    )
  }

  processed_df |>
    filter(
      status != "PAST",
      start >= Sys.Date()
    ) |>
    select(
      summary,
      start,
      status
    ) |>
    distinct() |>
    arrange(status)
}

#' Read a calendar and parse it
#'
#' @param url The url for the calendar
#' @param name The name of the calendar
#' @param priority The priorty of the calendar
#' @return A data frame of events
read_calendar <- function(
  url,
  name,
  priority
) {
  print(
    glue(
      "Processing: {name} | Priority: {priority}"
    )
  )
  ical_parse_df(
    text = readLines(
      url,
      warn = FALSE
    )
  ) |>
    combine_calendar_df() |>
    mutate(
      label = toupper(name),
      priority = priority
    )
}

#' Get Combined Calendars
#'
#' Downloads and combines calendar data from specified URLs.
#'
#' @param calendars A dataframe containing calendars.
#' @return A combined dataframe of parsed calendar events.
#' @export
get_combined_calendars <- function(
  calendars = get_table_data("chronos_calendars")
) {
  process_combined_calendars(
    lapply(
      seq_len(
        nrow(
          calendars
        )
      ),
      function(index) {
        read_calendar(
          url = calendars$url[index],
          name = calendars$name[index],
          priority = calendars$priority[index]
        )
      }
    )
  )
}

#' Process Combined Calendars
#'
#' Combines and processes a list of processed calendar dataframes.
#'
#' @param processed_calendars A list of dataframes from parsed calendar events.
#' @return A single dataframe containing distinct calendar events.
process_combined_calendars <- function(
  processed_calendars
) {
  do.call(
    rbind,
    processed_calendars
  ) |>
    arrange(status) |>
    group_by(summary) |>
    slice_min(
      order_by = priority,
      with_ties = TRUE
    ) |>
    ungroup() |>
    distinct() |>
    select(
      -c(
        priority,
        start
      )
    )
}
