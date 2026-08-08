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
)

#' Get abbreviated weekday name.
#'
#' @param date A date object.
#' @return Abbreviated weekday name in uppercase.
get_parsed_weekday <- function(
  date
) {
  weekdays(date, abbreviate = TRUE) |>
    substr(start = 1, stop = 2) |>
    toupper()
}

#' Check if a date is the Nth occurrence of its weekday in its month.
#'
#' Interprets an RRULE MONTHLY BYDAY token such as "2WE" (2nd Wednesday)
#' or "-1FR" (last Friday). A token with no leading ordinal (e.g. "WE")
#' matches every occurrence of that weekday in the month. `date` must be
#' a single Date; `byday` may be a vector.
#'
#' @param date A single date object.
#' @param byday A character vector of RRULE BYDAY tokens.
#' @return A logical vector, one per element of `byday`.
matches_monthly_byday <- function(
  date,
  byday
) {
  code <- toupper(substr(byday, nchar(byday) - 1, nchar(byday)))
  ordinal <- substr(byday, 1, nchar(byday) - 2)

  day_of_month <- as.integer(format(date, "%d"))
  n_from_start <- (day_of_month - 1) %/% 7 + 1

  month_start <- as.Date(format(date, "%Y-%m-01"))
  next_month_start <- seq(month_start, by = "month", length.out = 2)[2]
  days_in_month <- as.integer(next_month_start - month_start)
  n_from_end <- (days_in_month - day_of_month) %/% 7 + 1

  get_parsed_weekday(date) == code & (
    ordinal == "" |
      (grepl("^[0-9]+$", ordinal) & as.integer(ordinal) == n_from_start) |
      (grepl("^-[0-9]+$", ordinal) & abs(as.integer(ordinal)) == n_from_end)
  )
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
#' Handles DAILY, WEEKLY, MONTHLY, and YEARLY recurrence rules. WEEKLY
#' events use rrule_byday when available, falling back to dtstart's
#' weekday. MONTHLY events match on dtstart's day-of-month, and YEARLY
#' events match on dtstart's month and day. Occurrences are bounded to
#' the series' active window: on or after dtstart, and on or before
#' rrule_until when present.
#'
#' @param calendar_df A data frame with calendar events.
#' @return A data frame of repeating events occurring today or tomorrow.
process_recurring_events <- function(
  calendar_df
) {
  today <- Sys.Date()
  tomorrow <- Sys.Date() + 1

  if (!("rrule_byday" %in% names(calendar_df))) {
    calendar_df$rrule_byday <- NA_character_
  }

  if (!("rrule_until" %in% names(calendar_df))) {
    calendar_df$rrule_until <- NA_character_
  }

  calendar_df |>
    filter(
      rrule_freq %in% c("DAILY", "WEEKLY", "MONTHLY", "YEARLY")
    ) |>
    mutate(
      dtstart = as.Date(dtstart),
      rrule_until = as.Date(rrule_until)
    ) |>
    separate_rows(rrule_byday, sep = ",") |>
    mutate(
      rrule_byday = trimws(rrule_byday),
      in_range_today = today >= dtstart &
        (is.na(rrule_until) | today <= rrule_until),
      in_range_tomorrow = tomorrow >= dtstart &
        (is.na(rrule_until) | tomorrow <= rrule_until),
      matches_today = in_range_today & case_when(
        rrule_freq == "DAILY" ~ TRUE,
        rrule_freq == "WEEKLY" & !is.na(rrule_byday) & rrule_byday != "" ~
          get_parsed_weekday(today) == rrule_byday,
        rrule_freq == "WEEKLY" ~
          get_parsed_weekday(today) == get_parsed_weekday(dtstart),
        rrule_freq == "MONTHLY" & !is.na(rrule_byday) & rrule_byday != "" ~
          matches_monthly_byday(today, rrule_byday),
        rrule_freq == "MONTHLY" ~ format(today, "%d") == format(dtstart, "%d"),
        rrule_freq == "YEARLY" ~ format(today, "%m-%d") == format(dtstart, "%m-%d"),
        TRUE ~ FALSE
      ),
      matches_tomorrow = in_range_tomorrow & case_when(
        rrule_freq == "DAILY" ~ TRUE,
        rrule_freq == "WEEKLY" & !is.na(rrule_byday) & rrule_byday != "" ~
          get_parsed_weekday(tomorrow) == rrule_byday,
        rrule_freq == "WEEKLY" ~
          get_parsed_weekday(tomorrow) == get_parsed_weekday(dtstart),
        rrule_freq == "MONTHLY" & !is.na(rrule_byday) & rrule_byday != "" ~
          matches_monthly_byday(tomorrow, rrule_byday),
        rrule_freq == "MONTHLY" ~ format(tomorrow, "%d") == format(dtstart, "%d"),
        rrule_freq == "YEARLY" ~ format(tomorrow, "%m-%d") == format(dtstart, "%m-%d"),
        TRUE ~ FALSE
      ),
      start = today,
      status = case_when(
        matches_today ~ "TODAY",
        matches_tomorrow ~ "TOMORROW",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(status)) |>
    select(
      -dtstart,
      -rrule_freq,
      -rrule_byday,
      -rrule_until,
      -in_range_today,
      -in_range_tomorrow,
      -matches_today,
      -matches_tomorrow
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
