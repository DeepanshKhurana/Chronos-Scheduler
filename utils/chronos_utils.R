box::use(
  dplyr[
    arrange,
    filter,
    bind_rows,
    case_when,
    distinct,
    group_by,
    left_join,
    mutate,
    select,
    slice_min,
    ungroup
  ],
  ical[
    ical_parse_df,
    ical_parse_full
  ],
  tidyr[
    separate_rows
  ],
  glue[
    glue
  ],
  supabaseR[
    sb_read
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

#' Extract per-event timezone metadata directly from raw ICS text.
#'
#' ical::ical_parse_df() silently drops the TZID parameter on DTSTART/DTEND:
#' a value like "DTSTART;TZID=America/New_York:20260810T093000" comes back
#' as the bare naive datetime "2026-08-10 09:30:00" with no indication it
#' isn't in the local system timezone. This walks the lower-level jCal-style
#' structure from ical_parse_full(), which does retain TZID and the value
#' type (date vs date-time), keyed by uid so it can be joined back onto the
#' ical_parse_df() output.
#'
#' @param raw_lines Character vector of raw ICS lines (as read by readLines).
#' @return A data frame with columns uid (unique, non-NA), dtstart_tzid (NA
#'   when the value was UTC-normalized or floating - no correction needed),
#'   and dtstart_is_date_only (TRUE for all-day/VALUE=DATE events).
extract_event_time_metadata <- function(
  raw_lines
) {
  vevents <- ical_parse_full(text = raw_lines)$items$vevents

  rows <- lapply(
    vevents,
    function(vevent) {
      properties <- vevent[[2]]
      uid_prop <- Find(function(p) identical(p[[1]], "uid"), properties)
      dtstart_prop <- Find(function(p) identical(p[[1]], "dtstart"), properties)

      dtstart_tzid <- if (!is.null(dtstart_prop)) dtstart_prop[[2]]$tzid else NULL
      dtstart_valuetype <- if (!is.null(dtstart_prop)) dtstart_prop[[3]] else NA_character_

      data.frame(
        uid = if (!is.null(uid_prop)) uid_prop[[4]] else NA_character_,
        dtstart_tzid = if (is.null(dtstart_tzid)) NA_character_ else dtstart_tzid,
        dtstart_is_date_only = isTRUE(dtstart_valuetype == "date"),
        stringsAsFactors = FALSE
      )
    }
  )

  metadata <- do.call(rbind, rows)

  # A left_join on uid would otherwise explode into a many-to-many match:
  # events with no uid at all share NA==NA, and RFC 5545 exception/override
  # instances of a recurring event legitimately share the master's uid.
  # Neither case needs more than one row's worth of tz metadata per uid.
  metadata <- metadata[!is.na(metadata$uid), ]
  metadata[!duplicated(metadata$uid), ]
}

#' Compute an occurrence's absolute start/end instants.
#'
#' Reinterprets dtstart's naive local time-of-day under the event's real
#' original timezone (falling back to CHRONOS_DEFAULT_TIMEZONE for
#' UTC-normalized/floating values, which ical_parse_df() already handles
#' correctly or per-convention), anchored on the date the occurrence
#' actually falls on (which may differ from dtstart's own date for
#' recurring events). Duration is preserved from the original dtstart/dtend
#' gap. All-day events get NA times rather than a fabricated midnight.
#'
#' @param occurrence_date The date this occurrence actually falls on.
#' @param dtstart The original (full, untruncated) dtstart POSIXct.
#' @param dtend The original (full, untruncated) dtend POSIXct.
#' @param dtstart_tzid The event's original IANA timezone, or NA.
#' @param is_date_only TRUE for all-day (VALUE=DATE) events.
#' @return A list with `start` and `end`, each a single POSIXct (or NA).
compute_event_times <- function(
  occurrence_date,
  dtstart,
  dtend,
  dtstart_tzid,
  is_date_only
) {
  if (isTRUE(is_date_only) || is.na(occurrence_date)) {
    return(list(start = as.POSIXct(NA), end = as.POSIXct(NA)))
  }

  local_tz <- if (is.na(dtstart_tzid) || dtstart_tzid == "") {
    Sys.getenv("CHRONOS_DEFAULT_TIMEZONE", "Asia/Kolkata")
  } else {
    dtstart_tzid
  }

  event_start_time <- as.POSIXct(
    paste(occurrence_date, format(dtstart, "%H:%M:%S")),
    tz = local_tz
  )
  event_end_time <- event_start_time +
    as.numeric(difftime(dtend, dtstart, units = "secs"))

  list(start = event_start_time, end = event_end_time)
}

#' Process calendar data frame.
#'
#' @param calendar_df A data frame with calendar events.
#' @param to_ignore A vector of event types to ignore.
#' @return A processed data frame with status labels and event times.
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
  if (!("dtstart_tzid" %in% names(calendar_df))) {
    calendar_df$dtstart_tzid <- NA_character_
  }
  if (!("dtstart_is_date_only" %in% names(calendar_df))) {
    calendar_df$dtstart_is_date_only <- FALSE
  }

  to_ignore <- paste(to_ignore, collapse = "|")
  processed_df <- calendar_df |>
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

  event_times <- pmap(
    list(
      occurrence_date = processed_df$start,
      dtstart = processed_df$dtstart,
      dtend = processed_df$dtend,
      dtstart_tzid = processed_df$dtstart_tzid,
      is_date_only = processed_df$dtstart_is_date_only
    ),
    compute_event_times
  )

  processed_df$event_start_time <- as.POSIXct(
    vapply(event_times, function(x) as.numeric(x$start), numeric(1)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  processed_df$event_end_time <- as.POSIXct(
    vapply(event_times, function(x) as.numeric(x$end), numeric(1)),
    origin = "1970-01-01",
    tz = "UTC"
  )

  processed_df
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
  if (!("dtstart_tzid" %in% names(calendar_df))) {
    calendar_df$dtstart_tzid <- NA_character_
  }
  if (!("dtstart_is_date_only" %in% names(calendar_df))) {
    calendar_df$dtstart_is_date_only <- FALSE
  }

  recurring_df <- calendar_df |>
    filter(
      rrule_freq %in% c("DAILY", "WEEKLY", "MONTHLY", "YEARLY")
    ) |>
    mutate(
      dtstart_date = as.Date(dtstart),
      rrule_until = as.Date(rrule_until),
      rrule_count = suppressWarnings(as.integer(rrule_count)),
      rrule_interval = suppressWarnings(as.integer(rrule_interval)),
      rrule_bymonth = suppressWarnings(as.integer(rrule_bymonth))
    ) |>
    separate_rows(rrule_byday, sep = ",") |>
    mutate(
      rrule_byday = trimws(rrule_byday)
    )

  drop_helper_cols <- function(df) {
    df |>
      select(
        -dtstart_date,
        -rrule_freq,
        -rrule_byday,
        -rrule_until,
        -rrule_count,
        -rrule_interval,
        -rrule_bymonth,
        -dtstart_tzid,
        -dtstart_is_date_only
      )
  }

  if (nrow(recurring_df) == 0) {
    recurring_df$start <- as.Date(character())
    recurring_df$status <- character()
    recurring_df$event_start_time <- as.POSIXct(character())
    recurring_df$event_end_time <- as.POSIXct(character())
    return(drop_helper_cols(recurring_df))
  }

  rschedules <- pmap(
    list(
      dtstart = recurring_df$dtstart_date,
      rrule_freq = recurring_df$rrule_freq,
      rrule_byday = recurring_df$rrule_byday,
      rrule_until = recurring_df$rrule_until,
      rrule_count = recurring_df$rrule_count,
      rrule_interval = recurring_df$rrule_interval,
      rrule_bymonth = recurring_df$rrule_bymonth
    ),
    build_rschedule
  )

  occurrence <- lapply(
    rschedules,
    function(rschedule) {
      if (alma_in(today, rschedule)) {
        list(status = "TODAY", start = today)
      } else if (alma_in(tomorrow, rschedule)) {
        list(status = "TOMORROW", start = tomorrow)
      } else {
        list(status = NA_character_, start = as.Date(NA))
      }
    }
  )

  recurring_df$status <- vapply(occurrence, `[[`, character(1), "status")
  recurring_df$start <- do.call(c, lapply(occurrence, `[[`, "start"))

  recurring_df <- recurring_df |> filter(!is.na(status))

  event_times <- pmap(
    list(
      occurrence_date = recurring_df$start,
      dtstart = recurring_df$dtstart,
      dtend = recurring_df$dtend,
      dtstart_tzid = recurring_df$dtstart_tzid,
      is_date_only = recurring_df$dtstart_is_date_only
    ),
    compute_event_times
  )

  recurring_df$event_start_time <- as.POSIXct(
    vapply(event_times, function(x) as.numeric(x$start), numeric(1)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  recurring_df$event_end_time <- as.POSIXct(
    vapply(event_times, function(x) as.numeric(x$end), numeric(1)),
    origin = "1970-01-01",
    tz = "UTC"
  )

  drop_helper_cols(recurring_df)
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
      status,
      event_start_time,
      event_end_time,
      attendees
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

  raw_lines <- readLines(
    url,
    warn = FALSE
  )

  ical_parse_df(text = raw_lines) |>
    left_join(
      extract_event_time_metadata(raw_lines),
      by = "uid"
    ) |>
    combine_calendar_df() |>
    mutate(
      label = toupper(name),
      priority = priority
    )
}

#' Get Combined Calendars
#'
#' Downloads and combines calendar data from specified URLs. Requires an
#' active supabaseR connection (call \code{supabaseR::sb_connect()} first)
#' if relying on the default \code{calendars} argument.
#'
#' @param calendars A dataframe containing calendars.
#' @return A combined dataframe of parsed calendar events.
#' @export
get_combined_calendars <- function(
  calendars = sb_read("chronos_calendars")
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
