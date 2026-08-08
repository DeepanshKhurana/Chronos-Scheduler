box::use(
  supabaseR[
    sb_connect,
    sb_disconnect,
    sb_insert,
    sb_truncate
  ],
)

box::use(
  utils/chronos_utils[
    get_combined_calendars
  ],
)

#' Refresh the calendar data on chronos_cache table
refresh_chronos_cache <- function() {
  sb_connect()
  on.exit(sb_disconnect(), add = TRUE)
  sb_truncate("chronos_cache")
  sb_insert("chronos_cache", data = get_combined_calendars())
}

refresh_chronos_cache()
