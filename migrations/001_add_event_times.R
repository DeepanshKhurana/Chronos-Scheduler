box::use(
  supabaseR[
    sb_connect,
    sb_disconnect
  ],
)

run_migration <- function() {
  conn <- sb_connect()
  on.exit(sb_disconnect(), add = TRUE)

  schema <- Sys.getenv("SUPABASE_SCHEMA", "public")

  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "
      ALTER TABLE {`schema`}.{`table`}
        ADD COLUMN IF NOT EXISTS event_start_time timestamptz,
        ADD COLUMN IF NOT EXISTS event_end_time timestamptz
      ",
      table = "chronos_cache",
      .con = conn
    )
  )
}

run_migration()
