box::use(
  DBI[
    dbExecute
  ],
  supabaseR[
    get_table_data,
    put_table_row,
    empty_table
  ],
)

box::use(
  utils/chronos_utils[
    get_combined_calendars
  ],
)

row_list <- apply(
  get_combined_calendars(),
  1,
  as.list
)

empty_table("chronos_cache")

lapply(
  row_list,
  function(row) {
    put_table_row(
      "chronos_cache",
      row
    )
  }
)
