# Problem plots (plan 8.8; D3.14, D8.15, D9.31, D23.24): the records behind each problem
# unit's rate, counted by the run into the results' unit_records table, so a rebuild ranks the
# units without the data.

#' The records of each problem unit, per group and table (8.8, 9.1; D9.31, D23.24)
#'
#' One row per group, problem unit and table holding records of that unit, `n_records` the
#' records there. A record's unit is its problem unit's key as its issues carry it (key_text()):
#' read in its own table where that holds the key column and a value there, otherwise through
#' the foreign keys (reach_column(), kept by conformance_input() in `key_rows`), so a unit's
#' records are those of its own table and of every table whose records reach it. A record whose
#' unit is "*" isn't counted. Each table holding the key column has its column numbered once
#' (text by match(), any other class by its dense rank, as reach_column() reads a record's own
#' value), the records are counted by group and the number of the value they read, and the key
#' is written as text for one record per count, never for every record (16.2); counts whose
#' keys read alike, read in two tables or written alike, are then added up. Rows come by table
#' in the dictionary's order, then by group and key. Without a problem unit the table is empty.
#' Returns the run's unit_records table, completed and checked.
#' @noRd
unit_record_counts <- function(input, run_id) {
  group_cols <- input$group_cols
  key_col <- input$key_col
  schema <- results_schema(group_cols, key_col)$unit_records
  if (is.null(key_col)) {
    return(empty_table(schema))
  }
  parts <- list()
  # Each holding table's values as numbers, after the numbers of the tables numbered before it,
  # so a value read in one table never shares its number with one read in another.
  numbered <- list()
  used <- 0L
  for (table in names(input$data)) {
    reached <- input$key_rows[[table]]
    holders <- unique(reached$home)
    holders <- holders[!is.na(holders)]
    if (length(holders) == 0L) {
      next
    }
    for (holder in setdiff(holders, names(numbered))) {
      at <- which(input$columns$table_name == holder & input$columns$attribute_name == key_col)
      x <- input$data[[holder]][[input$columns$column[[at]]]]
      ids <- if (is.character(x)) {
        match(x, x[!duplicated(x)])
      } else {
        frankv(x, ties.method = "dense", na.last = TRUE)
      }
      numbered[[holder]] <- used + ids
      used <- used + max(c(0L, ids))
    }
    # Each record's value as its number, NA where it reaches none (its row NA): every record
    # takes the first table's number at its row, then those read in another table take that
    # table's, so a table whose records read one table, as most do, is read in one subset.
    value <- numbered[[holders[[1L]]]][reached$rows]
    for (holder in holders[-1L]) {
      here <- which(reached$home == holder)
      value[here] <- numbered[[holder]][reached$rows[here]]
    }
    grouping <- input$groups[[table]]
    # The records that reach no value are one count of their own, left out.
    counts <- data.table(group = grouping$row_group, value = value)[
      , list(first = .I[[1L]], n = .N),
      by = c("group", "value")
    ]
    counts <- counts[!is.na(counts$value)]
    set(counts, j = "key", value = utf8_marked(key_text(input, table, counts$first)))
    counts <- counts[counts$key != "*", list(n = sum(n)), by = c("group", "key")]
    setorderv(counts, c("group", "key"))
    part <- c(list(run_id = rep(run_id, nrow(counts))), group_values(grouping, counts$group))
    if (!key_col %chin% group_cols) {
      part[[key_col]] <- counts$key
    }
    part$table_name <- rep(utf8_marked(table), nrow(counts))
    part$n_records <- counts$n
    parts[[length(parts) + 1L]] <- part
  }
  rows <- rbindlist(parts, use.names = TRUE)
  unit_records <- complete_table(if (nrow(rows) == 0L) NULL else rows, schema, "unit_records")
  validate_schema(unit_records, "unit_records", schema)
  unit_records
}
