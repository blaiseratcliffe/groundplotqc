# Problem plots (plan 8.8; D3.14, D8.15, D9.31, D23.24, D23.37): the records behind each
# problem unit's rate, counted by the run into the results' unit_records table, so a rebuild
# ranks the units without the data, and each unit's rate, worked out from those counts and
# the issues still open when the report is built (problem_plot_rates()).

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

#' Each problem unit's rate: its records with an open blocking issue over its records (8.8;
#' D3.14, D23.37)
#'
#' `unit_records` is a run's unit_records table, or the rows of it a report unit is given;
#' `issues` the issues of the same run or report unit; `group_cols` the group columns a unit is
#' counted within, each a column of both, such as a report's group columns; `key_col` the
#' problem unit's key column; `blocking_severities` the severities that count. A unit is one
#' value of `key_col` within one combination of `group_cols`: its records are the sum of its
#' `n_records` over the tables that hold them, and those with an open blocking issue are the
#' distinct records (table and row number) holding at least one open record-level issue of a
#' blocking severity. Column-level issues name no record, so they don't count (D23.37), and nor
#' does an issue whose key is "*" or NA, which reaches no unit. An issue of a unit with no
#' unit_records row isn't counted. A group's rate is the sum of its units' counts. The counts
#' are worked out under names of their own, so no caller's column meets them, and each unit
#' meets its count by one key join (16.2). Returns a
#' new data.table, one row per unit with records, by group and key in C order: the group
#' columns and the key column under the caller's names, `n_records`, `n_blocked` (0 where none
#' is blocked) and `rate`.
#' @noRd
problem_plot_rates <- function(unit_records, issues, group_cols, key_col, blocking_severities) {
  by <- unique(c(group_cols, key_col))
  cells <- sprintf("cell_%d", seq_along(by))
  units <- setDT(structure(lapply(by, function(column) unit_records[[column]]), names = cells))
  set(units, j = "n", value = unit_records$n_records)
  units <- units[, list(n_records = sum(n)), by = cells]
  blocked <- which(
    issues$status %chin% "open" & issues$level %chin% "record" &
      issues$severity %chin% blocking_severities & !issues[[key_col]] %chin% c("*", NA)
  )
  records <- setDT(structure(
    lapply(by, function(column) issues[[column]][blocked]),
    names = cells
  ))
  set(records, j = "table_name", value = issues$table_name[blocked])
  set(records, j = "row_number", value = issues$row_number[blocked])
  records <- unique(records)[, list(n = .N), by = cells]
  # Each unit's count, by one key join (key_matches(), R/lineage_classify.R).
  pairs <- key_matches(
    lapply(cells, function(cell) records[[cell]]), seq_len(nrow(records)), units, cells
  )
  n_blocked <- integer(nrow(units))
  n_blocked[pairs$at] <- records$n[pairs$row]
  set(units, j = "n_blocked", value = n_blocked)
  set(units, j = "rate", value = n_blocked / units$n_records)
  # The text keys read as UTF-8, so the radix sort's byte order is C order whatever a key's
  # encoding mark (D20.42, R94).
  by_key <- do.call(
    order,
    c(lapply(cells, function(cell) enc2utf8(units[[cell]])), list(method = "radix"))
  )
  units <- units[by_key]
  setnames(units, cells, by)
  units
}
