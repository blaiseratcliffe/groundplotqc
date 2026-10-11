# The cascade (plan 6.2; D2.10, D7.3, D7.9): a value that failed a check of Layer 1, 2 or 3
# is masked for the later checks that read it, which record the record as not_evaluated
# with root_issue_id set to the earlier issue. failed_cells() lists the failed cells once,
# each with its root; mark_not_evaluated() gives, for a check about to run, the records it
# mustn't evaluate. Cells are placed by row number, which every record-level issue
# carries. Column-level issues are made after the cascade, from record-level ones (6.3, M4).

#' The cells that failed a check of the given layers, one row each with its root
#'
#' From the open record-level issues of `layers` (Layers 1 to 3 by default, 6.2):
#' `table_name`, `column_name`, `row_number`, `layer` and `issue_id`, one row per cell, the
#' root being the cell's issue of the lowest layer, then the lowest issue ID. Ordered by
#' table, column and row.
#' @noRd
failed_cells <- function(issues, layers = 1:3) {
  failing <- which(
    issues$level == "record" & issues$status == "open" & issues$layer %in% layers &
      !is.na(issues$row_number)
  )
  cells <- data.table(
    table_name = issues$table_name[failing], column_name = issues$column_name[failing],
    row_number = issues$row_number[failing], layer = issues$layer[failing],
    issue_id = issues$issue_id[failing]
  )
  setorderv(cells, c("table_name", "column_name", "row_number", "layer", "issue_id"))
  first <- !duplicated(cells, by = c("table_name", "column_name", "row_number"))
  cells[first]
}

#' The records a check mustn't evaluate, each with the issue that masks it
#'
#' For a check of layer `layer` that reads `columns` of table `table_name`: every row with
#' a failed cell (failed_cells()) in one of those columns from an earlier layer, among
#' `rows` when given (the check's own records, by row number). Its root is the failure of
#' the lowest layer, then the lowest issue ID. Returns `row_number` and `root_issue_id`,
#' one row per masked record, ordered by row. `table_name` is one table name, a developer
#' error otherwise. It narrows the rows step by step (the cheap tests over all of `failed`,
#' `columns` and `rows` over the rows left), and takes only the columns the order and the
#' result need from the hits.
#' @noRd
mark_not_evaluated <- function(failed, table_name, columns, layer, rows = NULL) {
  if (!(is.character(table_name) && length(table_name) == 1L && !is.na(table_name))) {
    stop("`table_name` must be one table name.", call. = FALSE)
  }
  hit <- which(failed$layer < layer & failed$table_name == table_name)
  hit <- hit[failed$column_name[hit] %chin% columns]
  if (!is.null(rows)) {
    hit <- hit[failed$row_number[hit] %in% rows]
  }
  at <- hit[order(failed$row_number[hit], failed$layer[hit], failed$issue_id[hit])]
  first <- !duplicated(failed$row_number[at])
  data.table(row_number = failed$row_number[at][first], root_issue_id = failed$issue_id[at][first])
}
