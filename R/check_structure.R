# Layer 0, structure (plan 6.1; D2.34, D8.11, D16.8, D16.14): the tables and columns given,
# against the dictionary. Each rule checks units, not records: one outcome per table or
# column, which the run records ungrouped (D16.8). A column whose name differs from its
# attribute's only in letter case or spaces is column_name_mismatch, never column_missing
# or column_unexpected (D16.14).

#' Layer 0's outcomes, by rule
#'
#' `table_missing` per spec table; `table_unexpected` per given table; `column_missing` per
#' attribute of a given table; `column_unexpected` per given column of a spec table;
#' `column_name_mismatch` per attribute whose column was found. An attribute of a table not
#' given is not_run (no_table) for the three column rules; one with no column is not_run
#' (no_column) for `column_name_mismatch`.
#' @noRd
check_structure <- function(input, context) {
  columns <- input$columns
  spec_tables <- unique(columns$table_name)
  given <- input$given
  table_rows <- function(table) {
    if (table %chin% names(input$data)) nrow(input$data[[table]]) else 0L
  }
  list(
    table_missing = lapply(spec_tables, function(table) {
      unit_outcome(
        "table_missing", table, "*",
        failed = table %chin% input$missing, n_records = table_rows(table)
      )
    }),
    table_unexpected = lapply(seq_len(nrow(given)), function(i) {
      unit_outcome(
        "table_unexpected", given$table_name[[i]], "*",
        failed = given$table_name[[i]] %chin% input$unexpected, n_records = given$n_rows[[i]]
      )
    }),
    column_missing = lapply(seq_len(nrow(columns)), function(i) {
      table <- columns$table_name[[i]]
      attribute <- columns$attribute_name[[i]]
      if (!columns$table_present[[i]]) {
        return(not_run_outcome("column_missing", table, attribute, "no_table"))
      }
      unit_outcome(
        "column_missing", table, attribute,
        failed = is.na(columns$column[[i]]), n_records = table_rows(table)
      )
    }),
    column_unexpected = unlist(lapply(names(input$data), function(table) {
      extra <- input$extra$column[input$extra$table_name == table]
      lapply(names(input$data[[table]]), function(column) {
        unit_outcome(
          "column_unexpected", table, column,
          failed = column %chin% extra, n_records = table_rows(table)
        )
      })
    }), recursive = FALSE),
    column_name_mismatch = rule_outcomes(
      "column_name_mismatch", columns, input, function(table, attribute, x, row) {
        unit_outcome(
          "column_name_mismatch", table, attribute,
          failed = row$near, n_records = length(x), value = row$column, expected = attribute
        )
      }
    )
  )
}
