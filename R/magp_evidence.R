# MAGPlot's evidence for issue classes (plan 3.6, 10.3; D9.28, D19.29, D19.38, D19.58, D19.59):
# the evidence `computed`, from pipeline_computed.csv, and the evidence `crosswalk`, from the
# translation tables and crosswalk_provider_columns.csv. data-raw/build_magp_config.R checks
# both hand-kept files against the spec and copies them into inst/extdata/magp/; magp_run_qc()
# passes the two tables to gpq_classify_issues(evidence = ) from M7.

#' One of MAGPlot's evidence files in inst/extdata/magp/, read as text (D19.59)
#'
#' The file is a copy the build made of a hand-kept file it checked, so one that is missing
#' or doesn't read cleanly is an internal error. Every column character, a blank NA.
#' @noRd
read_evidence_file <- function(name) {
  path <- system.file("extdata", "magp", name, package = "groundplotqc")
  if (!nzchar(path)) {
    stop(name, " is missing from the package.", call. = FALSE)
  }
  read <- read_csv_text(path)
  if (nrow(read$malformed) > 0L || nrow(read$invalid) > 0L) {
    stop(name, " doesn't read cleanly.", call. = FALSE)
  }
  read$data
}

#' MAGPlot's evidence `computed` for gpq_classify_issues() (plan 3.6, 10.3 step 2; D9.28)
#'
#' The rows of pipeline_computed.csv, one per table, attribute and jurisdiction whose values
#' the pipeline computes for every record: `table_name`, `column_name` (the file's
#' `attribute_name`), `jurisdiction`, the evidence's caller-named column ("*" for every
#' jurisdiction), and `derivation`; the file's `note` is left out. All character.
#' @noRd
computed_evidence <- function() {
  rows <- read_evidence_file("pipeline_computed.csv")
  data.table(
    table_name = rows$table_name, column_name = rows$attribute_name,
    jurisdiction = rows$jurisdiction, derivation = rows$derivation
  )
}

#' MAGPlot's evidence `crosswalk` for gpq_classify_issues() (plan 3.6, 10.3 steps 4 and 5;
#' D19.20, D19.59)
#'
#' For each row of crosswalk_provider_columns.csv, the provider column's cells on the rows of
#' the translation table that the attribute's code list keeps, each paired with that row's
#' MAGPlot code: a row keeps its code column's value, where the list has a filter only if
#' its filter column holds one of the filter values, as build_codes() keeps a list's codes
#' (R/spec_read.R). A cell holding several provider codes is split on "," and "/", each part
#' trimmed as the conformance checks trim a value (space_padding()) and a blank part dropped,
#' so "1,2" gives "1" and "2" and "Pw / Pa" gives "Pw" and "Pa" (D19.59). Where one
#' jurisdiction has two or three provider columns for an attribute, their codes are pooled.
#' An attribute of two tables (species) has one code list here, and a map row whose
#' attribute's list in `spec` isn't that translation table, resolved, gives no rows.
#'
#' Reads `spec`'s `code_list_map` and `crosswalks` by column and joins them, never row by row
#' (16.2). Returns a data.table of distinct rows, all character: `jurisdiction`, the
#' evidence's caller-named column, `provider_code`, `code` and `column_name`, the attribute.
#' @noRd
crosswalk_evidence <- function(spec = magp_spec()) {
  map <- read_evidence_file("crosswalk_provider_columns.csv")
  coded <- spec$code_list_map
  coded <- coded[coded$source_type %chin% "crosswalk" & coded$status %chin% "resolved"]
  lists <- unique(data.table(
    crosswalk = coded$source_name, attribute_name = coded$attribute_name,
    code_column = coded$code_column, filter_column = coded$filter_column,
    filter_values = coded$filter_values
  ))
  set(lists, j = "list_id", value = seq_len(nrow(lists)))
  cells <- spec$crosswalks
  cells <- cells[
    cells$source_row > 1L & !is.na(cells$value),
    c("crosswalk", "source_row", "crosswalk_column", "value")
  ]
  # Each list's rows with a MAGPlot code, then, for a list with a filter, only the rows whose
  # filter column holds one of its values.
  coded_rows <- cells[
    lists,
    on = c("crosswalk", crosswalk_column = "code_column"), nomatch = NULL
  ]
  coded_rows <- data.table(
    list_id = coded_rows$list_id, source_row = coded_rows$source_row, code = coded_rows$value
  )
  filtered <- lists[!is.na(lists$filter_column)]
  wanted <- strsplit(filtered$filter_values, "; ", fixed = TRUE)
  wanted <- data.table(
    crosswalk = rep(filtered$crosswalk, lengths(wanted)),
    crosswalk_column = rep(filtered$filter_column, lengths(wanted)),
    value = unlist(wanted, use.names = FALSE),
    list_id = rep(filtered$list_id, lengths(wanted))
  )
  kept <- cells[wanted, on = c("crosswalk", "crosswalk_column", "value"), nomatch = NULL]
  kept <- unique(data.table(list_id = kept$list_id, source_row = kept$source_row))
  coded_rows <- rbind(
    coded_rows[!coded_rows$list_id %in% filtered$list_id],
    coded_rows[kept, on = c("list_id", "source_row"), nomatch = NULL]
  )
  # Each map row's provider cells, on the rows of its attribute's list.
  pairs <- lists[map, on = c("crosswalk", "attribute_name"), nomatch = NULL]
  provided <- cells[
    pairs,
    on = c("crosswalk", crosswalk_column = "provider_column"), nomatch = NULL
  ]
  provided <- data.table(
    list_id = provided$list_id, source_row = provided$source_row,
    provider_cell = provided$value, jurisdiction = provided$jurisdiction,
    column_name = provided$attribute_name
  )
  paired <- coded_rows[provided, on = c("list_id", "source_row"), nomatch = NULL]
  parts <- strsplit(paired$provider_cell, "[,/]")
  n_parts <- lengths(parts)
  part <- unlist(parts, use.names = FALSE)
  padding <- space_padding(part)
  keep <- !is.na(part) & !padding$blank
  unique(data.table(
    jurisdiction = rep(paired$jurisdiction, n_parts)[keep],
    provider_code = padding$trimmed[keep],
    code = rep(paired$code, n_parts)[keep],
    column_name = rep(paired$column_name, n_parts)[keep]
  ))
}
