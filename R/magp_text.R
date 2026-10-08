# MAGPlot's report text (plan 11.7; D4.19, D14.8): MAGPlot's wording where it differs from
# the engine's, each row replacing the engine row of the same text_id and lang in a MAGPlot
# run. The file ships header-only until a rule's wording differs; magp_run_qc() passes the
# table to the engine as `text` from M7.

#' MAGPlot's report text table, checked
#'
#' A problem in the file's rows stops with validate_text_table()'s condition, its message
#' prefixed with "report_text_magp.csv: " (D14.38).
#' @noRd
magp_texts <- function() {
  path <- system.file("extdata", "text", "report_text_magp.csv", package = "groundplotqc")
  if (!nzchar(path)) {
    stop("report_text_magp.csv is missing from the package.", call. = FALSE)
  }
  read <- read_csv_text(path)
  if (nrow(read$malformed) > 0L || nrow(read$invalid) > 0L) {
    stop("report_text_magp.csv doesn't read cleanly.", call. = FALSE)
  }
  # A stop on the file's rows is the table check's own condition with the file's name in
  # front of its message: same class, rows still counted as data rows (D14.38).
  tryCatch(
    validate_text_table(read$data),
    error = function(cond) {
      cond$message <- paste0("report_text_magp.csv: ", conditionMessage(cond))
      stop(cond)
    }
  )
}
