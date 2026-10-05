# Report text: one CSV of text_id, lang and text, with {placeholder} filling (plan 11.7;
# D4.19, D9.19, D12.21). Every string pre-flight's table, its conditions and its pages
# show is a row there; errors meant for a caller or developer are English in the code
# (D12.37).
# Completed at M3 with the rule texts and the language fallback (D7.6).

#' The report text table
#' @noRd
report_texts <- function() {
  path <- system.file("extdata", "text", "report_text_engine.csv", package = "groundplotqc")
  if (!nzchar(path)) {
    stop("report_text_engine.csv is missing from the package.", call. = FALSE)
  }
  read_csv_text(path)$data
}

#' One report text, its placeholders filled
#'
#' Vectorised over the `...` values; stops on a missing row or an unfilled placeholder.
#' @noRd
report_text <- function(text_id, lang = "en", ...) {
  texts <- report_texts()
  template <- texts$text[texts$text_id == text_id & texts$lang == lang]
  if (length(template) != 1L) {
    stop(sprintf(
      "Report text %s has %d rows in language %s; it needs one.",
      text_id, length(template), lang
    ), call. = FALSE)
  }
  fill_placeholders(template, list(...))
}

#' A template's {placeholders} filled from named values
#' @noRd
fill_placeholders <- function(template, values) {
  pieces <- regmatches(template, gregexpr("\\{[a-z0-9_]+\\}", template), invert = NA)[[1L]]
  if (length(pieces) < 2L) {
    return(template)
  }
  slots <- seq(2L, length(pieces), by = 2L)
  wanted <- substr(pieces[slots], 2L, nchar(pieces[slots]) - 1L)
  missing <- setdiff(wanted, names(values))
  if (length(missing) > 0L) {
    stop(sprintf(
      "Report text needs a value for %s.", paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  if (any(lengths(values[wanted]) == 0L)) {
    return(character())
  }
  parts <- as.list(pieces)
  parts[slots] <- lapply(wanted, function(name) as.character(values[[name]]))
  do.call(paste0, parts)
}
