# Report text: one CSV of text_id, lang and text, with {placeholder} filling (plan 11.7;
# D4.19, D9.19, D12.21, D12.45, D12.55). Every string pre-flight's table, its conditions
# and its pages show is a row there; errors meant for a caller or developer are English in
# the code (D12.37).
# Completed at M3 with the rule texts and the language fallback (D7.6).

# Slots that take a value read from the specification: quoted at fill time, a blank shown
# as the blank text (D12.45, D12.55).
quoted_slots <- c(
  "value", "value_a", "value_b", "code", "label", "other_label", "source_text", "name",
  "data_type", "lookup", "reference_table", "key_value"
)

# Slots that take a name from the specification: shown as written, a blank shown as the
# blank text (D12.54, D12.55). Every other slot stops on NA.
blank_slots <- c(
  "table_name", "attribute_name", "spec_type", "contributor_label", "key_col", "sheet",
  "column", "crosswalk"
)

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
#' The result is plain text, never escaped for HTML. A placeholder that lists several
#' things for one finding takes one string per finding, already joined. Values are matched
#' to `...` by name after `text_id` and `lang`, so a placeholder can't be named `text`,
#' `la` or any other start of those two names: R would match it to them instead.
#' @noRd
report_text <- function(text_id, lang = "en", ...) {
  ok <- is.character(text_id) && length(text_id) == 1L && !is.na(text_id) &&
    is.character(lang) && length(lang) == 1L && !is.na(lang)
  if (!ok) {
    stop("`text_id` and `lang` must each be one string.", call. = FALSE)
  }
  texts <- report_texts()
  template <- texts$text[texts$text_id == text_id & texts$lang == lang]
  if (length(template) != 1L) {
    stop(sprintf(
      "Report text %s has %d rows in language %s; it needs one.",
      text_id, length(template), lang
    ), call. = FALSE)
  }
  fill_placeholders(template, list(...), lang)
}

#' A template's {placeholders} filled from named values
#'
#' Each value is written as text (numbers in full, D12.45); a slot of quoted_slots is
#' quoted, a slot of blank_slots shows a blank as the blank text, and any other slot stops
#' on NA. Every value the template uses has length 1 or one common length.
#' @noRd
fill_placeholders <- function(template, values, lang = "en") {
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
  used <- unique(wanted)
  sizes <- lengths(values[used])
  if (length(unique(sizes[sizes != 1L])) > 1L) {
    stop(sprintf(
      "Report text values must have length 1 or one common length; got %s.",
      paste0(used, " ", sizes, collapse = ", ")
    ), call. = FALSE)
  }
  if (any(sizes == 0L)) {
    return(character())
  }
  filled <- lapply(used, function(name) {
    x <- as_text(values[[name]])
    if (name %in% quoted_slots) {
      return(quote_value(x, lang))
    }
    if (name %in% blank_slots) {
      return(blank_as_text(x, lang))
    }
    if (anyNA(x)) {
      stop(sprintf("Report text slot {%s} has an NA value.", name), call. = FALSE)
    }
    x
  })
  names(filled) <- used
  parts <- as.list(pieces)
  parts[slots] <- filled[wanted]
  do.call(paste0, parts)
}

#' A value from the specification in straight double quotes, a blank as the blank text
#' @noRd
quote_value <- function(x, lang = "en") {
  quoted <- paste0("\"", x, "\"")
  if (!anyNA(x)) {
    return(quoted)
  }
  fifelse(is.na(x), report_text("preflight_blank_value", lang), quoted)
}

#' A blank shown as the report text "(blank)"
#' @noRd
blank_as_text <- function(x, lang = "en") {
  if (!anyNA(x)) {
    return(x)
  }
  fifelse(is.na(x), report_text("preflight_blank_value", lang), x)
}
