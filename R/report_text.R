# Report text: CSVs of text_id, lang and text, with {placeholder} filling (plan 11.7;
# D4.19, D9.19, D12.21, D12.29, D12.45, D12.55, D14.7, D14.8). Every string pre-flight's
# table, its conditions and its pages show is a row of the engine's file; errors meant for a
# caller or developer are English in the code (D12.37). The engine's table is read once per
# session. A caller's text table, such as MAGPlot's (R/magp_text.R), overrides the engine's
# row of the same text_id and lang: a text is looked up in the caller's table in the
# language, then the engine's, then the caller's English, then the engine's English (D14.8).

# Slots that take a value read from the specification: quoted at fill time, a blank shown
# as the blank text (D12.45, D12.55).
quoted_slots <- c(
  "value", "value_a", "value_b", "code", "label", "other_label", "source_text", "name",
  "data_type", "lookup", "reference_table", "key_value"
)

# Slots that take a name from the specification or the rule set: shown as written, a blank
# shown as the blank text (D12.54, D12.55). Every other slot stops on NA.
blank_slots <- c(
  "table_name", "attribute_name", "spec_type", "contributor_label", "key_col", "sheet",
  "column", "crosswalk", "rule_id"
)

# The engine's text table, read once per session (D12.29); reset_report_texts() empties it.
text_cache <- new.env(parent = emptyenv())

#' The engine's report text table, read once per session
#'
#' The table is shared by every call, so a caller never changes it by reference.
#' @noRd
report_texts <- function() {
  if (is.null(text_cache$engine)) {
    path <- system.file("extdata", "text", "report_text_engine.csv", package = "groundplotqc")
    if (!nzchar(path)) {
      stop("report_text_engine.csv is missing from the package.", call. = FALSE)
    }
    text_cache$engine <- read_csv_text(path)$data
  }
  text_cache$engine
}

#' The cached report text emptied, so the next call reads the file again (for tests)
#' @noRd
reset_report_texts <- function() {
  rm(list = ls(text_cache, all.names = TRUE), envir = text_cache)
  invisible(NULL)
}

#' One report text, its placeholders filled
#'
#' Vectorised over the `...` values; stops on a missing row or an unfilled placeholder.
#' The result is plain text, never escaped for HTML. A placeholder that lists several
#' things for one finding takes one string per finding, already joined. Values are matched
#' to `...` by name after `text_id` and `lang`, so a placeholder can't be named `text`,
#' `la` or any other start of those two names: R would match it to them instead. `text`, a
#' table checked by validate_text_table(), overrides the engine's rows (D14.8).
#' @noRd
report_text <- function(text_id, lang = "en", ..., text = NULL) {
  ok <- is.character(text_id) && length(text_id) == 1L && !is.na(text_id) &&
    is.character(lang) && length(lang) == 1L && !is.na(lang)
  if (!ok) {
    stop("`text_id` and `lang` must each be one string.", call. = FALSE)
  }
  template <- text_template(text_id, lang, text)
  # as.vector() strips the template's `lang` attribute, so the string is plain text; the
  # language of the row it came from goes in separately, from attr(template, "lang").
  fill_placeholders(as.vector(template), list(...), lang, text_id, attr(template, "lang"))
}

#' A text's template, looked up in D14.8's order
#'
#' The caller's table in the language, the engine's in the language, the caller's in
#' English, the engine's in English; the first that has the row gives it. Two rows for one
#' text and language in one table, or no row at all, stop. The string carries the language
#' of the row it came from as its attribute `lang`, which differs from the asked language
#' when a fallback row gave it.
#' @noRd
text_template <- function(text_id, lang, text = NULL) {
  engine <- report_texts()
  steps <- list(list(text, lang), list(engine, lang), list(text, "en"), list(engine, "en"))
  for (step in steps) {
    table <- step[[1L]]
    if (is.null(table)) {
      next
    }
    found <- table[["text"]][table[["text_id"]] == text_id & table[["lang"]] == step[[2L]]]
    if (length(found) > 1L) {
      stop(sprintf(
        "Report text %s has %d rows in language %s; it needs one.",
        text_id, length(found), step[[2L]]
      ), call. = FALSE)
    }
    if (length(found) == 1L) {
      return(structure(found, lang = step[[2L]]))
    }
  }
  if (identical(lang, "en")) {
    stop(sprintf("Report text %s has no row in English.", text_id), call. = FALSE)
  }
  stop(sprintf(
    "Report text %s has no row in language %s or in English.", text_id, lang
  ), call. = FALSE)
}

#' The text_ids with a row in a language, in the engine's table or the caller's
#' @noRd
text_ids <- function(lang, text = NULL) {
  engine <- report_texts()
  unique(c(engine$text_id[engine$lang == lang], text[["text_id"]][text[["lang"]] == lang]))
}

#' A caller's text table checked and copied (D14.8, D14.13, D14.20)
#'
#' NULL stays NULL. Exactly the columns text_id, lang and text, each holding one value per
#' row, as UTF-8 text; every cell filled; each lang a language code of two or three
#' lower-case letters, as the lang setting takes; one row per text_id and lang. Anything else
#' is a caller error.
#' @noRd
validate_text_table <- function(text) {
  if (is.null(text)) {
    return(NULL)
  }
  columns <- c("text_id", "lang", "text")
  if (!is.data.frame(text) || !setequal(names(text), columns) || ncol(text) != 3L) {
    stop("`text` must be a table with exactly the columns text_id, lang and text.", call. = FALSE)
  }
  # A list column, or a matrix one, would be written as text over more or fewer rows than
  # the table has.
  for (column in columns) {
    value <- text[[column]]
    if (is.list(value) || !is.null(dim(value)) || length(value) != nrow(text)) {
      stop(sprintf("Column `%s` of `text` must hold one value per row.", column), call. = FALSE)
    }
  }
  table <- as.data.table(lapply(columns, function(column) as_text(text[[column]])))
  setnames(table, columns)
  # The first ten of a list of rows or values for a message, then how many more.
  shown <- function(x, n = 10L) {
    if (length(x) <= n) {
      return(paste(x, collapse = ", "))
    }
    paste0(paste(x[seq_len(n)], collapse = ", "), " and ", length(x) - n, " more")
  }
  blank <- which(rowSums(is.na(table)) > 0L)
  if (length(blank) > 0L) {
    stop(sprintf(
      "`text` has blank cells in rows %s; every text_id, lang and text must be filled.",
      shown(blank)
    ), call. = FALSE)
  }
  for (column in columns) {
    invalid <- which(!validUTF8(table[[column]]))
    if (length(invalid) > 0L) {
      stop(sprintf(
        "Column `%s` of `text` holds text that isn't valid UTF-8, in rows %s.",
        column, shown(invalid)
      ), call. = FALSE)
    }
  }
  not_code <- which(!grepl(lang_pattern, table$lang))
  if (length(not_code) > 0L) {
    stop(sprintf(
      paste(
        "`text` has lang values that aren't a language code of two or three lower-case",
        "letters, in rows %s."
      ),
      shown(not_code)
    ), call. = FALSE)
  }
  twice <- duplicated(table, by = c("text_id", "lang"))
  if (any(twice)) {
    id <- table$text_id[twice]
    code <- table$lang[twice]
    pairs <- unique(paste0(id, " (", code, ")"))
    if (length(pairs) == 1L) {
      stop(sprintf(
        "`text` has more than one row for text_id %s in language %s.", id[1L], code[1L]
      ), call. = FALSE)
    }
    stop(sprintf(
      "`text` has more than one row for each of these text_id (language) pairs: %s.",
      shown(pairs)
    ), call. = FALSE)
  }
  table
}

#' A template's {placeholders} filled from named values
#'
#' Each value is written as text (numbers in full, D12.45); a slot of quoted_slots is
#' quoted, a slot of blank_slots shows a blank as the blank text, and any other slot stops
#' on NA. Every value the template uses has length 1 or one common length. `text_id`, when
#' given, is named in each stop with `row_lang`, the language of the row the template came
#' from (the run's `lang` unless a fallback row gave it), so the caller can find the row.
#' @noRd
fill_placeholders <- function(template, values, lang = "en", text_id = NULL, row_lang = lang) {
  pieces <- regmatches(template, gregexpr("\\{[a-z0-9_]+\\}", template), invert = NA)[[1L]]
  if (length(pieces) < 2L) {
    return(template)
  }
  # The words naming the text in a stop.
  which_text <- function() {
    if (is.null(text_id)) {
      return("Report text")
    }
    sprintf("Report text %s (language %s)", text_id, row_lang)
  }
  slots <- seq(2L, length(pieces), by = 2L)
  wanted <- substr(pieces[slots], 2L, nchar(pieces[slots]) - 1L)
  absent <- setdiff(wanted, names(values))
  if (length(absent) > 0L) {
    stop(sprintf(
      "%s needs a value for %s.", which_text(), paste(absent, collapse = ", ")
    ), call. = FALSE)
  }
  used <- unique(wanted)
  sizes <- lengths(values[used])
  if (length(unique(sizes[sizes != 1L])) > 1L) {
    stop(sprintf(
      "%s values must have length 1 or one common length; got %s.",
      which_text(), paste0(used, " ", sizes, collapse = ", ")
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
      stop(sprintf("%s slot {%s} has an NA value.", which_text(), name), call. = FALSE)
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
