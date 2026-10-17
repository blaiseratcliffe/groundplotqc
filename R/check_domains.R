# Layer 3, value domains (plan 6.1; D8.24, D9.26, D18.2, D18.9, D18.10, D18.19, D18.20, D18.21):
# a value against its code list, the data dictionary's range and format columns, its
# contributor's site-ID band, and a text column that holds a few values without a code list.
# Each rule reads the values as given. NA, a blank, a sentinel of the attribute's type and an
# unknown code of the attribute are Layer 2's to judge, so no Layer 3 rule reports one (D18.9,
# R12); a value that failed Layers 1 or 2 is masked by the run (6.2). The engine fixes no
# bound, pattern or code: they come from the specification and the settings (CLAUDE.md
# "Code").
#
# Task cuts (M5): task 4 writes the file with domain_skipped(), code_rows() and
# check_domains() holding code_not_in_list only; task 5 adds bound_rows(), decimal_places(),
# dd_rule() and the six DD-column rules; task 6 site_id_out_of_range; task 7
# undeclared_code_set. Each cut keeps the rules in this order.

#' Which values a Layer 3 rule leaves alone: NA, a blank (any Unicode space in text that isn't
#' plain ASCII, D16.42, D18.15), a sentinel of the attribute's type or an unknown code of the
#' attribute (D18.9, R12)
#'
#' `x` holds the values as given, `row` is the attribute's row of the input's columns table,
#' and `number`, when given, the values read as numbers: a sentinel or unknown code is then
#' matched as a number too, so "-1.0" read from text is the numeric family's -1 (D18.9).
#' Returns one logical per value.
#' @noRd
domain_skipped <- function(x, row, context, number = NULL) {
  sentinels <- context$spec$sentinels
  unknown <- context$unknown_codes
  codes <- c(
    sentinels$value[sentinels$data_type == row$family],
    unname(unknown[names(unknown) == row$attribute_name])
  )
  text <- value_text(x)
  skipped <- is.na(text) | text %chin% codes
  if (is.character(text)) {
    # Only the values that may be blank are read by space_padding() (16.2).
    maybe <- which(!skipped & grepl("^[ \t\r\n]*$|[^\001-\177]", text, useBytes = TRUE))
    skipped[maybe] <- space_padding(text[maybe])$blank
  }
  if (!is.null(number)) {
    numbers <- suppressWarnings(as.numeric(codes))
    skipped <- skipped | number %in% numbers[!is.na(numbers)]
  }
  skipped
}

#' The rows of a column holding a code of its list (R7): compared as numbers where the column
#' is stored as plain numbers, a code that isn't a number then matching nothing, and as text
#' otherwise, a column of another class (a factor, a date, integer64) by its values' text
#' @noRd
code_rows <- function(x, codes) {
  if (is.numeric(x) && !is.object(x)) {
    numbers <- suppressWarnings(as.numeric(codes))
    return(which(x %in% numbers[!is.na(numbers)]))
  }
  text <- if (is.character(x)) x else value_text(x)
  which(text %chin% codes)
}

#' Layer 3's outcomes, by rule
#'
#' `code_not_in_list` per coded attribute, keys included: a value its resolved list (the
#' `codes` component, translation tables included, D7.28) doesn't hold, compared as
#' code_rows() compares (R7).
#' @noRd
check_domains <- function(input, context) {
  columns <- input$columns
  list(
    code_not_in_list = rule_outcomes(
      "code_not_in_list", columns[columns$coded], input, function(table, attribute, x, row) {
        codes <- context$codes[[paste(table, attribute, sep = "\r")]]
        listed <- logical(length(x))
        listed[code_rows(x, codes)] <- TRUE
        fail <- which(!listed)
        number <- if (row$family == "numeric") coerce_column(x[fail], "double")$value
        fail <- fail[!domain_skipped(x[fail], row, context, number)]
        map <- context$spec$code_list_map
        source <- map$source_name[map$table_name == table & map$attribute_name == attribute]
        record_outcome(
          "code_not_in_list", table, attribute, fail, value_text(x[fail]), source[1L]
        )
      }
    )
  )
}
