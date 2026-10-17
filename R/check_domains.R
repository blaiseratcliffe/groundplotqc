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

#' The rows of numbers beyond one bound: below a minimum or above a maximum, the bound itself
#' failing where it isn't inclusive; NA and NaN fail neither side
#' @noRd
bound_rows <- function(number, bound, inclusive, side) {
  inclusive <- !isFALSE(inclusive)
  beyond <- if (side == "min") {
    if (inclusive) number < bound else number <= bound
  } else {
    if (inclusive) number > bound else number >= bound
  }
  which(beyond)
}

#' Each number's decimal places as written to 15 significant digits (value_text()): "1.25"
#' has 2, "3" and "Inf" none, NA is NA
#' @noRd
decimal_places <- function(number) {
  text <- value_text(number)
  point <- regexpr(".", text, fixed = TRUE)
  fifelse(point > 0L, nchar(text) - point, 0L)
}

#' Layer 3's outcomes, by rule
#'
#' `code_not_in_list` per coded attribute, keys included: a value its resolved list (the
#' `codes` component, translation tables included, D7.28) doesn't hold, compared as
#' code_rows() compares (R7). `value_below_min` and `value_above_max` per numeric attribute
#' with the dictionary's `min_value` or `max_value`, its inclusive flag read, the attributes
#' the `coordinate_attributes` setting names left to `coordinate_out_of_bounds` (D18.9).
#' `string_too_long` per attribute with a `max_length`, in characters; `pattern_mismatch` per
#' attribute with a `pattern`, read with grepl()'s default engine (R3); a date stored as a date
#' is written in its type's format first. `decimals_exceeded` per numeric attribute with
#' `decimals`, each distinct number read once. `coordinate_out_of_bounds` per attribute the
#' setting names that has a bound; without the setting one not_run row (no_input). A rule
#' whose dictionary column is absent or blank in every row, or gives no attribute in its scope
#' a value, writes one not_run row (no_dd_values, D18.19).
#' @noRd
check_domains <- function(input, context) {
  columns <- input$columns
  attributes <- context$spec$attributes
  # Each attribute's row of the dictionary's attributes, as the columns table lists them.
  at <- match(
    paste(columns$table_name, columns$attribute_name, sep = "\r"),
    paste(attributes$table_name, attributes$attribute_name, sep = "\r")
  )
  dd <- function(column) attributes[[column]][at]
  numeric_family <- columns$family == "numeric"
  coordinate <- columns$attribute_name %chin% context$coordinate_attributes
  # A rule's outcomes per attribute in `scope` (a logical over the columns table), or one
  # not_run row where the dictionary gives none of them a value (D18.19).
  dd_rule <- function(rule_id, scope, evaluate) {
    if (!any(scope)) {
      return(list(not_run_outcome(rule_id, "*", "*", "no_dd_values")))
    }
    rule_outcomes(rule_id, columns[scope], input, function(table, attribute, x, row) {
      k <- which(columns$table_name == table & columns$attribute_name == attribute)
      evaluate(table, attribute, x, row, k)
    })
  }
  # The range rules on one side, outside the coordinate attributes (D18.9).
  range_rule <- function(rule_id, side) {
    bound <- dd(if (side == "min") "min_value" else "max_value")
    inclusive <- dd(if (side == "min") "min_inclusive" else "max_inclusive")
    dd_rule(
      rule_id, numeric_family & !is.na(bound) & !coordinate,
      function(table, attribute, x, row, k) {
        number <- coerce_column(x, "double")$value
        fail <- bound_rows(number, bound[[k]], inclusive[[k]], side)
        fail <- fail[!domain_skipped(x[fail], row, context, number[fail])]
        record_outcome(rule_id, table, attribute, fail, value_text(x[fail]), as_text(bound[[k]]))
      }
    )
  }
  # A column's values as text, a date stored as a date written in its type's format (D16.9).
  as_written <- function(x, row) coerce_column(x, "character", row$date_format)$value
  max_length <- dd("max_length")
  pattern <- dd("pattern")
  decimals <- dd("decimals")
  min_value <- dd("min_value")
  max_value <- dd("max_value")
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
    ),
    value_below_min = range_rule("value_below_min", "min"),
    value_above_max = range_rule("value_above_max", "max"),
    string_too_long = dd_rule(
      "string_too_long", !numeric_family & !is.na(max_length),
      function(table, attribute, x, row, k) {
        length_of <- nchar(utf8_converted(as_written(x, row)), type = "chars", allowNA = TRUE)
        fail <- which(length_of > max_length[[k]])
        fail <- fail[!domain_skipped(x[fail], row, context)]
        record_outcome(
          "string_too_long", table, attribute, fail, value_text(x[fail]),
          as_text(max_length[[k]])
        )
      }
    ),
    pattern_mismatch = dd_rule(
      "pattern_mismatch", !numeric_family & !is.na(pattern),
      function(table, attribute, x, row, k) {
        text <- utf8_converted(as_written(x, row))
        # Text that still isn't valid UTF-8 is read by no pattern, which a UTF-8 locale's
        # grepl() can't do; Layer 2's whitespace_padding reads such values byte by byte.
        readable <- which(!is.na(text) & validUTF8(text))
        fail <- readable[!grepl(pattern[[k]], text[readable])]
        fail <- fail[!domain_skipped(x[fail], row, context)]
        record_outcome(
          "pattern_mismatch", table, attribute, fail, value_text(x[fail]), pattern[[k]]
        )
      }
    ),
    decimals_exceeded = dd_rule(
      "decimals_exceeded", numeric_family & !is.na(decimals),
      function(table, attribute, x, row, k) {
        number <- coerce_column(x, "double")$value
        # Each distinct number is written once (16.2).
        distinct <- unique(number)
        places <- decimal_places(distinct)[match(number, distinct)]
        fail <- which(places > decimals[[k]])
        fail <- fail[!domain_skipped(x[fail], row, context, number[fail])]
        record_outcome(
          "decimals_exceeded", table, attribute, fail, value_text(x[fail]),
          as_text(decimals[[k]])
        )
      }
    ),
    coordinate_out_of_bounds = if (length(context$coordinate_attributes) == 0L) {
      list(not_run_outcome("coordinate_out_of_bounds", "*", "*", "no_input"))
    } else {
      dd_rule(
        "coordinate_out_of_bounds",
        numeric_family & coordinate & (!is.na(min_value) | !is.na(max_value)),
        function(table, attribute, x, row, k) {
          number <- coerce_column(x, "double")$value
          low <- if (is.na(min_value[[k]])) {
            integer()
          } else {
            bound_rows(number, min_value[[k]], dd("min_inclusive")[[k]], "min")
          }
          high <- if (is.na(max_value[[k]])) {
            integer()
          } else {
            bound_rows(number, max_value[[k]], dd("max_inclusive")[[k]], "max")
          }
          # A value on a minimum and maximum that are equal and both excluded fails once.
          fail <- sort(unique(c(low, high)))
          kept <- !domain_skipped(x[fail], row, context, number[fail])
          expected <- as_text(fifelse(fail %in% low, min_value[[k]], max_value[[k]]))
          record_outcome(
            "coordinate_out_of_bounds", table, attribute, fail[kept], value_text(x[fail[kept]]),
            expected[kept]
          )
        }
      )
    }
  )
}
