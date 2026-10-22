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
#' matched as a number too, so "-1.0" read from text is the numeric family's -1 (D18.9); in
#' `x` stored as integer64 as rows_holding() matches it (D18.117, D18.119): exactly where bit64
#' reads the code's text, as the code's nearest double where it doesn't (R156, m129), and not at
#' all in integer64's top band, from 2^63 - 512 up or its negative down (m128).
#' With `number` given, `x` stored as plain numbers isn't written as text (16.2). Returns one
#' logical per value.
#' @noRd
domain_skipped <- function(x, row, context, number = NULL) {
  sentinels <- context$spec$sentinels
  unknown <- context$unknown_codes
  codes <- c(
    sentinels$value[sentinels$data_type == row$family],
    unname(unknown[names(unknown) == row$attribute_name])
  )
  if (!is.null(number) && is.numeric(x) && !is.object(x)) {
    # Plain numbers aren't all written as text (16.2): NA is NA, and only NaN and the values
    # whose 15-digit text may be a code are written, each distinct value once (m34). A value
    # written as a number's code lies within 1e-14 of it, relative, so the wider margin here
    # only narrows what is written; the text decides, as for every other value. The margin
    # rests on value_text() writing 15 significant digits: a change to them revisits it (R84).
    numbers <- suppressWarnings(as.numeric(codes))
    near <- is.nan(x)
    for (code in numbers[!is.na(numbers)]) {
      margin <- if (is.finite(code)) abs(code) * 1e-12 else 0
      near <- near | x == code | abs(x - code) <= margin
    }
    skipped <- is.na(x) & !is.nan(x)
    maybe <- which(near)
    held <- x[maybe]
    distinct <- unique(held)
    skipped[maybe] <- (value_text(distinct) %chin% codes)[match(held, distinct)]
  } else {
    text <- value_text(x)
    skipped <- is.na(text) | text %chin% codes
    if (is.character(text)) {
      # Only the values that may be blank are read by space_padding() (16.2).
      maybe <- which(!skipped & grepl("^[ \t\r\n]*$|[^\001-\177]", text, useBytes = TRUE))
      skipped[maybe] <- space_padding(text[maybe])$blank
    }
  }
  if (!is.null(number)) {
    if (inherits(x, "integer64")) {
      # integer64's codes are found as rows_holding() finds them, not through the nearest
      # doubles `number` holds (D18.112, D18.117, D18.119): exact where bit64 reads a code's
      # text, its nearest double where it doesn't (R156, m129), none in the top band (m128).
      skipped[rows_holding(x, codes)] <- TRUE
    } else {
      numbers <- suppressWarnings(as.numeric(codes))
      skipped <- skipped | number %in% numbers[!is.na(numbers)]
    }
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
#' a value, writes one not_run row (no_dd_values, D18.19). `site_id_out_of_range` on the
#' column `id_band_columns` names: the ID, as a number, within the band whose key its chain
#' reaches, bounds inclusive (D18.2); a row whose chain breaks or ends in a key no band has,
#' or whose ID is NA, blank or a sentinel, isn't checked (D18.21); without the column one
#' not_run row (no_input). `undeclared_code_set` per text attribute that isn't coded, a key
#' or marked as an ID, and that the `code_set_skip_*` settings don't name: one column-level
#' warning where the column holds more than `code_set_max_values` values (not NA, blank, a
#' sentinel of its type or an unknown code of the attribute) and no more than that many
#' distinct ones, each distinct value read once (D8.24, D9.26, D18.20, R11); its issue lists
#' them all in C order and its results row's examples the first `example_cap` (D18.103).
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
  bands <- context$spec$id_band_columns
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
    site_id_out_of_range = if (nrow(bands) == 0L) {
      list(not_run_outcome("site_id_out_of_range", "*", "*", "no_input"))
    } else {
      banded <- columns$table_name == bands$table_name[[1L]] &
        columns$attribute_name == bands$attribute_name[[1L]]
      rule_outcomes(
        "site_id_out_of_range", columns[banded], input, function(table, attribute, x, row) {
          spec <- context$spec
          chain <- parse_chain(bands$key_attribute[[1L]], table, spec$keys, spec$attributes)
          # Each distinct key reached is written as text once (16.2).
          reached <- chain_values(input, table, chain)
          distinct <- unique(reached)
          band_list <- spec$id_bands
          band <- match(value_text(distinct), band_list$key_value, incomparables = NA)[
            match(reached, distinct)
          ]
          number <- coerce_column(x, "double")$value
          # A row whose chain breaks, ends in NA, a blank or a sentinel (which no band's key
          # is) or a key no band has, or whose ID is NA, blank or a sentinel, isn't compared
          # (D18.2, D18.21).
          unchecked <- is.na(band) | is.na(number) | domain_skipped(x, row, context, number)
          start <- band_list$band_start[band]
          end <- band_list$band_end[band]
          fail <- which(!unchecked & (number < start | number > end))
          record_outcome(
            "site_id_out_of_range", table, attribute, fail, value_text(x[fail]),
            paste(as_text(start[fail]), "to", as_text(end[fail]), recycle0 = TRUE),
            unchecked = which(unchecked)
          )
        }
      )
    },
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
    },
    undeclared_code_set = {
      skip_pattern <- context$code_set_skip_pattern
      skipped_name <- columns$attribute_name %chin% context$code_set_skip_attributes
      if (length(skip_pattern) == 1L) {
        skipped_name <- skipped_name | grepl(skip_pattern, columns$attribute_name)
      }
      judged <- columns$family == "character" & !columns$coded & is.na(columns$key_type) &
        !columns$id_marked & !skipped_name
      most <- context$code_set_max_values
      outcomes <- rule_outcomes(
        "undeclared_code_set", columns[judged], input, function(table, attribute, x, row) {
          text <- as_written(x, row)
          # Each distinct value is read once (16.2): domain_skipped() judges value by value.
          distinct <- unique(text)
          kept <- distinct[!domain_skipped(distinct, row, context)]
          # More distinct values than `most` means more values too: the column passes.
          if (length(kept) > most) {
            return(unit_outcome("undeclared_code_set", table, attribute, FALSE, length(x)))
          }
          # A column of `most` values or fewer isn't judged: any small column would look like
          # a code set (D18.20).
          if (sum(text %in% kept) <= most) {
            return(NULL)
          }
          listed <- sort(kept, method = "radix")
          # The issue lists every value; the results row shows the first example_cap of them, NA
          # at 0, as a record rule's examples do (D18.103).
          shown <- listed[seq_len(min(length(listed), context$example_cap))]
          unit_outcome(
            "undeclared_code_set", table, attribute, TRUE, length(x),
            value = paste(listed, collapse = "; "),
            examples = if (length(shown) > 0L) paste(shown, collapse = "; ") else NA_character_
          )
        }
      )
      Filter(Negate(is.null), outcomes)
    }
  )
}
