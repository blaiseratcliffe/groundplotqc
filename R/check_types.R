# Layer 1, data types (plan 6.1, 16.2; D2.33, D8.8, D9.23, D16.8, D16.9, D16.32): each
# column's class against the type map, and the values its coercion to that class would lose.
# The storage-class rules check columns, not records (D16.8); the record rules read the
# values as given. A value Layer 2 reports (a blank outside the keys, another type's
# sentinel, a rogue code outside the keys, the text "NA" in an ID) is left to Layer 2, so it
# is reported once (D8.8). A blank in a key column is Layer 4's (D7.9), which Layer 1 leaves
# alone too.

#' Which of a column's values a later layer reports: a blank (in a key column Layer 4's,
#' `pk_missing` and `fk_unassigned`, otherwise Layer 2's), a sentinel of another type, a
#' rogue code that isn't a code of the column, or the text "NA" in an ID column (D8.8)
#'
#' `text` is the values as text; `row` the attribute's row of the input's columns table.
#' Key columns take no rogue codes: Layer 2 leaves their values to Layer 4 (D7.9). A sentinel
#' of another type is Layer 2's only where the column's own type has sentinels, since
#' `sentinel_wrong_type` has nothing to check it against otherwise (D16.32).
#' @noRd
missing_like <- function(text, row, context) {
  sentinels <- context$spec$sentinels
  own <- sentinels$value[sentinels$data_type == row$family]
  other <- if (length(own) > 0L) {
    setdiff(sentinels$value[sentinels$data_type != row$family], own)
  } else {
    character()
  }
  key <- !is.na(row$key_type)
  rogue <- if (key) {
    character()
  } else {
    setdiff(
      context$rogue_codes,
      context$codes[[paste(row$table_name, row$attribute_name, sep = "\r")]]
    )
  }
  id <- key || isTRUE(row$id_marked)
  is_blank(text) | text %chin% other | text %chin% rogue | (id & text %chin% "NA")
}

#' Layer 1's outcomes, by rule
#'
#' `class_mismatch` per attribute: integer and double storage stand in for each other,
#' which `integer_stored_as_double` reports instead. `coercion_loss` per attribute: the
#' values coerce_column() loses, Layer 2's left out. `date_unparseable` per date attribute:
#' a value that isn't a sentinel and doesn't read back as written in the type's date format.
#' `integer_stored_as_double` per numeric attribute. `value_not_finite` per numeric
#' attribute: NaN, Inf or -Inf, as stored or as text.
#' @noRd
check_types <- function(input, context) {
  columns <- input$columns
  numeric <- columns[columns$family == "numeric"]
  dates <- columns[columns$family == "date"]
  sentinels <- context$spec$sentinels
  numbers <- c("integer", "double")
  list(
    class_mismatch = rule_outcomes(
      "class_mismatch", columns, input, function(table, attribute, x, row) {
        stored <- storage_class(x)
        fits <- stored == row$r_class || (row$r_class %chin% numbers && stored %chin% numbers)
        unit_outcome(
          "class_mismatch", table, attribute,
          failed = !fits, n_records = length(x), value = stored, expected = row$r_class
        )
      }
    ),
    coercion_loss = rule_outcomes(
      "coercion_loss", columns, input, function(table, attribute, x, row) {
        # Text holds anything, so nothing is lost: the column is left as it is (16.2).
        if (row$r_class == "character") {
          return(record_outcome(
            "coercion_loss", table, attribute, integer(), character(), row$r_class
          ))
        }
        lost <- which(coerce_column(x, row$r_class, row$date_format)$lost)
        text <- value_text(x[lost])
        reported <- !missing_like(text, row, context)
        record_outcome(
          "coercion_loss", table, attribute, lost[reported], text[reported], row$r_class
        )
      }
    ),
    date_unparseable = rule_outcomes(
      "date_unparseable", dates, input, function(table, attribute, x, row) {
        text <- coerce_column(x, "character", row$date_format)$value
        own <- sentinels$value[sentinels$data_type == row$family]
        open <- which(!is.na(text))
        given <- text[open]
        # Each distinct value is read once, in UTC so that a time in the format reads back as
        # written; every row holding a value that doesn't read back fails.
        distinct <- unique(given)
        distinct <- distinct[!missing_like(distinct, row, context) & !distinct %chin% own]
        parsed <- strptime(distinct, row$date_format, tz = "UTC")
        unread <- is.na(parsed) | format(parsed, row$date_format) != distinct
        bad <- open[given %chin% distinct[unread]]
        record_outcome("date_unparseable", table, attribute, bad, text[bad], row$date_format)
      }
    ),
    integer_stored_as_double = rule_outcomes(
      "integer_stored_as_double", numeric, input, function(table, attribute, x, row) {
        stored <- storage_class(x)
        swapped <- (row$r_class == "integer" && stored == "double") ||
          (row$r_class == "double" && stored == "integer")
        unit_outcome(
          "integer_stored_as_double", table, attribute,
          failed = swapped, n_records = length(x), value = stored, expected = row$r_class
        )
      }
    ),
    value_not_finite = rule_outcomes(
      "value_not_finite", numeric, input, function(table, attribute, x, row) {
        number <- if (is.numeric(x) && !is.object(x)) {
          x
        } else if (is.character(x)) {
          suppressWarnings(as.numeric(x))
        } else {
          # Any other column is written as text once for each distinct value (16.2).
          distinct <- unique(x)
          suppressWarnings(as.numeric(value_text(distinct)))[match(x, distinct)]
        }
        bad <- which(is.nan(number) | is.infinite(number))
        record_outcome("value_not_finite", table, attribute, bad, value_text(x[bad]))
      }
    )
  )
}
