# Layer 2, missing-value codes (plan 6.1, 3.5; D1.8a, D7.3, D7.4, D7.9, D8.8, D8.11, D9.18,
# D16.11, D16.42): how a missing value is written. Each rule reads the values as given, since
# it checks their spelling (a coerced copy has lost it). PK and FK columns are left to Layer 4
# by raw_na, blank_string and sentinel_rogue_code (D7.9). Without a matrix both sentinels
# pass (D7.3). Each value is reported by the one rule that owns it. In text that isn't plain
# ASCII any Unicode space counts as a space (D16.42, m35). The engine fixes no code: the
# sentinels, rogue codes and unknown codes are inputs (CLAUDE.md "Code").

#' The values of `x` among `codes`, compared as numbers when `x` is stored as numbers, plain
#' or integer64 (a code that isn't a number then matches nothing), and as text otherwise;
#' returns their rows
#'
#' integer64, as a DBI query of a BIGINT column returns it, holds whole numbers: each value
#' is compared with each whole code its range holds by integer64's own `==`, since `%in%`
#' would write every value as text (16.2; D18.64, m55). Text marked as bytes has no encoding
#' to compare it in, and %chin% stops on it: it is compared as NA, which no code is, so it
#' matches no code (D16.36; a run refuses such text first, D16.38, so for it this is a second
#' line). Other text that isn't valid UTF-8 is compared as it is, and matches no code.
#' @noRd
rows_holding <- function(x, codes) {
  if (length(codes) == 0L) {
    return(integer())
  }
  if (is.numeric(x) && !is.object(x)) {
    numbers <- suppressWarnings(as.numeric(codes))
    return(which(x %in% numbers[!is.na(numbers)]))
  }
  if (inherits(x, "integer64")) {
    numbers <- suppressWarnings(as.numeric(codes))
    # A code beyond integer64's range would overflow, and one with a fraction equals no value.
    whole <- !is.na(numbers) & abs(numbers) < 2^63 & numbers == trunc(numbers)
    hit <- logical(length(x))
    for (number in unique(numbers[whole])) {
      hit <- hit | x == number
    }
    return(which(hit))
  }
  text <- stored_text(x)
  if (is.null(text)) {
    return(integer())
  }
  bytes <- which(Encoding(text) == "bytes")
  if (length(bytes) > 0L) {
    text[bytes] <- NA_character_
  }
  which(text %chin% codes)
}

#' Layer 2's outcomes, by rule
#'
#' `raw_na` per attribute outside the keys, as `raw_na_policy` says: "never" leaves it no
#' attribute; "when_sentinels" records not_run (no_sentinels) where the attribute's type
#' has none. `blank_string` per attribute outside the keys, on text. `sentinel_wrong_type`
#' per attribute: a sentinel of another type that isn't one of its own, not_run
#' (no_sentinels) where its type has none. In a column stored as numbers, plain or integer64
#' (D18.64, m55), the numeric family's codes match as numbers; where the column's type isn't
#' numeric, the character and
#' date families' codes match too, where the value's text is the code, so a real 0 isn't the
#' date code "00000000" (R68; D16.42, m37); an unknown code of the attribute is left to
#' `species_unknown_code` (D16.45, m51). `sentinel_rogue_code` per attribute outside the
#' keys: a rogue code that isn't a code of the column, and isn't a value another rule owns:
#' one of the column's own sentinels, in a column stored as numbers also where the value's
#' text is one, another type's sentinel that `sentinel_wrong_type` reports, or an unknown code
#' of the attribute (D7.3, D16.40, D16.41; D16.42, m36, m39); the text "NA" in an ID column
#' is left to `na_text_in_id`. `species_unknown_code` per attribute `unknown_codes` names, one
#' not_run (no_input) without any; the column's own sentinels pass (D7.3; D16.45, m51).
#' `whitespace_padding` per coded or ID attribute, on text that isn't blank. `na_text_in_id`
#' per ID attribute (a key, or marked by the spec's ID pattern): the text "NA".
#' @noRd
check_sentinels <- function(input, context) {
  columns <- input$columns
  keyed <- !is.na(columns$key_type)
  outside_keys <- columns[!keyed]
  id_columns <- columns[keyed | columns$id_marked]
  padded_scope <- columns[keyed | columns$id_marked | columns$coded]
  sentinels <- context$spec$sentinels
  own <- function(row) sentinels[sentinels$data_type == row$family]
  joined <- function(values) {
    if (length(values) == 0L) NA_character_ else paste(values, collapse = "; ")
  }
  policy <- context$raw_na_policy
  unknown <- context$unknown_codes
  unknown_of <- function(attribute) unname(unknown[names(unknown) == attribute])
  # Whether a column is stored as numbers: plain, or integer64, which a DBI query of a BIGINT
  # column returns (D18.64, m55).
  stored_numbers <- function(x) is.numeric(x) && (!is.object(x) || inherits(x, "integer64"))
  # The rows of `x` holding one of the column's own sentinels, which pass (D7.3): compared as
  # rows_holding() compares them and, in a column stored as numbers whose type isn't numeric,
  # where the value's text is one, so a real 0 isn't the date code "00000000" (m39). `x` is
  # the failing rows only, so only they are written as text.
  own_rows <- function(x, row) {
    mine <- own(row)$value
    if (stored_numbers(x) && row$family != "numeric") {
      which(value_text(x) %chin% mine)
    } else {
      rows_holding(x, mine)
    }
  }
  # The rows of `x` holding another type's sentinel that isn't one of the column's own, each
  # with its sentinel's role, and the column's own sentinels; NULL where the column's type has
  # none. Stored as text, a value is compared as text. Stored as numbers, it is compared with
  # the numeric family's codes as numbers, and where the column's type isn't numeric, with
  # the other families' codes as numbers where its text is the code (R68; D16.42, m37); so is
  # integer64 (D18.64, m55). An unknown code of the attribute is left to species_unknown_code,
  # compared as that rule compares it (D16.45, m51).
  wrong_type <- function(x, row) {
    right <- own(row)
    if (nrow(right) == 0L) {
      return(NULL)
    }
    is_wrong <- sentinels$data_type != row$family & !sentinels$value %chin% right$value
    codes <- sentinels$value[is_wrong]
    roles <- sentinels$role[is_wrong]
    if (!stored_numbers(x)) {
      fail <- rows_holding(x, codes)
      role <- roles[match(value_text(x[fail]), codes)]
    } else {
      by_number <- sentinels$data_type[is_wrong] == "numeric"
      fail <- rows_holding(x, codes[by_number])
      # The failing integer64 values as doubles, each a whole code's number, which match()
      # would otherwise compare as text.
      held <- x[fail]
      if (is.object(held)) {
        held <- suppressWarnings(as.double(held))
      }
      role <- roles[by_number][match(held, suppressWarnings(as.numeric(codes[by_number])))]
      if (row$family != "numeric") {
        written <- setdiff(rows_holding(x, codes[!by_number]), fail)
        text <- value_text(x[written])
        hit <- text %chin% codes[!by_number]
        fail <- c(fail, written[hit])
        role <- c(role, roles[!by_number][match(text[hit], codes[!by_number])])
        in_order <- order(fail)
        fail <- fail[in_order]
        role <- role[in_order]
      }
    }
    unknown_rows <- rows_holding(x[fail], unknown_of(row$attribute_name))
    keep <- !seq_along(fail) %in% unknown_rows
    list(fail = fail[keep], role = role[keep], right = right)
  }
  list(
    raw_na = if (policy == "never") {
      list()
    } else {
      rule_outcomes("raw_na", outside_keys, input, function(table, attribute, x, row) {
        values <- own(row)$value
        if (policy == "when_sentinels" && length(values) == 0L) {
          return(not_run_outcome("raw_na", table, attribute, "no_sentinels"))
        }
        fail <- if (is.double(x)) which(is.na(x) & !is.nan(x)) else which(is.na(x))
        record_outcome(
          "raw_na", table, attribute, fail, rep(NA_character_, length(fail)), joined(values)
        )
      })
    },
    blank_string = rule_outcomes(
      "blank_string", outside_keys, input, function(table, attribute, x, row) {
        text <- stored_text(x)
        # One pass over the column; space_padding() reads the values that may be blank, those
        # of ASCII spaces only or that aren't plain ASCII (16.2; D16.42, m35).
        fail <- if (is.null(text)) {
          integer()
        } else {
          maybe <- which(grepl("^[ \t\r\n]*$|[^\001-\177]", text, useBytes = TRUE))
          maybe[space_padding(text[maybe])$blank]
        }
        record_outcome(
          "blank_string", table, attribute, fail, text[fail], joined(own(row)$value)
        )
      }
    ),
    sentinel_wrong_type = rule_outcomes(
      "sentinel_wrong_type", columns, input, function(table, attribute, x, row) {
        found <- wrong_type(x, row)
        if (is.null(found)) {
          return(not_run_outcome("sentinel_wrong_type", table, attribute, "no_sentinels"))
        }
        right <- found$right
        expected <- right$value[match(found$role, right$role)]
        record_outcome(
          "sentinel_wrong_type", table, attribute, found$fail, value_text(x[found$fail]),
          expected
        )
      }
    ),
    sentinel_rogue_code = rule_outcomes(
      "sentinel_rogue_code", outside_keys, input, function(table, attribute, x, row) {
        codes <- context$codes[[paste(table, attribute, sep = "\r")]]
        rogue <- setdiff(context$rogue_codes, codes)
        if (row$id_marked) {
          rogue <- setdiff(rogue, "NA")
        }
        fail <- rows_holding(x, rogue)
        # A value another rule owns is left to it, so no value is reported by two rules: the
        # column's own sentinels pass (D7.3), compared as rows_holding() compares them and, in
        # a column stored as numbers whose type isn't numeric, where the value's text is one
        # (m39); another type's are sentinel_wrong_type's where that rule reports them (none
        # where the column's type has no sentinels, m36, m37); the attribute's unknown codes
        # are species_unknown_code's (D16.40, D16.41). Among the failing rows only.
        if (length(fail) > 0L) {
          held <- x[fail]
          owned <- c(
            own_rows(held, row), wrong_type(held, row)$fail,
            rows_holding(held, unknown_of(attribute))
          )
          fail <- fail[!seq_along(fail) %in% owned]
        }
        record_outcome(
          "sentinel_rogue_code", table, attribute, fail, value_text(x[fail]),
          joined(own(row)$value)
        )
      }
    ),
    species_unknown_code = if (length(unknown) == 0L) {
      list(not_run_outcome("species_unknown_code", "*", "*", "no_input"))
    } else {
      rule_outcomes(
        "species_unknown_code", columns[columns$attribute_name %chin% names(unknown)], input,
        function(table, attribute, x, row) {
          fail <- rows_holding(x, unknown_of(attribute))
          # The column's own sentinels pass, though listed as unknown codes (D7.3; D16.45,
          # m51). Among the failing rows only.
          if (length(fail) > 0L) {
            fail <- fail[!seq_along(fail) %in% own_rows(x[fail], row)]
          }
          missing <- own(row)$value[own(row)$role == "missing"]
          record_outcome(
            "species_unknown_code", table, attribute, fail, value_text(x[fail]),
            joined(missing)
          )
        }
      )
    },
    whitespace_padding = rule_outcomes(
      "whitespace_padding", padded_scope, input, function(table, attribute, x, row) {
        text <- stored_text(x)
        if (is.null(text)) {
          return(record_outcome("whitespace_padding", table, attribute, integer(), character()))
        }
        # One pass over the column for the values that may be padded: a space, tab, CR or LF
        # at either end, or text that isn't plain ASCII, read by space_padding() (16.2;
        # D16.42, m35). The fix is the value trimmed: text read as UTF-8 is held so, and text
        # marked as bytes without the mark, as the failing value is (D16.33, D16.36, R67).
        maybe <- which(grepl("^[ \t\r\n]|[ \t\r\n]$|[^\001-\177]", text, useBytes = TRUE))
        read <- space_padding(text[maybe])
        fail <- maybe[read$padded]
        trimmed <- bytes_unmarked(read$trimmed[read$padded])
        record_outcome("whitespace_padding", table, attribute, fail, text[fail], trimmed)
      }
    ),
    na_text_in_id = rule_outcomes(
      "na_text_in_id", id_columns, input, function(table, attribute, x, row) {
        text <- stored_text(x)
        fail <- if (is.null(text)) integer() else which(text == "NA")
        record_outcome("na_text_in_id", table, attribute, fail, rep("NA", length(fail)))
      }
    )
  )
}
