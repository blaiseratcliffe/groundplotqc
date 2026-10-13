# Layer 2, missing-value codes (plan 6.1, 3.5; D1.8a, D7.3, D7.4, D7.9, D8.8, D8.11, D9.18,
# D16.11): how a missing value is written. Each rule reads the values as given, since it
# checks their spelling (a coerced copy has lost it). PK and FK columns are left to Layer 4
# by raw_na, blank_string and sentinel_rogue_code (D7.9). Without a matrix both sentinels
# pass (D7.3). The engine fixes no code: the sentinels, rogue codes and unknown codes are
# inputs (CLAUDE.md "Code").

#' The values of `x` among `codes`, compared as numbers when `x` is stored as numbers (a code
#' that isn't a number then matches nothing) and as text otherwise; returns their rows
#'
#' Text marked as bytes has no encoding to compare it in, and %chin% stops on it: it is
#' compared as NA, which no code is, so no rule reads it (D16.36; a run refuses such text
#' first, D16.38, so for it this is a second line). Other text that isn't valid UTF-8 is
#' compared as it is, and matches no code.
#' @noRd
rows_holding <- function(x, codes) {
  if (is.numeric(x) && !is.object(x)) {
    numbers <- suppressWarnings(as.numeric(codes))
    return(which(x %in% numbers[!is.na(numbers)]))
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
#' (no_sentinels) where its type has none. `sentinel_rogue_code` per attribute outside the
#' keys: a rogue code that isn't a code of the column, the text "NA" in an ID column left to
#' `na_text_in_id`. `species_unknown_code` per attribute `unknown_codes` names, one not_run
#' (no_input) without any. `whitespace_padding` per coded or ID attribute, on text that
#' isn't blank. `na_text_in_id` per ID attribute (a key, or marked by the spec's ID
#' pattern): the text "NA".
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
        fail <- if (is.null(text)) integer() else which(is_blank(text))
        record_outcome(
          "blank_string", table, attribute, fail, text[fail], joined(own(row)$value)
        )
      }
    ),
    sentinel_wrong_type = rule_outcomes(
      "sentinel_wrong_type", columns, input, function(table, attribute, x, row) {
        right <- own(row)
        if (nrow(right) == 0L) {
          return(not_run_outcome("sentinel_wrong_type", table, attribute, "no_sentinels"))
        }
        wrong <- sentinels[sentinels$data_type != row$family & !sentinels$value %chin% right$value]
        fail <- rows_holding(x, wrong$value)
        text <- value_text(x[fail])
        number <- suppressWarnings(as.numeric(wrong$value))
        found <- if (is.numeric(x) && !is.object(x)) {
          match(x[fail], number)
        } else {
          match(text, wrong$value)
        }
        expected <- right$value[match(wrong$role[found], right$role)]
        record_outcome("sentinel_wrong_type", table, attribute, fail, text, expected)
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
          fail <- rows_holding(x, unname(unknown[names(unknown) == attribute]))
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
        fail <- which(
          !is.na(text) & !is_blank(text) & grepl("^[ \t\r\n]|[ \t\r\n]$", text, useBytes = TRUE)
        )
        # The fix is the value trimmed. Text marked latin1, or native in a locale that isn't
        # UTF-8, is converted to UTF-8 first, only the values that aren't valid UTF-8 as they
        # are; what is valid is trimmed as characters, so its encoding is kept. Text marked as
        # bytes, or that isn't valid UTF-8 even so, is trimmed byte by byte, since trimws()
        # stops on it, and held without the mark, as the failing value is (D16.33, D16.36).
        trimmed <- text[fail]
        bytes <- Encoding(trimmed) == "bytes"
        valid <- !bytes & validUTF8(trimmed)
        convert <- which(!valid & !bytes)
        trimmed[convert] <- enc2utf8(trimmed[convert])
        valid[convert] <- validUTF8(trimmed[convert])
        trimmed[valid] <- trimws(trimmed[valid], whitespace = "[ \t\r\n]")
        trimmed[!valid] <- gsub("^[ \t\r\n]+|[ \t\r\n]+$", "", trimmed[!valid], useBytes = TRUE)
        marked <- which(bytes)
        if (length(marked) > 0L) {
          unmarked <- trimmed[marked]
          Encoding(unmarked) <- "unknown"
          trimmed[marked] <- unmarked
        }
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
