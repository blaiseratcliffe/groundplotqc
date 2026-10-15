# Layer 1, data types (plan 6.1, 16.2; D2.33, D8.8, D9.23, D16.8, D16.9, D16.32 to D16.34,
# D16.36, D16.42): each column's class against the type map, and the values its coercion to
# that class would lose. The storage-class rules check columns, not records (D16.8); the
# record rules read the values as given. A value Layer 2 reports (a blank outside the keys,
# another type's sentinel, a rogue code outside the keys, an unknown code of the attribute,
# the text "NA" in an ID) is left to Layer 2, so it is reported once (D8.8); so is a padded
# value that whitespace_padding checks and whose trimmed form passes (D16.42, m25). A blank
# in a key column is Layer 4's (D7.9), which Layer 1 leaves alone too. Text marked latin1 is
# read as UTF-8 (D16.36). Text marked as bytes, text that isn't valid UTF-8 even so, and a
# date longer than its format can write are read by nothing that stops or warns on them:
# each is lost or unparseable as given (D16.33, D16.34, D16.36). A run refuses a table
# holding text marked as bytes before Layer 1 (D16.38), so for such text this is a second
# line. A table read from CSV is all text, so the storage-class rules don't run on it (D9.23).

#' Which of a column's values a later layer reports: a blank (in a key column Layer 4's,
#' `pk_missing` and `fk_unassigned`, otherwise Layer 2's), a sentinel of another type, a
#' rogue code that isn't a code of the column, an unknown code of the attribute, or the text
#' "NA" in an ID column (D8.8)
#'
#' `text` is the values as text; `row` the attribute's row of the input's columns table.
#' Key columns take no rogue codes: Layer 2 leaves their values to Layer 4 (D7.9). A sentinel
#' of another type is Layer 2's only where the column's own type has sentinels, since
#' `sentinel_wrong_type` has nothing to check it against otherwise (D16.32). Outside the keys
#' a blank is blank_string's, as space_padding() reads it, any Unicode space counting in text
#' that isn't plain ASCII (D16.42, m35); in a key a blank is as is_blank() reads it. An
#' unknown code is species_unknown_code's, compared as text (D16.42, m25). Text marked as
#' bytes is none of these, so the rule reading it reports it (D16.36).
#' @noRd
missing_like <- function(text, row, context) {
  # %chin% stops on text marked as bytes, which has no encoding to compare it in: it is
  # compared as NA, which no blank, sentinel or code is.
  plain <- Encoding(text) != "bytes"
  if (!all(plain)) {
    text[!plain] <- NA_character_
  }
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
  unknown <- context$unknown_codes
  unknown <- unname(unknown[names(unknown) == row$attribute_name])
  blank <- if (key) is_blank(text) else space_padding(text)$blank
  id <- key || isTRUE(row$id_marked)
  layer_two <- blank | text %chin% other | text %chin% rogue | text %chin% unknown
  plain & (layer_two | (id & text %chin% "NA"))
}

#' Layer 1's outcomes, by rule
#'
#' `class_mismatch` per attribute: integer and double storage stand in for each other,
#' which `integer_stored_as_double` reports instead. On a table read from CSV, every column
#' text, neither runs: not_run with reason text_input (D9.23). `coercion_loss` per attribute: the
#' values coerce_column() loses, Layer 2's left out. `date_unparseable` per date attribute:
#' a value that isn't a sentinel and doesn't read back as written in the type's date format.
#' `integer_stored_as_double` per numeric attribute. `value_not_finite` per numeric
#' attribute: NaN, Inf or -Inf, as stored or as text, the rows `coercion_loss`'s coercion
#' found, so a column is read once (D17.22). In a column whitespace_padding checks
#' (a key, an ID or a coded column, stored as text or a factor), `coercion_loss` and
#' `date_unparseable` leave it a padded value whose trimmed form they would pass; one whose
#' trimmed form still fails is theirs (D16.42, m25).
#' @noRd
check_types <- function(input, context) {
  columns <- input$columns
  numeric <- columns[columns$family == "numeric"]
  dates <- columns[columns$family == "date"]
  sentinels <- context$spec$sentinels
  numbers <- c("integer", "double")
  text_tables <- input$given$table_name[input$given$text_input]
  # Whether whitespace_padding checks the column: a key, an ID or a coded column, stored as
  # text or a factor (D16.42, m25).
  padding_checked <- function(x, row) {
    (!is.na(row$key_type) || isTRUE(row$id_marked) || isTRUE(row$coded)) &&
      (is.character(x) || is.factor(x))
  }
  # The rows of each numeric column whose number isn't finite, as coercion_loss's coercion
  # reads them, kept for value_not_finite so that a column of text is parsed once a run: row
  # numbers only, the coerced column released (16.2, D17.22).
  not_finite <- new.env(parent = emptyenv())
  list(
    class_mismatch = rule_outcomes(
      "class_mismatch", columns, input, function(table, attribute, x, row) {
        if (table %chin% text_tables) {
          return(not_run_outcome("class_mismatch", table, attribute, "text_input"))
        }
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
        coerced <- coerce_column(x, row$r_class, row$date_format)
        assign(paste(table, attribute, sep = "\r"), coerced$not_finite, envir = not_finite)
        lost <- which(coerced$lost)
        rm(coerced)
        text <- value_text(x[lost])
        reported <- !missing_like(text, row, context)
        if (padding_checked(x, row) && any(reported)) {
          # A padded value whose trimmed form would pass is whitespace_padding's (m25); read as
          # that rule reads it, a value marked as bytes keeping its mark and so still lost.
          at <- which(reported)
          read <- space_padding(text[at])
          trimmed <- read$trimmed[read$padded]
          fails <- coerce_column(trimmed, row$r_class)$lost & !missing_like(trimmed, row, context)
          reported[at[read$padded][!fails]] <- FALSE
        }
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
        # Nor is a value longer, in bytes, than the longest date the format writes in this
        # locale: it can't read back as written. Counting bytes is safe though strptime(),
        # which stops on more than 1000 characters, counts characters: every date the format
        # writes is within the bound, and a value within it has no more characters than the
        # bound counts bytes, as no character is shorter than a byte. The format is written
        # for every month with every weekday, before noon and after, on two-digit days and
        # hours, so names, AM and PM are at their longest (D16.34).
        days <- rep(seq(as.Date("2021-01-22"), by = "month", length.out = 12L), each = 7L) + 0:6
        moments <- as.POSIXct(rep(days, 2L)) + rep(c(43199, 86399), each = length(days))
        written <- format(moments, row$date_format, tz = "UTC")
        longest <- max(nchar(enc2utf8(written), type = "bytes"))
        # Which of some values, none marked as bytes, don't read back as written: each is read
        # in UTC so that a time in the format reads back as written. Text that isn't valid
        # UTF-8 is read as UTF-8 where it converts to it (marked latin1, or native in a locale
        # that isn't UTF-8); text that still isn't is no date and is not read: strptime()
        # stops on it in a UTF-8 locale (D16.33, D16.36). Nor is a value over the bound.
        unreadable <- function(values) {
          utf8 <- utf8_converted(values)
          valid <- validUTF8(utf8) & nchar(utf8, type = "bytes") <= longest
          parsed <- strptime(utf8[valid], row$date_format, tz = "UTC")
          unread <- !valid
          unread[valid] <- is.na(parsed) | format(parsed, row$date_format) != utf8[valid]
          unread
        }
        # Each distinct value is read once; every row holding a value that doesn't read back
        # fails.
        distinct <- unique(given)
        # Text marked as bytes has no encoding to read it in: it is no sentinel and no date,
        # and is never compared, since %chin% and strptime() stop on it (D16.36).
        bytes <- Encoding(distinct) == "bytes"
        read <- distinct[!bytes]
        read <- read[!missing_like(read, row, context) & !read %chin% own]
        failing <- read[unreadable(read)]
        if (padding_checked(x, row) && length(failing) > 0L) {
          # A padded value whose trimmed form would pass is whitespace_padding's (m25), read as
          # that rule reads it.
          spaced <- space_padding(failing)
          trimmed <- spaced$trimmed[spaced$padded]
          checked <- !missing_like(trimmed, row, context) & !trimmed %chin% own
          fails <- checked
          fails[checked] <- unreadable(trimmed[checked])
          failing <- failing[!seq_along(failing) %in% which(spaced$padded)[!fails]]
        }
        # Each row's value is looked up with %in%, which takes text marked as bytes, where
        # %chin% stops.
        bad <- open[given %in% c(distinct[bytes], failing)]
        record_outcome("date_unparseable", table, attribute, bad, text[bad], row$date_format)
      }
    ),
    integer_stored_as_double = rule_outcomes(
      "integer_stored_as_double", numeric, input, function(table, attribute, x, row) {
        if (table %chin% text_tables) {
          return(not_run_outcome("integer_stored_as_double", table, attribute, "text_input"))
        }
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
        # The rows coercion_loss's coercion found (D17.22); for a column it didn't coerce, the
        # column is read as coerce_column() reads it: text as it is, any other column once for
        # each distinct value (16.2).
        key <- paste(table, attribute, sep = "\r")
        bad <- if (exists(key, envir = not_finite, inherits = FALSE)) {
          get(key, envir = not_finite, inherits = FALSE)
        } else {
          coerce_column(x, "double")$not_finite
        }
        record_outcome("value_not_finite", table, attribute, bad, value_text(x[bad]))
      }
    )
  )
}
