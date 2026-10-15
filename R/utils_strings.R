# String helpers (plan 3.3, 3.6; D12.14, D12.27, D12.28, D12.36, D12.45, D14.20, D14.39), and
# the encoding helpers the checks share (D16.47, m63).

#' Which cells are blank: empty after trimming
#'
#' The test runs on bytes, so a string that isn't valid UTF-8 yet, before
#' fix_invalid_utf8(), can't make it fail as trimws() would (D12.27).
#' @noRd
is_blank <- function(x) {
  !is.na(x) & grepl("^[ \t\r\n]*$", x, useBytes = TRUE)
}

#' Which values are blank and which are padded, and each value trimmed, as the conformance
#' checks read spaces in values (D16.42, m21 and m35; D16.44)
#'
#' In plain ASCII text a space, tab, CR or LF counts; in text that isn't plain ASCII any
#' Unicode space does (`(*UCP)\s`). Valid UTF-8 left unmarked, as fread() leaves it, is read
#' as UTF-8 in any locale. Text marked latin1, or native in a locale that isn't UTF-8, is read
#' as UTF-8, only the values that aren't valid UTF-8 as they are converted, so the rest keep
#' their encoding (D16.36). Text marked as bytes, or that isn't valid UTF-8 even so, is read
#' byte by byte, its ASCII spaces only, since trimws() and a Unicode pattern stop on it, and
#' keeps its mark (D16.33, D16.36). Returns `blank` (empty once trimmed), `padded` (not blank,
#' with a space at either end) and `trimmed` (a padded value without those spaces, any other
#' value as read, as UTF-8 where it was read so), one per value; NA is neither, and stays NA.
#' is_blank() is unchanged for the specification reader.
#' @noRd
space_padding <- function(text) {
  bytes <- Encoding(text) == "bytes"
  # A Unicode pattern reads unmarked text in the session's encoding, byte by byte outside a
  # UTF-8 locale, where the last byte of a UTF-8 a-grave reads as a no-break space: valid
  # UTF-8 left unmarked is marked so on this copy, those values only (16.2).
  utf8 <- utf8_marked(utf8_converted(text))
  valid <- !bytes & validUTF8(utf8)
  wide <- valid & grepl("[^\001-\177]", utf8, useBytes = TRUE)
  blank <- is_blank(text)
  blank[wide] <- grepl("(*UCP)^\\s*$", utf8[wide], perl = TRUE)
  padded <- grepl("^[ \t\r\n]|[ \t\r\n]$", text, useBytes = TRUE)
  padded[wide] <- grepl("(*UCP)^\\s|\\s$", utf8[wide], perl = TRUE)
  padded <- padded & !blank
  # Only the padded values are trimmed.
  trimmed <- utf8
  ascii <- padded & valid & !wide
  trimmed[ascii] <- trimws(utf8[ascii], whitespace = "[ \t\r\n]")
  spaced <- padded & wide
  trimmed[spaced] <- gsub("(*UCP)^\\s+|\\s+$", "", utf8[spaced], perl = TRUE)
  raw <- padded & !valid
  trimmed[raw] <- gsub("^[ \t\r\n]+|[ \t\r\n]+$", "", utf8[raw], useBytes = TRUE)
  list(blank = blank, padded = padded, trimmed = trimmed)
}

#' Blank cells as NA
#'
#' A cell that is empty after trimming is blank, so NA (D12.14, D12.27); every other cell
#' is returned exactly as written (D12.9).
#' @noRd
blank_to_na <- function(x) {
  x[is_blank(x)] <- NA_character_
  x
}

#' Any vector as UTF-8 text, blanks as NA
#'
#' A double is written in full to 15 significant digits, never in scientific notation
#' (1e5 as "100000", 0.1 as "0.1"); integers, text and classed values such as dates are
#' written as `as.character()` writes them (D12.45). No names or other attributes are kept.
#' @noRd
as_text <- function(x) {
  if (is.double(x) && !is.object(x)) {
    # formatC() formats each value on its own, as format(x[i], digits = 15) would;
    # format() on the whole vector would give every value the decimals of the longest.
    text <- trimws(formatC(x, digits = 15L, format = "fg"))
    text[is.na(x) & !is.nan(x)] <- NA_character_
    return(as.vector(text))
  }
  as.vector(blank_to_na(enc2utf8(as.character(x))))
}

#' A value cut to about `width` characters around its first <xx> marker
#'
#' "..." marks where text is cut, so a long cell stays readable in a finding's detail;
#' the cell itself keeps all its text (D12.28).
#' @noRd
shorten_marked <- function(value, width = 40L) {
  n <- nchar(value)
  at <- regexpr("<[0-9a-fA-F]{2}>", value)
  at[at < 1L] <- 1L
  start <- pmax(1L, pmin(at - width %/% 2L, n - width + 1L))
  end <- pmin(n, start + width - 1L)
  fifelse(
    n > width,
    paste0(fifelse(start > 1L, "...", ""), substr(value, start, end), fifelse(end < n, "...", "")),
    value
  )
}

#' Column numbers as spreadsheet letters (1 A, 27 AA); NA for NA
#' @noRd
column_letters <- function(j) {
  j <- as.integer(j)
  out <- character(length(j))
  out[is.na(j)] <- NA_character_
  while (any(j > 0L, na.rm = TRUE)) {
    k <- !is.na(j) & j > 0L
    out[k] <- paste0(LETTERS[(j[k] - 1L) %% 26L + 1L], out[k])
    j[k] <- (j[k] - 1L) %/% 26L
  }
  out
}

#' Spreadsheet column letters as numbers, in any case
#'
#' NA for anything else, and for letters beyond the integer range, with no warning: the
#' sum is taken in double and compared with the maximum before it becomes an integer.
#' @noRd
column_numbers <- function(x) {
  x <- toupper(x)
  out <- rep(NA_integer_, length(x))
  for (i in which(grepl("^[A-Z]+$", x))) {
    digits <- match(strsplit(x[[i]], "", fixed = TRUE)[[1L]], LETTERS)
    number <- sum(digits * 26^(rev(seq_along(digits)) - 1L))
    if (number <= .Machine$integer.max) {
      out[[i]] <- as.integer(number)
    }
  }
  out
}

#' Text with each byte that isn't valid UTF-8 written as <xx>, for a caller error (D12.28)
#' @noRd
mark_invalid_utf8 <- function(x) {
  iconv(x, "UTF-8", "UTF-8", sub = "byte")
}

#' Text with each value that is valid UTF-8, isn't plain ASCII and is left unmarked, as
#' fread() leaves it, marked UTF-8; nothing converted, every other value as given (D16.36,
#' D16.47, m63)
#'
#' A Unicode pattern, paste() and the results' writer read unmarked text in the session's
#' encoding, so valid UTF-8 is marked so before they read it, in any locale. Only the values
#' that aren't plain ASCII are read (16.2).
#' @noRd
utf8_marked <- function(x) {
  wide <- which(grepl("[^\001-\177]", x, useBytes = TRUE))
  unmarked <- wide[Encoding(x[wide]) == "unknown" & validUTF8(x[wide])]
  if (length(unmarked) > 0L) {
    held <- x[unmarked]
    Encoding(held) <- "UTF-8"
    x[unmarked] <- held
  }
  x
}

#' Text with each value that isn't valid UTF-8, and isn't marked as bytes, converted with
#' enc2utf8(): text marked latin1, or native in a locale that isn't UTF-8, read as UTF-8
#' (D16.33, D16.36, m63)
#'
#' Only those values are converted (16.2); every other value is as given, so one that still
#' isn't valid UTF-8 is one no reader can read.
#' @noRd
utf8_converted <- function(x) {
  convert <- which(Encoding(x) != "bytes" & !validUTF8(x))
  if (length(convert) > 0L) {
    x[convert] <- enc2utf8(x[convert])
  }
  x
}

#' Text with the bytes mark dropped from each value marked so, its bytes kept, as text that
#' isn't valid UTF-8 is held: data.table's grouping and the results' writer stop on the mark
#' (D16.36, m63)
#' @noRd
bytes_unmarked <- function(x) {
  marked <- which(Encoding(x) == "bytes")
  if (length(marked) > 0L) {
    held <- x[marked]
    Encoding(held) <- "unknown"
    x[marked] <- held
  }
  x
}

# A language code: two or three lower-case letters, for the lang setting and the lang
# column of a caller's text table (D14.20).
lang_pattern <- "^[a-z]{2,3}$"
