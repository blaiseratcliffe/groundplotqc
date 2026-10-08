# String helpers (plan 3.3, 3.6; D12.14, D12.27, D12.28, D12.36, D12.45, D14.20, D14.39).

#' Which cells are blank: empty after trimming
#'
#' The test runs on bytes, so a string that isn't valid UTF-8 yet, before
#' fix_invalid_utf8(), can't make it fail as trimws() would (D12.27).
#' @noRd
is_blank <- function(x) {
  !is.na(x) & grepl("^[ \t\r\n]*$", x, useBytes = TRUE)
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

# A language code: two or three lower-case letters, for the lang setting and the lang
# column of a caller's text table (D14.20).
lang_pattern <- "^[a-z]{2,3}$"
