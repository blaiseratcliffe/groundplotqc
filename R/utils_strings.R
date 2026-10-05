# String helpers (plan 3.3, 3.6; D12.14, D12.27, D12.36, D12.45).

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
