# String helpers (plan 3.3, 3.6; D12.14, D12.27, D12.36).

#' Blank cells as NA
#'
#' A cell that is empty after trimming is blank, so NA (D12.14, D12.27); every other cell
#' is returned exactly as written (D12.9). The test runs on bytes, so a string that isn't
#' valid UTF-8 yet, before fix_invalid_utf8(), can't make it fail as trimws() would.
#' @noRd
blank_to_na <- function(x) {
  x[!is.na(x) & grepl("^[ \t\r\n]*$", x, useBytes = TRUE)] <- NA_character_
  x
}

#' Any vector as UTF-8 text, blanks as NA
#' @noRd
as_text <- function(x) {
  blank_to_na(enc2utf8(as.character(x)))
}
