# Building a run's provider reports (plan 11.9; D20.8): each report unit's file name, encoded
# from its value (unit_file_name()). M7a's tasks 7 and 11 add route_unit() and
# gpq_build_reports() to this file.

#' The stems of the files the package itself writes in a run's `reports/` and `tables/`
#' folders (D20.70, D20.73)
#'
#' Pre-flight's page (`reports/preflight.html`), M8a's national summary
#' (`reports/national_summary.html`), the one report of a run with no group columns
#' (`reports/report.html`, `tables/report_issues.csv`) and the run's own tables
#' (`tables/results.csv`, `tables/issues.csv`, `tables/corrections.csv`). unit_file_name()
#' encodes the first character of a unit that is one of them, in any case, so no provider's
#' file can take the name of one of them; a test holds every file a run and its build write
#' in those folders to a unit's file or one of these stems. A file added later adds its stem here.
#' @noRd
package_file_stems <- c(
  "preflight", "national_summary", "report", "results", "issues", "corrections"
)

#' Each report unit's file stem, for `reports/<stem>.html` and `tables/<stem>_issues.csv`
#' (D20.8, D20.37, D20.70)
#'
#' ASCII letters, digits, "-", "_" and "." are kept; every other byte of the unit's UTF-8 is
#' written as "%" and its two hex digits in upper case, "%" itself as %25, so a stem decodes
#' back to one value. A leading or trailing "." is written so too, and so is the first
#' character of a unit whose part before its first "." is a Windows reserved name (CON, PRN,
#' AUX, NUL, COM1 to COM9, LPT1 to LPT9, in any case: "CON" gives "%43ON"), or that is one of
#' the package's own file stems (package_file_stems), in any case: "Issues" gives "%49ssues".
#' Every stem is ASCII. The caller gives every unit at once, before any file is written: the
#' call stops when a stem is longer than 200 bytes, or when two stems are equal ignoring ASCII
#' case, as Windows and macOS read file names (D20.8). Returns the stems in `units`' order.
#' @noRd
unit_file_name <- function(units) {
  # Letter case is folded in ASCII only, so no other letter can read as a reserved name.
  folded <- chartr(paste(LETTERS, collapse = ""), paste(letters, collapse = ""), units)
  dot <- regexpr(".", folded, fixed = TRUE)
  base <- fifelse(dot > 0L, substr(folded, 1L, dot - 1L), folded)
  reserved <- c("con", "prn", "aux", "nul", paste0("com", 1:9), paste0("lpt", 1:9))
  # The package's own files (11.9), which no unit's file may replace (D20.70).
  first <- startsWith(units, ".") | base %chin% reserved | folded %chin% package_file_stems
  last <- endsWith(units, ".")
  # Units are few, one per provider, so each is encoded from its own bytes: 48 to 57 are the
  # digits, 65 to 90 and 97 to 122 the ASCII letters, 45 "-", 46 "." and 95 "_".
  stems <- vapply(seq_along(units), function(k) {
    bytes <- as.integer(charToRaw(enc2utf8(units[[k]])))
    n <- length(bytes)
    kept <- (bytes >= 48L & bytes <= 57L) | (bytes >= 65L & bytes <= 90L) |
      (bytes >= 97L & bytes <= 122L) | bytes %in% c(45L, 46L, 95L)
    if (n > 0L) {
      kept[[1L]] <- kept[[1L]] && !first[[k]]
      kept[[n]] <- kept[[n]] && !last[[k]]
    }
    chars <- character(n)
    chars[kept] <- intToUtf8(bytes[kept], multiple = TRUE)
    chars[!kept] <- sprintf("%%%02X", bytes[!kept])
    paste(chars, collapse = "")
  }, character(1))
  size <- nchar(stems, type = "bytes")
  long <- which(size > 200L)
  if (length(long) > 0L) {
    stop(sprintf(
      paste0(
        "Report unit %s gives a file name of %d bytes once encoded, over the limit of 200; ",
        "use a shorter unit value."
      ),
      units[[long[[1L]]]], size[[long[[1L]]]]
    ), call. = FALSE)
  }
  # Folded in ASCII as the reserved names are, whatever the session's locale: on Linux, in a
  # Turkish one, tolower() gives "I" a dotless i, so "I" and "i" wouldn't stop. The first pair
  # is named.
  lower <- chartr(paste(LETTERS, collapse = ""), paste(letters, collapse = ""), stems)
  again <- which(duplicated(lower))
  if (length(again) > 0L) {
    seen <- match(lower[[again[[1L]]]], lower)
    stop(sprintf(
      paste0(
        "Report units %s and %s give the same file name, %s, on a system that ignores ",
        "letter case; rename one of them."
      ),
      units[[seen]], units[[again[[1L]]]], paste0(stems[[seen]], ".html")
    ), call. = FALSE)
  }
  stems
}
