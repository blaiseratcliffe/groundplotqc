# Small helpers for reading text into data.tables (plan 3.6, 16.2; D12.14, D12.24).
# Every CSV the package reads goes through read_csv_text(): fread() keeps a quoted
# field's doubled quotes, so they are undone here, which is exact for RFC 4180 files.

#' Invalid UTF-8 in a data.table kept as <xx>
#'
#' Rewrites each invalid character cell and column name by reference, its bad bytes
#' written as `<xx>`, and returns what it changed: `row` (0 for a column name), `column`
#' (an index) and `value`, in reading order (D12.24, D12.28).
#' @noRd
fix_invalid_utf8 <- function(dt) {
  found <- list()
  # Column names are checked too, as row 0, the header (D12.28).
  header <- names(dt)
  bad_names <- which(!validUTF8(header))
  if (length(bad_names) > 0L) {
    fixed_names <- iconv(header[bad_names], "UTF-8", "UTF-8", sub = "byte")
    setnames(dt, bad_names, fixed_names)
    found[[1L]] <- data.table(row = 0L, column = bad_names, value = fixed_names)
  }
  for (j in seq_along(dt)) {
    x <- dt[[j]]
    if (!is.character(x)) {
      next
    }
    bad <- which(!is.na(x) & !validUTF8(x))
    if (length(bad) == 0L) {
      next
    }
    fixed <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
    set(dt, i = bad, j = j, value = fixed)
    found[[length(found) + 1L]] <- data.table(row = bad, column = j, value = fixed)
  }
  if (length(found) == 0L) {
    return(data.table(row = integer(), column = integer(), value = character()))
  }
  # Reading order, row then column, whatever the input form (D12.28).
  out <- rbindlist(found)
  setorderv(out, c("row", "column"))
  out
}

#' A CSV file read as text, exactly as written
#'
#' Every column character, blanks NA, spaces kept, doubled quotes undone and invalid bytes
#' kept as `<xx>`; returns `list(data, invalid)` (D12.9, D12.14, D12.24, D12.27).
#' @noRd
read_csv_text <- function(path) {
  data <- fread(
    path,
    colClasses = "character", na.strings = "", encoding = "UTF-8",
    strip.white = FALSE, showProgress = FALSE
  )
  invalid <- fix_invalid_utf8(data)
  for (j in seq_along(data)) {
    set(data, j = j, value = blank_to_na(data[[j]]))
    x <- data[[j]]
    hit <- which(!is.na(x) & grepl("\"\"", x, fixed = TRUE))
    if (length(hit) > 0L) {
      set(data, i = hit, j = j, value = gsub("\"\"", "\"", x[hit], fixed = TRUE))
    }
  }
  list(data = data, invalid = invalid)
}
