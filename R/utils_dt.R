# Small helpers for reading text into data.tables (plan 3.6, 16.2; D12.14, D12.24,
# D12.45, D12.54). Every CSV the package reads goes through read_csv_text(): fread()
# keeps a quoted field's doubled quotes, so they are undone here, which is exact for
# RFC 4180 files.

#' Invalid UTF-8 in a data.table kept as <xx>
#'
#' Rewrites each invalid character cell and column name of `data` by reference, so the
#' table passed in is changed, its bad bytes written as `<xx>`; returns what it changed:
#' `row` (0 for a column name), `column` (an index) and `value`, in reading order (D12.24,
#' D12.28, D12.45).
#' @noRd
fix_invalid_utf8 <- function(data) {
  found <- list()
  # Column names are checked too, as row 0, the header (D12.28).
  header <- names(data)
  bad_names <- which(!validUTF8(header))
  if (length(bad_names) > 0L) {
    fixed_names <- iconv(header[bad_names], "UTF-8", "UTF-8", sub = "byte")
    setnames(data, bad_names, fixed_names)
    found[[1L]] <- data.table(row = 0L, column = bad_names, value = fixed_names)
  }
  for (j in seq_along(data)) {
    x <- data[[j]]
    if (!is.character(x)) {
      next
    }
    bad <- which(!is.na(x) & !validUTF8(x))
    if (length(bad) == 0L) {
      next
    }
    fixed <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
    set(data, i = bad, j = j, value = fixed)
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
#' Read as comma-separated text with a header, fread()'s warnings on a malformed file
#' collected rather than shown. Every column character, blanks NA, spaces kept, doubled
#' quotes undone in cells and names, names made unique, invalid bytes kept as `<xx>`.
#' Returns `list(data, invalid, malformed, lines)`: `invalid` what fix_invalid_utf8()
#' changed, each value as kept; `malformed` one row per problem fread() met, `kind`
#' ("fields", "empty" or "quote"), the file `line` where known and the header's `fields`;
#' `lines` the file line each data row starts on, the header being line 1. Line 1 is the
#' header even where fread() would skip it (D12.9, D12.14, D12.24, D12.27, D12.45, D12.54,
#' D12.56). In a file of one column, an unquoted comma is part of the value (D12.56 (5)).
#' @noRd
read_csv_text <- function(path) {
  warned <- character()
  read <- function(...) {
    withCallingHandlers(
      fread(
        ...,
        sep = ",", header = TRUE, colClasses = "character", na.strings = "",
        encoding = "UTF-8", strip.white = FALSE, showProgress = FALSE
      ),
      warning = function(w) {
        # Collected and muffled, never caught: fread() then finishes its read (D12.54).
        known <- "Stopped early|Discarded single-line footer|has size 0|improper quoting"
        if (grepl(known, conditionMessage(w))) {
          warned <<- c(warned, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  # The file line each row starts on: a quoted cell spanning lines moves every later row
  # down (D12.54). Counted on the cells as read, before a blank one becomes NA.
  newlines <- function(x) {
    x[is.na(x)] <- ""
    nchar(x, type = "bytes") -
      nchar(gsub("\n", "", x, fixed = TRUE, useBytes = TRUE), type = "bytes")
  }
  data <- read(file = path)
  bytes <- NULL
  header_line <- NA_integer_
  repeat {
    row_lines <- Reduce(`+`, lapply(data, newlines), rep(0L, nrow(data))) + 1L
    header_end <- if (ncol(data) > 0L) 1L + sum(newlines(names(data))) else 0L
    ends <- header_end + cumsum(row_lines)
    read_to <- if (length(ends) > 0L) ends[[length(ends)]] else header_end
    stopped <- regmatches(warned, regexec("Stopped early on line ([0-9]+)", warned))
    stopped <- as.integer(unlist(lapply(stopped, `[`, -1L)))
    footer <- any(grepl("Discarded single-line footer", warned, fixed = TRUE))
    # fread() skips lines above the header it chooses, a title, a blank line or, in a
    # one-column file, every line above one with more fields, and can't be told not to.
    # The lines it skipped are those before its last row, or before the line it stopped
    # on, beyond the header's and the rows' own. Before a discarded footer they can't be
    # told from blank lines between the rows and the footer, so none are assumed.
    if (is.null(bytes) && length(stopped) == 0L && !footer && header_end > 0L) {
      bytes <- readBin(path, "raw", file.size(path))
    }
    skipped <- if (length(stopped) > 0L) {
      stopped[[1L]] - 1L - read_to
    } else if (footer || header_end == 0L) {
      0L
    } else {
      filled <- which(!bytes %in% as.raw(c(0x0A, 0x0D)))
      sum(bytes[seq_len(max(filled))] == as.raw(0x0A)) + 1L - read_to
    }
    if (skipped <= 0L) {
      break
    }
    # Line 1 is the header (D12.56): the lines fread() skipped are read again on their
    # own, and the line it took as its header is the first that lacks line 1's fields.
    # Each pass reads fewer lines, so the loop ends.
    header_line <- skipped + 1L
    if (is.null(bytes)) {
      bytes <- readBin(path, "raw", file.size(path))
    }
    bytes <- bytes[seq_len(which(bytes == as.raw(0x0A))[[skipped]] - 1L)]
    warned <- character()
    blank_lines <- all(bytes %in% as.raw(c(0x09, 0x0A, 0x0D, 0x20)))
    data <- if (blank_lines) data.table() else read(text = rawToChar(bytes))
  }
  lines <- as.integer(ends - row_lines + 1L)
  problem <- function(kind, line = NA_integer_, fields = NA_integer_) {
    data.table(kind = kind, line = as.integer(line), fields = as.integer(fields))
  }
  short <- length(stopped) > 0L || footer
  malformed <- rbindlist(list(
    problem(character()),
    if (length(stopped) > 0L) problem("fields", stopped[[1L]], ncol(data)),
    if (footer) problem("fields", read_to + 1L, ncol(data)),
    if (!short && !is.na(header_line)) problem("fields", header_line, ncol(data)),
    if (any(grepl("has size 0", warned, fixed = TRUE))) problem("empty"),
    if (any(grepl("improper quoting", warned, fixed = TRUE))) problem("quote")
  ))
  invalid <- fix_invalid_utf8(data)
  if (ncol(data) > 0L) {
    setnames(data, make.unique(gsub("\"\"", "\"", names(data), fixed = TRUE)))
  }
  # Only the blank cells are set, each column left otherwise as it is (D12.27, D12.45).
  for (j in seq_along(data)) {
    blank <- which(is_blank(data[[j]]))
    if (length(blank) > 0L) {
      set(data, i = blank, j = j, value = NA_character_)
    }
  }
  for (j in seq_along(data)) {
    x <- data[[j]]
    hit <- which(!is.na(x) & grepl("\"\"", x, fixed = TRUE))
    if (length(hit) > 0L) {
      set(data, i = hit, j = j, value = gsub("\"\"", "\"", x[hit], fixed = TRUE))
    }
  }
  # Each finding's value as kept, after the names and cells changed above (D12.45).
  if (nrow(invalid) > 0L) {
    kept <- Map(
      function(row, column) if (row == 0L) names(data)[[column]] else data[[column]][[row]],
      invalid$row, invalid$column
    )
    set(invalid, j = "value", value = unlist(kept, use.names = FALSE))
  }
  list(data = data, invalid = invalid, malformed = malformed, lines = lines)
}
