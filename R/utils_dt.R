# Small helpers for reading text into data.tables (plan 3.6, 16.2; D12.14, D12.24,
# D12.45, D12.54, D12.58, D12.59). Every CSV the package reads goes through read_csv_text():
# fread() keeps a quoted field's doubled quotes, so they are undone here, which is exact
# for RFC 4180 files.

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
#' Read as comma-separated text with a header, every warning fread() gives collected
#' rather than shown. Every column character, blanks NA, spaces kept, doubled quotes undone
#' in cells and names, names made unique, invalid bytes kept as `<xx>`. Returns
#' `list(data, invalid, malformed, lines, blank_header)`: `invalid` what fix_invalid_utf8()
#' changed, each value as kept; `malformed` one row per problem met, `kind` ("fields",
#' "empty", "quote", "unknown" or "short"), the file `line` where known, the header's
#' `fields`, for "short" the file's `n_records` after its header and the `n_read` rows read,
#' and for "unknown" fread()'s warning as `value`, a bad byte as `<xx>`; `lines` the file
#' line each data row starts on, the header being line 1; `blank_header` TRUE for each
#' column whose header cell is blank in the file (empty, quoted empty or spaces only), which
#' fread() names V<j> where empty, so a header written V<j> isn't taken for one (D12.65).
#' Line 1 is the header even where fread() would skip it; a file with no bytes, or only
#' blank lines or spaces, is "empty" (D12.9, D12.14, D12.24, D12.27, D12.45, D12.54,
#' D12.56). fread() runs with English messages, whatever the session's language, since its
#' warnings are recognised by their text, matched as bytes because a warning can quote a
#' line that isn't valid UTF-8, and with the option
#' `warn` at 1, the caller's value put back after, so a session's `warn = 2` doesn't make
#' them errors. In a file of one column, fread()'s error on a quote it can't read is a
#' "quote" problem, no rows read, as an empty file returns; every other error stops the
#' read (D12.59). The read fails closed (D12.58): a warning it doesn't recognise is
#' "unknown", unless it is about the text's encoding and the file has invalid bytes, which
#' spec_encoding_invalid reports; fewer rows than the file's records after its header are
#' "short" where no other problem, and no header line read again, explains them; the
#' warnings it doesn't recognise before fread() stops on a quote are kept too. Records are
#' counted on the file's bytes: a newline inside a quoted field doesn't end one, a quote
#' opening a field only at its start, as RFC 4180 writes it, and a line with no byte above
#' a space, such as one of only spaces or tabs, isn't one. In a file of one column, an
#' unquoted comma is part of the value (D12.56 (5)).
#' @noRd
read_csv_text <- function(path) {
  language <- Sys.getenv("LANGUAGE", unset = NA)
  Sys.setenv(LANGUAGE = "en")
  invisible(bindtextdomain(NULL))
  on.exit({
    if (is.na(language)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = language)
    invisible(bindtextdomain(NULL))
  })
  problem <- function(kind, line = NA_integer_, fields = NA_integer_, n_records = NA_integer_,
                      n_read = NA_integer_, value = NA_character_) {
    data.table(
      kind = kind, line = as.integer(line), fields = as.integer(fields),
      n_records = as.integer(n_records), n_read = as.integer(n_read), value = as.character(value)
    )
  }
  # One "unknown" problem for each distinct warning the reader doesn't recognise, kept as
  # fread() wrote it, a bad byte as <xx> (D12.24, D12.58).
  unknown_problems <- function(texts) {
    if (length(texts) > 0L) {
      problem("unknown", value = iconv(unique(texts), "UTF-8", "UTF-8", sub = "byte"))
    }
  }
  # No rows read: an empty file, or one fread() stops on, with the warnings it doesn't
  # recognise that came before the stop (D12.58, D12.59).
  no_rows <- function(kind, unknown = character()) {
    list(
      data = data.table(), invalid = fix_invalid_utf8(data.table()),
      malformed = rbindlist(list(problem(kind), unknown_problems(unknown))), lines = integer(),
      blank_header = logical()
    )
  }
  # A file with no bytes, or only blank lines or spaces, is empty: fread() stops on it. A
  # missing file or a folder is left to fread(), whose error names it (D12.54).
  bytes <- if (file.exists(path) && !dir.exists(path)) readBin(path, "raw", file.size(path))
  blank_file <- !is.null(bytes) && !any(bytes > as.raw(0x20)) &&
    all(bytes %in% as.raw(c(0x09, 0x0A, 0x0D, 0x20)))
  if (blank_file) {
    return(no_rows("empty"))
  }
  warned <- character()
  unknown <- character()
  quote_stop <- FALSE
  read <- function(...) {
    # Under the session's warn = 2 data.table makes fread()'s warnings errors, so warn is 1
    # while it reads and the caller's value is put back after, an error included (D12.59).
    session_warn <- options(warn = 1L)
    on.exit(options(session_warn))
    tryCatch(
      withCallingHandlers(
        fread(
          ...,
          sep = ",", header = TRUE, colClasses = "character", na.strings = "",
          encoding = "UTF-8", strip.white = FALSE, showProgress = FALSE
        ),
        warning = function(w) {
          # Every warning is collected and muffled, never caught: fread() then finishes its
          # read (D12.54). One the reader doesn't recognise is kept apart, so none is shown
          # or lost (D12.58). The match runs on bytes: a warning quotes the line it
          # discarded, which may not be valid UTF-8.
          known <- "Stopped early|Discarded single-line footer|improper quoting"
          if (grepl(known, conditionMessage(w), useBytes = TRUE)) {
            warned <<- c(warned, conditionMessage(w))
          } else {
            unknown <<- c(unknown, conditionMessage(w))
          }
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        # In a file of one column fread() stops on a quote it can't read, an error where
        # other files give a warning: it is the quote problem, no rows read. Every other
        # error stops the read (D12.59).
        one_column_quote <- "Single column input contains invalid quotes"
        if (!grepl(one_column_quote, conditionMessage(e), fixed = TRUE, useBytes = TRUE)) {
          stop(e)
        }
        quote_stop <<- TRUE
        data.table()
      }
    )
  }
  # The newlines that end a record, found on the bytes apart from fread() (D12.58): one
  # inside a quoted field doesn't. A quote opens a field only at the field's start, so one
  # inside an unquoted field is text, as fread() reads it. A run of quotes changes whether
  # a field is open only if its length is odd: at a field's start it opens or closes one,
  # elsewhere it closes the field it is in.
  record_breaks <- function(bytes) {
    newline_at <- which(bytes == as.raw(0x0A))
    quote_at <- which(bytes == as.raw(0x22))
    if (length(quote_at) == 0L) {
      return(newline_at)
    }
    first <- which(c(TRUE, diff(quote_at) != 1L))
    run <- quote_at[first[diff(c(first, length(quote_at) + 1L)) %% 2L == 1L]]
    bom <- length(bytes) >= 3L && all(bytes[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))
    before <- bytes[pmax(run - 1L, 1L)]
    at_start <- run == 1L | (bom & run == 4L) | before == as.raw(0x0A) |
      before == as.raw(0x0D) | before == as.raw(0x2C)
    # A field is open after a run when the runs at a field's start since the last run
    # elsewhere, which closes any field, are odd in number.
    opened <- cumsum(at_start)
    in_field <- (opened - cummax(fifelse(at_start, 0L, opened))) %% 2L == 1L
    after <- findInterval(newline_at, run)
    newline_at[after == 0L | !in_field[pmax(after, 1L)]]
  }
  # The names of the record that starts at byte `from`, read on their own; NULL for a
  # record with no byte above a space. Its end is looked for in a window that doubles until
  # it holds one, so the rest of the file isn't copied. What the read meets, a warning or
  # a quote fread() stops on, isn't the file's (D12.58, D12.59).
  names_at <- function(bytes, from) {
    if (from > length(bytes)) {
      return(NULL)
    }
    size <- 256
    repeat {
      window <- bytes[seq.int(from, min(from + size - 1, length(bytes)))]
      breaks <- record_breaks(window)
      if (length(breaks) > 0L || from + size - 1 >= length(bytes)) {
        break
      }
      size <- size * 2
    }
    record <- window[seq_len(if (length(breaks) > 0L) breaks[[1L]] - 1L else length(window))]
    if (any(record > as.raw(0x20))) {
      met_before <- list(warned, unknown, quote_stop)
      on.exit({
        warned <<- met_before[[1L]]
        unknown <<- met_before[[2L]]
        quote_stop <<- met_before[[3L]]
      })
      names(read(text = rawToChar(record)))
    }
  }
  # The file's records after its header, a record with no byte above a space, such as one
  # of only spaces, tabs or a carriage return, not being one (D12.58).
  records_after_header <- function(bytes) {
    breaks <- record_breaks(bytes)
    content <- which(bytes > as.raw(0x20))
    record_start <- c(1L, breaks + 1L)
    record_end <- c(breaks - 1L, length(bytes))
    filled <- findInterval(record_end, content) > findInterval(record_start - 1L, content)
    sum(filled[-1L])
  }
  # The file line each row starts on: a quoted cell spanning lines moves every later row
  # down (D12.54). Counted on the cells as read, before a blank one becomes NA.
  newlines <- function(x) {
    hit <- which(grepl("\n", x, fixed = TRUE, useBytes = TRUE))
    out <- integer(length(x))
    out[hit] <- nchar(x[hit], type = "bytes") -
      nchar(gsub("\n", "", x[hit], fixed = TRUE, useBytes = TRUE), type = "bytes")
    out
  }
  data <- read(file = path)
  if (quote_stop) {
    return(no_rows("quote", unknown))
  }
  # fread() skips a GB-18030 mark at the start, with a warning, so the bytes below are the
  # text after it, as fread() reads it; the mark holds no newline, so lines count as they
  # did. Where that text has invalid bytes, a warning about the encoding is taken for
  # spec_encoding_invalid's, as below, so it doesn't stop the skip check (D12.58). A NUL is
  # valid UTF-8, and rawToChar() can't hold one.
  if (length(bytes) >= 4L && all(bytes[1:4] == as.raw(c(0x84, 0x31, 0x95, 0x33)))) {
    bytes <- bytes[-(1:4)]
  }
  invalid_bytes <- !validUTF8(rawToChar(bytes[bytes != as.raw(0x00)]))
  unknown_before <- 0L
  header_line <- NA_integer_
  repeat {
    row_lines <- Reduce(`+`, lapply(data, newlines), rep(0L, nrow(data))) + 1L
    header_end <- if (ncol(data) > 0L) 1L + sum(newlines(names(data))) else 0L
    ends <- header_end + cumsum(row_lines)
    read_to <- if (length(ends) > 0L) ends[[length(ends)]] else header_end
    stopped <- regmatches(
      warned, regexec("Stopped early on line ([0-9]+)", warned, useBytes = TRUE)
    )
    stopped <- as.integer(unlist(lapply(stopped, `[`, -1L)))
    footer <- any(grepl("Discarded single-line footer", warned, fixed = TRUE, useBytes = TRUE))
    # This read's warnings the reader doesn't recognise, less one about the encoding where
    # the text has invalid bytes.
    pass_unknown <- unknown[seq_along(unknown) > unknown_before]
    if (invalid_bytes) {
      pass_unknown <- pass_unknown[
        !grepl("encoding", pass_unknown, ignore.case = TRUE, useBytes = TRUE)
      ]
    }
    # fread() skips lines above the header it chooses, a title, a blank line or, in a
    # one-column file, every line above one with more fields, and can't be told not to.
    # The lines it skipped are those before its last row, or before the record it stopped
    # on, beyond the header's and the rows' own. Before a discarded footer they can't be
    # told from blank lines between the rows and the footer, so none are assumed; nor after
    # a warning the reader doesn't recognise, which may have cut the rows short (D12.58).
    skipped <- if (length(stopped) > 0L) {
      # "Stopped early on line N" counts records from the first line, so a quoted cell
      # over several lines counts once: the header, the rows and the line it stopped on
      # are N less the lines skipped (D12.54).
      stopped[[1L]] - 2L - nrow(data)
    } else if (footer || header_end == 0L || length(pass_unknown) > 0L) {
      0L
    } else {
      # Back over the blank bytes at the end. A last line of only spaces or tabs is ignored
      # by fread() at the end of the file, unless the file has one column, where it is a
      # row. In the lines read again it is a line like another.
      blanks <- as.raw(c(0x0A, 0x0D, if (ncol(data) > 1L && is.na(header_line)) c(0x09, 0x20)))
      last <- length(bytes)
      while (last > 0L && bytes[[last]] %in% blanks) {
        last <- last - 1L
      }
      sum(bytes[seq_len(last)] == as.raw(0x0A)) + 1L - read_to
    }
    if (skipped > 0L && is.na(header_line) && length(stopped) == 0L) {
      # Lines missing are rows fread() left out, not lines it skipped, where line 1 has
      # fread()'s names and the line it would have skipped to doesn't; the record count
      # below finds them (D12.58).
      left_out <- identical(names_at(bytes, 1L), names(data)) &&
        !identical(names_at(bytes, which(bytes == as.raw(0x0A))[[skipped]] + 1L), names(data))
      if (left_out) {
        skipped <- 0L
      }
    }
    if (skipped <= 0L) {
      break
    }
    # Line 1 is the header (D12.56): the lines fread() skipped are read again on their
    # own, and the line it took as its header is the first that lacks line 1's fields.
    # Each pass reads fewer lines, so the loop ends.
    header_line <- skipped + 1L
    bytes <- bytes[seq_len(which(bytes == as.raw(0x0A))[[skipped]] - 1L)]
    # Each read's known warnings are its own; those it doesn't recognise are all kept.
    warned <- character()
    unknown_before <- length(unknown)
    blank_lines <- all(bytes %in% as.raw(c(0x09, 0x0A, 0x0D, 0x20)))
    data <- if (blank_lines) data.table() else read(text = rawToChar(bytes))
    if (quote_stop) {
      return(no_rows("quote", unknown))
    }
  }
  lines <- as.integer(ends - row_lines + 1L)
  ended_early <- length(stopped) > 0L || footer
  # Which of line 1's cells are blank (D12.65). One of spaces only keeps its spaces as its
  # name; an empty or quoted-empty one fread() names V<j> itself, so where column j is named
  # V<j>, line 1's record is read again as a row, under a plain header of as many fields
  # (fread() can't read a header cell over two lines with no plain line to go by), a blank
  # cell NA. What that read meets isn't the file's (D12.58, D12.59); a read that doesn't
  # give one row of the file's columns leaves each V<j> a name, as written.
  blank_header <- is_blank(names(data))
  if (any(names(data) == paste0("V", seq_along(data)))) {
    breaks <- record_breaks(bytes)
    record <- bytes[seq_len(if (length(breaks) > 0L) breaks[[1L]] - 1L else length(bytes))]
    if (length(record) >= 3L && all(record[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) {
      record <- record[-(1:3)]
    }
    record <- record[record != as.raw(0x00)]
    if (length(record) > 0L && record[[length(record)]] == as.raw(0x0D)) {
      record <- record[-length(record)]
    }
    if (any(record > as.raw(0x20))) {
      met_before <- list(warned, unknown, quote_stop)
      text <- paste0(paste0("V", seq_along(data), collapse = ","), "\n", rawToChar(record))
      row <- tryCatch(read(text = text), error = function(e) NULL)
      warned <- met_before[[1L]]
      unknown <- met_before[[2L]]
      quote_stop <- met_before[[3L]]
      if (!is.null(row) && nrow(row) == 1L && ncol(row) == ncol(data)) {
        cells <- unlist(row, use.names = FALSE)
        blank_header <- is.na(cells) | is_blank(cells)
      }
    }
  }
  invalid <- fix_invalid_utf8(data)
  # A warning about the text's encoding is spec_encoding_invalid's where the file has
  # invalid bytes; any other the reader doesn't recognise is an "unknown" problem (D12.58).
  if (nrow(invalid) > 0L) {
    unknown <- unknown[!grepl("encoding", unknown, ignore.case = TRUE, useBytes = TRUE)]
  }
  malformed <- rbindlist(list(
    problem(character()),
    if (length(stopped) > 0L) problem("fields", read_to + 1L, ncol(data)),
    if (footer) problem("fields", read_to + 1L, ncol(data)),
    if (!ended_early && !is.na(header_line)) problem("fields", header_line, ncol(data)),
    if (any(grepl("improper quoting", warned, fixed = TRUE, useBytes = TRUE))) problem("quote"),
    unknown_problems(unknown)
  ))
  # Fewer rows than the file's records after its header are a problem of their own where
  # no other problem explains them; a header line read again always gives one (D12.58).
  if (nrow(malformed) == 0L) {
    n_records <- records_after_header(bytes)
    if (nrow(data) < n_records) {
      malformed <- problem("short", n_records = n_records, n_read = nrow(data))
    }
  }
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
  is_name <- which(invalid$row == 0L)
  if (length(is_name) > 0L) {
    set(invalid, i = is_name, j = "value", value = names(data)[invalid$column[is_name]])
  }
  for (j in unique(invalid$column[invalid$row > 0L])) {
    cells <- which(invalid$row > 0L & invalid$column == j)
    set(invalid, i = cells, j = "value", value = data[[j]][invalid$row[cells]])
  }
  list(
    data = data, invalid = invalid, malformed = malformed, lines = lines,
    blank_header = blank_header
  )
}
