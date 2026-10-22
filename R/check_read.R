# The tables a conformance run reads (plan 15, 9.5; D9.23, D17.1 to D17.6). `tables` is a
# named list of data.frames and paths of CSV files, mixed as the caller likes, a named
# character vector of paths, or a DBI connection, whose tables are read under the names
# `table_map` gives. Each is read into the named list of data.frames validate_tables()
# checks, with what the run's inputs metadata records of each table read from a file or a
# connection. A data.frame given is kept as it is, never copied or changed (16.2).

#' The run's tables: CSV paths and a DBI connection's tables read as data.tables
#'
#' Returns a list: `tables`, the named list validate_tables() checks next; `inputs`, one row
#' per table read from a file or a connection (`table_name`, `file`, `sha256`, `detail`, as
#' the inputs metadata takes them, 9.5); and `text_tables`, the names of the tables read from
#' CSV, every column of which is text (D9.23). `tables` that isn't a list named once by table
#' is returned as given, so validate_tables() refuses it before any file is read. Caller
#' errors, in English (D12.37).
#' @noRd
read_tables <- function(tables, spec, table_map = NULL) {
  none <- data.table(
    table_name = character(), file = character(), sha256 = character(), detail = character()
  )
  if (inherits(tables, "DBIConnection")) {
    return(read_db_tables(tables, spec, table_map))
  }
  if (!is.null(table_map)) {
    stop("`table_map` is only read when `tables` is a DBI connection.", call. = FALSE)
  }
  # A classed character vector, such as fs::path()'s, is read as its text: unclass() keeps
  # the names and never dispatches a method that could change the text (D18.12, m25).
  if (is.character(tables) && is.null(dim(tables))) {
    tables <- as.list(unclass(tables))
  }
  # The test of validate_tables(), so a list it would refuse is left to it.
  named <- is.list(tables) && !is.data.frame(tables) && length(tables) > 0L &&
    !is.null(names(tables)) && !anyNA(names(tables)) && !any(is_blank(names(tables))) &&
    !anyDuplicated(names(tables))
  if (!named) {
    return(list(tables = tables, inputs = none, text_tables = character()))
  }
  path <- vapply(tables, function(x) {
    is.character(x) && is.null(dim(x)) && length(x) == 1L && !is.na(x)
  }, logical(1))
  frame <- vapply(tables, is.data.frame, logical(1))
  if (!all(path | frame)) {
    stop(sprintf(
      "`tables` holds elements that are neither a data.frame nor the path of one CSV file: %s.",
      paste(mark_invalid_utf8(names(tables)[!(path | frame)]), collapse = ", ")
    ), call. = FALSE)
  }
  if (!any(path)) {
    return(list(tables = tables, inputs = none, text_tables = character()))
  }
  paths <- vapply(tables[path], function(x) unname(unclass(x)), "")
  absent <- !file.exists(paths) | dir.exists(paths)
  if (any(absent)) {
    stop(sprintf(
      "No file at the path given for these tables: %s.",
      paste0(
        mark_invalid_utf8(names(paths)[absent]), " (",
        mark_invalid_utf8(enc2utf8(paths[absent])), ")",
        collapse = ", "
      )
    ), call. = FALSE)
  }
  read <- lapply(names(paths), function(name) read_csv_table(paths[[name]], name))
  tables[names(paths)] <- lapply(read, `[[`, "data")
  inputs <- data.table(
    table_name = names(paths),
    file = vapply(read, `[[`, "", "file"),
    sha256 = vapply(read, `[[`, "", "sha256"),
    detail = vapply(read, `[[`, "", "detail")
  )
  list(tables = tables, inputs = inputs, text_tables = names(paths))
}

#' One CSV file read as a table: every column text, as written (D9.23, D17.2 to D17.5)
#'
#' An empty cell is NA and everything else is read as written: a quoted empty cell an empty
#' string, the text "NA" the text, spaces kept, a quoted cell's doubled quotes undone, and a
#' header name's too (D17.2, undo_doubled_quotes(), R5, R30). fread() detects the separator
#' (D17.4); the one it used is the first of fread()'s candidates that reads the same header
#' from the file's first line. A cell that isn't valid UTF-8 is converted from Windows-1252,
#' the header's names too, the five bytes Windows-1252 leaves undefined kept as given (D17.3,
#' from_windows_1252()). A file that doesn't end with a line break, which may be cut off
#' (D17.14), any error or warning of fread() (D17.5), and a header fread() took from a later
#' line than the first, in a file of more than one column (D17.16, D17.24), stop the run,
#' naming the table and the file, whatever the session's `warn`. A file cut exactly at a line
#' end can't be told from a shorter one. A file whose size or modification time changes
#' between the stamp taken before its last byte is read and the one taken after its hash stops
#' the run too (D17.32). Returns `data`, `file` (the
#' file's name without its folder, R2), `sha256` and `detail` (the rows, the separator as a
#' word and the values converted, D17.27).
#' @noRd
read_csv_table <- function(path, table) {
  # The file's size and modification time, read again after its hash: a file that changed
  # while it was read stops the run, its rows and hash not being one file's (D17.32, m28).
  stamp <- function() {
    info <- file.info(path, extra_cols = FALSE)
    c(info$size, as.numeric(info$mtime))
  }
  before <- stamp()
  # A data file that doesn't end with a line break may be cut off (D17.14): its last byte is
  # read alone. An empty file is left to fread()'s own warning.
  size <- before[[1L]]
  if (size > 0) {
    connection <- file(path, "rb")
    last <- tryCatch(
      {
        seek(connection, size - 1)
        readBin(connection, "raw", 1L)
      },
      finally = close(connection)
    )
    if (!last %in% as.raw(c(0x0a, 0x0d))) {
      stop(sprintf(
        paste0(
          "Table %s: the file %s doesn't end with a line break, so it may be cut off, or was ",
          "written without one; nothing was checked. Save it again, for example with ",
          "data.table::fwrite() or from Excel as CSV."
        ),
        mark_invalid_utf8(table), mark_invalid_utf8(basename(path))
      ), call. = FALSE)
    }
  }
  problem <- NULL
  read <- function(...) {
    fread(
      file = path, colClasses = "character", na.strings = "", strip.white = FALSE,
      encoding = "UTF-8", header = TRUE, showProgress = FALSE, ...
    )
  }
  data <- withCallingHandlers(
    tryCatch(read(), error = function(e) {
      problem <<- c(problem, conditionMessage(e))
      NULL
    }),
    warning = function(w) {
      problem <<- c(problem, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (length(problem) > 0L) {
    stop(sprintf(
      paste0(
        "Table %s: the file %s can't be read cleanly, so nothing was checked. ",
        "data.table::fread() said: %s"
      ),
      mark_invalid_utf8(table), mark_invalid_utf8(basename(path)),
      paste(mark_invalid_utf8(problem), collapse = " ")
    ), call. = FALSE)
  }
  # The separator fread() used: the first of its candidates that reads the same header from
  # the file's first line (`fill`). fread() takes its header from the first line of the
  # longest run of lines with one number of fields, so a ragged or short line near the top,
  # a header shorter than the rows or a title line moves it down without a word; then no
  # candidate reads the same header, and the run stops (D17.16). A table of one column has no
  # separator, and fread() never moves its header: a title line above it is read as the
  # column's name, which Layer 0 reports (D17.24). The separator is named by its word
  # (D17.27); fread() looks for no other than these six, and data.table 1.18 not the colon.
  separator <- NA_character_
  if (ncol(data) > 1L) {
    candidates <- c(
      comma = ",", tab = "\t", pipe = "|", semicolon = ";", colon = ":", space = " "
    )
    for (k in seq_along(candidates)) {
      header <- tryCatch(
        suppressWarnings(names(read(sep = candidates[[k]], fill = TRUE, nrows = 0L))),
        error = function(e) character()
      )
      if (identical(header, names(data))) {
        separator <- names(candidates)[[k]]
        break
      }
    }
    if (is.na(separator)) {
      stop(sprintf(
        paste0(
          "Table %s: the file %s can't be read cleanly, so nothing was checked: ",
          "data.table::fread() would take its header from a later line than the first. ",
          "Look for a title line above the header, or a line near the top with more or ",
          "fewer fields than the header."
        ),
        mark_invalid_utf8(table), mark_invalid_utf8(basename(path))
      ), call. = FALSE)
    }
  }
  converted <- 0L
  for (j in seq_along(data)) {
    cells <- from_windows_1252(data[[j]])
    if (length(cells$rows) > 0L) {
      set(data, i = cells$rows, j = j, value = cells$value)
      converted <- converted + length(cells$rows)
    }
  }
  header <- from_windows_1252(names(data))
  if (length(header$rows) > 0L) {
    setnames(data, header$rows, header$value)
  }
  # fread() keeps a quoted cell's and a quoted name's doubled quotes: undone, so each reads as
  # written.
  undo_doubled_quotes(data)
  labels <- data.table(name = names(data))
  undo_doubled_quotes(labels)
  setnames(data, labels$name)
  detail <- paste0(
    nrow(data), if (nrow(data) == 1L) " row" else " rows",
    if (!is.na(separator)) paste0("; separator ", separator),
    if (converted > 0L) {
      paste0(
        "; ", converted, if (converted == 1L) " value" else " values",
        " converted from Windows-1252"
      )
    }
  )
  sha256 <- unname(tools::sha256sum(path))
  if (!identical(stamp(), before)) {
    stop(sprintf(
      paste0(
        "Table %s: the file %s changed while it was read, so nothing was checked. Run again ",
        "once the file is complete."
      ),
      mark_invalid_utf8(table), mark_invalid_utf8(basename(path))
    ), call. = FALSE)
  }
  list(data = data, file = basename(path), sha256 = sha256, detail = detail)
}

#' The cells of `x` that aren't valid UTF-8, converted from Windows-1252 (D17.3)
#'
#' Returns `rows`, their positions, and `value`, their text as UTF-8. A cell holding one of
#' the five bytes Windows-1252 leaves undefined (0x81, 0x8D, 0x8F, 0x90, 0x9D), matched byte
#' by byte, keeps its bytes as given on every platform and isn't among them: iconv()
#' converts 0x81 on Windows and gives NA on Linux, so it can't be left to decide. Valid
#' UTF-8 and NA are never touched.
#' @noRd
from_windows_1252 <- function(x) {
  rows <- which(!validUTF8(x))
  undefined <- rawToChar(as.raw(c(0x5b, 0x81, 0x8d, 0x8f, 0x90, 0x9d, 0x5d)))
  rows <- rows[!grepl(undefined, x[rows], useBytes = TRUE)]
  list(rows = rows, value = iconv(x[rows], from = "CP1252", to = "UTF-8"))
}

#' A DBI connection's tables read under `table_map`'s names (D8.24, D9.23, D17.6)
#'
#' Each spec table is read with DBI::dbReadTable() where the database has it, under the name
#' `table_map` gives it ("schema.table" split at its first dot, db_table_id()) or, with no
#' entry, its own name as given, a dot in it kept; a spec table the database lacks isn't
#' read, so Layer 0 reports it missing, and the database's other tables are never read. The
#' connection is left open and unchanged. Each table read is recorded as "DBI connection, not
#' hashed" with its rows, the database's name for it and the driver's class, never a host,
#' user or database name (9.5, D9.23).
#' @noRd
read_db_tables <- function(connection, spec, table_map) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop(
      "A DBI connection as `tables` needs the package DBI: install.packages(\"DBI\").",
      call. = FALSE
    )
  }
  # A classed character vector is read as its text, its names kept (D18.12, m25).
  if (is.character(table_map)) {
    table_map <- unclass(table_map)
  }
  mapped <- is.null(table_map) || (
    is.character(table_map) && is.null(dim(table_map)) &&
      length(table_map) > 0L && !is.null(names(table_map)) && !anyNA(names(table_map)) &&
      !any(is_blank(names(table_map))) && !anyDuplicated(names(table_map)) &&
      !anyNA(table_map) && !any(is_blank(table_map)) &&
      !any(startsWith(table_map, ".") | endsWith(table_map, "."))
  )
  if (!mapped) {
    stop(
      paste0(
        "`table_map` must be NULL or a named character vector: each name a table of the ",
        "specification, each value that table's name in the database, \"schema.table\" allowed."
      ),
      call. = FALSE
    )
  }
  spec_tables <- unique(spec$attributes$table_name)
  stray <- setdiff(names(table_map), spec_tables)
  if (length(stray) > 0L) {
    stop(sprintf(
      "`table_map` names tables the specification doesn't have: %s.",
      paste(mark_invalid_utf8(stray), collapse = ", ")
    ), call. = FALSE)
  }
  # Only a `table_map` value is split at its first dot; a spec table with no entry is looked
  # for under its own name as given, a dot in it included (D17.6).
  at <- match(names(table_map), spec_tables)
  db_names <- spec_tables
  db_names[at] <- unname(table_map)
  ids <- as.list(spec_tables)
  ids[at] <- lapply(unname(table_map), db_table_id)
  present <- vapply(ids, function(id) isTRUE(DBI::dbExistsTable(connection, id)), logical(1))
  tables <- lapply(ids[present], function(id) setDT(DBI::dbReadTable(connection, id)))
  names(tables) <- spec_tables[present]
  rows <- vapply(tables, nrow, integer(1), USE.NAMES = FALSE)
  inputs <- data.table(
    table_name = spec_tables[present], file = rep("DBI connection, not hashed", length(rows)),
    sha256 = rep(NA_character_, length(rows)),
    detail = sprintf(
      "%d %s; table %s; driver %s", rows, fifelse(rows == 1L, "row", "rows"),
      db_names[present], class(connection)[[1L]]
    )
  )
  list(tables = tables, inputs = inputs, text_tables = character())
}

#' A database's name for a table as DBI reads it: "schema.table" split at its first dot into
#' DBI::Id(), any other name as given (D17.6)
#' @noRd
db_table_id <- function(name) {
  dot <- regexpr(".", name, fixed = TRUE)
  if (dot < 1L) {
    return(name)
  }
  DBI::Id(
    schema = substr(name, 1L, dot - 1L), table = substr(name, dot + 1L, nchar(name))
  )
}
