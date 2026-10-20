# The tables a conformance run reads, matched to its specification (plan 6.1, 6.2, 16.2;
# D7.5, D9.24, D16.9, D16.13, D16.14, D16.33, D16.35, D16.36, D16.38). Each spec table is
# matched to the input table of its name, and each dictionary attribute to its column:
# exactly, or failing that by letter case and spaces. A column is coerced to its expected
# class only when a check asks, one column at a time. Each record finds its group values and
# the problem unit's key through the foreign keys. The caller's tables are read, never
# changed or copied whole; a table holding text marked as bytes is refused.

#' The caller's tables checked: a list of data.frames, each named once by its table, each
#' column named once and holding one value per row, and no text marked as bytes (D16.38)
#'
#' Caller errors, in English (D12.37). Returns `tables`, invisibly.
#' @noRd
validate_tables <- function(tables) {
  # is_blank() reads bytes, so a name that isn't valid UTF-8 is checked too, not a stop.
  named <- is.list(tables) && !is.data.frame(tables) && (length(tables) == 0L || (
    !is.null(names(tables)) && !anyNA(names(tables)) && !any(is_blank(names(tables))) &&
      !anyDuplicated(names(tables))
  ))
  if (!named) {
    stop(
      paste0(
        "`tables` must be a list of data.frames or CSV paths, each named once by its table, ",
        "a named character vector of paths, or a DBI connection."
      ),
      call. = FALSE
    )
  }
  frames <- vapply(tables, is.data.frame, logical(1))
  if (!all(frames)) {
    stop(sprintf(
      "`tables` holds elements that aren't data.frames: %s.",
      paste(mark_invalid_utf8(names(tables)[!frames]), collapse = ", ")
    ), call. = FALSE)
  }
  for (name in names(tables)) {
    columns <- names(tables[[name]])
    shown <- mark_invalid_utf8(name)
    if (anyNA(columns) || any(!nzchar(columns))) {
      stop(sprintf("Table %s has a column with no name.", shown), call. = FALSE)
    }
    twice <- unique(columns[duplicated(columns)])
    if (length(twice) > 0L) {
      stop(sprintf(
        "Table %s has the columns %s more than once.", shown,
        paste(mark_invalid_utf8(twice), collapse = ", ")
      ), call. = FALSE)
    }
    listed <- vapply(tables[[name]], function(x) is.list(x) || !is.null(dim(x)), logical(1))
    if (any(listed)) {
      stop(sprintf(
        "Table %s has columns that don't hold one value per row: %s.", shown,
        paste(mark_invalid_utf8(columns[listed]), collapse = ", ")
      ), call. = FALSE)
    }
    # Text marked as bytes has no encoding to read it in, and data.table stops on it in a
    # sort, leaving the session's later sorts broken (D16.38). Each text column's marks are
    # read once, a factor's from its levels; no text is copied or converted (16.2).
    marked <- vapply(tables[[name]], function(x) {
      text <- if (is.factor(x)) levels(x) else if (is.character(x)) x else character()
      any(Encoding(text) == "bytes")
    }, logical(1))
    if (any(marked)) {
      stop(sprintf(
        paste0(
          "Table %s has columns holding text marked as bytes: %s; read the table with its ",
          "encoding, such as UTF-8 or latin1."
        ),
        shown, paste(mark_invalid_utf8(columns[marked]), collapse = ", ")
      ), call. = FALSE)
    }
  }
  invisible(tables)
}

#' The family a dictionary type's sentinels attach by: numeric, date or character (3.5;
#' D12.19)
#' @noRd
type_family <- function(r_class, date_format) {
  fifelse(
    r_class %chin% c("integer", "double"), "numeric",
    fifelse(!is.na(date_format), "date", "character")
  )
}

#' A column's storage as a word: "integer", "double", "character", "logical", or the first
#' class of any other object ("factor", "Date", "IDate", "POSIXct")
#' @noRd
storage_class <- function(x) {
  if (is.object(x)) {
    return(class(x)[[1L]])
  }
  if (is.double(x)) "double" else typeof(x)
}

#' A column's values as text, for failing records only (16.2)
#'
#' Text stays as written, blanks included; a factor gives its labels; a double is written in
#' full to 15 significant digits ("NaN", "Inf" and "-Inf" as such); NA stays NA.
#' @noRd
value_text <- function(x) {
  if (is.character(x)) {
    return(as.vector(x))
  }
  if (is.double(x) && !is.object(x)) {
    text <- trimws(formatC(x, digits = 15L, format = "fg"))
    text[is.na(x) & !is.nan(x)] <- NA_character_
    return(as.vector(text))
  }
  as.vector(as.character(x))
}

#' A column stored as text, or NULL: a factor gives its labels
#' @noRd
stored_text <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  NULL
}

#' A column coerced to its expected class, and which values the coercion loses (D16.9)
#'
#' `value` is the column itself when it already has the class, otherwise a new vector of the
#' class (16.2's coercion copy); for a class of numbers, `not_finite` is the rows whose value,
#' read as a number, is NaN, Inf or -Inf, which value_not_finite reports (D17.22); `lost` is
#' TRUE where a value that isn't NA can't be held:
#' text that isn't a number, a number with a fraction or beyond an integer's range where an
#' integer is wanted. Text is a number only when written as a decimal number: a sign, digits,
#' one point and an exponent, with spaces, tabs and line breaks around, or as one of the
#' spellings of Inf and NaN that as.numeric() reads; hexadecimal is lost (D16.42, m26). A
#' value that isn't finite (NaN, Inf) is never lost but becomes NA where an integer is wanted:
#' value_not_finite reports it. A date column stored as a date is written in the type's date
#' format; text is never lost. A column that isn't text, where text is wanted, and one that
#' is neither text nor plain numbers (a factor, a date, a logical), where numbers are wanted,
#' is written as text once for each distinct value (16.2). Text that isn't
#' valid UTF-8 is read as UTF-8 where it converts to it (marked latin1, or native in a locale
#' that isn't UTF-8); text marked as bytes, or that isn't valid UTF-8 even so, is no number,
#' so it is lost without being read: as.numeric() stops on it in a UTF-8 locale (D16.33,
#' D16.36; a run refuses text marked as bytes before, D16.38).
#' @noRd
coerce_column <- function(x, r_class, date_format = NA_character_) {
  n <- length(x)
  as_number <- function(text) {
    bytes <- Encoding(text) == "bytes"
    valid <- !bytes & validUTF8(text)
    if (all(valid)) {
      number <- suppressWarnings(as.numeric(text))
    } else {
      # Only the values that aren't valid UTF-8 are converted (16.2).
      utf8 <- utf8_converted(text)
      readable <- !bytes & validUTF8(utf8)
      number <- rep(NA_real_, length(text))
      number[readable] <- suppressWarnings(as.numeric(utf8[readable]))
    }
    # as.numeric() also reads hexadecimal ("0x10" as 16), and on Windows a number with a
    # Unicode space after it: a value it reads is kept only when written as a decimal number,
    # Inf or NaN, in plain ASCII (D16.42, m26). Of the values it reads, those are the ones
    # holding no "x" or "X" and no byte outside ASCII, which byte tests find at a tenth of a
    # pattern's cost (D17.22). A number is plain ASCII, so its bytes are read.
    read <- which(!is.na(number))
    held <- text[read]
    refused <- grepl("x", held, fixed = TRUE, useBytes = TRUE) |
      grepl("X", held, fixed = TRUE, useBytes = TRUE) |
      grepl("[\\x80-\\xff]", held, perl = TRUE, useBytes = TRUE)
    number[read[refused]] <- NA_real_
    number
  }
  # Whole numbers as integers: a fraction, a number beyond an integer's range or a value that
  # isn't a number is lost where `given`, NaN and Inf are left to value_not_finite.
  as_whole <- function(number, given) {
    finite <- is.finite(number)
    whole <- finite & number == trunc(number) & abs(number) <= .Machine$integer.max
    value <- rep(NA_integer_, length(number))
    value[whole] <- as.integer(number[whole])
    not_finite <- is.nan(number) | is.infinite(number)
    list(value = value, lost = given & !whole & !not_finite)
  }
  # Each row's distinct value. A factor, Date, IDate or POSIXct column is matched on its codes,
  # days or seconds, since match() would write every row as text (16.2), and a logical column
  # on its values. Any other class is matched as match() reads it: its stored numbers needn't
  # be its values, as integer64's aren't, whose NA is stored as a number equal to 0 (D16.49).
  distinct_at <- function(distinct) {
    if (is.logical(x) || is.factor(x) || inherits(x, c("Date", "POSIXct"))) {
      match(unclass(x), unclass(distinct))
    } else {
      match(x, distinct)
    }
  }
  if (r_class == "character") {
    if (is.character(x)) {
      return(list(value = x, lost = logical(n)))
    }
    # Any other column is written as text once for each distinct value, a date stored as a
    # date in the type's date format, and each row takes its value's text (16.2).
    distinct <- unique(x)
    at <- distinct_at(distinct)
    text <- if (inherits(x, c("Date", "POSIXt")) && !is.na(date_format)) {
      format(distinct, date_format)
    } else {
      value_text(distinct)
    }
    return(list(value = text[at], lost = logical(n)))
  }
  if (is.numeric(x) && !is.object(x)) {
    not_finite <- if (is.double(x)) which(is.nan(x) | is.infinite(x)) else integer()
    if (r_class == "double") {
      return(list(
        value = if (is.double(x)) x else as.double(x), lost = logical(n), not_finite = not_finite
      ))
    }
    if (is.integer(x)) {
      return(list(value = x, lost = logical(n), not_finite = not_finite))
    }
    return(c(as_whole(x, !is.na(x)), list(not_finite = not_finite)))
  }
  # Text is read as it is. Any other column is written as text once for each distinct value,
  # each row taking its value's result (16.2).
  at <- NULL
  if (is.character(x)) {
    text <- x
  } else {
    distinct <- unique(x)
    at <- distinct_at(distinct)
    text <- value_text(distinct)
  }
  number <- as_number(text)
  out <- if (r_class == "double") {
    list(value = number, lost = !is.na(text) & is.na(number) & !is.nan(number))
  } else {
    as_whole(number, !is.na(text))
  }
  not_finite <- is.nan(number) | is.infinite(number)
  if (is.null(at)) {
    return(c(out, list(not_finite = which(not_finite))))
  }
  list(value = out$value[at], lost = out$lost[at], not_finite = which(not_finite[at]))
}

#' Each dictionary attribute matched to its column (D16.14)
#'
#' An exact name wins; an attribute left over then takes the first column left over whose
#' name differs from it only in letter case or spaces, any Unicode space in a name that isn't
#' plain ASCII (D16.42, m21). A name that isn't valid UTF-8, a column's or an attribute's,
#' matches exactly or not at all. Returns `column` (the column's name, NA where none matched)
#' and `near` per attribute, and `extra`, the columns no attribute took, in the table's order.
#' @noRd
match_columns <- function(given, attributes) {
  column <- given[match(attributes, given)]
  near <- logical(length(attributes))
  left <- given[!given %chin% column]
  squeeze <- function(x) {
    out <- rep(NA_character_, length(x))
    ok <- validUTF8(x)
    wide <- ok & grepl("[^\001-\177]", x, useBytes = TRUE)
    narrow <- ok & !wide
    out[narrow] <- tolower(gsub("[[:space:]]", "", x[narrow]))
    # A name that isn't plain ASCII is read as UTF-8, marked so on a copy where it is
    # unmarked, as fread() leaves it, since a Unicode pattern reads unmarked text byte by byte
    # outside a UTF-8 locale (D16.36).
    out[wide] <- tolower(gsub("(*UCP)\\s", "", utf8_marked(x[wide]), perl = TRUE))
    out
  }
  for (i in which(is.na(column))) {
    target <- squeeze(attributes[[i]])
    if (is.na(target)) {
      next
    }
    squeezed <- squeeze(left)
    hit <- left[!is.na(squeezed) & squeezed == target]
    if (length(hit) > 0L) {
      column[[i]] <- hit[[1L]]
      near[[i]] <- TRUE
      left <- left[left != hit[[1L]]]
    }
  }
  list(column = column, near = near, extra = left)
}

#' The shortest path of foreign keys from a table to one of `targets` (D16.13)
#'
#' A breadth-first walk of the spec's foreign keys (`keys` rows of type FK that name their
#' parent's PK), each table's keys in the dictionary's order, so a tie goes to the first key.
#' Returns the path's key rows in order, or NULL when no target is reached.
#' @noRd
fk_path <- function(keys, from, targets) {
  is_edge <- keys$key_type == "FK" & !is.na(keys$reference_table) &
    !is.na(keys$reference_attribute)
  edges <- keys[is_edge]
  paths <- list()
  paths[[from]] <- integer()
  queue <- from
  while (length(queue) > 0L) {
    node <- queue[[1L]]
    queue <- queue[-1L]
    for (edge in which(edges$table_name == node)) {
      parent <- edges$reference_table[[edge]]
      if (!is.null(paths[[parent]])) {
        next
      }
      path <- c(paths[[node]], edge)
      if (parent %chin% targets) {
        return(edges[path])
      }
      paths[[parent]] <- path
      queue <- c(queue, parent)
    }
  }
  NULL
}

#' The run's input matched to the spec
#'
#' `tables` as validate_tables() takes it; `group_cols` and `key_col` the caller's group
#' columns and the problem unit's key column (D7.5); `text_tables` the names of the tables
#' read from CSV, every column text (D9.23). Returns a list:
#' - `spec`; `data`: the spec's tables that were given, in the dictionary's order, as given;
#'   `given`: every given table's name, row count and whether it was read as text
#'   (`text_input`); `missing` and `unexpected`: the spec
#'   tables not given and the given tables the spec lacks;
#' - `columns`: one row per dictionary attribute: `table_name`, `attribute_name`, `column`
#'   (its column, NA where none), `near` (matched by case or spaces), `r_class`,
#'   `date_format`, `family`, `key_type` (PK, FK or NA), `id_marked`, `coded`,
#'   `table_present`;
#' - `extra`: the given columns of spec tables that no attribute took (`table_name`,
#'   `column`);
#' - `n_rows`; `groups`, per given spec table: `row_group` (each row's group, an index into
#'   `values`) and `values` (one row per group, the group columns as text, "*" where a
#'   record can't reach a column, D9.24, D16.13), ordered by their text, and `n`, the number
#'   of groups, 1 without group columns; `key_rows`, per table, the home table and each row's
#'   row there for the key column;
#' - `group_cols`, `key_col`, and `cache`, an environment the PK helpers fill.
#' @noRd
conformance_input <- function(tables, spec, group_cols = character(), key_col = NULL,
                              text_tables = character()) {
  attributes <- spec$attributes
  spec_tables <- unique(attributes$table_name)
  present <- spec_tables[spec_tables %chin% names(tables)]
  type_at <- match(attributes$data_type, spec$type_map$data_type)
  date_format <- spec$type_map$date_format[type_at]
  keys <- spec$keys
  key_at <- match(
    paste(attributes$table_name, attributes$attribute_name, sep = "\r"),
    paste(keys$table_name, keys$attribute_name, sep = "\r")
  )
  coded_pairs <- paste(spec$code_list_map$table_name, spec$code_list_map$attribute_name, sep = "\r")
  columns <- data.table(
    table_name = attributes$table_name, attribute_name = attributes$attribute_name,
    column = NA_character_, near = FALSE, r_class = attributes$r_class,
    date_format = date_format, family = type_family(attributes$r_class, date_format),
    key_type = keys$key_type[key_at], id_marked = attributes$id_marked %in% TRUE,
    coded = paste(attributes$table_name, attributes$attribute_name, sep = "\r") %chin%
      coded_pairs,
    table_present = attributes$table_name %chin% present
  )
  extra <- list()
  for (table in present) {
    at <- which(columns$table_name == table)
    given <- names(tables[[table]])
    matched <- match_columns(given, columns$attribute_name[at])
    set(columns, i = at, j = "column", value = matched$column)
    set(columns, i = at, j = "near", value = matched$near)
    extra[[table]] <- data.table(
      table_name = rep(table, length(matched$extra)), column = matched$extra
    )
  }
  input <- list(
    spec = spec, data = tables[present],
    given = data.table(
      table_name = as.character(names(tables)),
      n_rows = vapply(tables, nrow, integer(1), USE.NAMES = FALSE),
      text_input = as.character(names(tables)) %chin% text_tables
    ),
    missing = setdiff(spec_tables, present),
    unexpected = setdiff(names(tables), spec_tables),
    columns = columns,
    extra = rbindlist(c(list(data.table(table_name = character(), column = character())), extra)),
    n_rows = vapply(tables[present], nrow, integer(1)),
    group_cols = group_cols, key_col = key_col, cache = new.env(parent = emptyenv())
  )
  input$groups <- lapply(present, function(table) group_index(input, table))
  names(input$groups) <- present
  input$key_rows <- if (is.null(key_col)) {
    list()
  } else {
    structure(lapply(present, function(table) reach_column(input, table, key_col)), names = present)
  }
  input
}

#' A key column's values for joining, and the sentinels of its type
#'
#' `value` is the caller's column itself where it is stored as plain numbers, whatever its
#' type, or else the column coerced to its expected class, the caller's column itself where
#' its class is right: never masked or kept for the run (16.2; D16.42, m9). `open` holds every
#' sentinel of the key's type, which reach_column() makes incomparable on both sides of a
#' join, so no record reaches a parent through one (D16.26; D16.42, m8). Where `value` is
#' numbers, `open` is numbers too: another type's text code only where the number is written
#' as the code, so a key 7 isn't the code "007". `value` is NA throughout, and `open` empty,
#' when the table or its column wasn't given.
#' @noRd
key_values <- function(input, table, attribute) {
  n <- if (table %chin% names(input$data)) nrow(input$data[[table]]) else 0L
  at <- which(input$columns$table_name == table & input$columns$attribute_name == attribute)
  column <- if (length(at) == 1L) input$columns$column[[at]] else NA_character_
  if (is.na(column)) {
    return(list(value = rep(NA, n), open = character()))
  }
  row <- input$columns[at]
  x <- input$data[[table]][[column]]
  value <- if (is.numeric(x) && !is.object(x)) {
    x
  } else {
    coerce_column(x, row$r_class, row$date_format)$value
  }
  sentinels <- input$spec$sentinels
  open <- sentinels$value[sentinels$data_type == row$family]
  if (is.numeric(value)) {
    number <- suppressWarnings(as.numeric(open))
    if (row$family != "numeric") {
      number <- number[!is.na(number) & value_text(number) == open]
    }
    open <- number
  }
  list(value = value, open = open)
}

#' The table a record reaches a column in, and each record's row there (D16.13)
#'
#' A table holding the column uses its own rows; otherwise the shortest path of foreign keys
#' to a table holding it (fk_path()). A record whose path breaks (an NA, blank, sentinel or
#' orphan key on the way, or a table or column on it not given) gets NA. `home` is NA when no
#' table holding the column is reachable.
#' @noRd
reach_column <- function(input, table, column_name) {
  n <- nrow(input$data[[table]])
  columns <- input$columns
  holders <- unique(columns$table_name[columns$attribute_name == column_name])
  if (table %chin% holders) {
    return(list(home = table, rows = seq_len(n)))
  }
  path <- fk_path(input$spec$keys, table, holders)
  if (is.null(path)) {
    return(list(home = NA_character_, rows = rep(NA_integer_, n)))
  }
  # The path followed one key join per hop, as a key_pairs side or the bands' key is
  # (chain_rows(), D18.27).
  list(home = path$reference_table[[nrow(path)]], rows = chain_rows(input, table, path))
}

#' The values of a column in its home table at the rows a record reached, as given; NA where
#' the record reached none or the column wasn't given
#' @noRd
reached_values <- function(input, reached, column_name) {
  if (is.na(reached$home)) {
    return(rep(NA, length(reached$rows)))
  }
  at <- which(
    input$columns$table_name == reached$home & input$columns$attribute_name == column_name
  )
  column <- input$columns$column[at]
  if (is.na(column)) {
    return(rep(NA, length(reached$rows)))
  }
  input$data[[reached$home]][[column]][reached$rows]
}

#' Each row's group in a table, and the groups' values as text (D7.5, D9.24, D16.13)
#'
#' Without group columns every row is in group 1 and `values` is NULL. Otherwise each group
#' column's value is read where the record reaches it, "*" where it reaches none or the
#' value is blank; the groups are numbered in the order of their text, each held as UTF-8:
#' unmarked valid UTF-8 marked so, latin1 and invalid text converted (D16.36; D16.47, I1).
#' A group value marked as bytes, or that isn't valid UTF-8 once converted to it, stops the
#' run, naming its column, table and first row (D16.35, D16.36). Returns `row_group` (each
#' row's group), `values` (one row per group) and `n`, the number of groups, 1 without group
#' columns.
#' @noRd
group_index <- function(input, table) {
  n <- nrow(input$data[[table]])
  group_cols <- input$group_cols
  if (length(group_cols) == 0L) {
    return(list(row_group = rep(1L, n), values = NULL, n = 1L))
  }
  codes <- list()
  labels <- list()
  for (column_name in group_cols) {
    reached <- reach_column(input, table, column_name)
    value <- reached_values(input, reached, column_name)
    distinct <- unique(value)
    # Each distinct value as text, "*" for a blank or a value no record reached, so values
    # that read the same are one group.
    text <- value_text(distinct)
    # A group value goes into every results row and file, so it is held as UTF-8, which the
    # order below needs too. Valid UTF-8 left unmarked, as fread() leaves it, is marked so in
    # any locale, never converted; text marked latin1, and text that isn't valid UTF-8 (native
    # in a locale that isn't UTF-8), is converted (D16.36; D16.47, I1). Text marked as bytes
    # has no encoding to convert from. A value marked so, or that isn't valid UTF-8 even so,
    # stops the run: the caller is told the column, the table it is read from and its first
    # such row (D16.35, D16.36). validate_tables() refuses text marked as bytes first
    # (D16.38), so for it this is a second line.
    mark <- Encoding(text)
    bytes <- mark == "bytes"
    valid <- !bytes & validUTF8(text)
    text <- utf8_marked(text)
    convert <- which(!bytes & (mark == "latin1" | !valid))
    text[convert] <- enc2utf8(text[convert])
    if (any(bytes) || !all(validUTF8(text))) {
      at <- which(
        input$columns$table_name == reached$home & input$columns$attribute_name == column_name
      )
      whole <- value_text(input$data[[reached$home]][[input$columns$column[[at]]]])
      marked <- Encoding(whole) == "bytes"
      # Only the values that aren't valid UTF-8 as given are converted (16.2).
      odd <- marked | !validUTF8(utf8_converted(whole))
      stop(sprintf(
        paste0(
          "Group column %s of table %s holds text that isn't valid UTF-8, first in row %d; ",
          "the table may have been read with the wrong encoding."
        ),
        mark_invalid_utf8(column_name), mark_invalid_utf8(reached$home), which(odd)[[1L]]
      ), call. = FALSE)
    }
    text[is.na(text) | is_blank(text)] <- "*"
    labels[[column_name]] <- unique(text)
    # Each row takes its value's group. A factor, Date, IDate or POSIXct column is matched on
    # its codes, days or seconds, since match() would write every row as text (16.2), and a
    # logical column on its values; any other class as match() reads it, since its stored
    # numbers needn't be its values, as integer64's aren't (D16.49).
    at <- if (is.logical(value) || is.factor(value) || inherits(value, c("Date", "POSIXct"))) {
      match(unclass(value), unclass(distinct))
    } else {
      match(value, distinct)
    }
    codes[[column_name]] <- match(text, labels[[column_name]])[at]
  }
  # The codes are ranked as the list they are, not copied into a table (16.2).
  row_group <- if (n > 0L) as.integer(frankv(codes, ties.method = "dense")) else integer()
  lead <- match(seq_len(max(c(0L, row_group))), row_group)
  values <- as.data.table(lapply(group_cols, function(column_name) {
    labels[[column_name]][codes[[column_name]][lead]]
  }))
  setnames(values, group_cols)
  if (nrow(values) > 0L) {
    order_of <- do.call(order, c(unname(as.list(values)), method = "radix"))
    renumber <- integer(length(order_of))
    renumber[order_of] <- seq_along(order_of)
    row_group <- renumber[row_group]
    values <- values[order_of]
  }
  list(row_group = row_group, values = values, n = max(1L, nrow(values)))
}

#' The problem unit's key for some rows of a table, as text, "*" where a row reaches none
#' (D16.13); NULL without a key column
#' @noRd
key_text <- function(input, table, rows) {
  if (is.null(input$key_col)) {
    return(NULL)
  }
  reached <- input$key_rows[[table]]
  text <- value_text(reached_values(
    input, list(home = reached$home, rows = reached$rows[rows]), input$key_col
  ))
  text[is.na(text) | is_blank(text)] <- "*"
  text
}

#' Each given row's PK as text and whether it is positional (9.3, 9.6; D8.16, D15.9)
#'
#' A composite PK's parts are joined with "|" in key_part order. A PK with a part missing
#' (NA, blank, or its column not given) is NA; a row is positional when its PK is missing or
#' the PK repeats in the table. The repeats are found once per table and kept in the cache.
#' Returns `record_pk` and `positional` for `rows`.
#' @noRd
record_pk_text <- function(input, table, rows) {
  keys <- input$spec$keys
  # Worked out before the brackets, so no name inside them is taken for a column.
  is_pk <- keys$table_name == table & keys$key_type == "PK"
  pk <- keys[is_pk]
  in_order <- order(pk$key_part)
  pk <- pk[in_order]
  columns <- input$columns
  at <- match(
    paste(table, pk$attribute_name, sep = "\r"),
    paste(columns$table_name, columns$attribute_name, sep = "\r")
  )
  pk_columns <- columns$column[at]
  data <- input$data[[table]]
  parts <- lapply(pk_columns, function(column) {
    if (is.na(column)) rep(NA_character_, length(rows)) else value_text(data[[column]][rows])
  })
  if (length(parts) == 0L) {
    return(list(record_pk = rep(NA_character_, length(rows)), positional = rep(TRUE, length(rows))))
  }
  missing <- Reduce(`|`, lapply(parts, function(part) is.na(part) | is_blank(part)))
  # A composite PK's parts that are valid UTF-8 left unmarked are marked UTF-8 before they are
  # joined, since paste() reads unmarked text beside UTF-8 in the session's encoding; the PK
  # is then held as gpq_check_conformance() holds the caller's other text (D16.47, I1).
  if (length(parts) > 1L) {
    parts <- lapply(parts, utf8_marked)
  }
  record_pk <- do.call(paste, c(parts, sep = "|"))
  record_pk[missing] <- NA_character_
  name <- paste("pk_repeats", table, sep = "\r")
  repeats <- input$cache[[name]]
  if (is.null(repeats)) {
    given <- pk_columns[!is.na(pk_columns)]
    if (length(given) == 0L) {
      repeats <- logical(nrow(data))
    } else {
      # The caller's columns are read where they are: a one-column PK as it is, a composite
      # one, or one stored as integer64, whose own duplicated() ignores fromLast, as a table
      # made by reference over them, none copied (16.2).
      keyed <- if (length(given) == 1L && !inherits(data[[given]], "integer64")) {
        data[[given]]
      } else {
        setDT(lapply(given, function(column) data[[column]]))
      }
      # data.table reads the table's doubles exactly, whatever setNumericRounding() the session
      # has set, as check_keys() does; the session's setting is put back on exit (m67; the
      # minors pass's fix round, dt M2).
      rounding <- setNumericRounding(0L)
      on.exit(setNumericRounding(rounding), add = TRUE)
      repeats <- duplicated(keyed) | duplicated(keyed, fromLast = TRUE)
    }
    assign(name, repeats, envir = input$cache)
  }
  list(record_pk = record_pk, positional = missing | repeats[rows])
}
