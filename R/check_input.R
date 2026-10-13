# The tables a conformance run reads, matched to its specification (plan 6.1, 6.2, 16.2;
# D7.5, D9.24, D16.9, D16.13, D16.14). Each spec table is matched to the input table of its
# name, and each dictionary attribute to its column: exactly, or failing that by letter case
# and spaces. A column is coerced to its expected class only when a check asks, one column
# at a time. Each record finds its group values and the problem unit's key through the
# foreign keys. The caller's tables are read, never changed or copied whole.

#' The caller's tables checked: a list of data.frames, each named once by its table, each
#' column named once and holding one value per row
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
    stop("`tables` must be a list of data.frames, each named once by its table.", call. = FALSE)
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
#' class (16.2's coercion copy); `lost` is TRUE where a value that isn't NA can't be held:
#' text that isn't a number, a number with a fraction or beyond an integer's range where an
#' integer is wanted. A value that isn't finite (NaN, Inf) is never lost but becomes NA where
#' an integer is wanted: value_not_finite reports it. A date column stored as a date is
#' written in the type's date format; text is never lost. Text that isn't valid UTF-8 is no
#' number, so it is lost without being read: as.numeric() stops on it in a UTF-8 locale.
#' @noRd
coerce_column <- function(x, r_class, date_format = NA_character_) {
  n <- length(x)
  as_number <- function(text) {
    valid <- validUTF8(text)
    if (all(valid)) {
      return(suppressWarnings(as.numeric(text)))
    }
    number <- rep(NA_real_, length(text))
    number[valid] <- suppressWarnings(as.numeric(text[valid]))
    number
  }
  if (r_class == "character") {
    if (is.character(x)) {
      return(list(value = x, lost = logical(n)))
    }
    if (inherits(x, c("Date", "POSIXt")) && !is.na(date_format)) {
      return(list(value = format(x, date_format), lost = logical(n)))
    }
    return(list(value = value_text(x), lost = logical(n)))
  }
  plain_number <- is.numeric(x) && !is.object(x)
  if (r_class == "double") {
    if (is.double(x) && !is.object(x)) {
      return(list(value = x, lost = logical(n)))
    }
    if (plain_number) {
      return(list(value = as.double(x), lost = logical(n)))
    }
    text <- value_text(x)
    number <- as_number(text)
    return(list(value = number, lost = !is.na(text) & is.na(number) & !is.nan(number)))
  }
  if (is.integer(x) && !is.object(x)) {
    return(list(value = x, lost = logical(n)))
  }
  if (plain_number) {
    number <- x
    given <- !is.na(x)
  } else {
    text <- value_text(x)
    number <- as_number(text)
    given <- !is.na(text)
  }
  finite <- is.finite(number)
  whole <- finite & number == trunc(number) & abs(number) <= .Machine$integer.max
  value <- rep(NA_integer_, n)
  value[whole] <- as.integer(number[whole])
  not_finite <- is.nan(number) | is.infinite(number)
  list(value = value, lost = given & !whole & !not_finite)
}

#' Each dictionary attribute matched to its column (D16.14)
#'
#' An exact name wins; an attribute left over then takes the first column left over whose
#' name differs from it only in letter case or spaces (a name that isn't valid UTF-8 matches
#' exactly or not at all). Returns `column` (the column's name, NA where none matched) and
#' `near` per attribute, and `extra`, the columns no attribute took, in the table's order.
#' @noRd
match_columns <- function(given, attributes) {
  column <- given[match(attributes, given)]
  near <- logical(length(attributes))
  left <- given[!given %chin% column]
  squeeze <- function(x) {
    out <- rep(NA_character_, length(x))
    ok <- validUTF8(x)
    out[ok] <- tolower(gsub("[[:space:]]", "", x[ok]))
    out
  }
  for (i in which(is.na(column))) {
    hit <- left[!is.na(squeeze(left)) & squeeze(left) == squeeze(attributes[[i]])]
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
#' columns and the problem unit's key column (D7.5). Returns a list:
#' - `spec`; `data`: the spec's tables that were given, in the dictionary's order, as given;
#'   `given`: every given table's name and row count; `missing` and `unexpected`: the spec
#'   tables not given and the given tables the spec lacks;
#' - `columns`: one row per dictionary attribute: `table_name`, `attribute_name`, `column`
#'   (its column, NA where none), `near` (matched by case or spaces), `r_class`,
#'   `date_format`, `family`, `key_type` (PK, FK or NA), `id_marked`, `coded`,
#'   `table_present`;
#' - `extra`: the given columns of spec tables that no attribute took (`table_name`,
#'   `column`);
#' - `n_rows`; `groups`, per given spec table: `row_group` (each row's group, an index into
#'   `values`) and `values` (one row per group, the group columns as text, "*" where a
#'   record can't reach a column, D9.24, D16.13), ordered by their text; `key_rows`, per
#'   table, the home table and each row's row there for the key column;
#' - `group_cols`, `key_col`, and `cache`, an environment the PK helpers fill.
#' @noRd
conformance_input <- function(tables, spec, group_cols = character(), key_col = NULL) {
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
      table_name = names(tables),
      n_rows = vapply(tables, nrow, integer(1), USE.NAMES = FALSE)
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

#' A key column's values for joining, and the sentinels a foreign key may hold there
#'
#' `value` is the column coerced to its expected class, the caller's column itself where its
#' class is right: never masked or kept for the run (16.2). `open` holds the sentinels a
#' foreign key of its type may hold (allowed_in_fk), which reach_column() makes incomparable,
#' so no record reaches a parent through one (D12.31). `value` is NA throughout, and `open`
#' empty, when the table or its column wasn't given.
#' @noRd
key_values <- function(input, table, attribute) {
  n <- if (table %chin% names(input$data)) nrow(input$data[[table]]) else 0L
  at <- which(input$columns$table_name == table & input$columns$attribute_name == attribute)
  column <- if (length(at) == 1L) input$columns$column[[at]] else NA_character_
  if (is.na(column)) {
    return(list(value = rep(NA, n), open = character()))
  }
  row <- input$columns[at]
  value <- coerce_column(input$data[[table]][[column]], row$r_class, row$date_format)$value
  sentinels <- input$spec$sentinels
  open <- sentinels$value[sentinels$allowed_in_fk & sentinels$data_type == row$family]
  if (is.numeric(value)) {
    open <- suppressWarnings(as.numeric(open))
  }
  list(value = value, open = open)
}

#' The table a record reaches a column in, and each record's row there (D16.13)
#'
#' A table holding the column uses its own rows; otherwise the shortest path of foreign keys
#' to a table holding it (fk_path()). A record whose path breaks (an NA, sentinel or orphan
#' key on the way, or a table or column on it not given) gets NA. `home` is NA when no table
#' holding the column is reachable.
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
  rows <- seq_len(n)
  current <- table
  for (k in seq_len(nrow(path))) {
    parent <- path$reference_table[[k]]
    child <- key_values(input, current, path$attribute_name[[k]])
    parent_keys <- key_values(input, parent, path$reference_attribute[[k]])
    # NA and the sentinels a foreign key may hold, the child's and the parent's types', match
    # nothing, masked inside the match rather than in a copy of the column (16.2, D12.31,
    # D16.26).
    rows <- match(
      child$value[rows], parent_keys$value,
      incomparables = c(NA, child$open, parent_keys$open)
    )
    current <- parent
  }
  list(home = current, rows = rows)
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
#' value is blank; the groups are numbered in the order of their text.
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
    value <- reached_values(input, reach_column(input, table, column_name), column_name)
    distinct <- unique(value)
    # Each distinct value as text, "*" for a blank or a value no record reached, so values
    # that read the same are one group.
    text <- value_text(distinct)
    text[is.na(text) | is_blank(text)] <- "*"
    labels[[column_name]] <- unique(text)
    codes[[column_name]] <- match(text, labels[[column_name]])[match(value, distinct)]
  }
  coded <- as.data.table(codes)
  row_group <- if (n > 0L) as.integer(frank(coded, ties.method = "dense")) else integer()
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
  pk <- keys[keys$table_name == table & keys$key_type == "PK"]
  pk <- pk[order(pk$key_part)]
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
  record_pk <- do.call(paste, c(parts, sep = "|"))
  record_pk[missing] <- NA_character_
  name <- paste("pk_repeats", table, sep = "\r")
  repeats <- input$cache[[name]]
  if (is.null(repeats)) {
    given <- pk_columns[!is.na(pk_columns)]
    if (length(given) == 0L) {
      repeats <- logical(nrow(data))
    } else {
      keyed <- as.data.table(lapply(given, function(column) data[[column]]))
      repeats <- duplicated(keyed) | duplicated(keyed, fromLast = TRUE)
    }
    assign(name, repeats, envir = input$cache)
  }
  list(record_pk = record_pk, positional = missing | repeats[rows])
}
