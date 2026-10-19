# Reading a specification into a gpq_spec object (plan 3.4, 3.6, 4.1; D12.13 to D12.25).
# The reader never stops on a defect in the specification: it records each one in
# read_findings, and gpq_preflight() is the only place that stops (D2.11, D12.13). It
# does stop on a caller's error, such as a file that doesn't exist.

#' The spec object's components and their column classes, in 3.6's order
#'
#' `datasets` is `character()`: every column of the input, all character.
#' @noRd
spec_schema <- function() {
  chr <- "character"
  int <- "integer"
  lgl <- "logical"
  dbl <- "numeric"
  list(
    attributes = c(
      table_name = chr, attribute_name = chr, data_type = chr, r_class = chr,
      key_type = chr, reference_table = chr, lookup = chr, description = chr,
      lineage_flag = lgl, id_marked = lgl, source_row = int, min_value = dbl, max_value = dbl,
      min_inclusive = lgl, max_inclusive = lgl, pattern = chr, max_length = int, decimals = int,
      units = chr
    ),
    keys = c(
      table_name = chr, attribute_name = chr, key_type = chr, key_part = int,
      reference_table = chr, reference_attribute = chr
    ),
    code_lists = c(
      sheet = chr, source_row = int, sheet_column = chr, value = chr, source_cell = chr
    ),
    code_list_sheets = c(sheet = chr),
    code_list_map = c(
      table_name = chr, attribute_name = chr, lookup = chr, source_type = chr,
      source_name = chr, code_column = chr, filter_column = chr, filter_values = chr,
      status = chr
    ),
    codes = c(
      table_name = chr, attribute_name = chr, code = chr, source_type = chr,
      source_name = chr, source_row = int
    ),
    non_code_sheets = c(sheet = chr),
    datasets = character(),
    lineage_spec = c(
      contributor_label = chr, table_name = chr, attribute_name = chr, spec_type = chr,
      alternative = chr, part_order = int, part_source = chr, part_kind = chr,
      id_status = chr, source_text = chr, note = chr, source_cell = chr
    ),
    crosswalks = c(
      crosswalk = chr, source_row = int, crosswalk_column = chr, value = chr,
      source_cell = chr
    ),
    id_bands = c(
      label = chr, band_start = dbl, band_end = dbl, reserved = lgl, source_row = int,
      source_cell = chr, key_value = chr
    ),
    id_band_columns = c(
      table_name = chr, attribute_name = chr, key_attribute = chr, labels_sheet = chr,
      labels_column = chr
    ),
    key_pairs = c(table_name = chr, left = chr, right = chr, note = chr),
    one_to_many = c(
      contributor_label = chr, table_name = chr, attribute_name = chr, note = chr,
      source_cell = chr
    ),
    reach = c(table_name = chr, attribute_name = chr, how = chr),
    type_map = c(data_type = chr, r_class = chr, date_format = chr),
    sentinels = c(
      data_type = chr, role = chr, value = chr, allowed_in_pk = lgl, allowed_in_fk = lgl
    ),
    clashes = c(
      rule_id = chr, kind = chr, key_col = chr, key_value = chr, column_name = chr,
      source_a = chr, value_a = chr, source_b = chr, value_b = chr, winner = chr,
      basis = chr, source_cell_a = chr, source_cell_b = chr
    ),
    read_findings = c(
      rule_id = chr, input = chr, file = chr, detail = chr, source_cell = chr
    ),
    manifest = c(input = chr, file = chr, file_date = chr, sha256 = chr)
  )
}

#' The columns of the precedence, lineage-spec, site-ID-band, key-pair, one-to-many and reach inputs
#' @noRd
spec_input_schema <- function() {
  list(
    precedence = c("sheet", "key_col", "attribute_name", "winner", "blank_rule"),
    lineage_spec = c(
      "contributor_label", "table_name", "attribute_name", "spec_type", "source_text",
      "note", "source_cell"
    ),
    id_bands = c(
      "sheet", "label_col", "start_col", "end_col", "labels_from", "reserved_pattern", "table",
      "attribute", "key"
    ),
    key_pairs = c("table_name", "left", "right", "note"),
    one_to_many = c("contributor_label", "table_name", "attribute_name", "note"),
    reach = c("table_name", "attribute_name", "how")
  )
}

#' An empty data.table with a schema's columns
#' @noRd
empty_table <- function(schema) {
  as.data.table(lapply(schema, function(cls) vector(if (cls == "numeric") "double" else cls, 0L)))
}

#' Tables bound onto an empty component, so the result always has its columns
#' @noRd
bind_component <- function(name, parts) {
  rbindlist(c(list(empty_table(spec_schema()[[name]])), parts), use.names = TRUE)
}

#' The gpq_spec validator (D12.14)
#'
#' Checks the components with validate_gpq_spec() and drops the tables' indices (D12.27).
#' Both gpq_read_spec() and read_compiled_spec() end here.
#' @noRd
new_gpq_spec <- function(components) {
  validate_gpq_spec(components)
  # The tables here are the reader's own, never the caller's: dropping their indices
  # keeps identical() exact however the spec was built (D12.27).
  for (component in components) {
    setindex(component, NULL)
  }
  structure(components, class = "gpq_spec")
}

#' A spec's components checked, changing nothing (D12.14, D12.54)
#'
#' Their names, order, columns and classes, and that no character column holds an empty
#' string; stops on the first problem. It reads only, so gpq_preflight() can check the
#' caller's spec with it.
#' @noRd
validate_gpq_spec <- function(components) {
  schema <- spec_schema()
  if (!identical(names(components), names(schema))) {
    stop(
      "A gpq_spec has the components ", paste(names(schema), collapse = ", "),
      ", in that order.",
      call. = FALSE
    )
  }
  for (name in names(schema)) {
    component <- components[[name]]
    if (!is.data.table(component)) {
      stop(sprintf("Component %s must be a data.table.", name), call. = FALSE)
    }
    expected <- schema[[name]]
    classes <- vapply(component, function(x) class(x)[[1L]], character(1))
    if (length(expected) > 0L && !identical(names(component), names(expected))) {
      stop(sprintf(
        "Component %s has the columns %s.", name, paste(names(expected), collapse = ", ")
      ), call. = FALSE)
    }
    wanted <- if (length(expected) > 0L) expected else rep("character", length(classes))
    wrong <- names(component)[classes != wanted]
    if (length(wrong) > 0L) {
      stop(sprintf(
        "Component %s has columns of the wrong class: %s.", name, paste(wrong, collapse = ", ")
      ), call. = FALSE)
    }
    # An internal invariant: the reader turns every blank into NA (blank_to_na(),
    # D12.27), so only a component built some other way can hold an empty string.
    for (column in names(component)[classes == "character"]) {
      x <- component[[column]]
      if (any(!is.na(x) & !nzchar(x))) {
        stop(sprintf(
          "Component %s, column %s, holds an empty string; a blank is NA.", name, column
        ), call. = FALSE)
      }
    }
  }
  invisible(NULL)
}

#' One table input read as text
#'
#' A data.frame (with its origin, D12.33), a CSV path, or for the dictionary an `.xlsx`
#' path (its first sheet); returns `list(data, where, manifest, invalid, findings,
#' blank_header)`, `blank_header` TRUE for each column whose header is blank, which `data`
#' names V<j> in every form (D12.65).
#' @noRd
read_input_table <- function(x, input, origin = NULL) {
  path <- NA_character_
  if (is.data.frame(x)) {
    # An origin is checked against the data.frame as given, before its names are fixed
    # (D12.33); without one, the input is located as a data.frame (D12.28).
    where <- list(kind = "memory", file = "in memory")
    if (!is.null(origin)) {
      where <- origin_where(origin, x, input)
      path <- origin$path
    }
    # A caller's error: a matrix column would repeat the rows, a list column hold R's
    # code as text (R6).
    plain <- vapply(x, function(column) {
      is.atomic(column) && is.null(dim(column)) && length(column) == nrow(x)
    }, logical(1))
    if (!all(plain)) {
      stop(sprintf(
        "`%s` must have one plain value per row in every column; %s doesn't.", input,
        paste(names(x)[!plain], collapse = ", ")
      ), call. = FALSE)
    }
    # The caller's columns stay the caller's: as.data.table() copies each column of the list
    # it is given, so fix_invalid_utf8() below, which rewrites cells with set(), changes the
    # reader's table and never a caller's vector. Pinned by the test "a caller's table is
    # left as it was" (D12.27).
    data <- as.data.table(lapply(x, as_text))
    # Names as a CSV file's are read: a blank one V<j>, then each unique (R7, D12.54).
    blank_header <- logical(ncol(data))
    if (ncol(data) > 0L) {
      names_in <- names(x)
      blank_header <- is.na(names_in) | is_blank(names_in)
      names_in[blank_header] <- paste0("V", which(blank_header))
      setnames(data, make.unique(names_in))
    }
    invalid <- fix_invalid_utf8(data)
    malformed <- NULL
  } else if (!is.null(origin)) {
    stop(sprintf("`%s` is given as a path, so it takes no origin.", input), call. = FALSE)
  } else if (is_path_to(x, "csv")) {
    read <- read_csv_text(x)
    data <- read$data
    invalid <- read$invalid
    blank_header <- read$blank_header
    # A header of spaces only is blank too: named V<j>, as fread() names an empty one, then
    # each name made unique, as a data.frame's are (R7, D12.65).
    if (any(blank_header)) {
      names_in <- names(data)
      names_in[blank_header] <- paste0("V", which(blank_header))
      setnames(data, make.unique(names_in))
    }
    # Rows are located by the file line each starts on (D12.54).
    where <- list(kind = "csv", file = basename(x), row_map = c(1L, read$lines))
    malformed <- malformed_findings(read$malformed, input, basename(x))
    path <- x
  } else if (input == "dictionary" && is_path_to(x, "xlsx")) {
    # Only the dictionary is read from a workbook, its first sheet (D12.22, D12.34);
    # code_lists reads its workbook itself.
    sheet <- workbook_sheets(x)[[1L]]
    raw <- read_xlsx_raw(x, sheet)
    # The header is the raw table's row 1, so it is checked with the cells; rows then
    # count as fix_invalid_utf8() counts them, the header as row 0 (D12.28).
    invalid <- fix_invalid_utf8(raw)
    set(invalid, j = "row", value = invalid$row - 1L)
    blank_header <- if (nrow(raw) > 0L) is.na(unlist(raw[1L], use.names = FALSE)) else logical()
    data <- raw_to_table(raw)
    where <- list(kind = "xlsx", file = basename(x), sheet = sheet)
    malformed <- NULL
    path <- x
  } else {
    forms <- if (input == "dictionary") ".csv or .xlsx file" else ".csv file"
    stop(sprintf(
      "`%s` must be a data.frame or the path of an existing %s.", input, forms
    ), call. = FALSE)
  }
  list(
    data = data, where = where, manifest = manifest_row(input, path), invalid = invalid,
    findings = bind_component("read_findings", list(
      encoding_findings(invalid, names(data), input, where), malformed
    )),
    blank_header = blank_header
  )
}

#' Whether x is the path of an existing file, not a folder, with this extension
#' @noRd
is_path_to <- function(x, extension) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl(paste0("[.]", extension, "$"), tolower(x)) && file.exists(x) && !dir.exists(x)
}

#' A data.frame input's origin, checked, as the reader's `where` (D12.33)
#'
#' The origin is the file the data.frame came from. With rows, the input is located as
#' that file is, a workbook's cells when a sheet is given and a CSV's lines otherwise,
#' through row_map (the file row of each raw row, the header first) and col_map (the file
#' column of each column); without rows the file is named and the input is located as a
#' data.frame.
#' @noRd
origin_where <- function(origin, x, input) {
  fields <- c("path", "sheet", "rows", "columns")
  shaped <- is.list(origin) && !is.data.frame(origin) && !is.null(names(origin)) &&
    all(names(origin) %in% fields)
  if (!shaped) {
    stop(sprintf(
      "The origin of %s must be list(path = , sheet = , rows = , columns = ).", input
    ), call. = FALSE)
  }
  path <- origin$path
  path_ok <- is.character(path) && length(path) == 1L && !is.na(path) && file.exists(path) &&
    !dir.exists(path)
  if (!path_ok) {
    stop(sprintf("The origin of %s must name an existing file as its path.", input),
      call. = FALSE
    )
  }
  sheet <- origin$sheet
  sheet_ok <- is.null(sheet) || is.character(sheet) && length(sheet) == 1L && !is.na(sheet) &&
    !is_blank(sheet)
  if (!sheet_ok) {
    stop(sprintf("The origin of %s must give its sheet as NULL or one name.", input),
      call. = FALSE
    )
  }
  rows <- origin$rows
  n <- nrow(x)
  if (!is.null(rows)) {
    # Every file row is a whole number from 2 up to the integer maximum; one number must
    # also leave room for the last data row.
    whole <- is.numeric(rows) && length(rows) > 0L && !anyNA(rows) &&
      all(rows == round(rows) & rows >= 2 & rows <= .Machine$integer.max)
    fits <- !whole || length(rows) != 1L || rows + (n - 1) <= .Machine$integer.max
    if (!whole || !fits || !(length(rows) == 1L || length(rows) == n)) {
      stop(sprintf(
        paste(
          "The origin of %s must give its rows as NULL, the file row of the first data row",
          "(2 or more), or one file row per data row; no row can pass the integer maximum, %d."
        ),
        input, .Machine$integer.max
      ), call. = FALSE)
    }
  }
  col_map <- origin_columns(origin$columns, names(x), input)
  if (is.null(rows)) {
    return(list(kind = "memory", file = basename(path)))
  }
  data_rows <- if (length(rows) == 1L) rows + (seq_len(n) - 1L) else rows
  list(
    kind = if (is.null(sheet)) "csv" else "xlsx", file = basename(path), sheet = sheet,
    row_map = as.integer(c(rows[[1L]] - 1L, data_rows)), col_map = col_map
  )
}

#' An origin's file column for each column of the data.frame (D12.33)
#'
#' `columns` is NULL for the file's order, or the file column, by number or letter, of
#' every column of the data.frame, each named once and no two the same; every problem is
#' named in one message.
#' @noRd
origin_columns <- function(columns, names, input) {
  if (is.null(columns)) {
    return(seq_along(names))
  }
  numbers <- if (is.character(columns)) column_numbers(columns) else columns
  numbered <- !is.null(names(columns)) && is.numeric(numbers) && !anyNA(numbers) &&
    all(numbers >= 1 & numbers <= .Machine$integer.max & numbers == round(numbers))
  if (!numbered) {
    stop(sprintf(paste(
      "The origin of %s must give its columns as NULL or file columns, numbers or letters,",
      "named by the data.frame's columns."
    ), input), call. = FALSE)
  }
  numbers <- as.integer(numbers)
  names(numbers) <- names(columns)
  unknown <- setdiff(names(numbers), names)
  unplaced <- setdiff(names, names(numbers))
  repeated <- unique(numbers[duplicated(numbers)])
  named_twice <- unique(names(numbers)[duplicated(names(numbers))])
  problems <- c(
    if (length(unknown) > 0L) paste("not a column:", paste(unknown, collapse = ", ")),
    if (length(unplaced) > 0L) paste("not placed:", paste(unplaced, collapse = ", ")),
    if (length(repeated) > 0L) {
      paste("position given twice:", paste(repeated, collapse = ", "))
    },
    if (length(named_twice) > 0L) {
      paste("name given twice:", paste(named_twice, collapse = ", "))
    }
  )
  if (length(problems) > 0L) {
    stop(sprintf(
      "In the origin of %s, `columns` must place every column of the data once: %s.",
      input, paste(problems, collapse = "; ")
    ), call. = FALSE)
  }
  unname(numbers[names])
}

#' Raw rows as the file's rows (D12.33)
#'
#' Raw rows count the header as row 1 and data row r as r + 1: the same numbers for a file
#' the reader read, an origin's row_map for a data.frame that came from a file.
#' @noRd
file_rows <- function(where, rows) {
  if (is.null(where$row_map)) rows else where$row_map[rows]
}

#' Column positions as the file's columns, through an origin's col_map (D12.33)
#' @noRd
file_columns <- function(where, columns) {
  if (is.null(where$col_map)) columns else where$col_map[columns]
}

#' The spec_encoding_invalid findings of one input's invalid cells and names
#' @noRd
encoding_findings <- function(invalid, columns, input, where) {
  if (nrow(invalid) == 0L) {
    return(empty_table(spec_schema()$read_findings))
  }
  rows <- invalid$row + 1L
  value <- shorten_marked(invalid$value)
  location <- location_text(where, input, rows, invalid$column, columns[invalid$column])
  # A data.frame's column name has a text of its own (D12.45).
  header <- where$kind == "memory" & invalid$row == 0L
  data.table(
    rule_id = "spec_encoding_invalid", input = input, file = where$file,
    detail = fifelse(
      header,
      report_text(
        "preflight_detail_spec_encoding_invalid_header",
        column = invalid$column, input = input, value = value
      ),
      report_text("preflight_detail_spec_encoding_invalid", location = location, value = value)
    ),
    source_cell = cell_references(where, rows, invalid$column)
  )
}

#' The spec_csv_malformed findings of one CSV file's malformed lines (D12.54, D12.55)
#'
#' `{n}` is the header's fields for a "fields" problem and the file's records after its
#' header for a "short" one; an "unknown" problem's `{value}` is fread()'s warning. Neither
#' of those two has a line, so neither has a cell (D12.58).
#' @noRd
malformed_findings <- function(malformed, input, file) {
  if (nrow(malformed) == 0L) {
    return(empty_table(spec_schema()$read_findings))
  }
  n <- fifelse(malformed$kind == "short", malformed$n_records, malformed$fields)
  detail <- vapply(seq_len(nrow(malformed)), function(k) {
    report_text(
      paste0("preflight_detail_spec_csv_malformed_", malformed$kind[[k]]),
      file = file, line = malformed$line[[k]], n = n[[k]], n_read = malformed$n_read[[k]],
      value = malformed$value[[k]]
    )
  }, character(1))
  data.table(
    rule_id = "spec_csv_malformed", input = input, file = file, detail = detail,
    source_cell = fifelse(is.na(malformed$line), NA_character_, paste0(file, ":", malformed$line))
  )
}

#' Where a finding is, in words, by input form (D12.28)
#'
#' A CSV by file, line and column, the header being line 1 and its columns given by
#' position; a workbook by cell; a data.frame by its input name and its row as R counts
#' it, `df[row, ]` (a data.frame's column name has its own detail text, D12.45). `rows`
#' count the header as 1, as cell_references() does; an origin's maps, or a CSV's file
#' lines, give the file's row and column (D12.33, D12.54).
#' @noRd
location_text <- function(where, input, rows, columns, names) {
  header <- rows == 1L
  switch(where$kind,
    csv = report_text(
      "location_csv",
      file = where$file, line = file_rows(where, rows),
      column = fifelse(header, as.character(file_columns(where, columns)), names)
    ),
    xlsx = report_text("location_xlsx", cell = cell_references(where, rows, columns)),
    memory = report_text("location_memory", input = input, row = rows - 1L, column = names)
  )
}

#' A workbook's sheet names, readxl being required (classed error otherwise)
#' @noRd
workbook_sheets <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(structure(
      class = c("gpq_missing_package_error", "error", "condition"),
      list(
        message = "Reading an .xlsx file needs the readxl package; install it or pass data.frames.",
        call = NULL
      )
    ))
  }
  readxl::excel_sheets(path)
}

#' Every cell of a sheet from A1 as text, columns V1, V2, ..., spaces kept
#' @noRd
read_xlsx_raw <- function(path, sheet) {
  raw <- readxl::read_excel(
    path,
    sheet = sheet, col_names = FALSE, col_types = "text", trim_ws = FALSE, na = "",
    range = readxl::cell_limits(c(1L, 1L), c(NA, NA)), .name_repair = "minimal"
  )
  raw <- as.data.table(lapply(raw, as_text))
  if (ncol(raw) > 0L) {
    setnames(raw, paste0("V", seq_len(ncol(raw))))
  }
  raw
}

#' A raw sheet as a table, its row 1 as the names
#' @noRd
raw_to_table <- function(raw) {
  if (nrow(raw) == 0L) {
    return(data.table())
  }
  header <- unlist(raw[1L], use.names = FALSE)
  header[is.na(header)] <- paste0("V", which(is.na(header)))
  data <- raw[-1L]
  setnames(data, make.unique(header))
  data
}

#' Cell references by input form: file:row for a CSV, sheet!A1 for a workbook, NA in memory
#' @noRd
cell_references <- function(where, rows, columns) {
  switch(where$kind,
    memory = rep(NA_character_, length(rows)),
    csv = paste0(where$file, ":", file_rows(where, rows), recycle0 = TRUE),
    xlsx = paste0(
      where$sheet, "!", column_letters(file_columns(where, columns)), file_rows(where, rows),
      recycle0 = TRUE
    )
  )
}

#' The YYYYMMDD date in a file's name, else NA
#'
#' Eight digits not inside a longer run of digits, and a real date (R8).
#' @noRd
file_date <- function(name) {
  found <- regmatches(name, gregexpr("(?<![0-9])[0-9]{8}(?![0-9])", name, perl = TRUE))[[1L]]
  dated <- found[!is.na(as.Date(found, format = "%Y%m%d"))]
  if (length(dated) == 0L) NA_character_ else dated[[1L]]
}

#' One manifest row: the file's name, date and SHA-256, or "in memory"
#' @noRd
manifest_row <- function(input, path = NA_character_) {
  if (is.na(path)) {
    return(data.table(
      input = input, file = "in memory", file_date = NA_character_, sha256 = NA_character_
    ))
  }
  data.table(
    input = input, file = basename(path), file_date = file_date(basename(path)),
    sha256 = unname(tools::sha256sum(path))
  )
}

#' An "in memory" manifest row for an input given, NULL for one not given
#' @noRd
memory_row <- function(x, input) {
  if (is.null(x)) NULL else manifest_row(input)
}

#' Which of the column map's type names the dictionary has: one, none or several
#' @noRd
detect_type_column <- function(columns, candidates) {
  present <- intersect(candidates, columns)
  status <- if (length(present) == 1L) "ok" else if (length(present) == 0L) "none" else "both"
  column <- if (length(present) > 0L) present[[1L]] else NA_character_
  list(column = column, status = status, present = present)
}

#' The dictionary as the attributes component, with its read findings
#'
#' Stops, a caller's error, when the table or attribute column is missing (D12.33).
#' @noRd
read_dictionary <- function(table, column_map, type_map, id_pattern = NULL) {
  data <- table$data
  columns <- names(data)
  # A caller's error, not a spec defect: one message names every missing column, its
  # role in the column map, the file and the columns found (D12.33).
  wanted <- c(table = column_map$table, attribute = column_map$attribute)
  absent <- wanted[!wanted %in% columns]
  if (length(absent) > 0L) {
    stop(sprintf(
      "The dictionary (%s) has %s; its columns are %s.", table$where$file,
      paste0("no column ", absent, " (the column map's ", names(absent), ")",
        collapse = " and "
      ),
      paste(columns, collapse = ", ")
    ), call. = FALSE)
  }
  type <- detect_type_column(columns, column_map$type)
  findings <- empty_table(spec_schema()$read_findings)
  if (type$status != "ok") {
    named <- if (type$status == "none") column_map$type else type$present
    findings <- data.table(
      rule_id = "dd_type_column_ambiguous", input = "dictionary", file = table$where$file,
      detail = report_text(
        paste0("preflight_detail_dd_type_column_ambiguous_", type$status),
        columns = paste(named, collapse = ", ")
      ),
      source_cell = NA_character_
    )
  }
  n <- nrow(data)
  pick <- function(column) {
    if (!is.na(column) && column %in% columns) data[[column]] else rep(NA_character_, n)
  }
  flag <- column_map$lineage_flag
  lineage_flag <- if (is.null(flag) || !flag[["column"]] %in% columns) {
    rep(NA, n)
  } else {
    !is.na(data[[flag[["column"]]]]) & data[[flag[["column"]]]] == flag[["value"]]
  }
  attribute <- data[[column_map$attribute]]
  data_type <- pick(type$column)
  id_marked <- if (is.null(id_pattern)) {
    rep(NA, n)
  } else {
    !is.na(attribute) & grepl(id_pattern, attribute)
  }
  attributes <- data.table(
    table_name = data[[column_map$table]], attribute_name = attribute,
    data_type = data_type, r_class = type_map$r_class[match(data_type, type_map$data_type)],
    key_type = pick(column_map$key_type), reference_table = pick(column_map$reference),
    lookup = pick(column_map$lookup), description = pick(column_map$description),
    lineage_flag = lineage_flag,
    id_marked = id_marked,
    # The row in the dictionary's file, through an origin's rows (D12.33); a data.frame
    # without file rows has none to give, so D12.25's <file>:<row> is NA for it.
    source_row = if (table$where$kind == "memory") {
      rep(NA_integer_, n)
    } else {
      file_rows(table$where, seq_len(n) + 1L)
    }
  )
  # The range and format columns, typed, and a finding per cell that doesn't read (D18.9).
  formats <- read_dd_formats(data, column_map, attributes, table$where)
  attributes <- cbind(attributes, formats$columns)
  list(attributes = attributes, findings = rbindlist(list(findings, formats$findings)))
}

#' The dictionary's range and format columns typed, and their dd_format_invalid findings
#' (D18.9, D18.19, D18.28, R4, R13)
#'
#' A column the dictionary lacks is NA throughout. A bound is a decimal number, finite; a
#' flag TRUE or FALSE in any case, a blank one TRUE where its bound is given; a pattern one
#' that grepl() compiles; `max_length` a whole number of 1 or more and `decimals` of 0 or
#' more. A cell that doesn't read is one finding and NA. A bound or `decimals` on a type that
#' isn't a number type (integer or numeric), or `max_length` or `pattern` on one that is, is
#' one finding and NA; an attribute of a type the type map lacks is left to
#' `dd_type_unknown`. A minimum above its maximum is one finding, and both are NA (R13).
#' Each finding names its cell, through the dictionary's origin (D12.33). Returns `columns`,
#' a data.table of the eight columns in the attributes' order, and `findings`.
#' @noRd
read_dd_formats <- function(data, column_map, attributes, where) {
  n <- nrow(data)
  present <- names(data)
  roles <- c(
    "min_value", "max_value", "min_inclusive", "max_inclusive", "pattern", "max_length",
    "decimals", "units"
  )
  text <- lapply(roles, function(role) {
    name <- column_map[[role]]
    if (!is.null(name) && name %in% present) data[[name]] else rep(NA_character_, n)
  })
  names(text) <- roles
  # The findings so far, held in an environment that finding() adds to.
  found <- new.env(parent = emptyenv())
  found$tables <- list(empty_table(spec_schema()$read_findings))
  # One finding per cell, at its cell; `values` fills the detail's other slots.
  finding <- function(role, rows, detail_id, values = list()) {
    if (length(rows) == 0L) {
      return(invisible(NULL))
    }
    column <- match(column_map[[role]], present)
    detail <- do.call(report_text, c(
      list(
        paste0("preflight_detail_dd_format_invalid_", detail_id),
        table_name = attributes$table_name[rows],
        attribute_name = attributes$attribute_name[rows], column = column_map[[role]],
        value = text[[role]][rows]
      ),
      values
    ))
    found$tables[[length(found$tables) + 1L]] <- data.table(
      rule_id = "dd_format_invalid", input = "dictionary", file = where$file, detail = detail,
      source_cell = cell_references(where, rows + 1L, rep(column, length(rows)))
    )
  }
  number_type <- attributes$r_class %chin% c("integer", "double")
  known_type <- !is.na(attributes$r_class)
  # A cell of a type it doesn't fit is one finding, its value NA.
  fits <- function(role, value, wants_number) {
    wrong <- which(!is.na(value) & known_type & number_type != wants_number)
    finding(
      role, wrong, if (wants_number) "type_number" else "type_text",
      list(data_type = attributes$data_type[wrong])
    )
    value[wrong] <- NA
    value
  }
  as_bound <- function(role) {
    x <- text[[role]]
    decimal <- grepl("^ *[-+]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][-+]?[0-9]+)? *$", x)
    number <- suppressWarnings(as.numeric(x))
    readable <- decimal & is.finite(number)
    finding(role, which(!is.na(x) & !readable), "number")
    fits(role, fifelse(readable, number, NA_real_), TRUE)
  }
  as_whole <- function(role, lowest) {
    x <- text[[role]]
    number <- suppressWarnings(as.numeric(x))
    whole <- grepl("^ *[0-9]+ *$", x) & !is.na(number) & number >= lowest &
      number <= .Machine$integer.max
    finding(role, which(!is.na(x) & !whole), "whole", list(n = lowest))
    out <- rep(NA_integer_, n)
    out[whole] <- as.integer(number[whole])
    out
  }
  min_value <- as_bound("min_value")
  max_value <- as_bound("max_value")
  inverted <- which(!is.na(min_value) & !is.na(max_value) & min_value > max_value)
  finding(
    "min_value", inverted, "order",
    list(value_a = text$min_value[inverted], value_b = text$max_value[inverted])
  )
  min_value[inverted] <- NA_real_
  max_value[inverted] <- NA_real_
  as_flag <- function(role, bound) {
    x <- toupper(trimws(text[[role]]))
    flag <- fifelse(x == "TRUE", TRUE, fifelse(x == "FALSE", FALSE, NA))
    finding(role, which(!is.na(x) & is.na(flag)), "flag")
    # A blank flag is TRUE where its bound is given (D18.9).
    flag[is.na(x) & !is.na(bound)] <- TRUE
    flag
  }
  pattern <- text$pattern
  distinct <- unique(pattern[!is.na(pattern)])
  compiles <- vapply(distinct, function(p) {
    tryCatch(is.logical(grepl(p, "")), error = function(e) FALSE, warning = function(w) FALSE)
  }, logical(1))
  broken <- which(pattern %chin% distinct[!compiles])
  finding("pattern", broken, "pattern")
  pattern[broken] <- NA_character_
  columns <- data.table(
    min_value = min_value, max_value = max_value,
    min_inclusive = as_flag("min_inclusive", min_value),
    max_inclusive = as_flag("max_inclusive", max_value),
    pattern = fits("pattern", pattern, FALSE),
    max_length = fits("max_length", as_whole("max_length", 1L), FALSE),
    decimals = fits("decimals", as_whole("decimals", 0L), TRUE),
    units = text$units
  )
  list(columns = columns, findings = rbindlist(found$tables))
}

#' The keys component from the attributes: PK parts numbered, FK targets read
#'
#' An FK whose target table has no PK keeps its row with reference_attribute empty;
#' dd_pk_missing and dd_fk_target_missing report it (D12.26).
#' @noRd
build_keys <- function(attributes) {
  kind <- toupper(attributes$key_type)
  keyed <- attributes[kind %chin% c("PK", "FK"), list(
    table_name, attribute_name,
    key_type = toupper(key_type), reference_table
  )]
  if (nrow(keyed) == 0L) {
    return(empty_table(spec_schema()$keys))
  }
  keyed[, key_part := fifelse(key_type == "PK", cumsum(key_type == "PK"), 1L), by = table_name]
  keyed[key_type == "PK", reference_table := NA_character_]
  keyed[, `:=`(n_ref = NA_integer_, ref_pk = NA_character_)]
  pk_rows <- keyed[key_type == "PK"]
  # An FK whose target table has no PK keeps its row with reference_attribute empty;
  # dd_pk_missing reports the table and dd_fk_target_missing the FK (D12.26). With no
  # PK anywhere there is nothing to look up. A PK row with no table name isn't left in the
  # lookup, or an FK with no reference table would join to it, NA to NA.
  pk_rows <- pk_rows[!is.na(table_name)]
  if (nrow(pk_rows) > 0L) {
    pk <- pk_rows[, list(n_pk = .N, pk = attribute_name[[1L]]), by = table_name]
    keyed[pk, on = list(reference_table = table_name), `:=`(n_ref = i.n_pk, ref_pk = i.pk)]
  }
  keyed[, reference_attribute := fifelse(
    key_type == "FK" & !is.na(n_ref) & n_ref == 1L, ref_pk, NA_character_
  )]
  keyed[, list(
    table_name, attribute_name, key_type,
    key_part = as.integer(key_part), reference_table, reference_attribute
  )]
}

#' A raw table, row 1 its header, as long cells with their references
#'
#' NULL for an empty table.
#' @noRd
sheet_cells <- function(raw, name, where, name_col = "sheet", column_col = "sheet_column") {
  n <- nrow(raw)
  k <- ncol(raw)
  if (n == 0L || k == 0L) {
    return(NULL)
  }
  header <- unlist(raw[1L], use.names = FALSE)
  rows <- rep(seq_len(n), times = k)
  columns <- rep(seq_len(k), each = n)
  out <- data.table(
    name, rows, rep(header, each = n), unlist(raw, use.names = FALSE),
    cell_references(where, rows, columns)
  )
  setnames(out, c(name_col, "source_row", column_col, "value", "source_cell"))
  out
}

#' A table's names put back as its first row
#' @noRd
with_header <- function(data) {
  rbindlist(list(as.list(names(data)), data), use.names = FALSE)
}

#' The code lists in long form, with every sheet's name, manifest rows and findings
#' @noRd
read_code_lists <- function(x, origins = NULL) {
  # `sheets` lists every sheet in reading order, a completely empty one included, which
  # has no cells in `long` (D12.29). A sheet given as a data.frame may have an origin
  # (D12.33).
  if (is.null(x)) {
    return(list(
      long = empty_table(spec_schema()$code_lists), sheets = character(), manifest = NULL,
      findings = NULL
    ))
  }
  if (is.character(x) && length(x) == 1L) {
    if (!is_path_to(x, "xlsx")) {
      stop("`code_lists` names no existing .xlsx file.", call. = FALSE)
    }
    sheets <- workbook_sheets(x)
    parts <- lapply(sheets, function(sheet) {
      where <- list(kind = "xlsx", file = basename(x), sheet = sheet)
      raw <- read_xlsx_raw(x, sheet)
      # Every sheet is checked for invalid bytes, its header row included (D12.24,
      # D12.28); rows then count the header as row 0, as fix_invalid_utf8() does.
      invalid <- fix_invalid_utf8(raw)
      set(invalid, j = "row", value = invalid$row - 1L)
      header <- if (nrow(raw) > 0L) unlist(raw[1L], use.names = FALSE) else character()
      # A blank header cell is named V<j> and the names made unique, as in every input form
      # (D12.54, D12.61); its row-1 cell is then put back to NA, so pre-flight sees a column
      # without a header (D12.64).
      blank <- which(is.na(header))
      unique_header <- header
      unique_header[blank] <- paste0("V", blank)
      unique_header <- make.unique(unique_header)
      for (j in which(is.na(header) | unique_header != header)) {
        set(raw, i = 1L, j = j, value = unique_header[[j]])
      }
      long <- sheet_cells(raw, sheet, where)
      if (length(blank) > 0L) {
        long[source_row == 1L & sheet_column %chin% unique_header[blank], value := NA_character_]
      }
      list(
        long = long,
        findings = encoding_findings(invalid, unique_header, "code_lists", where)
      )
    })
    return(list(
      long = bind_component("code_lists", lapply(parts, `[[`, "long")), sheets = sheets,
      manifest = manifest_row("code_lists", x),
      findings = rbindlist(lapply(parts, `[[`, "findings"))
    ))
  }
  if (!is.list(x) || is.data.frame(x) || is.null(names(x))) {
    stop(paste(
      "`code_lists` must be the path of an .xlsx workbook or a named list of data.frames",
      "or CSV paths."
    ), call. = FALSE)
  }
  # A caller's error: a repeated name would read one table twice and lose the other (R11);
  # a name given in R must be valid UTF-8 (D12.54).
  sheets <- names(x)
  names_ok <- !anyNA(sheets) && !any(is_blank(sheets)) && anyDuplicated(sheets) == 0L &&
    all(validUTF8(sheets))
  if (!names_ok) {
    stop(
      "`code_lists`'s names must be unique sheet names, none blank, all valid UTF-8.",
      call. = FALSE
    )
  }
  parts <- lapply(seq_along(x), function(k) {
    sheet <- sheets[[k]]
    input <- paste0("code_lists:", sheet)
    table <- read_input_table(x[[k]], input, origins[[input]])
    long <- sheet_cells(with_header(table$data), sheet, table$where)
    # A blank header keeps its V<j> as sheet_column, its row-1 cell NA, as in a workbook
    # (D12.64, D12.65).
    if (any(table$blank_header)) {
      blank <- names(table$data)[table$blank_header]
      long[source_row == 1L & sheet_column %chin% blank, value := NA_character_]
    }
    list(long = long, manifest = table$manifest, findings = table$findings)
  })
  list(
    long = bind_component("code_lists", lapply(parts, `[[`, "long")), sheets = names(x),
    manifest = rbindlist(lapply(parts, `[[`, "manifest")),
    findings = rbindlist(lapply(parts, `[[`, "findings"))
  )
}

#' The translation tables in long form, with their declared code columns and filters
#' @noRd
read_crosswalks <- function(x, origins = NULL) {
  if (is.null(x)) {
    return(list(
      long = empty_table(spec_schema()$crosswalks), declared = list(),
      manifest = NULL, findings = NULL
    ))
  }
  if (!is.list(x) || is.data.frame(x) || is.null(names(x))) {
    stop("`crosswalks` must be a named list.", call. = FALSE)
  }
  # As for code_lists: unique names, none blank, valid UTF-8 (R11, D12.54).
  walks <- names(x)
  names_ok <- !anyNA(walks) && !any(is_blank(walks)) && anyDuplicated(walks) == 0L &&
    all(validUTF8(walks))
  if (!names_ok) {
    stop(
      "`crosswalks`'s names must be unique table names, none blank, all valid UTF-8.",
      call. = FALSE
    )
  }
  parts <- lapply(seq_along(x), function(k) {
    read_one_crosswalk(walks[[k]], x[[k]], origins[[paste0("crosswalks:", walks[[k]])]])
  })
  declared <- lapply(parts, `[[`, "declared")
  names(declared) <- walks
  list(
    long = bind_component("crosswalks", lapply(parts, `[[`, "long")), declared = declared,
    manifest = rbindlist(lapply(parts, `[[`, "manifest")),
    findings = rbindlist(lapply(parts, `[[`, "findings"))
  )
}

#' One translation table: a data.frame (with its origin) or a CSV path
#'
#' A table that can't be read, or isn't UTF-8, is a crosswalk_unreadable finding.
#' @noRd
read_one_crosswalk <- function(name, element, origin = NULL) {
  input <- paste0("crosswalks:", name)
  if (!is.list(element) || is.data.frame(element)) {
    element <- list(table = element)
  }
  fields <- c("table", "code_col", "filter_col", "filter_values")
  # A filter needs both its column and its values (D12.54).
  shapeless <- is.null(element$table) || !all(names(element) %in% fields) ||
    xor(is.null(element$filter_values), is.null(element$filter_col))
  if (shapeless) {
    stop(sprintf(paste(
      "Crosswalk %s must be a table or list(table = , code_col = , filter_col = ,",
      "filter_values = )."
    ), name), call. = FALSE)
  }
  # The code-list map keeps filter values "; "-joined, so none may hold "; " or be blank.
  filter_text <- as_text(unlist(element$filter_values, use.names = FALSE))
  if (anyNA(filter_text) || any(grepl("; ", filter_text, fixed = TRUE))) {
    stop(sprintf(
      "Crosswalk %s's filter_values can't be blank or NA, or hold \"; \".", name
    ), call. = FALSE)
  }
  declared <- list(
    code_col = element$code_col, filter_col = element$filter_col,
    filter_values = element$filter_values
  )
  unreadable <- function(file, detail_id, lines = NA_integer_) {
    data.table(
      rule_id = "crosswalk_unreadable", input = input, file = file,
      detail = report_text(detail_id, crosswalk = name, line = lines),
      source_cell = if (all(is.na(lines))) NA_character_ else paste0(file, ":", lines)
    )
  }
  table <- element$table
  if (is.data.frame(table)) {
    read <- read_input_table(table, input, origin)
    bad <- read$invalid
    where <- read$where
    findings <- NULL
    if (nrow(bad) > 0L) {
      # A table with an origin's file rows is located as that file is (D12.33); one
      # without has its own text naming the table, a row as R counts it (D12.28, D12.29).
      detail <- if (where$kind == "memory") {
        fifelse(
          bad$row == 0L,
          report_text(
            "preflight_detail_crosswalk_unreadable_memory_header",
            column = bad$column, crosswalk = name, value = shorten_marked(bad$value)
          ),
          report_text(
            "preflight_detail_crosswalk_unreadable_memory",
            crosswalk = name, row = bad$row, column = names(read$data)[bad$column],
            value = shorten_marked(bad$value)
          )
        )
      } else {
        report_text(
          "preflight_detail_spec_encoding_invalid",
          location = location_text(
            where, input, bad$row + 1L, bad$column, names(read$data)[bad$column]
          ),
          value = shorten_marked(bad$value)
        )
      }
      findings <- data.table(
        rule_id = "crosswalk_unreadable", input = input, file = where$file,
        detail = detail, source_cell = cell_references(where, bad$row + 1L, bad$column)
      )
    }
    long <- sheet_cells(with_header(read$data), name, where, "crosswalk", "crosswalk_column")
    return(list(long = long, declared = declared, manifest = read$manifest, findings = findings))
  }
  if (!is.character(table) || length(table) != 1L) {
    stop(sprintf("Crosswalk %s's table must be a data.frame or a CSV path.", name),
      call. = FALSE
    )
  }
  if (!is.null(origin)) {
    stop(sprintf("`%s` is given as a path, so it takes no origin.", input), call. = FALSE)
  }
  file <- basename(table)
  # read_csv_text() records and muffles every fread() warning as a malformed row (R12,
  # D12.58), so a table it found malformed can't be read as written.
  read <- if (file.exists(table) && !dir.exists(table)) {
    tryCatch(read_csv_text(table), error = function(e) NULL)
  }
  if (is.null(read) || nrow(read$malformed) > 0L) {
    return(list(
      long = NULL, declared = declared,
      manifest = manifest_row(input, table),
      findings = unreadable(file, "preflight_detail_crosswalk_unreadable_file")
    ))
  }
  # A header of spaces only is blank too: named V<j>, as fread() names an empty one, then
  # each name made unique, as read_input_table() names a data.frame's (R7, D12.65, D12.66).
  if (any(read$blank_header)) {
    names_in <- names(read$data)
    names_in[read$blank_header] <- paste0("V", which(read$blank_header))
    setnames(read$data, make.unique(names_in))
  }
  lines <- readLines(table, warn = FALSE, encoding = "UTF-8")
  bad <- which(!validUTF8(lines))
  # Cells are located by the file line each row starts on (D12.54).
  where <- list(kind = "csv", file = file, row_map = c(1L, read$lines))
  findings <- if (length(bad) > 0L) {
    unreadable(file, "preflight_detail_crosswalk_unreadable_encoding", bad)
  }
  list(
    long = sheet_cells(with_header(read$data), name, where, "crosswalk", "crosswalk_column"),
    declared = declared, manifest = manifest_row(input, table), findings = findings
  )
}

#' The code column: named after the attribute, else the sheet or table, else declared
#' @noRd
resolve_code_column <- function(attribute, name, headers, declared = NULL) {
  headers <- headers[!is.na(headers)]
  for (candidate in c(attribute, name, declared)) {
    if (candidate %in% headers) {
      return(candidate)
    }
  }
  NA_character_
}

#' The code-list map: each coded attribute's sheet or translation table and code column
#'
#' "Y", in any case, names the sheet after the attribute, otherwise the value names a
#' sheet, otherwise a translation table (D5.28, D12.5, D12.31).
#' @noRd
build_code_list_map <- function(attributes, code_lists, sheets, crosswalks, declared) {
  coded <- attributes[!is.na(lookup), list(table_name, attribute_name, lookup)]
  if (nrow(coded) == 0L) {
    return(empty_table(spec_schema()$code_list_map))
  }
  target <- fifelse(toupper(coded$lookup) == "Y", coded$attribute_name, coded$lookup)
  sheet_header <- code_lists$source_row == 1L
  sheet_headers <- split(code_lists$value[sheet_header], code_lists$sheet[sheet_header])
  table_header <- crosswalks$source_row == 1L
  table_headers <- split(crosswalks$value[table_header], crosswalks$crosswalk[table_header])
  resolved <- lapply(seq_len(nrow(coded)), function(i) {
    attribute <- coded$attribute_name[[i]]
    name <- target[[i]]
    # An empty sheet is still a sheet: it resolves with no code column (D12.29).
    if (name %in% sheets) {
      code_column <- resolve_code_column(attribute, name, sheet_headers[[name]])
      return(list("sheet", name, code_column, NA_character_, NA_character_))
    }
    if (name %in% names(declared)) {
      d <- declared[[name]]
      # A filter by attribute names every attribute that uses the table: a caller's error
      # otherwise, never the whole table unfiltered (D12.54).
      by_attribute <- is.list(d$filter_values)
      if (by_attribute && length(d$filter_values[[attribute]]) == 0L) {
        stop(sprintf(
          "Crosswalk %s's filter_values has no entry for attribute %s, which uses it.",
          name, attribute
        ), call. = FALSE)
      }
      values <- if (by_attribute) d$filter_values[[attribute]] else d$filter_values
      # A filter on a column the table lacks would give no codes silently (D12.54).
      # An unreadable or headerless table has no headers: no_code_column, not this error.
      headers <- table_headers[[name]]
      if (!is.null(values) && length(headers) > 0L && !d$filter_col %in% headers) {
        stop(sprintf(
          "Crosswalk %s's filter_col %s isn't a column of the table.", name, d$filter_col
        ), call. = FALSE)
      }
      code_column <- resolve_code_column(attribute, name, headers, d$code_col)
      return(list(
        "crosswalk", name, code_column,
        if (is.null(values)) NA_character_ else d$filter_col,
        if (is.null(values)) NA_character_ else paste(as_text(values), collapse = "; ")
      ))
    }
    list(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  })
  # One field of every resolution, by position: vapply() rather than transpose(), which
  # older data.table may not take a list of lists in (D12.39).
  field <- function(k) vapply(resolved, `[[`, character(1), k)
  map <- data.table(
    table_name = coded$table_name, attribute_name = coded$attribute_name, lookup = coded$lookup,
    source_type = field(1L), source_name = field(2L), code_column = field(3L),
    filter_column = field(4L), filter_values = field(5L)
  )
  map[, status := fifelse(
    is.na(source_type), "no_source",
    fifelse(is.na(code_column), "no_code_column", "resolved")
  )]
  map
}

#' The codes component: each resolved list's codes
#'
#' A translation table's list is the distinct codes of its code column, filtered where a
#' filter is declared (D12.16, D12.21).
#' @noRd
build_codes <- function(code_list_map, code_lists, crosswalks) {
  resolved <- code_list_map[status == "resolved"]
  codes <- lapply(seq_len(nrow(resolved)), function(i) {
    one <- resolved[i]
    if (one$source_type == "sheet") {
      in_list <- code_lists$sheet == one$source_name &
        code_lists$sheet_column == one$code_column & code_lists$source_row > 1L &
        !is.na(code_lists$value)
      cells <- code_lists[in_list]
    } else {
      rows <- crosswalks[crosswalk == one$source_name & source_row > 1L]
      cells <- rows[crosswalk_column == one$code_column & !is.na(value)]
      if (!is.na(one$filter_column)) {
        wanted <- strsplit(one$filter_values, "; ", fixed = TRUE)[[1L]]
        keep <- rows[crosswalk_column == one$filter_column & value %chin% wanted, source_row]
        cells <- cells[source_row %in% keep]
      }
      cells <- cells[!duplicated(value)]
    }
    if (nrow(cells) == 0L) {
      return(NULL)
    }
    data.table(
      table_name = one$table_name, attribute_name = one$attribute_name, code = cells$value,
      source_type = one$source_type, source_name = one$source_name, source_row = cells$source_row
    )
  })
  bind_component("codes", codes)
}

#' The id_bands and id_band_columns components and their site_id_range_invalid findings
#'
#' The reader makes every finding of the check, where the bands sheet's file is known
#' (D12.28): a missing sheet or column, a bad bound, a blank or unknown label, a band
#' starting after it ends, and two valid bands overlapping, citing both. `table`,
#' `attribute` and `key` come together or not at all (R8, R33); with them, also a banded
#' column the dictionary lacks, a key without `labels_from`, a key that isn't a chain of
#' foreign keys from `table` (parse_chain()), and a key whose last attribute isn't a column of
#' the `labels_from` sheet (D18.2, D18.18). Each band's `key_value` is that column's value on
#' the `labels_from` row of the band's label, NA for a reserved or unknown label or without a
#' key; each band after the first of its key value is a finding naming the first (D18.57).
#' `columns` has the one row of id_band_columns, `labels_from`'s sheet and column with it
#' (D18.38), when the three are given and none of the column's or the key's findings is made.
#' @noRd
build_id_bands <- function(id_bands, code_lists, file,
                           attributes = empty_table(spec_schema()$attributes),
                           keys = empty_table(spec_schema()$keys)) {
  out <- list(
    bands = empty_table(spec_schema()$id_bands),
    columns = empty_table(spec_schema()$id_band_columns),
    findings = empty_table(spec_schema()$read_findings)
  )
  if (is.null(id_bands)) {
    return(out)
  }
  # A caller's error unless each element has its shape and the pattern compiles (R10).
  one_name <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && !is_blank(x) && validUTF8(x)
  }
  from <- if (is.list(id_bands)) id_bands$labels_from
  from_ok <- is.null(from) || is.character(from) && length(from) == 2L &&
    setequal(names(from), c("sheet", "column")) && all(vapply(from, one_name, TRUE))
  pattern <- if (is.list(id_bands)) id_bands$reserved_pattern
  pattern_ok <- is.null(pattern) || one_name(pattern)
  if (pattern_ok && !is.null(pattern)) {
    pattern_ok <- tryCatch(
      is.logical(grepl(pattern, "")),
      error = function(e) FALSE, warning = function(w) FALSE
    )
  }
  # The six of M2, and the banded column's three together or not at all (R8, R33).
  banded <- c("table", "attribute", "key")
  required <- setdiff(spec_input_schema()$id_bands, banded)
  given <- if (is.list(id_bands)) names(id_bands) else character()
  with_column <- all(banded %in% given)
  named <- c("sheet", "label_col", "start_col", "end_col", if (with_column) banded)
  shaped <- is.list(id_bands) && all(required %in% given) &&
    all(given %in% spec_input_schema()$id_bands) && !anyDuplicated(given) &&
    (with_column || !any(banded %in% given)) &&
    all(vapply(id_bands[named], one_name, TRUE)) && from_ok && pattern_ok
  if (!shaped) {
    stop(paste(
      "`id_bands` is list(sheet, label_col, start_col, end_col, labels_from,",
      "reserved_pattern) and optionally table, attribute and key, all three or none: one name",
      "each for sheet, label_col, start_col, end_col, table, attribute and key; labels_from",
      "NULL or c(sheet = , column = ); reserved_pattern NULL or one regular expression."
    ), call. = FALSE)
  }
  finding <- function(detail, cell = NA_character_) {
    data.table(
      rule_id = "site_id_range_invalid", input = "code_lists", file = file,
      detail = detail, source_cell = cell
    )
  }
  needed <- c(id_bands$label_col, id_bands$start_col, id_bands$end_col)
  cells <- code_lists[sheet == id_bands$sheet]
  from_header <- if (!is.null(from)) {
    code_lists[sheet == from[["sheet"]] & source_row == 1L, value]
  }
  columns_missing <- !all(needed %in% cells[source_row == 1L, value]) ||
    !is.null(from) && !from[["column"]] %in% from_header
  if (columns_missing) {
    out$findings <- finding(report_text(
      "preflight_detail_site_id_range_invalid_sheet",
      sheet = id_bands$sheet, columns = paste(c(needed, from[["column"]]), collapse = ", ")
    ))
    return(out)
  }
  body <- cells[source_row > 1L & sheet_column %chin% needed]
  value_of <- function(column) {
    one <- body[sheet_column == column]
    one[match(sort(unique(body$source_row)), one$source_row)]
  }
  label <- value_of(id_bands$label_col)
  start <- value_of(id_bands$start_col)
  end <- value_of(id_bands$end_col)
  keep <- !(is.na(label$value) & is.na(start$value) & is.na(end$value))
  label <- label[keep]
  start <- start[keep]
  end <- end[keep]
  # A bound is whole only when written in decimal digits, finite and without a fraction:
  # "Inf" and "0x10" aren't (R16).
  as_bound <- function(x) {
    decimal <- grepl("^ *[-+]?[0-9]+([.][0-9]*)?([eE][-+]?[0-9]+)? *$", x)
    number <- suppressWarnings(as.numeric(x))
    fifelse(decimal & is.finite(number) & number == round(number), number, NA_real_)
  }
  bands <- data.table(
    label = label$value, band_start = as_bound(start$value), band_end = as_bound(end$value),
    reserved = !is.null(pattern) & !is.na(label$value) & grepl(pattern %||% "^$", label$value),
    source_row = label$source_row, source_cell = label$source_cell
  )
  # A band with a blank label gets only that finding, below; its bounds, order and
  # overlaps are checked once it has a label (D12.36, D12.37).
  labelled <- !is.na(bands$label)
  # One finding per bad bound, citing its own cell (D12.33): a blank bound has its own
  # text, one that isn't a whole number shows its value. Either leaves the band's bound
  # NA, so the order and overlap checks below leave the band out.
  blank_text <- c(
    start = "preflight_detail_site_id_range_invalid_blank_start",
    end = "preflight_detail_site_id_range_invalid_blank_end"
  )
  bounds <- list(start = start, end = end)
  # The bands' bounds as read above, each band's own, so as_bound() runs once per column.
  bound_values <- list(start = bands$band_start, end = bands$band_end)
  bound_findings <- do.call(c, lapply(names(bounds), function(side) {
    cells <- bounds[[side]]
    blank <- which(labelled & is.na(cells$value))
    not_whole <- which(labelled & !is.na(cells$value) & is.na(bound_values[[side]]))
    list(
      if (length(blank) > 0L) {
        finding(
          report_text(blank_text[[side]], label = bands$label[blank]),
          cells$source_cell[blank]
        )
      },
      if (length(not_whole) > 0L) {
        finding(
          report_text(
            "preflight_detail_site_id_range_invalid_bound",
            label = bands$label[not_whole], value = cells$value[not_whole]
          ),
          cells$source_cell[not_whole]
        )
      }
    )
  }))
  known <- if (is.null(from)) {
    bands$label
  } else {
    code_lists[
      sheet == from[["sheet"]] & sheet_column == from[["column"]] & source_row > 1L, value
    ]
  }
  # A blank label is a finding of its own, labels_from or not, the band named by where it
  # is (D12.36).
  blank_label <- which(!labelled)
  blank_label_findings <- if (length(blank_label) > 0L) {
    finding(
      report_text(
        "preflight_detail_site_id_range_invalid_blank_label",
        location = band_location(bands$source_cell[blank_label], bands$source_row[blank_label])
      ),
      bands$source_cell[blank_label]
    )
  }
  unknown <- which(labelled & !bands$reserved & !bands$label %chin% known)
  # The text names the labels' sheet and column, not what the labels are (D12.35).
  label_findings <- if (length(unknown) > 0L) {
    finding(
      report_text(
        "preflight_detail_site_id_range_invalid_label",
        label = bands$label[unknown], column = from[["column"]], sheet = from[["sheet"]]
      ),
      bands$source_cell[unknown]
    )
  }
  # Order and overlap are found here too, where the bands sheet's file is known, so
  # every site_id_range_invalid finding names it (D12.28). Overlap is among valid bands
  # only (D12.22), and an overlap finding cites both bands, the earlier first.
  has_bounds <- labelled & !is.na(bands$band_start) & !is.na(bands$band_end)
  inverted <- which(has_bounds & bands$band_start > bands$band_end)
  order_findings <- if (length(inverted) > 0L) {
    finding(
      report_text(
        "preflight_detail_site_id_range_invalid_order",
        label = bands$label[inverted], band_start = bands$band_start[inverted],
        band_end = bands$band_end[inverted]
      ),
      bands$source_cell[inverted]
    )
  }
  valid <- which(has_bounds & bands$band_start <= bands$band_end)
  valid <- valid[order(bands$band_start[valid], bands$band_end[valid])]
  overlap_findings <- NULL
  if (length(valid) > 1L) {
    ends <- bands$band_end[valid]
    running <- cummax(ends)
    holder <- valid[match(running, ends)]
    previous_end <- shift(running)
    hit <- which(!is.na(previous_end) & bands$band_start[valid] <= previous_end)
    if (length(hit) > 0L) {
      later <- valid[hit]
      earlier <- shift(holder)[hit]
      overlap_findings <- finding(
        report_text(
          "preflight_detail_site_id_range_invalid_overlap",
          label = bands$label[earlier],
          location = band_location(bands$source_cell[earlier], bands$source_row[earlier]),
          other_label = bands$label[later],
          other_location = band_location(bands$source_cell[later], bands$source_row[later])
        ),
        bands$source_cell[later]
      )
    }
  }
  # The banded column and its key (D18.2, D18.18): each problem a finding of its own, the
  # column's row only when there is none.
  key_value <- rep(NA_character_, nrow(bands))
  column_findings <- NULL
  duplicate_findings <- NULL
  if (with_column) {
    problems <- character()
    known_column <- any(
      attributes$table_name == id_bands$table & attributes$attribute_name == id_bands$attribute
    )
    if (!known_column) {
      problems <- c(problems, report_text(
        "preflight_detail_site_id_range_invalid_column",
        table_name = id_bands$table, attribute_name = id_bands$attribute
      ))
    }
    chain <- parse_chain(id_bands$key, id_bands$table, keys, attributes)
    labels_sheet <- if (!is.null(from)) code_lists[sheet == from[["sheet"]]]
    if (is.null(from)) {
      problems <- c(problems, report_text(
        "preflight_detail_site_id_range_invalid_key_labels",
        value = id_bands$key
      ))
    } else if (is.null(chain)) {
      problems <- c(problems, report_text(
        "preflight_detail_site_id_range_invalid_key",
        value = id_bands$key, table_name = id_bands$table
      ))
    } else if (!chain$attribute %chin% labels_sheet[source_row == 1L, value]) {
      problems <- c(problems, report_text(
        "preflight_detail_site_id_range_invalid_key_sheet",
        attribute_name = chain$attribute, sheet = from[["sheet"]]
      ))
    } else {
      # Each band's key: the chain's last attribute's column of the labels sheet, on the row
      # whose labels column holds the band's label; a reserved band has none.
      body <- labels_sheet[source_row > 1L]
      labels <- body[sheet_column == from[["column"]]]
      values <- body[sheet_column == chain$attribute]
      row_of <- labels$source_row[match(bands$label, labels$value)]
      key_value <- values$value[match(row_of, values$source_row)]
      key_value[bands$reserved] <- NA_character_
      # One band per key value (D18.57): each band after the first of its key value is a
      # finding naming the first, at its own cell.
      repeated <- which(duplicated(key_value, incomparables = NA))
      if (length(repeated) > 0L) {
        first <- match(key_value[repeated], key_value)
        duplicate_findings <- finding(
          report_text(
            "preflight_detail_site_id_range_invalid_duplicate_key",
            label = bands$label[first],
            location = band_location(bands$source_cell[first], bands$source_row[first]),
            other_label = bands$label[repeated],
            other_location = band_location(bands$source_cell[repeated], bands$source_row[repeated]),
            attribute_name = chain$attribute, value = key_value[repeated]
          ),
          bands$source_cell[repeated]
        )
      }
    }
    if (length(problems) == 0L) {
      out$columns <- data.table(
        table_name = id_bands$table, attribute_name = id_bands$attribute,
        key_attribute = id_bands$key, labels_sheet = from[["sheet"]],
        labels_column = from[["column"]]
      )
    } else {
      column_findings <- finding(problems)
    }
  }
  set(bands, j = "key_value", value = key_value)
  out$bands <- bands
  out$findings <- bind_component("read_findings", c(
    bound_findings, list(
      blank_label_findings, label_findings, order_findings, overlap_findings, column_findings,
      duplicate_findings
    )
  ))
  out
}

#' A band's place in words, by input form (D12.28)
#'
#' A CSV's line, a workbook's cell, a data.frame's row as R counts it. A band's
#' source_cell is NA in memory and holds "!" only for a workbook (sheet!A1; a CSV's is
#' file:line, whose line is taken from it, since an origin may move the file's rows,
#' D12.33).
#' @noRd
band_location <- function(source_cell, source_row) {
  # Each text is filled for its own bands only: a text's slot never takes an NA (D12.45).
  out <- source_cell
  memory <- is.na(source_cell)
  csv <- !memory & !grepl("!", source_cell, fixed = TRUE)
  out[memory] <- report_text("location_band_memory", row = source_row[memory] - 1L)
  out[csv] <- report_text("location_band_csv", line = sub("^.*:", "", source_cell[csv]))
  out
}

#' The file a code-list sheet came from: its own CSV, or else the workbook (D12.28)
#' @noRd
sheet_file <- function(manifest, sheet) {
  if (is.null(manifest) || is.null(sheet)) {
    return(NA_character_)
  }
  own <- match(paste0("code_lists:", sheet), manifest$input)
  if (!is.na(own)) {
    return(manifest$file[[own]])
  }
  workbook <- match("code_lists", manifest$input)
  if (is.na(workbook)) NA_character_ else manifest$file[[workbook]]
}

#' A stop unless an input has exactly its columns (D12.33)
#'
#' A precedence or lineage input without exactly its columns is a caller's error: one
#' message names every missing and every extra column, as 3.6's input_unparseable does
#' for run inputs.
#' @noRd
check_input_columns <- function(data, columns, input) {
  absent <- setdiff(columns, names(data))
  extra <- setdiff(names(data), columns)
  if (length(absent) == 0L && length(extra) == 0L) {
    return(invisible(NULL))
  }
  stop(sprintf(
    "`%s` must have exactly the columns %s; %s.", input, paste(columns, collapse = ", "),
    paste(c(
      if (length(absent) > 0L) paste("missing:", paste(absent, collapse = ", ")),
      if (length(extra) > 0L) paste("not taken:", paste(extra, collapse = ", "))
    ), collapse = "; ")
  ), call. = FALSE)
}

#' The precedence input, checked
#' @noRd
read_precedence <- function(x, origin = NULL) {
  table <- read_input_table(x, "precedence", origin)
  columns <- spec_input_schema()$precedence
  check_input_columns(table$data, columns, "precedence")
  data <- table$data[, columns, with = FALSE]
  known <- all(data$winner %in% c("datasets", "code_lists")) &&
    all(data$blank_rule %in% c("yields", "wins"))
  if (!known) {
    stop(
      "`precedence`'s winner is datasets or code_lists and its blank_rule yields or wins.",
      call. = FALSE
    )
  }
  # One rule per sheet, key and attribute: two would leave the winner to row order (D12.54).
  if (anyDuplicated(data, by = c("sheet", "key_col", "attribute_name")) > 0L) {
    stop("`precedence` has more than one row for a sheet, key and attribute.", call. = FALSE)
  }
  list(data = data, manifest = table$manifest, findings = table$findings)
}

#' The lineage-spec input, its columns checked
#' @noRd
read_lineage_input <- function(x, origin = NULL) {
  table <- read_input_table(x, "lineage_spec", origin)
  columns <- spec_input_schema()$lineage_spec
  check_input_columns(table$data, columns, "lineage_spec")
  table$data <- table$data[, columns, with = FALSE]
  table
}

#' The key_pairs input, read and checked (D18.1, D18.36)
#'
#' Exactly its columns, a caller's error otherwise (D12.33). A row whose table is blank or
#' isn't the dictionary's is a key_pair_invalid finding, and so is each side of a row with a
#' known table that is blank, or isn't an attribute of that table or a chain of its foreign
#' keys ending in one (parse_chain()). Each finding cites its cell, as the input's form
#' allows.
#' @noRd
read_key_pairs <- function(x, attributes, keys, origin = NULL) {
  table <- read_input_table(x, "key_pairs", origin)
  columns <- spec_input_schema()$key_pairs
  check_input_columns(table$data, columns, "key_pairs")
  data <- table$data[, columns, with = FALSE]
  # One finding per row in `rows`, at its cell in `column`; none for no row.
  finding <- function(rows, column, text_id, ...) {
    if (length(rows) == 0L) {
      return(NULL)
    }
    data.table(
      rule_id = "key_pair_invalid", input = "key_pairs", file = table$where$file,
      detail = report_text(text_id, ...),
      source_cell = cell_references(
        table$where, rows + 1L, rep(match(column, names(table$data)), length(rows))
      )
    )
  }
  known <- data$table_name %chin% attributes$table_name
  unknown <- which(!known)
  findings <- list(table$findings, finding(
    unknown, "table_name", "preflight_detail_key_pair_invalid_table",
    table_name = data$table_name[unknown]
  ))
  for (side in c("left", "right")) {
    chains <- data[[side]]
    parsed <- vapply(seq_len(nrow(data)), function(i) {
      !known[[i]] || !is.null(parse_chain(chains[[i]], data$table_name[[i]], keys, attributes))
    }, logical(1))
    bad <- which(!parsed)
    findings[[length(findings) + 1L]] <- finding(
      bad, side, "preflight_detail_key_pair_invalid_side",
      value = chains[bad], table_name = data$table_name[bad]
    )
  }
  list(
    data = data, manifest = table$manifest, findings = bind_component("read_findings", findings)
  )
}

#' The one_to_many input, read and checked (D18.3, D18.6, D18.36)
#'
#' Exactly its columns, a caller's error otherwise (D12.33). Each row's `source_cell` is its
#' contributor cell, as the input's form allows (NA in memory), for one_to_many_invalid's
#' findings; what the rows say is pre-flight's to check (one_to_many_problems()).
#' @noRd
read_one_to_many <- function(x, origin = NULL) {
  table <- read_input_table(x, "one_to_many", origin)
  columns <- spec_input_schema()$one_to_many
  check_input_columns(table$data, columns, "one_to_many")
  data <- table$data[, columns, with = FALSE]
  n <- nrow(data)
  set(data, j = "source_cell", value = cell_references(
    table$where, seq_len(n) + 1L, rep(match("contributor_label", names(table$data)), n)
  ))
  list(data = data, manifest = table$manifest, findings = table$findings)
}

#' The reach input, read and checked (D18.4, D18.7, D18.36)
#'
#' Exactly its columns, a caller's error otherwise (D12.33). A row whose table is blank or
#' isn't the dictionary's is a reach_invalid finding; so is a row of a known table whose `how`
#' isn't root, exempt, from_parent or from_child, a from_parent or from_child row whose
#' attribute isn't a foreign key of its table that the dictionary links to a parent's key
#' (D18.42), and a root or exempt row that names an attribute. So is each table the walk
#' checks that no walk from a root reaches (D18.65), the walk read as check_keys() and
#' reach_rows() read it. Each finding cites its cell, as the input's form allows.
#' @noRd
read_reach <- function(x, attributes, keys, origin = NULL) {
  table <- read_input_table(x, "reach", origin)
  columns <- spec_input_schema()$reach
  check_input_columns(table$data, columns, "reach")
  data <- table$data[, columns, with = FALSE]
  # One finding per row in `rows`, at its cell in `column` (one, or one per row); none for no
  # row.
  finding <- function(rows, column, text_id, ...) {
    if (length(rows) == 0L) {
      return(NULL)
    }
    data.table(
      rule_id = "reach_invalid", input = "reach", file = table$where$file,
      detail = report_text(text_id, ...),
      source_cell = cell_references(
        table$where, rows + 1L, rep_len(match(column, names(table$data)), length(rows))
      )
    )
  }
  linked <- keys$key_type == "FK" & !is.na(keys$reference_table) &
    !is.na(keys$reference_attribute)
  links <- paste(keys$table_name, keys$attribute_name, sep = "\r")[linked]
  known <- data$table_name %chin% attributes$table_name
  how_known <- data$how %chin% c("root", "exempt", "from_parent", "from_child")
  is_link <- data$how %chin% c("from_parent", "from_child")
  on_key <- paste(data$table_name, data$attribute_name, sep = "\r") %chin% links
  bad_table <- which(!known)
  bad_how <- which(known & !how_known)
  bad_key <- which(known & is_link & !on_key)
  bad_attribute <- which(known & how_known & !is_link & !is.na(data$attribute_name))
  # The walk, read as check_keys() and reach_rows() read it (D18.65). The tables it checks are
  # those a row names, but the exempt ones, and the parents a from_child row's key names, each
  # with a primary key. The roots that aren't exempt are reached; a from_parent row's table is
  # reached when its key's parent is, and a from_child row's parent when the row's table is;
  # a link to or from an exempt table is followed by none.
  parent <- keys$reference_table[linked][match(
    paste(data$table_name, data$attribute_name, sep = "\r"), links
  )]
  exempt <- data$table_name[data$how %chin% "exempt"]
  is_child_link <- data$how %chin% "from_child"
  keyed <- keys$table_name[keys$key_type %chin% "PK" & keys$key_part %in% 1L]
  checked <- setdiff(
    unique(c(data$table_name[!data$how %chin% "exempt"], parent[is_child_link])), c(exempt, NA)
  )
  checked <- checked[checked %chin% keyed]
  checked <- checked[order(match(checked, attributes$table_name))]
  usable <- is_link & !is.na(parent) & !data$table_name %chin% exempt & !parent %chin% exempt
  reached <- setdiff(data$table_name[data$how %chin% "root"], c(exempt, NA))
  repeat {
    up <- usable & is_child_link & data$table_name %chin% reached
    down <- usable & data$how %chin% "from_parent" & parent %chin% reached
    more <- setdiff(c(parent[up], data$table_name[down]), reached)
    if (length(more) == 0L) {
      break
    }
    reached <- c(reached, more)
  }
  # Each table no walk reaches, at its own first row's table cell or, without a row of its
  # own, at the key cell of the first from_child row naming it.
  unreached <- setdiff(checked, reached)
  own <- match(unreached, data$table_name)
  named <- match(unreached, fifelse(is_child_link, parent, NA_character_))
  findings <- list(
    table$findings,
    finding(
      bad_table, "table_name", "preflight_detail_reach_invalid_table",
      table_name = data$table_name[bad_table]
    ),
    finding(
      bad_how, "how", "preflight_detail_reach_invalid_how",
      value = data$how[bad_how], table_name = data$table_name[bad_how]
    ),
    finding(
      bad_key, "attribute_name", "preflight_detail_reach_invalid_key",
      table_name = data$table_name[bad_key], attribute_name = data$attribute_name[bad_key]
    ),
    finding(
      bad_attribute, "attribute_name", "preflight_detail_reach_invalid_attribute",
      table_name = data$table_name[bad_attribute],
      attribute_name = data$attribute_name[bad_attribute], value = data$how[bad_attribute]
    ),
    finding(
      fifelse(is.na(own), named, own), fifelse(is.na(own), "attribute_name", "table_name"),
      "preflight_detail_reach_invalid_unreachable",
      table_name = unreached
    )
  )
  list(
    data = data, manifest = table$manifest, findings = bind_component("read_findings", findings)
  )
}

#' Read a specification
#'
#' @description
#' Reads a data dictionary and its companion inputs into one specification object, which
#' [gpq_preflight()] checks and the checks of later layers read. Reading never stops on a
#' defect in the specification: each one is recorded in the `read_findings` component,
#' and [gpq_preflight()] decides whether it stops.
#'
#' It does stop, with an error, on a mistake in the call: an argument in the wrong form;
#' a file that doesn't exist, or a CSV file that can't be read, such as one in UTF-16,
#' unless it is a translation table, which is then a finding; a
#' dictionary without its table or attribute column; a `precedence`, `lineage_spec`,
#' `key_pairs`, `one_to_many` or `reach` table without exactly its columns; a `precedence`
#' table with an unknown `winner` or `blank_rule`, or with two rows for one sheet, key and
#' attribute; a name given in R that isn't valid UTF-8; a translation table's filter on a
#' column it lacks, or with no values for an attribute that uses it; or an origin that
#' doesn't fit its input.
#'
#' @param dictionary The data dictionary: a data.frame, or the path of a CSV file or of an
#'   `.xlsx` workbook, read from its first sheet. One row per table and attribute.
#' @param code_lists `NULL`, the path of an `.xlsx` workbook (every sheet is a code list
#'   or a reference table), or a named list of data.frames or CSV paths, named by sheet.
#' @param non_code_sheets `NULL`, or the names of sheets that aren't code lists; the
#'   `code_list_*` checks skip them. A blank name is dropped, as a repeated one is.
#' @param datasets `NULL`, or the datasets table (a data.frame or a CSV path), read as text.
#' @param lineage_spec `NULL`, or the lineage spec in long form (a data.frame or a CSV
#'   path), with exactly the columns
#'   `contributor_label`, `table_name`, `attribute_name`, `spec_type` (`id` or
#'   `compiled`), `source_text`, `note` and `source_cell`.
#' @param crosswalks `NULL`, or a named list of translation tables: each a data.frame, a
#'   CSV path, or `list(table = , code_col = , filter_col = , filter_values = )`, where
#'   `filter_values` may be a list named by attribute.
#' @param id_pattern `NULL`, or one regular expression marking ID attributes by name.
#' @param id_bands `NULL`, or `list(sheet, label_col, start_col, end_col, labels_from =
#'   c(sheet = , column = ), reserved_pattern)` naming the code-list sheet of ID bands, and
#'   optionally, all three or none, `table` and `attribute`, the column the bands apply to, and
#'   `key`, the foreign keys followed from that table and the attribute they reach, joined by
#'   " > ", such as `"dataset_id > contributor_id"`. `key` needs `labels_from`, and its last
#'   attribute must be a column of the `labels_from` sheet, which gives each band's value of it;
#'   each value can have only one band.
#' @param precedence `NULL`, or a table (data.frame or CSV path) with exactly the columns
#'   `sheet`,
#'   `key_col`, `attribute_name`, `winner` (`datasets` or `code_lists`) and `blank_rule`
#'   (`yields` or `wins`), saying which input wins where the datasets table and a sheet
#'   overlap.
#' @param key_pairs `NULL`, or a table (data.frame or CSV path) with exactly the columns
#'   `table_name`, `left`, `right` and `note`: each row two ways to reach one value from a
#'   record of `table_name`, each side an attribute of the table or a chain of foreign keys
#'   joined by " > " ending in an attribute, such as `"haul_id > station_id"`.
#'   `key_incoherent` compares the two sides and reports a record where they differ on the
#'   left side's first attribute.
#' @param one_to_many `NULL`, or a table (data.frame or CSV path) with exactly the columns
#'   `contributor_label`, `table_name`, `attribute_name` and `note`: each row an ID attribute
#'   a contributor documents as one-to-many, whose repeats `src_id_duplicate` doesn't report
#'   among that contributor's records. A record's contributor is found through `id_bands`'
#'   `key` and `labels_from`, so the rows apply only when `id_bands` gives `table`,
#'   `attribute`, `key` and `labels_from`; pre-flight warns of a row it can't apply
#'   (`one_to_many_invalid`), which is left out.
#' @param reach `NULL`, or a table (data.frame or CSV path) with exactly the columns
#'   `table_name`, `attribute_name` and `how`, saying how `record_unreachable` walks the
#'   tables: `root`, a table whose records are all reached; `exempt`, a table not walked or
#'   checked; `from_parent`, a record reached through this foreign key when its parent is;
#'   `from_child`, a reached record reaching its parent through this foreign key.
#'   `attribute_name` is blank for `root` and `exempt`. The tables checked are those the rows
#'   name, but the exempt ones, and the parents `from_child` links reach. A record reachable
#'   only through a table or key column not given is left unchecked.
#' @param origins `NULL`, or, for inputs given as data.frames that came from files, a list
#'   named by input, each once (`dictionary`, `datasets`, `lineage_spec`, `precedence`,
#'   `key_pairs`, `one_to_many`, `reach`, `code_lists:<sheet>`, `crosswalks:<name>`) of
#'   `list(path = , sheet = , rows = , columns = )`: the file, named in findings and hashed in
#'   the manifest; a workbook's sheet, or `NULL` for a CSV file; the file row of the first
#'   data row, later rows following on, or the file row of every data row, or `NULL` to count
#'   rows as R counts them; and the file column, by number or letter, of every column of the
#'   data.frame, or `NULL` for the file's order. Findings in such an input then name the
#'   file's cells.
#' @param column_map The dictionary's column names, from [gpq_column_map()].
#' @param type_map The type map, from [gpq_type_map()]. What was found reading its file
#'   joins `read_findings`.
#' @param sentinels The sentinel table, from [gpq_sentinels()].
#' @return An object of class `gpq_spec`: a named list of data.tables, the components
#'   `attributes`, `keys`, `code_lists`, `code_list_sheets`, `code_list_map`, `codes`,
#'   `non_code_sheets`,
#'   `datasets`, `lineage_spec`, `crosswalks`, `id_bands`, `id_band_columns`, `key_pairs`,
#'   `one_to_many`, `reach`, `type_map`, `sentinels`, `clashes`, `read_findings` and
#'   `manifest`. An input not given leaves its component empty, with its columns. A blank cell
#'   is `NA`; text is kept exactly as written.
#' @section Components:
#' Each component is a data.table with these columns:
#'
#' `r rd_spec_components()`
#'
#' In `clashes`, `basis` says how a clash was settled: `precedence`, by the `precedence`
#' input; or `fixed_dictionary_placement`, where a lineage-spec row names another table
#' for an attribute and the dictionary's table is used. `source_cell_a` and
#' `source_cell_b` are the two sides' cells, `NA` where unknown; a side with several
#' cells, such as every lineage-spec row behind one placement clash, lists them with ", ".
#' @examples
#' example <- function(file) system.file("extdata", "examples", file, package = "groundplotqc")
#' fish <- gpq_read_spec(
#'   dictionary = example("fish_dictionary.csv"),
#'   code_lists = list(
#'     water_body = example("fish_water_body.csv"), gear = example("fish_gear.csv"),
#'     species = example("fish_species.csv")
#'   ),
#'   column_map = gpq_column_map(
#'     table = "table", attribute = "field", type = "kind", key_type = "key",
#'     reference = "parent", lookup = "codes", description = "notes",
#'     min_value = "low", max_value = "high", min_inclusive = "low_in", max_inclusive = "high_in",
#'     pattern = "format", max_length = "width", decimals = "places", units = "unit"
#'   ),
#'   type_map = gpq_type_map(example("fish_types.csv")),
#'   sentinels = gpq_sentinels(
#'     numeric = c(missing = -99, not_applicable = -88),
#'     character = c(missing = "?", not_applicable = "~"),
#'     date = c(missing = "?", not_applicable = "~")
#'   )
#' )
#' fish$keys
#' subset(fish$codes, attribute_name == "gear")
#' @export
gpq_read_spec <- function(dictionary, code_lists = NULL, non_code_sheets = NULL,
                          datasets = NULL, lineage_spec = NULL, crosswalks = NULL,
                          id_pattern = NULL, id_bands = NULL, precedence = NULL,
                          key_pairs = NULL, one_to_many = NULL, reach = NULL, origins = NULL,
                          column_map = gpq_column_map(),
                          type_map = gpq_type_map(), sentinels = gpq_sentinels()) {
  # data.table's automatic indexing would add index attributes as the reader subsets;
  # off here and restored on exit, so the spec reloads identical() (D12.27).
  auto_index <- options(datatable.auto.index = FALSE)
  on.exit(options(auto_index), add = TRUE)
  if (!inherits(column_map, "gpq_column_map")) {
    stop("`column_map` must come from gpq_column_map().", call. = FALSE)
  }
  pattern_ok <- is.null(id_pattern) || is.character(id_pattern) && length(id_pattern) == 1L &&
    !is.na(id_pattern)
  # The pattern must compile, or grepl()'s own error would reach the caller (R10).
  if (pattern_ok && !is.null(id_pattern)) {
    pattern_ok <- tryCatch(
      is.logical(grepl(id_pattern, "")),
      error = function(e) FALSE, warning = function(w) FALSE
    )
  }
  if (!pattern_ok) {
    stop("`id_pattern` must be NULL or one regular expression.", call. = FALSE)
  }
  if (!is.null(non_code_sheets)) {
    if (!is.character(non_code_sheets) || !all(validUTF8(non_code_sheets))) {
      stop(
        "`non_code_sheets` must be NULL or a character vector of sheet names in UTF-8.",
        call. = FALSE
      )
    }
    # A blank name is NA, as in every input form (R17); the component drops it, as it drops a
    # repeat, since an NA names no sheet (D12.69).
    non_code_sheets <- as_text(non_code_sheets)
  }
  # A type map read from a file brings that file's findings (D12.55), with
  # read_findings' columns; a hand-set attribute without them is a caller's error.
  type_findings <- attr(type_map, "gpq_read_findings")
  finding_columns <- names(spec_schema()$read_findings)
  findings_ok <- is.null(type_findings) || is.data.frame(type_findings) &&
    setequal(names(type_findings), finding_columns) && anyDuplicated(names(type_findings)) == 0L
  if (!findings_ok) {
    stop(
      "`type_map`'s attribute `gpq_read_findings` must be a table with the columns ",
      paste(finding_columns, collapse = ", "), ", as gpq_type_map() makes it.",
      call. = FALSE
    )
  }
  type_map <- validate_type_map(type_map)
  sentinels <- validate_sentinels(sentinels)
  check_origins(
    origins,
    c(
      "dictionary",
      if (is.list(code_lists) && !is.data.frame(code_lists)) {
        paste0("code_lists:", names(code_lists))
      },
      if (!is.null(datasets)) "datasets", if (!is.null(lineage_spec)) "lineage_spec",
      if (is.list(crosswalks)) paste0("crosswalks:", names(crosswalks)),
      if (!is.null(precedence)) "precedence", if (!is.null(key_pairs)) "key_pairs",
      if (!is.null(one_to_many)) "one_to_many", if (!is.null(reach)) "reach"
    ),
    no_origin = c(
      if (!is.null(non_code_sheets)) "non_code_sheets", if (!is.null(id_pattern)) "id_pattern",
      if (!is.null(id_bands)) "id_bands"
    ),
    code_lists_workbook = is.character(code_lists) && length(code_lists) == 1L
  )
  dictionary_in <- read_input_table(dictionary, "dictionary", origins[["dictionary"]])
  dictionary_read <- read_dictionary(dictionary_in, column_map, type_map, id_pattern)
  lists <- read_code_lists(code_lists, origins)
  walks <- read_crosswalks(crosswalks, origins)
  datasets_in <- if (!is.null(datasets)) {
    read_input_table(datasets, "datasets", origins[["datasets"]])
  }
  lineage_in <- if (!is.null(lineage_spec)) {
    read_lineage_input(lineage_spec, origins[["lineage_spec"]])
  }
  precedence_in <- if (!is.null(precedence)) {
    read_precedence(precedence, origins[["precedence"]])
  }
  attributes <- dictionary_read$attributes
  code_list_map <- build_code_list_map(
    attributes, lists$long, lists$sheets, walks$long, walks$declared
  )
  # build_id_bands() checks id_bands' shape; until then only a list's sheet is read.
  bands_sheet <- if (is.list(id_bands)) id_bands[["sheet"]]
  bands_file <- if (is.character(bands_sheet) && length(bands_sheet) == 1L) {
    sheet_file(lists$manifest, bands_sheet)
  } else {
    NA_character_
  }
  keys <- build_keys(attributes)
  bands <- build_id_bands(id_bands, lists$long, bands_file, attributes, keys)
  pairs_in <- if (!is.null(key_pairs)) {
    read_key_pairs(key_pairs, attributes, keys, origins[["key_pairs"]])
  }
  many_in <- if (!is.null(one_to_many)) {
    read_one_to_many(one_to_many, origins[["one_to_many"]])
  }
  reach_in <- if (!is.null(reach)) {
    read_reach(reach, attributes, keys, origins[["reach"]])
  }
  datasets_data <- if (is.null(datasets_in)) data.table() else datasets_in$data
  # Each precedence sheet's own file, its CSV or the workbook, named by sheet (D12.28).
  list_files <- vapply(
    unique(precedence_in$data$sheet), sheet_file, character(1),
    manifest = lists$manifest
  )
  # NULL when the datasets table isn't given; a given one with no rows is checked (R14).
  clashes <- resolve_clashes(
    datasets_in$data, lists$long, precedence_in$data,
    list(datasets = datasets_in$where$file, code_lists = list_files),
    datasets_where = datasets_in$where %||% list(kind = "memory")
  )
  # A placement clash names the dictionary's file only where its rows are the file's
  # (D12.33).
  dictionary_file <- if (dictionary_in$where$kind == "memory") {
    NA_character_
  } else {
    dictionary_in$where$file
  }
  lineage <- build_lineage_spec(
    lineage_in$data, attributes, lineage_in$where$file, lineage_tokens(sentinels),
    dictionary_file
  )
  new_gpq_spec(list(
    attributes = attributes,
    keys = keys,
    code_lists = lists$long,
    code_list_sheets = data.table(sheet = lists$sheets),
    code_list_map = code_list_map,
    codes = build_codes(code_list_map, lists$long, walks$long),
    non_code_sheets = data.table(
      sheet = unique(as.character(non_code_sheets[!is.na(non_code_sheets)]))
    ),
    datasets = datasets_data,
    lineage_spec = lineage$component,
    crosswalks = walks$long,
    id_bands = bands$bands,
    id_band_columns = bands$columns,
    key_pairs = if (is.null(pairs_in)) empty_table(spec_schema()$key_pairs) else pairs_in$data,
    one_to_many = if (is.null(many_in)) empty_table(spec_schema()$one_to_many) else many_in$data,
    reach = if (is.null(reach_in)) empty_table(spec_schema()$reach) else reach_in$data,
    type_map = type_map,
    sentinels = sentinels,
    clashes = bind_component("clashes", list(clashes$clashes, lineage$clashes)),
    read_findings = bind_component("read_findings", list(
      dictionary_in$findings, dictionary_read$findings, type_findings, lists$findings,
      walks$findings, datasets_in$findings, lineage_in$findings, precedence_in$findings,
      bands$findings, clashes$findings, lineage$findings, pairs_in$findings, many_in$findings,
      reach_in$findings
    )),
    manifest = bind_component("manifest", list(
      dictionary_in$manifest, lists$manifest, memory_row(non_code_sheets, "non_code_sheets"),
      datasets_in$manifest, lineage_in$manifest, walks$manifest,
      memory_row(id_pattern, "id_pattern"), memory_row(id_bands, "id_bands"),
      precedence_in$manifest, pairs_in$manifest, many_in$manifest, reach_in$manifest
    ))
  ))
}

#' A stop unless `origins` is NULL or a list named by inputs given (D12.33)
#'
#' Each name once (D12.62). An input given that takes no origin (`no_origin`), or a sheet
#' of a code-list workbook given as a path (`code_lists_workbook`, D12.63), is refused
#' saying why (D12.62); an origin for any other input given as a path is refused where
#' that input is read.
#' @noRd
check_origins <- function(origins, inputs, no_origin = character(),
                          code_lists_workbook = FALSE) {
  if (is.null(origins)) {
    return(invisible(NULL))
  }
  named <- is.list(origins) && !is.data.frame(origins) && !is.null(names(origins)) &&
    all(nzchar(names(origins)))
  if (!named) {
    stop("`origins` must be NULL or a list named by input.", call. = FALSE)
  }
  given <- names(origins)
  # A repeated name would leave one origin unused, as for code_lists' names (R11).
  twice <- unique(given[duplicated(given)])
  if (length(twice) > 0L) {
    stop(
      "`origins` names an input more than once: ", paste(twice, collapse = ", "), ".",
      call. = FALSE
    )
  }
  none <- intersect(given, no_origin)
  if (length(none) > 0L) {
    stop(
      "`origins` names inputs that take no origin: ", paste(none, collapse = ", "), ".",
      call. = FALSE
    )
  }
  sheets <- if (code_lists_workbook) given[startsWith(given, "code_lists:")] else character()
  if (length(sheets) > 0L) {
    stop(
      "`code_lists` is given as a path, so its sheets take no origin: ",
      paste(sheets, collapse = ", "), ".",
      call. = FALSE
    )
  }
  unknown <- setdiff(given, inputs)
  if (length(unknown) > 0L) {
    stop(
      "`origins` names inputs that weren't given: ", paste(unknown, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' The components and their columns as markdown, for gpq_read_spec()'s help page
#'
#' Built from spec_schema(), so the page can't drift from it (D12.30).
#' @noRd
rd_spec_components <- function() {
  schema <- spec_schema()
  columns <- vapply(schema, function(x) {
    if (length(x) == 0L) {
      return("the input's own columns, all text")
    }
    paste0("`", names(x), "`", collapse = ", ")
  }, character(1))
  paste0("- `", names(schema), "`: ", columns, collapse = "\n")
}
