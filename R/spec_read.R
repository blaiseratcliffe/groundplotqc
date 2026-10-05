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
      lineage_flag = lgl, id_marked = lgl, source_row = int
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
      source_cell = chr
    ),
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

#' The columns of the precedence, lineage-spec and site-ID-band inputs
#' @noRd
spec_input_schema <- function() {
  list(
    precedence = c("sheet", "key_col", "attribute_name", "winner", "blank_rule"),
    lineage_spec = c(
      "contributor_label", "table_name", "attribute_name", "spec_type", "source_text",
      "note", "source_cell"
    ),
    id_bands = c(
      "sheet", "label_col", "start_col", "end_col", "labels_from", "reserved_pattern"
    )
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
          "Component %s, column %s, holds an empty string; a blank is NA (D12.14).", name, column
        ), call. = FALSE)
      }
    }
  }
  invisible(NULL)
}

#' One table input read as text
#'
#' A data.frame (with its origin, D12.33), a CSV path, or for the dictionary an `.xlsx`
#' path (its first sheet); returns `list(data, where, manifest, invalid, findings)`.
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
    data <- as.data.table(lapply(x, as_text))
    # Names as a CSV file's are read: a blank one V<j>, then each unique (R7, D12.54).
    if (ncol(data) > 0L) {
      names_in <- names(x)
      blank <- is.na(names_in) | is_blank(names_in)
      names_in[blank] <- paste0("V", which(blank))
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
    ))
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
      stop(sprintf(paste(
        "The origin of %s must give its rows as NULL, the file row of the first data row",
        "(2 or more), or one file row per data row."
      ), input), call. = FALSE)
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
  list(attributes = attributes, findings = findings)
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
  # PK anywhere there is nothing to look up.
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
