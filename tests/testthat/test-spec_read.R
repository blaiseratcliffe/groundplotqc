# Tests for reading a specification (plan 3.6, 4.1; D12.13 to D12.25, D12.45, D12.54, D12.58,
# D12.59).

csv_file <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  path
}

empty_components <- function() {
  lapply(spec_schema(), empty_table)
}

test_that("the schema lists 3.6's components in order", {
  expect_named(spec_schema(), c(
    "attributes", "keys", "code_lists", "code_list_sheets", "code_list_map", "codes",
    "non_code_sheets",
    "datasets", "lineage_spec", "crosswalks", "id_bands", "type_map", "sentinels",
    "clashes", "read_findings", "manifest"
  ))
  expect_equal(spec_schema()$manifest, c(
    input = "character", file = "character", file_date = "character", sha256 = "character"
  ))
})

test_that("new_gpq_spec accepts the empty components and refuses bad ones", {
  spec <- new_gpq_spec(empty_components())
  expect_s3_class(spec, "gpq_spec")
  expect_equal(vapply(spec$id_bands, class, character(1)), spec_schema()$id_bands)
  bad <- empty_components()
  expect_error(new_gpq_spec(rev(bad)), "order")
  bad$keys$key_part <- character()
  expect_error(new_gpq_spec(bad), "key_part")
  blank <- empty_components()
  blank$non_code_sheets <- data.table::data.table(sheet = "")
  expect_error(new_gpq_spec(blank), "empty string")
})

test_that("validate_gpq_spec checks without dropping an index (D12.54)", {
  components <- empty_components()
  data.table::setindex(components$keys, table_name)
  expect_null(validate_gpq_spec(components))
  expect_false(is.null(attr(components$keys, "index")))
})

test_that("new_gpq_spec drops the components' indices (D12.27, D12.54)", {
  components <- empty_components()
  data.table::setindex(components$keys, table_name)
  expect_false(is.null(attr(components$keys, "index")))
  spec <- new_gpq_spec(components)
  expect_null(attr(spec$keys, "index"))
})

test_that("read_input_table reads a data.frame as text, blanks as NA", {
  read <- read_input_table(data.frame(a = c("x", ""), b = c(1.5, NA)), "datasets")
  expect_equal(read$data$a, c("x", NA))
  expect_equal(read$data$b, c("1.5", NA))
  expect_equal(read$manifest$file, "in memory")
  expect_true(is.na(read$manifest$sha256))
  expect_equal(nrow(read$findings), 0L)
})

test_that("a data.frame's names are read as a CSV's; a matrix or list column stops (D12.54)", {
  frame <- data.frame("1", "2", "3", "4")
  names(frame) <- c("code", "", "code", "  ")
  expect_named(read_input_table(frame, "datasets")$data, c("code", "V2", "code.1", "V4"))
  matrix_column <- data.frame(id = 1:2)
  matrix_column$m <- I(matrix(c("p", "q", "r", "s"), 2))
  expect_error(read_input_table(matrix_column, "datasets"), "one plain value per row")
  list_column <- data.frame(id = 1:2)
  list_column$l <- list("a", c("b", "c"))
  expect_error(read_input_table(list_column, "datasets"), "l doesn't")
})

test_that("read_input_table reads a CSV with its text as written and a manifest row", {
  path <- csv_file(c("id,name", "01,\" NT_PSP\"", "NA,café"))
  read <- read_input_table(path, "datasets")
  expect_equal(read$data$id, c("01", "NA"))
  expect_equal(read$data$name, c(" NT_PSP", "café"))
  expect_equal(read$manifest$input, "datasets")
  expect_equal(read$manifest$file, basename(path))
  expect_equal(read$manifest$sha256, unname(tools::sha256sum(path)))
})

test_that("a folder isn't a file, whatever its name (R5)", {
  folder <- file.path(withr::local_tempdir(), "folder.csv")
  dir.create(folder)
  expect_error(read_input_table(folder, "datasets"), "existing .csv file")
})

test_that("an invalid byte becomes one spec_encoding_invalid finding with file and line", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("id,comments\n100.01,ok\n100.02,a"), as.raw(0x97), charToRaw("b\n")), path)
  read <- read_input_table(path, "datasets")
  expect_equal(read$data$comments[[2L]], "a<97>b")
  expect_equal(read$findings$rule_id, "spec_encoding_invalid")
  expect_equal(read$findings$source_cell, paste0(basename(path), ":3"))
  expect_match(read$findings$detail, "column comments", fixed = TRUE)
})

test_that("a finding after a multi-line cell names the file's line (D12.54)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(
    charToRaw("id,comments\n1,\"two\nlines\"\n2,a"), as.raw(0x97), charToRaw("b\n")
  ), path)
  read <- read_input_table(path, "datasets")
  expect_equal(read$findings$source_cell, paste0(basename(path), ":4"))
  expect_match(read$findings$detail, "line 4, column comments", fixed = TRUE)
})

test_that("a malformed CSV is one spec_csv_malformed finding per problem (D12.54, D12.55)", {
  ragged <- csv_file(c("id,comments", "1,ok", "2,has, a comma", "3,x"))
  read <- read_input_table(ragged, "datasets")
  file <- basename(ragged)
  expect_equal(read$data$id, "1")
  expect_equal(read$findings$rule_id, "spec_csv_malformed")
  expect_equal(read$findings$detail, paste(
    file, "line 3 doesn't have as many fields as the header (2), so it and the lines after it",
    "aren't read."
  ))
  expect_equal(read$findings$source_cell, paste0(file, ":3"))
  empty <- withr::local_tempfile(fileext = ".csv")
  file.create(empty)
  expect_equal(
    read_input_table(empty, "datasets")$findings$detail, paste(basename(empty), "is empty.")
  )
  quote <- csv_file(c("id,comments", "1,\"open", "2,next"))
  expect_equal(read_input_table(quote, "datasets")$findings$detail, paste(
    basename(quote),
    "has a quote that isn't closed or doubled as CSV needs, so some cells may not read as written."
  ))
})

test_that("an unknown warning and a row shortfall are findings without a line (D12.58)", {
  malformed <- data.table::data.table(
    kind = c("unknown", "short"), line = NA_integer_, fields = NA_integer_,
    n_records = c(NA, 3L), n_read = c(NA, 2L), value = c("Odd.", NA)
  )
  found <- malformed_findings(malformed, "datasets", "d.csv")
  expect_equal(found$rule_id, c("spec_csv_malformed", "spec_csv_malformed"))
  expect_equal(found$detail, c(
    paste(
      "Reading d.csv gave a warning the package doesn't recognise, so some of its lines may",
      "not have been read: \"Odd.\"."
    ),
    "d.csv has 3 records after its header, but only 2 were read."
  ))
  expect_equal(found$source_cell, c(NA_character_, NA_character_))
})

test_that("rows the CSV read leaves out without a warning are one finding (D12.58)", {
  path <- csv_file(c("id,name", "1,a", "2,b"))
  real_fread <- data.table::fread
  # Only this file's read loses its last row; the report text's own CSV reads in full.
  local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    if (identical(list(...)$file, path)) out[seq_len(nrow(out) - 1L)] else out
  })
  read <- read_input_table(path, "datasets")
  expect_equal(read$data$id, "1")
  expect_equal(
    read$findings$detail,
    paste(basename(path), "has 2 records after its header, but only 1 were read.")
  )
})

test_that("a warning the CSV read doesn't recognise is one finding without a cell (D12.58)", {
  path <- csv_file(c("id,name", "1,a"))
  real_fread <- data.table::fread
  # Only this file's read warns; the report text's own CSV reads as it is.
  local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    if (identical(list(...)$file, path)) warning("A new warning.", call. = FALSE)
    out
  })
  expect_no_warning(read <- read_input_table(path, "datasets"))
  expect_equal(read$data$id, "1")
  expect_equal(read$findings$rule_id, "spec_csv_malformed")
  expect_equal(read$findings$detail, paste0(
    "Reading ", basename(path), " gave a warning the package doesn't recognise, so some of its",
    " lines may not have been read: \"A new warning.\"."
  ))
  expect_true(is.na(read$findings$source_cell))
})

test_that("a one-column CSV fread() stops on for a quote is one finding, no error (D12.59)", {
  path <- csv_file(c("code", "A", "", "\"B\" extra"))
  expect_no_warning(expect_no_error(read <- read_input_table(path, "datasets")))
  expect_equal(read$findings$rule_id, "spec_csv_malformed")
  expect_equal(read$findings$detail, paste(
    basename(path),
    "has a quote that isn't closed or doubled as CSV needs, so some cells may not read as written."
  ))
  expect_equal(nrow(read$data), 0L)
  expect_error(read_input_table(file.path(tempdir(), "no such file.csv"), "datasets"))
})

test_that("cell references follow each kind of input", {
  expect_equal(
    cell_references(list(kind = "xlsx", sheet = "DD"), c(2L, 59L), c(3L, 8L)),
    c("DD!C2", "DD!H59")
  )
  expect_equal(cell_references(list(kind = "csv", file = "a.csv"), 4L, 2L), "a.csv:4")
  expect_equal(cell_references(list(kind = "memory"), 1:2, 1:2), c(NA_character_, NA_character_))
})

test_that("no rows give no cell references, in every form", {
  none <- integer()
  expect_identical(cell_references(list(kind = "xlsx", sheet = "DD"), none, none), character())
  expect_identical(cell_references(list(kind = "csv", file = "a.csv"), none, none), character())
  expect_identical(cell_references(list(kind = "memory"), none, none), character())
})

test_that("manifest rows take the date in the file's name", {
  expect_equal(file_date("20260925_magpv2_DD.xlsx"), "20260925")
  expect_true(is.na(file_date("dictionary.csv")))
  expect_true(is.na(file_date("v123456789_dd.csv")))
  expect_true(is.na(file_date("dd_99999999.csv")))
  expect_null(memory_row(NULL, "precedence"))
  expect_equal(memory_row("^x$", "id_pattern")$file, "in memory")
})

test_that("a data.frame's blank and spaces-only cells are NA, text as written (D12.27)", {
  read <- read_input_table(data.frame(value = c("", "   ", "NA", " NT_PSP")), "datasets")
  expect_equal(read$data$value, c(NA, NA, "NA", " NT_PSP"))
})

test_that("a workbook's blank and spaces-only cells are NA, text as written (D12.27)", {
  testthat::skip_if_not_installed("readxl")
  path <- testthat::test_path("fixtures", "blank_cells.xlsx")
  read <- read_input_table(path, "dictionary")
  # readxl blanks the spaces-only cell itself, so the mocked test below pins the
  # reader's own blank_to_na(). The leading space shows that readxl reads with
  # trim_ws = FALSE (D12.9).
  expect_equal(read$data$value, c(NA, NA, "NA", " NT_PSP"))
  expect_equal(read$data$value[[4L]], " NT_PSP")
  # Only the dictionary is read from a workbook (D12.22, D12.34).
  expect_error(
    read_input_table(path, "datasets"),
    "`datasets` must be a data.frame or the path of an existing .csv file.",
    fixed = TRUE
  )
})

test_that("read_xlsx_raw makes a blank or spaces-only cell NA whatever readxl gives (D12.27)", {
  testthat::skip_if_not_installed("readxl")
  testthat::local_mocked_bindings(
    read_excel = function(...) data.frame(cells = c("   ", "", " x")),
    .package = "readxl"
  )
  expect_equal(read_xlsx_raw("any.xlsx", "any")$V1, c(NA, NA, " x"))
})

test_that("a sheet's first row is its names: a blank one V<j>, then each unique (D12.54)", {
  raw <- data.table::data.table(
    V1 = c("a", "1"), V2 = c(NA, "2"), V3 = c("a", "3"), V4 = c(NA, "4")
  )
  table <- raw_to_table(raw)
  expect_named(table, c("a", "V2", "a.1", "V4"))
  expect_equal(table$V2, "2")
  # A generated name that meets a name in the sheet is made unique too.
  met <- data.table::data.table(
    V1 = c("a", "1"), V2 = c(NA, "2"), V3 = c("a", "3"), V4 = c("V2", "4")
  )
  expect_named(raw_to_table(met), c("a", "V2", "a.1", "V2.1"))
  expect_equal(nrow(raw_to_table(data.table::data.table())), 0L)
})

# The same bad cell (row 2's comments) and bad header (column 3) in each input form;
# invalid_utf8.xlsx holds them on its sheet codes (D12.28).
bad_text <- function(bytes) {
  x <- rawToChar(as.raw(bytes))
  Encoding(x) <- "UTF-8"
  x
}
bad_cell <- bad_text(c(0x6F, 0x6B, 0x97))
bad_name <- bad_text(c(0x6E, 0x61, 0x97, 0x6D, 0x65))
hex_note <- ", each bad byte shown as its hex code in angle brackets."
kept <- function(location, value) {
  paste0("Text at ", location, " isn't valid UTF-8; it is kept as \"", value, "\"", hex_note)
}

test_that("a CSV's findings give file, line and column (D12.28)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(
    charToRaw("id,comments,"), charToRaw(bad_name), charToRaw("\n1,fine,x\n2,"),
    charToRaw(bad_cell), charToRaw(",y\n")
  ), path)
  read <- read_input_table(path, "datasets")
  file <- basename(path)
  expect_equal(read$findings$detail, c(
    kept(paste0(file, " line 1, column 3"), "na<97>me"),
    kept(paste0(file, " line 3, column comments"), "ok<97>")
  ))
  expect_equal(read$findings$source_cell, paste0(file, c(":1", ":3")))
})

test_that("a workbook's findings give the cell (D12.28)", {
  testthat::skip_if_not_installed("readxl")
  read <- read_input_table(testthat::test_path("fixtures", "invalid_utf8.xlsx"), "dictionary")
  expect_equal(read$findings$detail, c(
    kept("cell codes!C1", "na<97>me"),
    kept("cell codes!B3", "ok<97>")
  ))
  expect_equal(read$findings$source_cell, c("codes!C1", "codes!B3"))
})

test_that("a data.frame's findings give its input name and its row as R counts it (D12.28)", {
  frame <- data.frame(id = c("1", "2"), comments = c("fine", bad_cell), third = c("x", "y"))
  names(frame)[[3L]] <- bad_name
  read <- read_input_table(frame, "datasets")
  expect_equal(read$findings$detail, c(
    paste0(
      "The name of column 3 of datasets isn't valid UTF-8; it is kept as \"na<97>me\"", hex_note
    ),
    kept("datasets row 2, column comments", "ok<97>")
  ))
  expect_true(all(is.na(read$findings$source_cell)))
})

test_that("a caller's table is left as it was, a data.table or a data.frame (D12.27)", {
  # An invalid byte and a blank cell in a character column: the reader rewrites its own
  # copy of both, never the caller's.
  frame <- data.frame(id = c("1", "2", "3"), comments = c("fine", bad_cell, ""))
  dt <- data.table::as.data.table(frame)
  frame_before <- data.table::copy(frame)
  dt_before <- data.table::copy(dt)
  read_input_table(frame, "datasets")
  read_input_table(dt, "datasets")
  expect_identical(frame, frame_before)
  expect_identical(dt, dt_before)
})

test_that("a long value is cut to about 40 characters around its marker (D12.28)", {
  long <- paste0(strrep("a", 50), bad_cell, strrep("b", 50))
  read <- read_input_table(data.frame(comments = long), "datasets")
  # 106 characters; the 40 shown start 20 before the marker's "<".
  cut <- paste0("...", strrep("a", 18), "ok<97>", strrep("b", 16), "...")
  expect_equal(read$findings$detail, kept("datasets row 1, column comments", cut))
  expect_equal(nchar(read$data$comments), 106L)
})

# A stand-in file for an origin's path: only its name and hash are read (D12.33).
stand_in <- function(fileext, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = fileext, .local_envir = env)
  writeLines("stand-in", path)
  path
}

test_that("an origin locates a data.frame's findings in its file (D12.33)", {
  workbook <- stand_in(".xlsx")
  frame <- data.frame(id = c("1", "2"), comments = c("fine", bad_cell))
  read <- read_input_table(frame, "datasets", list(path = workbook, sheet = "DD", rows = 5L))
  expect_equal(read$findings$detail, kept("cell DD!B6", "ok<97>"))
  expect_equal(read$findings$source_cell, "DD!B6")
  expect_equal(read$findings$file, basename(workbook))
  expect_equal(read$manifest$file, basename(workbook))
  expect_equal(read$manifest$sha256, unname(tools::sha256sum(workbook)))
  csv <- stand_in(".csv")
  by_row <- read_input_table(frame, "datasets", list(path = csv, rows = c(2L, 9L)))
  expect_equal(
    by_row$findings$detail, kept(paste0(basename(csv), " line 9, column comments"), "ok<97>")
  )
  named <- read_input_table(frame, "datasets", list(path = csv))
  expect_equal(named$findings$detail, kept("datasets row 2, column comments", "ok<97>"))
  expect_equal(named$findings$file, basename(csv))
  expect_true(is.na(named$findings$source_cell))
})

test_that("an origin's columns place each column in the file (D12.33)", {
  workbook <- stand_in(".xlsx")
  frame <- data.frame(id = c("1", "2"), comments = c("fine", bad_cell))
  origin <- list(path = workbook, sheet = "s", rows = 2L, columns = c(id = "A", comments = "D"))
  read <- read_input_table(frame, "datasets", origin)
  expect_equal(read$findings$source_cell, "s!D3")
})

test_that("an origin is checked, every column problem in one message (D12.33)", {
  csv <- stand_in(".csv")
  frame <- data.frame(a = "1", b = "2", c = "3")
  expect_error(
    read_input_table(frame, "datasets", list(path = csv, columns = c(a = 1, b = 1, z = 3))),
    paste0(
      "In the origin of datasets, `columns` must place every column of the data once: ",
      "not a column: z; not placed: c; position given twice: 1."
    ),
    fixed = TRUE
  )
  expect_error(read_input_table(frame, "datasets", list(path = file.path(csv, "x"))), "path")
  expect_error(read_input_table(frame, "datasets", list(path = csv, rows = 1L)), "rows")
  expect_error(read_input_table(csv, "datasets", list(path = csv)), "given as a path")
  expect_error(read_input_table(frame, "datasets", list(path = tempdir())), "path")
  expect_error(read_input_table(frame, "datasets", list(path = csv, sheet = "")), "sheet")
  # Blank means empty after trimming (D12.27).
  expect_error(read_input_table(frame, "datasets", list(path = csv, sheet = "  ")), "sheet")
  expect_error(
    read_input_table(frame, "datasets", list(path = csv, columns = c(a = 1, b = 2, c = 1e10))),
    "columns"
  )
})

test_that("an origin's rows are file rows inside the integer range, with no warning (D12.33)", {
  csv <- stand_in(".csv")
  frame <- data.frame(a = c("1", "2"))
  refused <- function(rows) {
    expect_no_warning(expect_error(
      read_input_table(frame, "datasets", list(path = csv, rows = rows)), "rows"
    ))
  }
  refused(c(5L, 1L))
  refused(c(5L, -3L))
  refused(Inf)
  refused(1e10)
  # The last data row would pass the integer maximum.
  refused(.Machine$integer.max)
  # One data row ends at the maximum, which is allowed.
  one <- data.frame(a = "1")
  edge <- list(path = csv, sheet = "s", rows = .Machine$integer.max)
  expect_no_warning(read <- read_input_table(one, "datasets", edge))
  expect_equal(read$where$row_map, c(.Machine$integer.max - 1L, .Machine$integer.max))
})

test_that("an origin's column letters beyond the integer range are an error, not a warning", {
  csv <- stand_in(".csv")
  frame <- data.frame(a = "1")
  expect_no_warning(expect_error(
    read_input_table(frame, "datasets", list(path = csv, columns = c(a = "ZZZZZZZZZZ"))),
    "columns"
  ))
})

test_that("an origin's columns name each column once (D12.33)", {
  csv <- stand_in(".csv")
  frame <- data.frame(a = "1", b = "2")
  expect_error(
    read_input_table(frame, "datasets", list(path = csv, columns = c(a = 1, a = 2, b = 3))),
    paste0(
      "In the origin of datasets, `columns` must place every column of the data once: ",
      "name given twice: a."
    ),
    fixed = TRUE
  )
})

test_that("a type map read from a file carries its findings (D12.55)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(
    charToRaw("data_type,r_class,date_format\nte"), as.raw(0x97), charToRaw("xt,character,\n")
  ), path)
  map <- gpq_type_map(path)
  findings <- attr(map, "gpq_read_findings")
  expect_equal(findings$rule_id, "spec_encoding_invalid")
  expect_equal(findings$input, "type_map")
  expect_equal(findings$source_cell, paste0(basename(path), ":2"))
  expect_null(attr(gpq_type_map(), "gpq_read_findings"))
})

test_that("a type map with a malformed line carries a spec_csv_malformed finding (D12.57)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("data_type,r_class,date_format", "character,character,", "x,character,,extra"), path)
  map <- gpq_type_map(path)
  findings <- attr(map, "gpq_read_findings")
  expect_equal(findings$rule_id, "spec_csv_malformed")
  expect_equal(findings$input, "type_map")
  expect_equal(findings$source_cell, paste0(basename(path), ":3"))
  expect_equal(map$data_type, "character")
})

fish_dictionary <- function() {
  read_input_table(example_file("fish_dictionary.csv"), "dictionary")
}

fish_columns <- function(...) {
  gpq_column_map(
    table = "table", attribute = "field", type = "kind", key_type = "key",
    reference = "parent", lookup = "codes", description = "notes", ...
  )
}

test_that("read_dictionary maps the fish dictionary's own columns and types", {
  fish_types <- gpq_type_map(example_file("fish_types.csv"))
  dd <- read_dictionary(fish_dictionary(), fish_columns(), fish_types)
  a <- dd$attributes
  expect_equal(nrow(a), 15L)
  expect_equal(a$r_class[a$attribute_name == "minutes"], "integer")
  expect_equal(a$source_row[[1L]], 2L)
  expect_true(all(is.na(a$lineage_flag)))
  expect_true(all(is.na(a$id_marked)))
  expect_equal(nrow(dd$findings), 0L)
})

test_that("the ID pattern and the lineage flag mark attributes", {
  dd <- read_dictionary(
    fish_dictionary(), fish_columns(lineage_flag = c(column = "codes", value = "y")),
    gpq_type_map(example_file("fish_types.csv")),
    id_pattern = "_id$"
  )
  a <- dd$attributes
  marked <- a[a$id_marked, ]
  expect_equal(
    sort(paste(marked$table_name, marked$attribute_name, sep = ".")),
    sort(c(
      "stations.station_id", "hauls.station_id", "hauls.haul_id", "catches.haul_id",
      "catches.catch_id"
    ))
  )
  expect_equal(a$attribute_name[a$lineage_flag], c("water_body", "gear", "species"))
})

test_that("detect_type_column finds one, none or both", {
  expect_equal(detect_type_column(c("a", "data_type"), c("data_type", "datatype"))$status, "ok")
  expect_equal(detect_type_column("a", c("data_type", "datatype"))$status, "none")
  both <- detect_type_column(c("datatype", "data_type"), c("data_type", "datatype"))
  expect_equal(both$status, "both")
  expect_equal(both$column, "data_type")
})

test_that("two type columns are a finding, not a stop", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = "a", data_type = "character", datatype = "character"
  )
  table <- read_input_table(dictionary, "dictionary")
  dd <- read_dictionary(table, gpq_column_map(), gpq_type_map())
  expect_equal(dd$findings$rule_id, "dd_type_column_ambiguous")
  expect_equal(dd$attributes$data_type, "character")
})

test_that("a dictionary without its table and attribute columns names both at once (D12.33)", {
  expect_error(
    read_dictionary(fish_dictionary(), gpq_column_map(), gpq_type_map()),
    paste(
      "The dictionary (fish_dictionary.csv) has no column table_name (the column map's",
      "table) and no column attribute_name (the column map's attribute); its columns are",
      "table, field, kind, key, parent, codes, notes."
    ),
    fixed = TRUE
  )
})

test_that("a data.frame dictionary's rows are no file's rows (D12.33)", {
  dictionary <- data.frame(table_name = "t", attribute_name = "a", data_type = "character")
  table <- read_input_table(dictionary, "dictionary")
  dd <- read_dictionary(table, gpq_column_map(), gpq_type_map())
  expect_equal(dd$attributes$source_row, NA_integer_)
})

test_that("build_keys numbers PK parts, reads FK targets and ignores case", {
  a <- data.table::data.table(
    table_name = c("p", "p", "c", "c"), attribute_name = c("p1", "p2", "c_id", "p1"),
    key_type = c("PK", "pk", "PK", "fk"), reference_table = c(NA, NA, NA, "p")
  )
  keys <- build_keys(a)
  expect_equal(keys$key_part, c(1L, 2L, 1L, 1L))
  expect_equal(keys$key_type, c("PK", "PK", "PK", "FK"))
  expect_true(is.na(keys$reference_attribute[[4L]]))
  single <- build_keys(a[-2L])
  expect_equal(single$reference_attribute[single$key_type == "FK"], "p1")
})

test_that("build_keys keeps an FK whose target has no PK, without a target column (D12.26)", {
  a <- data.table::data.table(
    table_name = c("p", "c"), attribute_name = "p1", key_type = c(".", "FK"),
    reference_table = c(NA, "p")
  )
  keys <- build_keys(a)
  expect_equal(keys$table_name, "c")
  expect_equal(keys$key_type, "FK")
  expect_equal(keys$reference_table, "p")
  expect_equal(keys$reference_attribute, NA_character_)
})
