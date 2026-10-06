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
  # The whole message, so a decision ID, which a caller can't look up, can't creep back in.
  expect_error(
    new_gpq_spec(blank),
    "Component non_code_sheets, column sheet, holds an empty string; a blank is NA.",
    fixed = TRUE
  )
})

test_that("a component that isn't a data.table, or lacks the schema's columns, is refused", {
  frame <- empty_components()
  frame$keys <- as.data.frame(frame$keys)
  expect_error(validate_gpq_spec(frame), "Component keys must be a data.table.", fixed = TRUE)
  other <- empty_components()
  other$keys <- data.table::data.table(x = 1)
  columns <- paste(names(spec_schema()$keys), collapse = ", ")
  expect_error(
    validate_gpq_spec(other), paste0("Component keys has the columns ", columns, "."),
    fixed = TRUE
  )
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

test_that("read_input_table marks each blank header, and a CSV's of spaces is V<j> (D12.65)", {
  frame <- data.frame("1", "2", "3", "4")
  names(frame) <- c("code", "", "V3", "  ")
  from_frame <- read_input_table(frame, "datasets")
  from_csv <- read_input_table(csv_file(c("code,,V3,  ", "1,2,3,4")), "datasets")
  for (read in list(from_frame, from_csv)) {
    expect_named(read$data, c("code", "V2", "V3", "V4"))
    expect_equal(read$blank_header, c(FALSE, TRUE, FALSE, TRUE))
  }
  expect_equal(read_input_table(data.frame(), "datasets")$blank_header, logical())
  testthat::local_mocked_bindings(
    workbook_sheets = function(path) "dictionary",
    read_xlsx_raw = function(...) data.table::data.table(V1 = c("a", "1"), V2 = c(NA, "2"))
  )
  workbook <- read_input_table(testthat::test_path("fixtures", "blank_cells.xlsx"), "dictionary")
  expect_named(workbook$data, c("a", "V2"))
  expect_equal(workbook$blank_header, c(FALSE, TRUE))
})

test_that("reading a workbook without readxl is a classed error naming the package", {
  # base::requireNamespace() stands in for a library without readxl, for this test only.
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(
    workbook_sheets("any.xlsx"), "needs the readxl package; install it or pass data.frames.",
    class = "gpq_missing_package_error", fixed = TRUE
  )
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

test_that("a caller's table with an invalid byte and no blank cell is left as it was (D12.27)", {
  # No blank cell, so fix_invalid_utf8() is the one step that rewrites a cell of the reader's
  # copy; the caller's keeps its bad byte.
  frame <- data.frame(id = c("1", "2"), comments = c("fine", bad_cell))
  dt <- data.table::as.data.table(frame)
  frame_before <- data.table::copy(frame)
  dt_before <- data.table::copy(dt)
  for (table in list(frame, dt)) {
    read <- read_input_table(table, "datasets")
    expect_equal(read$data$comments, c("fine", "ok<97>"))
    expect_equal(read$invalid$row, 2L)
  }
  expect_identical(frame, frame_before)
  expect_identical(dt, dt_before)
  expect_false(validUTF8(frame$comments[[2L]]))
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
  # The message says where rows stop.
  expect_error(
    read_input_table(frame, "datasets", list(path = csv, rows = 1e10)),
    "no row can pass the integer maximum, 2147483647."
  )
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

test_that("read_dictionary maps the fish dictionary's own columns and types", {
  fish_types <- gpq_type_map(example_file("fish_types.csv"))
  dd <- read_dictionary(fish_dictionary(), fx_fish_column_map(), fish_types)
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
    fish_dictionary(), fx_fish_column_map(lineage_flag = c(column = "codes", value = "y")),
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

test_that("a dictionary's source_row follows its origin's rows (D12.33)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("a", "b", "c"), data_type = "character"
  )
  for (rows in list(5L, c(5L, 9L, 12L))) {
    origin <- list(path = stand_in(".csv"), rows = rows)
    table <- read_input_table(dictionary, "dictionary", origin)
    dd <- read_dictionary(table, gpq_column_map(), gpq_type_map())
    expect_equal(dd$attributes$source_row, if (length(rows) == 1L) 5:7 else rows)
  }
})

test_that("an .xlsx dictionary's source_row is its sheet's row, the header being row 1", {
  testthat::skip_if_not_installed("readxl")
  table <- read_input_table(testthat::test_path("fixtures", "blank_cells.xlsx"), "dictionary")
  # The fixture has one column, so the table and attribute roles share it.
  map <- gpq_column_map(table = "value", attribute = "value")
  dd <- read_dictionary(table, map, gpq_type_map())
  expect_equal(dd$attributes$source_row, 2:5)
})

test_that("a dictionary with none of the type columns is a finding, and no type is read", {
  dictionary <- data.frame(table_name = "t", attribute_name = c("a", "b"))
  table <- read_input_table(dictionary, "dictionary")
  dd <- read_dictionary(table, gpq_column_map(), gpq_type_map())
  expect_equal(dd$findings$rule_id, "dd_type_column_ambiguous")
  expect_equal(
    dd$findings$detail, "The dictionary has none of the type columns data_type, datatype."
  )
  expect_equal(dd$attributes$data_type, c(NA_character_, NA_character_))
  expect_equal(dd$attributes$r_class, c(NA_character_, NA_character_))
})

test_that("the lineage flag is NA for every row where its column isn't in the dictionary", {
  map <- fx_fish_column_map(lineage_flag = c(column = "no_such_column", value = "y"))
  dd <- read_dictionary(fish_dictionary(), map, gpq_type_map(example_file("fish_types.csv")))
  expect_equal(dd$attributes$lineage_flag, rep(NA, nrow(dd$attributes)))
  expect_equal(nrow(dd$findings), 0L)
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

test_that("build_keys doesn't match an NA table name to an FK with no reference table", {
  # A blank table name is no table, and a blank reference names none: no target for the FK.
  a <- data.table::data.table(
    table_name = c(NA, "c"), attribute_name = c("id", "fk"), key_type = c("PK", "FK"),
    reference_table = NA_character_
  )
  keys <- build_keys(a)
  expect_equal(keys$key_type, c("PK", "FK"))
  expect_equal(keys$reference_attribute, c(NA_character_, NA_character_))
})

test_that("build_keys gives the empty keys table where no row is a key (D12.26)", {
  a <- data.table::data.table(
    table_name = "t", attribute_name = c("a", "b"), key_type = c(".", NA),
    reference_table = NA_character_
  )
  expect_equal(build_keys(a), empty_table(spec_schema()$keys))
  expect_equal(build_keys(a[0L]), empty_table(spec_schema()$keys))
})

test_that("read_code_lists keeps every cell in long form, the header as row 1", {
  lists <- read_code_lists(fx_fish_code_lists())$long
  gear <- lists[lists$sheet == "gear", ]
  expect_equal(gear$value[gear$source_row == 1L], c("gear", "description"))
  gear_code <- gear$source_row == 2L & gear$sheet_column == "gear"
  expect_equal(gear$source_cell[gear_code], "fish_gear.csv:2")
  expect_equal(nrow(read_code_lists(NULL)$long), 0L)
  expect_error(read_code_lists(list(example_file("fish_gear.csv"))), "named list")
})

test_that("code_lists and crosswalks of the wrong shape are a caller's error (D12.54)", {
  # One string that isn't the path of an existing .xlsx: a CSV, a missing workbook.
  for (x in list("a.csv", "none.xlsx", example_file("fish_gear.csv"))) {
    expect_error(
      read_code_lists(x), "`code_lists` names no existing .xlsx file.",
      fixed = TRUE, info = x
    )
  }
  # Not a named list: a string, an unnamed list, a data.frame.
  for (x in list("x", list(data.frame(code = "A")), data.frame(code = "A"))) {
    expect_error(
      read_crosswalks(x), "`crosswalks` must be a named list.",
      fixed = TRUE, info = class(x)[[1L]]
    )
  }
  # A table that is neither a data.frame nor one path: a number, a list around one, two paths.
  for (x in list(5, list(table = 5), c("a.csv", "b.csv"))) {
    expect_error(
      read_crosswalks(list(td = x)), "Crosswalk td's table must be a data.frame or a CSV path.",
      fixed = TRUE, info = class(x)[[1L]]
    )
  }
})

test_that("code_lists and crosswalks names are unique, never blank, valid UTF-8 (R11, D12.54)", {
  twice <- list(kind = data.frame(kind = "A"), kind = data.frame(kind = "C"))
  expect_error(read_code_lists(twice), "unique")
  unnamed <- list(data.frame(a = "x"))
  names(unnamed) <- NA
  expect_error(read_code_lists(unnamed), "unique")
  invalid <- list(data.frame(a = "x"))
  names(invalid) <- bad_name
  expect_error(read_code_lists(invalid), "UTF-8")
  walks <- list(t = data.frame(code = "A"), t = data.frame(code = "B"))
  expect_error(read_crosswalks(walks), "unique")
})

test_that("read_code_lists lists every sheet, an empty one included (D12.29)", {
  expect_equal(read_code_lists(fx_fish_code_lists())$sheets, c("water_body", "gear", "species"))
  read <- read_code_lists(list(empty = data.frame(), gear = example_file("fish_gear.csv")))
  expect_equal(read$sheets, c("empty", "gear"))
  expect_false("empty" %in% read$long$sheet)
  expect_equal(read_code_lists(NULL)$sheets, character())
})

test_that("a code-list workbook's sheets are checked for invalid bytes (D12.24, D12.28)", {
  testthat::skip_if_not_installed("readxl")
  lists <- read_code_lists(testthat::test_path("fixtures", "invalid_utf8.xlsx"))
  expect_true(all(validUTF8(lists$long$value)))
  expect_equal(lists$findings$source_cell, c("codes!C1", "codes!B3"))
  expect_equal(lists$findings$detail, c(
    kept("cell codes!C1", "na<97>me"),
    kept("cell codes!B3", "ok<97>")
  ))
})

test_that("Excel numbers in a code-list workbook keep the text shown (D12.40)", {
  testthat::skip_if_not_installed("readxl")
  lists <- read_code_lists(testthat::test_path("fixtures", "excel_numbers.xlsx"))
  cells <- lists$long[lists$long$source_row > 1L, ]
  expect_equal(cells$value, c("170.03", "-1"))
  attributes <- data.table::data.table(table_name = "t", attribute_name = "class", lookup = "Y")
  none <- read_crosswalks(NULL)
  map <- build_code_list_map(attributes, lists$long, lists$sheets, none$long, none$declared)
  expect_equal(build_codes(map, lists$long, none$long)$code, c("170.03", "-1"))
})

test_that("a code-list sheet's repeated column names are made unique (D12.54)", {
  sheet <- csv_file(c("kind,kind,description", "A,B,first", "C,D,second"))
  attributes <- data.table::data.table(table_name = "t", attribute_name = "kind", lookup = "Y")
  read <- read_code_lists(list(kind = sheet))
  expect_equal(read$long$value[read$long$source_row == 1L], c("kind", "kind.1", "description"))
  none <- read_crosswalks(NULL)
  map <- build_code_list_map(attributes, read$long, read$sheets, none$long, none$declared)
  expect_equal(build_codes(map, read$long, none$long)$code, c("A", "C"))
})

test_that("the code-list map resolves y, named sheets and translation tables", {
  fish_types <- gpq_type_map(example_file("fish_types.csv"))
  dd <- read_dictionary(fish_dictionary(), fx_fish_column_map(), fish_types)
  read <- read_code_lists(fx_fish_code_lists())
  none <- read_crosswalks(NULL)
  map <- build_code_list_map(dd$attributes, read$long, read$sheets, none$long, none$declared)
  expect_equal(map$attribute_name, c("water_body", "gear", "species"))
  expect_equal(map$source_type, rep("sheet", 3L))
  expect_equal(map$code_column, c("water_body", "gear", "species"))
  expect_equal(map$status, rep("resolved", 3L))
  codes <- build_codes(map, read$long, none$long)
  expect_equal(codes$code[codes$attribute_name == "gear"], c("GN", "TN", "EF"))
})

test_that("a header-only sheet resolves and gives no codes; a missing list has no source", {
  attributes <- data.table::data.table(
    table_name = "t", attribute_name = c("kind", "colour"), lookup = c("Y", "palette")
  )
  read <- read_code_lists(list(kind = data.frame(kind = character(), description = character())))
  none <- read_crosswalks(NULL)
  map <- build_code_list_map(attributes, read$long, read$sheets, none$long, none$declared)
  expect_equal(map$status, c("resolved", "no_source"))
  expect_equal(nrow(build_codes(map, read$long, none$long)), 0L)
})

test_that("one translation table gives two attributes their own filtered lists", {
  table <- data.frame(
    code = c("FI", "HA", "PL", "FI"), treat_vs_dist = c("D", "T", "T", "D"),
    comment = c("fire", "harvest", "planting", "repeat")
  )
  walks <- read_crosswalks(list(treatment_disturbance = list(
    table = table, code_col = "code", filter_col = "treat_vs_dist",
    filter_values = list(disturbance_type = "D", treatment_type = "T")
  )))
  attributes <- data.table::data.table(
    table_name = c("dist", "treat"), attribute_name = c("disturbance_type", "treatment_type"),
    lookup = "treatment_disturbance"
  )
  no_sheets <- read_code_lists(NULL)$long
  map <- build_code_list_map(attributes, no_sheets, character(), walks$long, walks$declared)
  expect_equal(map$source_type, c("crosswalk", "crosswalk"))
  expect_equal(map$filter_values, c("D", "T"))
  codes <- build_codes(map, no_sheets, walks$long)
  expect_equal(codes$code[codes$attribute_name == "disturbance_type"], "FI")
  expect_equal(codes$code[codes$attribute_name == "treatment_type"], c("HA", "PL"))
})

test_that("a filter missing an attribute, or of the wrong shape, is a caller's error (D12.54)", {
  table <- data.frame(code = c("FI", "HA"), kind = c("D", "T"))
  walks <- read_crosswalks(list(td = list(
    table = table, code_col = "code", filter_col = "kind",
    filter_values = list(disturbance_type = "D")
  )))
  attributes <- data.table::data.table(
    table_name = "t", attribute_name = c("disturbance_type", "treatment_type"), lookup = "td"
  )
  no_sheets <- read_code_lists(NULL)$long
  expect_error(
    build_code_list_map(attributes, no_sheets, character(), walks$long, walks$declared),
    "no entry for attribute treatment_type"
  )
  expect_error(read_crosswalks(list(td = list(table = table, filter_col = "kind"))), "td")
  split_value <- list(table = table, filter_col = "kind", filter_values = "D; T")
  expect_error(read_crosswalks(list(td = split_value)), "; ")
  blank_value <- list(table = table, filter_col = "kind", filter_values = NA)
  expect_error(read_crosswalks(list(td = blank_value)), "NA")
})

test_that("a translation table that isn't UTF-8 or isn't there is crosswalk_unreadable", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(charToRaw("code,name\nA,caf"), as.raw(0xE9), charToRaw("\n")), path)
  walks <- read_crosswalks(list(condition = path, species = file.path(tempdir(), "none.csv")))
  expect_equal(walks$findings$rule_id, c("crosswalk_unreadable", "crosswalk_unreadable"))
  expect_equal(walks$findings$source_cell[[1L]], paste0(basename(path), ":2"))
  expect_true(all(validUTF8(walks$long$value)))
})

test_that("a CSV in UTF-16 stops a read, but is a finding as a translation table (R109)", {
  # UTF-16LE with its byte-order mark, built by hand from ASCII text: a zero byte after each
  # letter, so no locale or iconv is involved. fread() refuses it.
  utf16 <- function(text) {
    c(as.raw(c(0xFF, 0xFE)), as.vector(rbind(charToRaw(text), as.raw(0L))))
  }
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(utf16("code,name\nA,x\n"), path)
  expect_error(read_input_table(path, "dictionary"), "UTF-16")
  expect_error(gpq_read_spec(path), "UTF-16")
  dictionary <- data.frame(table_name = "t", attribute_name = "a", data_type = "character")
  spec <- gpq_read_spec(dictionary, crosswalks = list(cond = path))
  expect_equal(spec$read_findings$rule_id, "crosswalk_unreadable")
  expect_equal(
    spec$read_findings$detail, "Translation table cond can't be read as a CSV file."
  )
  expect_equal(nrow(spec$crosswalks), 0L)
  hashed <- spec$manifest$sha256[spec$manifest$input == "crosswalks:cond"]
  expect_equal(hashed, unname(tools::sha256sum(path)))
})

test_that("a malformed translation table is unreadable, and the next read is quiet (R12)", {
  ragged <- csv_file(c("code,name", "A,x", "B,y,z", "C,w"))
  walks <- read_crosswalks(list(td = ragged))
  expect_equal(walks$findings$detail, "Translation table td can't be read as a CSV file.")
  expect_no_warning(read_input_table(example_file("fish_gear.csv"), "code_lists:gear"))
})

test_that("a data.frame translation table's invalid byte names the table (D12.28, D12.29)", {
  bad <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xE9)))
  Encoding(bad) <- "UTF-8"
  walks <- read_crosswalks(list(cond = data.frame(code = c("A", bad))))
  expect_equal(walks$findings$rule_id, "crosswalk_unreadable")
  expect_equal(walks$findings$detail, paste0(
    "Text at translation table cond, row 2, column code, isn't valid UTF-8; it is kept as ",
    "\"caf<e9>\"", hex_note
  ))
})

test_that("a data.frame translation table's invalid column name names its column (D12.28)", {
  frame <- data.frame(code = c("A", "B"), other = c("p", "q"))
  names(frame)[[2L]] <- bad_name
  walks <- read_crosswalks(list(cond = frame))
  expect_equal(walks$findings$rule_id, "crosswalk_unreadable")
  expect_equal(walks$findings$detail, paste0(
    "The name of column 2 of translation table cond isn't valid UTF-8; it is kept as ",
    "\"na<97>me\"", hex_note
  ))
})

test_that("a data.frame translation table with an origin is located as its file (D12.33)", {
  bad <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xE9)))
  Encoding(bad) <- "UTF-8"
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("stand-in", path)
  walks <- read_crosswalks(
    list(cond = data.frame(code = c("A", bad))),
    origins = list(`crosswalks:cond` = list(path = path, rows = 2L))
  )
  expect_equal(
    walks$findings$detail, kept(paste0(basename(path), " line 3, column code"), "caf<e9>")
  )
  expect_equal(walks$findings$source_cell, paste0(basename(path), ":3"))
  expect_equal(walks$manifest$file, basename(path))
})

test_that("a translation table's header of spaces is V<j> as a CSV and a data.frame (D12.66)", {
  frame <- data.frame(code = c("A", "B"), x = c("p", "q"), y = c("r", "s"))
  names(frame) <- c("code", "  ", "V2")
  from_frame <- read_crosswalks(list(td = frame))$long
  from_csv <- read_crosswalks(list(td = csv_file(c("code,  ,V2", "A,p,r", "B,q,s"))))$long
  expect_equal(unique(from_csv$crosswalk_column), c("code", "V2", "V2.1"))
  expect_equal(from_csv$crosswalk_column, from_frame$crosswalk_column)
  expect_equal(from_csv$value, from_frame$value)
})

test_that("a blank workbook header is named V<j>, its cell left NA, as in other forms (D12.64)", {
  testthat::skip_if_not_installed("readxl")
  raw <- data.table::data.table(
    V1 = c("a", "x"), V2 = c(NA, "y"), V3 = c("c", "z")
  )
  testthat::local_mocked_bindings(read_xlsx_raw = function(...) data.table::copy(raw))
  workbook <- read_code_lists(testthat::test_path("fixtures", "blank_cells.xlsx"))
  from_csv <- read_code_lists(list(Sheet1 = csv_file(c("a,,c", "x,y,z"))))
  expect_equal(workbook$long$value[workbook$long$source_row == 1L], c("a", NA, "c"))
  expect_equal(workbook$long$value, from_csv$long$value)
  expect_equal(workbook$long$sheet_column, from_csv$long$sheet_column)
  expect_equal(workbook$long$sheet_column[1:3], c("a", "a", "V2"))
})

test_that("an unreadable translation table CSV still has its hash in the manifest (D12.61)", {
  ragged <- csv_file(c("code,name", "A,x", "B,y,z", "C,w"))
  walks <- read_crosswalks(list(td = ragged))
  expect_equal(walks$findings$rule_id, "crosswalk_unreadable")
  expect_equal(walks$manifest$sha256, unname(tools::sha256sum(ragged)))
  absent <- read_crosswalks(list(td = file.path(tempdir(), "none.csv")))
  expect_true(is.na(absent$manifest$sha256))
})

test_that("a filter entry that is NULL or empty, or a filter column that isn't there, is refused", {
  table <- data.frame(code = c("FI", "HA"), kind = c("D", "T"))
  attributes <- data.table::data.table(
    table_name = "t", attribute_name = c("disturbance_type", "treatment_type"), lookup = "td"
  )
  no_sheets <- read_code_lists(NULL)$long
  for (entry in list(NULL, character())) {
    values <- list(disturbance_type = "D", treatment_type = entry)
    walks <- read_crosswalks(list(td = list(
      table = table, code_col = "code", filter_col = "kind", filter_values = values
    )))
    expect_error(
      build_code_list_map(attributes, no_sheets, character(), walks$long, walks$declared),
      "no entry for attribute treatment_type"
    )
  }
  absent <- read_crosswalks(list(td = list(
    table = table, code_col = "code", filter_col = "missing", filter_values = "D"
  )))
  expect_error(
    build_code_list_map(attributes, no_sheets, character(), absent$long, absent$declared),
    "filter_col missing isn't a column"
  )
})

test_that("an unreadable translation table with a filter gives no_code_column, not an error", {
  ragged <- csv_file(c("code,kind", "A,D", "B,T,z"))
  filtered <- list(table = ragged, filter_col = "kind", filter_values = "D")
  walks <- read_crosswalks(list(td = filtered))
  attributes <- data.table::data.table(table_name = "t", attribute_name = "x", lookup = "td")
  no_sheets <- read_code_lists(NULL)$long
  expect_equal(walks$findings$rule_id, "crosswalk_unreadable")
  map <- build_code_list_map(attributes, no_sheets, character(), walks$long, walks$declared)
  expect_equal(map$status, "no_code_column")
})

test_that("a lower-case y names the sheet after the attribute (D12.31)", {
  attributes <- data.table::data.table(table_name = "t", attribute_name = "kind", lookup = "y")
  read <- read_code_lists(list(kind = data.frame(kind = c("A", "B"))))
  none <- read_crosswalks(NULL)
  map <- build_code_list_map(attributes, read$long, read$sheets, none$long, none$declared)
  expect_equal(map$source_name, "kind")
  expect_equal(map$status, "resolved")
})

test_that("a sheet with no code column is no_code_column and gives no codes", {
  attributes <- data.table::data.table(table_name = "t", attribute_name = "kind", lookup = "Y")
  read <- read_code_lists(list(kind = data.frame(other = c("A", "B"))))
  none <- read_crosswalks(NULL)
  map <- build_code_list_map(attributes, read$long, read$sheets, none$long, none$declared)
  expect_equal(map$status, "no_code_column")
  expect_equal(nrow(build_codes(map, read$long, none$long)), 0L)
})

test_that("a translation table's declared code_col is the fallback, and no filter keeps all", {
  table <- data.frame(abbr = c("FI", "HA", "FI"), kind = c("D", "T", "D"))
  walks <- read_crosswalks(list(td = list(table = table, code_col = "abbr")))
  attributes <- data.table::data.table(
    table_name = "t", attribute_name = "disturbance_type", lookup = "td"
  )
  no_sheets <- read_code_lists(NULL)$long
  map <- build_code_list_map(attributes, no_sheets, character(), walks$long, walks$declared)
  expect_equal(map$code_column, "abbr")
  expect_true(is.na(map$filter_column))
  expect_equal(build_codes(map, no_sheets, walks$long)$code, c("FI", "HA"))
})

test_that("a translation table whose read warns is unreadable, with no warning escaping", {
  path <- csv_file(c("code,name", "A,x"))
  real_fread <- data.table::fread
  # Only this file's read warns; the report text's own CSV reads as it is.
  testthat::local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    if (identical(list(...)$file, path)) warning("A new warning.", call. = FALSE)
    out
  })
  expect_no_warning(walks <- read_crosswalks(list(td = path)))
  expect_equal(walks$findings$rule_id, "crosswalk_unreadable")
  expect_equal(walks$findings$detail, "Translation table td can't be read as a CSV file.")
})

band_lists <- function() {
  read_code_lists(list(
    ranges = data.frame(
      contributor = c("AB", "PEI", "reserved_1", NA, "BC"),
      v2_start = c("1", "100", "200", NA, "1.5"), v2_end = c("99", "199", "299", NA, "9")
    ),
    contributor = data.frame(abbreviated_name = c("AB", "PE", "BC"))
  ))$long
}

band_spec <- list(
  sheet = "ranges", label_col = "contributor", start_col = "v2_start", end_col = "v2_end",
  labels_from = c(sheet = "contributor", column = "abbreviated_name"),
  reserved_pattern = "^reserved_[0-9]+$"
)

test_that("build_id_bands reads bands and reports unknown labels and bad bounds", {
  built <- build_id_bands(band_spec, band_lists(), "lookup.xlsx")
  expect_equal(built$bands$label, c("AB", "PEI", "reserved_1", "BC"))
  expect_equal(built$bands$reserved, c(FALSE, FALSE, TRUE, FALSE))
  expect_true(is.na(built$bands$band_start[[4L]]))
  expect_equal(built$findings$rule_id, rep("site_id_range_invalid", 2L))
  expect_match(built$findings$detail[[1L]], "1.5", fixed = TRUE)
  expect_match(built$findings$detail[[2L]], "PEI", fixed = TRUE)
})

test_that("a bands sheet without its columns is one finding", {
  wrong <- modifyList(band_spec, list(end_col = "v3_end"))
  built <- build_id_bands(wrong, band_lists(), "lookup.xlsx")
  expect_equal(nrow(built$bands), 0L)
  expect_equal(built$findings$rule_id, "site_id_range_invalid")
})

test_that("id_bands of the wrong shape is a caller's error (R10)", {
  expect_error(build_id_bands("ranges", band_lists(), "f"), "id_bands")
  two <- modifyList(band_spec, list(label_col = c("contributor", "v2_start")))
  expect_error(build_id_bands(two, band_lists(), "f"), "id_bands")
  pattern <- modifyList(band_spec, list(reserved_pattern = "("))
  expect_error(build_id_bands(pattern, band_lists(), "f"), "regular expression")
})

test_that("build_id_bands finds an inverted band and an overlap, citing both bands (D12.28)", {
  lists <- read_code_lists(list(
    ranges = data.frame(
      contributor = c("AB", "BC", "ON"), v2_start = c("1", "50", "300"),
      v2_end = c("100", "150", "250")
    ),
    contributor = data.frame(abbreviated_name = c("AB", "BC", "ON"))
  ))$long
  built <- build_id_bands(band_spec, lists, "in memory")
  expect_equal(built$findings$detail, c(
    "Band \"ON\" starts at 300, after it ends at 250.",
    "Bands \"AB\" (row 1) and \"BC\" (row 2) overlap."
  ))
  expect_equal(unique(built$findings$file), "in memory")
})

test_that("a band overlapping two others is cited against the one that ends last (D12.28)", {
  lists <- read_code_lists(list(
    ranges = data.frame(
      contributor = c("AB", "BC", "ON"), v2_start = c("1", "50", "60"),
      v2_end = c("100", "150", "70")
    ),
    contributor = data.frame(abbreviated_name = c("AB", "BC", "ON"))
  ))$long
  built <- build_id_bands(band_spec, lists, "in memory")
  # ON overlaps AB as well, but each band is cited once, against the band whose end is the
  # latest so far, so only the first two pairs are named. Order isn't part of the contract.
  expect_equal(sort(built$findings$detail), sort(c(
    "Bands \"AB\" (row 1) and \"BC\" (row 2) overlap.",
    "Bands \"BC\" (row 2) and \"ON\" (row 3) overlap."
  )))
  # Two bands inside a third that don't touch each other: each is cited against the third,
  # which holds the latest end, not against the band before it.
  nested <- read_code_lists(list(
    ranges = data.frame(
      contributor = c("AB", "BC", "ON"), v2_start = c("1", "10", "30"),
      v2_end = c("200", "20", "40")
    ),
    contributor = data.frame(abbreviated_name = c("AB", "BC", "ON"))
  ))$long
  expect_equal(sort(build_id_bands(band_spec, nested, "in memory")$findings$detail), sort(c(
    "Bands \"AB\" (row 1) and \"BC\" (row 2) overlap.",
    "Bands \"AB\" (row 1) and \"ON\" (row 3) overlap."
  )))
})

test_that("sheet_file finds a sheet's own CSV, else the workbook", {
  per_sheet <- data.table::data.table(
    input = c("code_lists:contributor", "code_lists:ranges"), file = c("c.csv", "r.csv")
  )
  expect_equal(sheet_file(per_sheet, "ranges"), "r.csv")
  workbook <- data.table::data.table(input = "code_lists", file = "lookup.xlsx")
  expect_equal(sheet_file(workbook, "ranges"), "lookup.xlsx")
  expect_true(is.na(sheet_file(NULL, "ranges")))
})

test_that("each bad bound is one finding, a blank one with its own text, no overlap (D12.33)", {
  lists <- read_code_lists(list(
    ranges = data.frame(
      contributor = c("AB", "BC", "ON", "QC"), v2_start = c(NA, "50", "x", "1"),
      v2_end = c("100", NA, NA, "100")
    ),
    contributor = data.frame(abbreviated_name = c("AB", "BC", "ON", "QC"))
  ))$long
  built <- build_id_bands(band_spec, lists, "in memory")
  # AB and BC would overlap QC if their bounds were whole; ON has two bad bounds.
  # Findings' order isn't part of the contract: compared sorted (D12.30).
  expect_equal(sort(built$findings$detail), sort(c(
    "Band \"AB\" has a blank start.", "Band \"BC\" has a blank end.",
    "Band \"ON\" has a bound that isn't a whole number: \"x\".", "Band \"ON\" has a blank end."
  )))
})

test_that("a bound must be written in decimal digits and be finite (R16)", {
  lists <- read_code_lists(list(
    ranges = data.frame(
      contributor = c("AB", "BC", "ON"), v2_start = c("1", "0x10", "1e3"),
      v2_end = c("Inf", "20", "2000")
    ),
    contributor = data.frame(abbreviated_name = c("AB", "BC", "ON"))
  ))$long
  built <- build_id_bands(band_spec, lists, "in memory")
  expect_equal(built$bands$band_start, c(1, NA, 1000))
  expect_equal(built$bands$band_end, c(NA, 20, 2000))
  expect_equal(sort(built$findings$detail), sort(c(
    "Band \"AB\" has a bound that isn't a whole number: \"Inf\".",
    "Band \"BC\" has a bound that isn't a whole number: \"0x10\"."
  )))
})

test_that("a blank label is the band's only finding, labels_from or not (D12.36, D12.37)", {
  lists <- read_code_lists(list(
    ranges = data.frame(
      contributor = c("AB", NA, NA), v2_start = c("1", "50", NA),
      v2_end = c("100", "150", "300")
    ),
    contributor = data.frame(abbreviated_name = "AB")
  ))$long
  # Row 2 would overlap AB and row 3 has a blank start: neither is checked without a label.
  only_blank <- c("The band at row 2 has a blank label.", "The band at row 3 has a blank label.")
  expect_equal(build_id_bands(band_spec, lists, "in memory")$findings$detail, only_blank)
  no_list <- band_spec
  no_list["labels_from"] <- list(NULL)
  expect_equal(build_id_bands(no_list, lists, "in memory")$findings$detail, only_blank)
})

test_that("a precedence input names every missing and extra column at once (D12.33)", {
  rows <- data.frame(sheet = "s", key_col = "k", attribute_name = "c", decision = "D1")
  expect_error(
    read_precedence(rows),
    paste(
      "`precedence` must have exactly the columns sheet, key_col, attribute_name, winner,",
      "blank_rule; missing: winner, blank_rule; not taken: decision."
    ),
    fixed = TRUE
  )
})

test_that("a precedence input has one row per sheet, key and attribute (D12.54)", {
  rows <- data.frame(
    sheet = "s", key_col = "k", attribute_name = "c", winner = c("datasets", "code_lists"),
    blank_rule = "wins"
  )
  expect_error(read_precedence(rows), "more than one row")
})

test_that("an origin's columns give a precedence input its file's letters (D12.33)", {
  bad <- rawToChar(as.raw(c(0x6E, 0x61, 0x97, 0x6D, 0x65)))
  Encoding(bad) <- "UTF-8"
  rows <- data.frame(
    sheet = "dataset", key_col = "magp_dataset_id", attribute_name = bad, winner = "datasets",
    blank_rule = "wins"
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("stand-in", path)
  # The file's column C held a note that was dropped, so attribute_name is its column D.
  origin <- list(
    path = path, sheet = "precedence", rows = 2L,
    columns = c(sheet = 1, key_col = 2, attribute_name = 4, winner = 5, blank_rule = 6)
  )
  read <- read_precedence(rows, origin)
  expect_equal(read$findings$source_cell, "precedence!D2")
  expect_equal(read$findings$detail, kept("cell precedence!D2", "na<97>me"))
})

test_that("both toy specs read cleanly", {
  fish <- fx_fish_spec()
  expect_s3_class(fish, "gpq_spec")
  expect_equal(nrow(fish$keys), 5L)
  expect_equal(nrow(fish$read_findings), 0L)
  expect_equal(fish$manifest$input, c(
    "dictionary", "code_lists:water_body", "code_lists:gear", "code_lists:species"
  ))
  forest <- fx_forest_spec()
  expect_equal(sort(unique(forest$codes$attribute_name)), c("SPECIES", "STATUS"))
  expect_equal(sum(forest$attributes$id_marked), 5L)
  expect_equal(nrow(forest$read_findings), 0L)
})

test_that("absent inputs give empty components with their columns", {
  fish <- fx_fish_spec()
  for (name in c("datasets", "lineage_spec", "crosswalks", "id_bands", "clashes")) {
    expect_equal(nrow(fish[[name]]), 0L, info = name)
  }
  expect_named(fish$id_bands, names(spec_schema()$id_bands))
})

test_that("given inputs each get a manifest row", {
  fish <- fx_fish_spec(non_code_sheets = "notes", id_pattern = "_id$")
  expect_true(all(c("non_code_sheets", "id_pattern") %in% fish$manifest$input))
  expect_equal(fish$non_code_sheets$sheet, "notes")
})

test_that("the arguments are checked", {
  expect_error(fx_fish_spec(id_pattern = c("a", "b")), "id_pattern")
  expect_error(fx_fish_spec(id_pattern = "("), "id_pattern")
  expect_error(
    gpq_read_spec(example_file("fish_dictionary.csv"), column_map = list()), "column_map"
  )
  expect_error(gpq_read_spec(file.path(tempdir(), "missing.csv")), "dictionary")
  expect_error(fx_fish_spec(non_code_sheets = bad_name), "non_code_sheets")
  # A blank name is no sheet, so it is dropped, as a repeat is (D12.69).
  blanks <- fx_fish_spec(non_code_sheets = c("notes", "  ", "", NA, "notes"))
  expect_equal(blanks$non_code_sheets$sheet, "notes")
  expect_true("non_code_sheets" %in% blanks$manifest$input)
  only_blank <- fx_fish_spec(non_code_sheets = c("", " "))
  expect_equal(nrow(only_blank$non_code_sheets), 0L)
  expect_named(only_blank$non_code_sheets, "sheet")
  expect_type(only_blank$non_code_sheets$sheet, "character")
})

test_that("a type map's findings join read_findings (D12.55)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("data_type,r_class,date_format", "character,character,", "x,character,,extra"), path)
  spec <- gpq_read_spec(
    data.frame(table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"),
    type_map = gpq_type_map(path)
  )
  expect_equal(spec$read_findings$rule_id, "spec_csv_malformed")
  expect_equal(spec$read_findings$input, "type_map")
  expect_null(attr(spec$type_map, "gpq_read_findings"))
})

test_that("a type map's invalid byte reaches read_findings (D12.57)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(
    charToRaw("data_type,r_class,date_format\ncharacter,character,\nte"), as.raw(0x97),
    charToRaw("xt,character,\n")
  ), path)
  spec <- gpq_read_spec(
    data.frame(table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"),
    type_map = gpq_type_map(path)
  )
  expect_equal(spec$read_findings$rule_id, "spec_encoding_invalid")
  expect_equal(spec$read_findings$input, "type_map")
  expect_equal(spec$read_findings$source_cell, paste0(basename(path), ":3"))
})

test_that("an in-memory dictionary with an origin is located as its workbook (D12.33)", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("stand-in", path)
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  dictionary <- data.frame(
    table_name = c("plots", "visits", "visits"),
    attribute_name = c("plot_id", "visit_id", "src_plot_id"), key_type = c("PK", "PK", "."),
    data_type = "character", description = c("Plot.", bad, "Source plot.")
  )
  spec <- gpq_read_spec(
    dictionary,
    lineage_spec = data.frame(
      contributor_label = "BC", table_name = "plots", attribute_name = "src_plot_id",
      spec_type = "id", source_text = "tbl.key", note = NA, source_cell = "A2!D11"
    ),
    origins = list(dictionary = list(path = path, sheet = "DD", rows = 2L))
  )
  expect_equal(spec$attributes$source_row, 2:4)
  expect_equal(spec$read_findings$source_cell, "DD!E3")
  expect_equal(spec$read_findings$detail, kept("cell DD!E3", "ok<97>"))
  expect_equal(spec$read_findings$file, basename(path))
  dictionary_row <- spec$manifest[spec$manifest$input == "dictionary", ]
  expect_equal(dictionary_row$file, basename(path))
  expect_equal(dictionary_row$sha256, unname(tools::sha256sum(path)))
  expect_equal(spec$clashes$source_cell_a, "A2!D11")
  expect_equal(spec$clashes$source_cell_b, paste0(basename(path), ":4"))
})

test_that("origins name only inputs given as data.frames (D12.33)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("stand-in", path)
  origin <- list(path = path)
  expect_error(
    gpq_read_spec(example_file("fish_dictionary.csv"), origins = list(dictionary = origin)),
    "given as a path"
  )
  dictionary <- data.frame(table_name = "t", attribute_name = "a", data_type = "character")
  expect_error(
    gpq_read_spec(dictionary, origins = list(datasets = origin)),
    "`origins` names inputs that weren't given: datasets.",
    fixed = TRUE
  )
})

test_that("origins refuse a repeated name and say why an input takes none (D12.62)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("stand-in", path)
  origin <- list(path = path)
  dictionary <- data.frame(table_name = "t", attribute_name = "a", data_type = "character")
  expect_error(
    gpq_read_spec(dictionary, origins = list(dictionary = origin, dictionary = origin)),
    "`origins` names an input more than once: dictionary.",
    fixed = TRUE
  )
  expect_error(
    gpq_read_spec(dictionary, id_pattern = "_id$", origins = list(id_pattern = origin)),
    "`origins` names inputs that take no origin: id_pattern.",
    fixed = TRUE
  )
  workbook <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("stand-in", workbook)
  expect_error(
    gpq_read_spec(dictionary, code_lists = workbook, origins = list(`code_lists:gear` = origin)),
    "given as a path"
  )
})

test_that("an origin, or origins, of the wrong shape is a caller's error (D12.33)", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("stand-in", path)
  dictionary <- data.frame(table_name = "t", attribute_name = "a", data_type = "character")
  # An origin that isn't a named list of path, sheet, rows and columns only.
  for (origin in list("x", list(1), list(path = path, extra = 1), data.frame(path = path))) {
    expect_error(
      gpq_read_spec(dictionary, origins = list(dictionary = origin)),
      "The origin of dictionary must be list(path = , sheet = , rows = , columns = ).",
      fixed = TRUE, info = class(origin)[[1L]]
    )
  }
  # origins that isn't a list with every element named.
  for (origins in list("x", list(1), list(dictionary = list(path = path), 2), data.frame(a = 1))) {
    expect_error(
      gpq_read_spec(dictionary, origins = origins),
      "`origins` must be NULL or a list named by input.",
      fixed = TRUE, info = class(origins)[[1L]]
    )
  }
})

test_that("each clash finding names its own sheet's file (D12.28)", {
  dir <- withr::local_tempdir()
  first <- file.path(dir, "first_list.csv")
  second <- file.path(dir, "second_list.csv")
  writeLines(c("first_list", "A"), first)
  writeLines(c("code_id,label", "k1,One", "k2,Two"), second)
  findings <- function(key_col) {
    gpq_read_spec(
      data.frame(table_name = "t", attribute_name = "a", data_type = "character"),
      code_lists = list(first_list = first, dataset = second),
      datasets = data.frame(code_id = "k1", label = "Uno"),
      precedence = data.frame(
        sheet = "dataset", key_col = key_col, attribute_name = "label", winner = "datasets",
        blank_rule = "wins"
      )
    )$read_findings
  }
  absent <- findings("code_id")
  expect_equal(absent$rule_id, "datasets_row_missing")
  expect_equal(absent$file, "second_list.csv")
  expect_equal(absent$source_cell, "second_list.csv:3")
  unresolved <- findings("no_such_key")
  expect_equal(unresolved$rule_id, "spec_clash_unresolved")
  expect_equal(unresolved$file, "second_list.csv")
})

test_that("id_bands that isn't a list of names is the package's own error", {
  # fx_fish_spec() gives code lists, so there is a manifest; a string isn't a list, so no
  # bands sheet is named and sheet_file() is never called: the error is the shape check's.
  expect_error(fx_fish_spec(id_bands = "bands"), "`id_bands` is list(", fixed = TRUE)
  expect_error(
    fx_fish_spec(id_bands = list(sheet = c("a", "b"))), "`id_bands` is list(",
    fixed = TRUE
  )
})

test_that("a type map's findings attribute must have the read_findings columns", {
  types <- gpq_type_map()
  attr(types, "gpq_read_findings") <- data.frame(rule_id = "x", note = "y")
  dictionary <- data.frame(table_name = "t", attribute_name = "a", data_type = "character")
  expect_error(gpq_read_spec(dictionary, type_map = types), "gpq_read_findings")
})

test_that("a caller's tables come back unchanged, sharing no column with the spec", {
  dictionary <- data.table::data.table(
    table_name = "t", attribute_name = c("a", "b"), key_type = c("PK", NA),
    data_type = "character", description = c(bad_cell, "fine")
  )
  data.table::setindex(dictionary, table_name)
  types <- gpq_type_map()
  dictionary_before <- data.table::copy(dictionary)
  types_before <- data.table::copy(types)
  spec <- gpq_read_spec(dictionary, type_map = types)
  expect_identical(dictionary, dictionary_before)
  expect_identical(types, types_before)
  expect_false(is.null(attr(dictionary, "index")))
  caller <- c(
    vapply(dictionary, data.table::address, ""), vapply(types, data.table::address, "")
  )
  in_spec <- unlist(lapply(spec, function(x) vapply(x, data.table::address, "")))
  expect_length(intersect(caller, in_spec), 0L)
})
