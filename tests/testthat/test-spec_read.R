# Tests for reading a specification (plan 3.6, 4.1; D12.13 to D12.25, D12.45, D12.54).

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

test_that("cell references follow each kind of input", {
  expect_equal(
    cell_references(list(kind = "xlsx", sheet = "DD"), c(2L, 59L), c(3L, 8L)),
    c("DD!C2", "DD!H59")
  )
  expect_equal(cell_references(list(kind = "csv", file = "a.csv"), 4L, 2L), "a.csv:4")
  expect_equal(cell_references(list(kind = "memory"), 1:2, 1:2), c(NA_character_, NA_character_))
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
  # The leading space shows that readxl reads with trim_ws = FALSE (D12.9).
  expect_equal(read$data$value, c(NA, NA, "NA", " NT_PSP"))
  expect_equal(read$data$value[[4L]], " NT_PSP")
  # Only the dictionary is read from a workbook (D12.22, D12.34).
  expect_error(
    read_input_table(path, "datasets"),
    "`datasets` must be a data.frame or the path of an existing .csv file.",
    fixed = TRUE
  )
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
  expect_error(
    read_input_table(frame, "datasets", list(path = csv, columns = c(a = 1, b = 2, c = 1e10))),
    "columns"
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
