# Tests for the compiled specification's writer and reader (plan 3.6, 4.4; D12.14,
# D12.15, D12.20, D12.36, D12.54, D12.59, D12.69).

has_index <- function(spec) {
  any(vapply(spec, function(component) !is.null(attr(component, "index")), logical(1)))
}

test_that("a compiled spec reloads identical, tricky text included", {
  spec <- fx_planted_spec()
  dir <- withr::local_tempdir()
  paths <- write_compiled_spec(spec, dir)
  expect_equal(basename(paths), paste0(names(spec_schema()), ".csv"))
  back <- read_compiled_spec(dir)
  # Base identical(), not expect_identical(): no index attribute may differ (D12.27).
  expect_true(identical(back, spec))
  expect_false(has_index(spec))
  expect_false(has_index(back))
  extra <- back$code_lists$value[back$code_lists$sheet == "extra"]
  expect_equal(extra, c("extra", " lead", "NA", "01", "say \"hi\"", "café"))
})

test_that("a toy spec with empty components reloads identical", {
  spec <- fx_fish_spec()
  dir <- withr::local_tempdir()
  write_compiled_spec(spec, dir)
  back <- read_compiled_spec(dir)
  expect_true(identical(back, spec))
  expect_false(has_index(back))
})

test_that("one-column components with spaces and long whole numbers reload identical (R4, R20)", {
  spec <- gpq_read_spec(
    data.frame(table_name = "t", attribute_name = "id", key_type = "PK", data_type = "character"),
    code_lists = list(
      `band list` = data.frame(label = "AA", start = "123456789012345678", end = "1e18"),
      `label list` = data.frame(name = "AA")
    ),
    non_code_sheets = c("band list", "label list"),
    id_bands = list(
      sheet = "band list", label_col = "label", start_col = "start", end_col = "end",
      labels_from = c(sheet = "label list", column = "name"), reserved_pattern = NULL
    )
  )
  expect_equal(spec$id_bands$band_start, 123456789012345678)
  dir <- withr::local_tempdir()
  write_compiled_spec(spec, dir)
  expect_true(identical(read_compiled_spec(dir), spec))
})

test_that("a missing component file is an error naming it", {
  dir <- withr::local_tempdir()
  write_compiled_spec(fx_fish_spec(), dir)
  file.remove(file.path(dir, "codes.csv"))
  expect_error(read_compiled_spec(dir), "has no codes\\.csv")
})

test_that("a compiled file that doesn't read cleanly is an internal error (D12.54)", {
  dir <- withr::local_tempdir()
  write_compiled_spec(fx_fish_spec(), dir)
  manifest <- file.path(dir, "manifest.csv")
  cat("\"x\",\"y\"\n\"a\",\"b\",\"c\",\"d\"\n", file = manifest, append = TRUE)
  expect_error(read_compiled_spec(dir), "manifest\\.csv.*doesn't read cleanly")
})

test_that("a ragged datasets file, read as text, is the same internal error (D12.54)", {
  dir <- withr::local_tempdir()
  write_compiled_spec(fx_planted_spec(), dir)
  datasets <- file.path(dir, "datasets.csv")
  cat("\"3\",\"c\",\"r\",\"extra\"\n", file = datasets, append = TRUE)
  expect_error(read_compiled_spec(dir), "datasets\\.csv.*doesn't read cleanly")
})

test_that("under warn = 2 a warning is still the error naming the file, warn kept (D12.59)", {
  dir <- withr::local_tempdir()
  spec <- fx_fish_spec()
  write_compiled_spec(spec, dir)
  withr::local_options(warn = 2)
  expect_true(identical(read_compiled_spec(dir), spec))
  manifest <- file.path(dir, "manifest.csv")
  cat("\"x\",\"y\"\n\"a\",\"b\",\"c\",\"d\"\n", file = manifest, append = TRUE)
  expect_error(read_compiled_spec(dir), "manifest\\.csv.*doesn't read cleanly")
  expect_equal(getOption("warn"), 2)
})

test_that("the files end their lines with LF whatever the OS (D12.69)", {
  dir <- withr::local_tempdir()
  paths <- write_compiled_spec(fx_planted_spec(), dir)
  has_cr <- vapply(paths, function(path) {
    any(readBin(path, "raw", file.size(path)) == as.raw(0x0D))
  }, logical(1))
  expect_false(any(has_cr))
})

test_that("a string marked latin1 is written as UTF-8 (D12.14)", {
  spec <- fx_fish_spec()
  cafe <- "caf\xe9"
  Encoding(cafe) <- "latin1"
  spec$non_code_sheets <- data.table::data.table(sheet = cafe)
  dir <- withr::local_tempdir()
  write_compiled_spec(spec, dir)
  path <- file.path(dir, "non_code_sheets.csv")
  bytes <- readBin(path, "raw", file.size(path))
  # The line endings are D12.69's, tested apart; the bytes here are the text's.
  bytes <- bytes[bytes != as.raw(0x0D)]
  expect_identical(bytes, charToRaw("\"sheet\"\n\"café\"\n"))
})

test_that("a double with NA reloads identical, and the caller's spec is left as it was (R20)", {
  spec <- fx_planted_spec()
  spec$id_bands <- data.table::data.table(
    label = c("A", "B"), band_start = c(NA, 0.1), band_end = c(5, NA),
    reserved = c(NA, FALSE), source_row = 2:3, source_cell = NA_character_
  )
  before <- lapply(spec, data.table::copy)
  dir <- withr::local_tempdir()
  write_compiled_spec(spec, dir)
  expect_true(identical(unclass(spec), before))
  expect_true(identical(read_compiled_spec(dir), spec))
})

test_that("a one-column datasets table with an NA cell reloads identical", {
  spec <- fx_fish_spec(datasets = data.frame(id = c("1", NA, "3")))
  dir <- withr::local_tempdir()
  write_compiled_spec(spec, dir)
  expect_true(identical(read_compiled_spec(dir), spec))
})

test_that("blank non-code sheet names are dropped, so the spec reloads identical (D12.69)", {
  spec <- fx_fish_spec(non_code_sheets = c("a", "say \"hi\"", "", "b"))
  expect_equal(spec$non_code_sheets$sheet, c("a", "say \"hi\"", "b"))
  dir <- withr::local_tempdir()
  write_compiled_spec(spec, dir)
  expect_true(identical(read_compiled_spec(dir), spec))
})

test_that("the writer refuses a spec that doesn't validate, writing nothing (D12.54)", {
  spec <- unclass(fx_fish_spec())
  spec$keys <- NULL
  dir <- file.path(withr::local_tempdir(), "compiled")
  expect_error(write_compiled_spec(spec, dir), "has the components")
  expect_false(dir.exists(dir))
})
