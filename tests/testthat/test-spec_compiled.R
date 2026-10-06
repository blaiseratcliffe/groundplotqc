# Tests for the compiled specification's writer and reader (plan 3.6, 4.4; D12.14,
# D12.15, D12.20, D12.36, D12.54).

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
  expect_error(read_compiled_spec(dir), "codes.csv")
})

test_that("a compiled file that doesn't read cleanly is an internal error (D12.54)", {
  dir <- withr::local_tempdir()
  write_compiled_spec(fx_fish_spec(), dir)
  manifest <- file.path(dir, "manifest.csv")
  cat("\"x\",\"y\"\n\"a\",\"b\",\"c\",\"d\"\n", file = manifest, append = TRUE)
  expect_error(read_compiled_spec(dir), "manifest.csv")
})
