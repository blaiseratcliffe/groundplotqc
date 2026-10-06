# Tests for the scripts in data-raw/ (D12.23). They run from the source tree and skip in a
# built package, which leaves data-raw out (plan 19.3).

load_data_raw <- function(name) {
  path <- testthat::test_path("..", "..", "data-raw", name)
  if (!file.exists(path)) {
    testthat::skip(paste(name, "is not in this tree"))
  }
  env <- new.env(parent = asNamespace("groundplotqc"))
  sys.source(path, envir = env)
  env
}

hand_kept <- function(name) {
  path <- testthat::test_path("..", "..", "data-raw", "magp", name)
  if (!file.exists(path)) {
    testthat::skip(paste(name, "is not in this tree"))
  }
  read_csv_text(path)$data
}

test_that("spec_files finds one file per kind and stops on two of one kind", {
  build <- load_data_raw("build_magp_config.R")
  dir <- withr::local_tempdir()
  kinds <- c("DD.xlsx", "Lookup_Tables.xlsx", "datasets.csv", "A2.xlsx", "species.csv")
  file.create(file.path(dir, paste0("20260925_magpv2_", kinds)))
  files <- build$spec_files(dir)
  expect_equal(sort(names(files)), sort(c("DD", "Lookup_Tables", "datasets", "A2", "species")))
  file.create(file.path(dir, "20261003_magpv2_DD.xlsx"))
  expect_error(build$spec_files(dir), "more than one dated file of: DD")
})

test_that("apply_spec_exceptions applies a row and stops on a stale one", {
  build <- load_data_raw("build_magp_config.R")
  dictionary <- data.table::data.table(
    table_name = "s", attribute_name = "ec_zone", data_type = "numeric"
  )
  rows <- data.table::data.table(
    exception_id = "x", table_name = "s", attribute_name = "ec_zone", dd_column = "data_type",
    dd_value = "numeric", applied_value = "character", decision = "D2.17", note = NA
  )
  applied <- build$apply_spec_exceptions(data.table::copy(dictionary), rows)
  expect_equal(applied$data_type, "character")
  dictionary$data_type <- "character"
  expect_error(build$apply_spec_exceptions(dictionary, rows), "row x is stale")
})

test_that("a DD row an exception fixes gives no finding; the raw row is located (D12.33)", {
  build <- load_data_raw("build_magp_config.R")
  workbook <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("stand-in", workbook)
  dictionary <- data.table::data.table(
    table_name = "s", attribute_name = c("s_id", "ec_zone"), key_type = c("PK", "."),
    data_type = c("character", "text")
  )
  rows <- data.table::data.table(
    exception_id = "x", table_name = "s", attribute_name = "ec_zone", dd_column = "data_type",
    dd_value = "text", applied_value = "character", decision = "D2.17", note = NA
  )
  origins <- list(dictionary = list(path = workbook, sheet = "DD", rows = 2L))
  unknown_type <- function(dd) {
    results <- preflight_checks(gpq_read_spec(dd, origins = origins))
    results[results$rule_id == "dd_type_unknown", ]
  }
  raw <- unknown_type(data.table::copy(dictionary))
  expect_equal(raw$source_cell, paste0(basename(workbook), ":3"))
  fixed <- unknown_type(build$apply_spec_exceptions(data.table::copy(dictionary), rows))
  expect_equal(fixed$outcome, "pass")
})

test_that("the manifest names hand-kept files by repo path, the exceptions included (D12.33)", {
  build <- load_data_raw("build_magp_config.R")
  config <- withr::local_tempdir()
  writeLines("exception_id", file.path(config, "spec_exceptions.csv"))
  writeLines("sheet", file.path(config, "precedence.csv"))
  spec <- gpq_read_spec(
    data.frame(table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"),
    precedence = data.frame(
      sheet = "s", key_col = "k", attribute_name = "c", winner = "datasets", blank_rule = "wins"
    ),
    origins = list(precedence = list(path = file.path(config, "precedence.csv"), rows = 2L))
  )
  manifest <- build$record_hand_kept(spec, config)$manifest
  expect_equal(manifest$file[manifest$input == "precedence"], "data-raw/magp/precedence.csv")
  exceptions <- manifest[manifest$input == "dictionary:exceptions", ]
  expect_equal(exceptions$file, "data-raw/magp/spec_exceptions.csv")
  expect_equal(
    exceptions$sha256, unname(tools::sha256sum(file.path(config, "spec_exceptions.csv")))
  )
})

test_that("build_lineage_input reads A2's layout into the long lineage input", {
  build <- load_data_raw("build_magp_config.R")
  raw <- data.table::data.table(
    V1 = c("Appendix A2", "notes", "need confirmation", "type", "id", "compiled"),
    V2 = c(NA, NA, NA, "magp_table", "magp_sites", "magp_plot_meas"),
    V3 = c(NA, NA, NA, "attribute", "src_site_id", "src_dbh_cutoff"),
    V4 = c(NA, NA, NA, "BC_src", "faib_header.site_identifier", "faib.x"),
    V5 = c(NA, NA, NA, "BC_note", NA, "a note"),
    V6 = c(NA, NA, NA, "ON_src", "tblPlot.plotkey", NA),
    V7 = c(NA, NA, NA, "ON_note", NA, NA)
  )
  long <- build$build_lineage_input(raw)
  expect_named(long, spec_input_schema()$lineage_spec)
  expect_equal(long$contributor_label, c("BC", "BC", "ON", "ON"))
  expect_equal(long$source_cell, c("A2!D5", "A2!D6", "A2!F5", "A2!F6"))
  expect_equal(long$note[[2L]], "a note")
})

test_that("the hand-kept files have their columns", {
  expect_named(hand_kept("spec_exceptions.csv"), c(
    "exception_id", "table_name", "attribute_name", "dd_column", "dd_value", "applied_value",
    "decision", "note"
  ))
  expect_named(hand_kept("non_code_sheets.csv"), c("sheet", "reason", "decision"))
  expect_named(hand_kept("crosswalk_columns.csv"), c(
    "crosswalk", "attribute_name", "code_col", "filter_col", "filter_values", "decision"
  ))
  expect_named(hand_kept("type_map.csv"), c("data_type", "r_class", "date_format"))
  expect_named(
    hand_kept("sentinels.csv"),
    c("data_type", "role", "value", "allowed_in_pk", "allowed_in_fk")
  )
  expect_named(hand_kept("precedence.csv"), c(spec_input_schema()$precedence, "decision"))
  expect_true(all(hand_kept("non_code_sheets.csv")$reason %in% c("reference", "superseded")))
})

test_that("the reader reads the DD workbook in spec/ from its first sheet", {
  testthat::skip_if_not_installed("readxl")
  build <- load_data_raw("build_magp_config.R")
  spec_dir <- testthat::test_path("..", "..", "spec")
  if (!dir.exists(spec_dir)) {
    testthat::skip("spec/ is not in this tree")
  }
  dd <- read_input_table(build$spec_files(spec_dir)[["DD"]], "dictionary")
  expect_true(all(c("table_name", "attribute_name", "data_type") %in% names(dd$data)))
  expect_equal(dd$where$sheet, "DD")
})
