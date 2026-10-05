# Tests for precedence and clashes (plan 2.2, 3.6; D12.13, D12.14, D12.20, D12.24).

clash_inputs <- function() {
  datasets <- data.table::data.table(
    magp_dataset_id = c("110.05", "110.06", "110.08", "100.01"),
    src_dataset_id = c("BC_SUP", "BC_VRI", "BC_YNS", "AB_PGYI"),
    plot_layout_fig = c("2", "2", NA, "10"),
    gp_type = "PSP"
  )
  sheet <- data.frame(
    magp_dataset_id = c("110.05", "110.06", "110.08", "100.01", "120.01"),
    src_dataset_id = c("BC_VRI", "BC_SUP", "BC_YNS", NA, "MB_PSP"),
    plot_layout_fig = c("11", "11", "11", "10", "12"),
    gp_type = c("PSP", "PSP", "PSP", "GP", "PSP")
  )
  list(datasets = datasets, code_lists = read_code_lists(list(dataset = sheet))$long)
}

precedence_rows <- function(blank_rule = "wins") {
  data.table::data.table(
    sheet = "dataset", key_col = "magp_dataset_id",
    attribute_name = c("src_dataset_id", "plot_layout_fig"),
    winner = c("code_lists", "datasets"), blank_rule = c("wins", blank_rule)
  )
}

files <- list(datasets = "datasets.csv", code_lists = "lookup.xlsx")

test_that("clashes follow precedence; a blank on the losing side never clashes", {
  inputs <- clash_inputs()
  out <- resolve_clashes(inputs$datasets, inputs$code_lists, precedence_rows(), files)
  resolved <- out$clashes[out$clashes$rule_id == "spec_clash_resolved", ]
  # The sheet's blank src_dataset_id for 100.01 is a winning-side blank, so it clashes and
  # the blank wins. Row order isn't part of the contract: compared sorted (D12.30).
  expect_equal(
    sort(paste(resolved$column_name, resolved$key_value, resolved$winner)),
    sort(c(
      "src_dataset_id 100.01 code_lists", "src_dataset_id 110.05 code_lists",
      "src_dataset_id 110.06 code_lists", "plot_layout_fig 110.05 datasets",
      "plot_layout_fig 110.06 datasets", "plot_layout_fig 110.08 datasets"
    ))
  )
  expect_equal(unique(resolved$basis), "precedence")
  unresolved <- out$clashes[out$clashes$rule_id == "spec_clash_unresolved", ]
  expect_equal(paste(unresolved$column_name, unresolved$key_value), "gp_type 100.01")
})

test_that("a blank that yields hands the win to the other side", {
  inputs <- clash_inputs()
  out <- resolve_clashes(inputs$datasets, inputs$code_lists, precedence_rows("yields"), files)
  row <- out$clashes[out$clashes$key_value == "110.08", ]
  expect_equal(row$winner, "code_lists")
})

test_that("sheet keys with no datasets row are datasets_row_missing", {
  inputs <- clash_inputs()
  out <- resolve_clashes(inputs$datasets, inputs$code_lists, precedence_rows(), files)
  expect_equal(out$findings$rule_id, "datasets_row_missing")
  expect_match(out$findings$detail, "120.01", fixed = TRUE)
})

test_that("a datasets table with no rows still has every sheet key missing (R14)", {
  inputs <- clash_inputs()
  empty <- inputs$datasets[0L]
  out <- resolve_clashes(empty, inputs$code_lists, precedence_rows(), files)
  expect_equal(nrow(out$findings), 5L)
  expect_equal(unique(out$findings$rule_id), "datasets_row_missing")
  none <- resolve_clashes(NULL, inputs$code_lists, precedence_rows(), files)
  expect_equal(nrow(none$findings), 0L)
})

test_that("a precedence sheet or key that doesn't exist is unresolved", {
  inputs <- clash_inputs()
  rows <- precedence_rows()
  rows$key_col <- "dataset_key"
  out <- resolve_clashes(inputs$datasets, inputs$code_lists, rows, files)
  expect_equal(out$findings$rule_id, "spec_clash_unresolved")
})

test_that("a precedence sheet with a header and no rows is skipped (D12.26)", {
  inputs <- clash_inputs()
  header_only <- read_code_lists(list(dataset = data.frame(
    magp_dataset_id = character(), src_dataset_id = character()
  )))$long
  out <- resolve_clashes(inputs$datasets, header_only, precedence_rows(), files)
  expect_equal(out$clashes, empty_table(spec_schema()$clashes))
  expect_equal(out$findings, empty_table(spec_schema()$read_findings))
})

test_that("blank keys match nothing and repeated keys clash pairwise (R13)", {
  datasets <- data.table::data.table(
    magp_dataset_id = c(NA, NA, NA, "1", "1", "1"), src_dataset_id = c("p", "q", "r", "a", "b", "c")
  )
  sheet <- data.frame(
    magp_dataset_id = c(NA, NA, NA, "1", "1", "1"), src_dataset_id = c("s", "t", "u", "d", "e", "f")
  )
  code_lists <- read_code_lists(list(dataset = sheet))$long
  out <- resolve_clashes(datasets, code_lists, precedence_rows()[1L], files)
  expect_equal(nrow(out$clashes), 9L)
  expect_equal(unique(out$clashes$key_value), "1")
})

test_that("a column named like the reader's own row numbers is just a column (R15)", {
  datasets <- data.table::data.table(
    magp_dataset_id = "1", src_dataset_id = "a", source_row = "9", datasets_row = "8"
  )
  sheet <- data.frame(
    magp_dataset_id = "1", src_dataset_id = "b", source_row = "7", datasets_row = "6"
  )
  code_lists <- read_code_lists(list(dataset = sheet))$long
  out <- resolve_clashes(datasets, code_lists, precedence_rows()[1L], files)
  expect_equal(
    sort(paste(out$clashes$column_name, out$clashes$value_a, out$clashes$value_b)),
    c("datasets_row 8 6", "source_row 9 7", "src_dataset_id a b")
  )
})

test_that("a clash names the datasets table's cell and the sheet's (D12.33)", {
  sheet <- file.path(withr::local_tempdir(), "dataset.csv")
  writeLines(c("magp_dataset_id,src_dataset_id", "110.05,BC_VRI", "110.06,BC_SUP"), sheet)
  code_lists <- read_code_lists(list(dataset = sheet))$long
  datasets <- data.table::data.table(
    magp_dataset_id = c("110.06", "110.05"), src_dataset_id = c("BC_VRI", "BC_SUP")
  )
  out <- resolve_clashes(
    datasets, code_lists, precedence_rows()[1L], files,
    datasets_where = list(kind = "csv", file = "datasets.csv")
  )
  cells <- out$clashes[order(out$clashes$key_value), ]
  expect_equal(cells$source_cell_a, c("datasets.csv:3", "datasets.csv:2"))
  expect_equal(cells$source_cell_b, c("dataset.csv:2", "dataset.csv:3"))
  unknown <- resolve_clashes(datasets, code_lists, precedence_rows()[1L], files)
  expect_true(all(is.na(unknown$clashes$source_cell_a)))
})
