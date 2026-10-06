# Tests for the pre-flight checks (plan 4.2, 9.5; D8.16, D12.15 to D12.25, D12.54).

summarise_checks <- function(results) {
  first <- results[!duplicated(results$rule_id), ]
  summary <- paste(first$outcome, first$n_findings)
  names(summary) <- first$rule_id
  summary
}

test_that("the checks run in 4.2's order with the approved outcomes", {
  rules <- preflight_rules()
  expect_equal(nrow(rules), 22L)
  expect_named(preflight_check_functions(), rules$rule_id)
  expect_equal(rules$on_failure[rules$rule_id == "code_list_empty_column"], "warn")
  expect_equal(rules$on_failure[rules$rule_id == "spec_encoding_invalid"], "stop")
  expect_equal(rules$on_failure[rules$rule_id == "spec_csv_malformed"], "stop")
  expect_equal(rules$on_failure[rules$rule_id == "datasets_row_missing"], "warn")
})

test_that("each planted defect gives its check's findings", {
  results <- preflight_checks(fx_planted_spec())
  expect_named(results, names(preflight_columns()))
  expect_equal(summarise_checks(results), c(
    dd_duplicate_attribute = "stop 1", dd_type_unknown = "stop 1",
    dd_type_column_ambiguous = "pass 0", dd_pk_missing = "stop 1",
    dd_fk_target_missing = "stop 1", code_list_missing = "stop 1",
    code_column_missing = "stop 1", code_list_duplicate_code = "warn 1",
    code_list_blank_row = "warn 1", code_list_unreferenced = "stop 1",
    code_list_empty_column = "warn 1", spec_clash_resolved = "warn 1",
    spec_clash_unresolved = "stop 1", datasets_row_missing = "warn 1",
    spec_encoding_invalid = "stop 1", spec_csv_malformed = "stop 1",
    site_id_range_invalid = "stop 2", lineage_spec_unparseable = "stop 2",
    lineage_name_unknown = "warn 1", lineage_spec_row_unflagged = "warn 1",
    lineage_id_unflagged = "warn 1", crosswalk_unreadable = "stop 1"
  ))
  site <- results[results$rule_id == "site_id_range_invalid", ]
  expect_equal(site$n_findings, c(2L, 2L))
  expect_true(any(grepl("ZZ", site$detail)))
})

test_that("the toy specs pass, and absent inputs are not_run with their reason", {
  fish <- preflight_checks(fx_fish_spec())
  # Zero findings on a clean spec: every check is tested for false positives (D12.31).
  expect_false(any(fish$outcome %in% c("stop", "warn")))
  expect_equal(sum(fish$n_findings, na.rm = TRUE), 0L)
  reasons <- fish$not_run_reason
  names(reasons) <- fish$rule_id
  expect_equal(reasons[["site_id_range_invalid"]], "no_input")
  expect_equal(reasons[["lineage_id_unflagged"]], "no_input")
  expect_equal(reasons[["crosswalk_unreadable"]], "no_input")
  expect_true(is.na(fish$n_findings[fish$outcome == "not_run"][[1L]]))
  forest <- preflight_checks(fx_forest_spec())
  expect_false(any(forest$outcome %in% c("stop", "warn")))
  expect_equal(forest$not_run_reason[forest$rule_id == "lineage_id_unflagged"], "no_dd_column")
})

test_that("dictionary findings point to <file>:<row>", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "table_name,attribute_name,key_type,data_type", "t,a,PK,character", "t,a,PK,character"
  ), path)
  results <- preflight_checks(gpq_read_spec(path))
  row <- results[results$rule_id == "dd_duplicate_attribute", ]
  expect_equal(row$source_cell, paste0(basename(path), ":3"))
  expect_equal(row$file, basename(path))
})

test_that("a type column that is all blank gives a finding per blank type (R9)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "a"), key_type = c("PK", "."), data_type = NA
  )
  results <- preflight_checks(gpq_read_spec(dictionary))
  expect_equal(results$detail[results$rule_id == "dd_type_unknown"], c(
    "t.id has type (blank), which the type map doesn't list.",
    "t.a has type (blank), which the type map doesn't list."
  ))
  no_column <- preflight_checks(gpq_read_spec(dictionary[, 1:3]))
  expect_equal(no_column$outcome[no_column$rule_id == "dd_type_unknown"], "pass")
})

test_that("a blank name shows as (blank), never NA (D12.54)", {
  dictionary <- data.frame(
    table_name = c("t", NA), attribute_name = c("id", NA), key_type = c("PK", NA),
    data_type = c("character", NA)
  )
  results <- preflight_checks(gpq_read_spec(dictionary))
  expect_equal(
    results$detail[results$rule_id == "dd_pk_missing"], "Table (blank) has no primary key."
  )
})

test_that("code_list_blank_row passes with no rows to check (D12.26)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind"), key_type = c("PK", "."),
    lookup_table = c(NA, "Y"), data_type = "character"
  )
  blank_row <- preflight_check_functions()$code_list_blank_row
  header_only <- gpq_read_spec(dictionary, code_lists = list(kind = data.frame(kind = character())))
  expect_equal(blank_row(header_only), no_findings())
  unused <- gpq_read_spec(
    dictionary[1L, ],
    code_lists = list(kind = data.frame(kind = "A"))
  )
  expect_equal(blank_row(unused), no_findings())
})

test_that("a header-only sheet in use has every column empty and no codes (D12.40)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind"), key_type = c("PK", "."),
    lookup_table = c(NA, "Y"), data_type = "character"
  )
  spec <- gpq_read_spec(dictionary, code_lists = list(
    kind = data.frame(kind = character(), description = character())
  ))
  expect_equal(nrow(spec$codes), 0L)
  results <- preflight_checks(spec)
  expect_equal(results$detail[results$rule_id == "code_list_empty_column"], c(
    "Column kind of sheet kind has a header but no values.",
    "Column description of sheet kind has a header but no values."
  ))
})

test_that("a code repeats within one code column, not across a sheet's columns (D12.34)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind", "colour"), key_type = c("PK", ".", "."),
    lookup_table = c(NA, "pairs", "pairs"), data_type = "character"
  )
  spec <- gpq_read_spec(dictionary, code_lists = list(
    pairs = data.frame(kind = c("A", "B", "B"), colour = c("B", "C", "D"))
  ))
  results <- preflight_checks(spec)
  expect_equal(
    results$detail[results$rule_id == "code_list_duplicate_code"],
    "Code \"B\" appears 2 times in column kind of sheet pairs, rows 3, 4."
  )
})

test_that("empty and header-only sheets are seen, and only the right checks fire (D12.29)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind", "colour"), key_type = c("PK", ".", "."),
    lookup_table = c(NA, "Y", "palette"), data_type = "character"
  )
  spec <- gpq_read_spec(dictionary, code_lists = list(
    kind = data.frame(), palette = data.frame(code = character()),
    unused_empty = data.frame(), unused_header = data.frame(code = character())
  ))
  expect_equal(
    spec$code_list_sheets$sheet, c("kind", "palette", "unused_empty", "unused_header")
  )
  results <- preflight_checks(spec)
  fired <- results[results$outcome %in% c("stop", "warn"), ]
  # No cell-level check, code_list_blank_row in particular, fires on these sheets.
  expect_equal(sort(unique(fired$rule_id)), c("code_column_missing", "code_list_unreferenced"))
  expect_equal(fired$detail[fired$rule_id == "code_list_unreferenced"], c(
    "Sheet unused_empty isn't used by any attribute and isn't declared as not a code list.",
    "Sheet unused_header isn't used by any attribute and isn't declared as not a code list."
  ))
  expect_equal(fired$detail[fired$rule_id == "code_column_missing"], c(
    "Code list kind is an empty sheet, so it has no column for t.kind.",
    "Code list palette has no column that holds the codes for t.colour."
  ))
})

test_that("site-ID findings name the bands sheet's file and cite both overlapping bands (D12.28)", {
  dir <- withr::local_tempdir()
  ranges <- file.path(dir, "ranges.csv")
  writeLines(c("contributor,v2_start,v2_end", "AA,1,100", "BB,50,150", "CC,300,250"), ranges)
  contributors <- file.path(dir, "contributor.csv")
  writeLines(c("name", "AA", "BB", "CC"), contributors)
  bands <- list(
    sheet = "ranges", label_col = "contributor", start_col = "v2_start", end_col = "v2_end",
    labels_from = c(sheet = "contributor", column = "name"), reserved_pattern = NULL
  )
  dictionary <- data.frame(
    table_name = "t", attribute_name = "id", key_type = "PK", data_type = "character"
  )
  site_rows <- function(code_lists) {
    spec <- gpq_read_spec(
      dictionary,
      code_lists = code_lists, non_code_sheets = c("ranges", "contributor"), id_bands = bands
    )
    results <- preflight_checks(spec)
    results[results$rule_id == "site_id_range_invalid", ]
  }
  # The contributor sheet comes first, so the first code-list file is the wrong one.
  from_csv <- site_rows(list(contributor = contributors, ranges = ranges))
  expect_equal(from_csv$file, c("ranges.csv", "ranges.csv"))
  expect_equal(from_csv$detail, c(
    "Band \"CC\" starts at 300, after it ends at 250.",
    "Bands \"AA\" (line 2) and \"BB\" (line 3) overlap."
  ))
  testthat::skip_if_not_installed("readxl")
  from_workbook <- site_rows(testthat::test_path("fixtures", "id_bands.xlsx"))
  expect_equal(from_workbook$file, c("id_bands.xlsx", "id_bands.xlsx"))
  expect_equal(from_workbook$detail, c(
    "Band \"CC\" starts at 300, after it ends at 250.",
    "Bands \"AA\" (ranges!A2) and \"BB\" (ranges!A3) overlap."
  ))
})

test_that("an FK whose target has no PK is reported by both key checks (D12.26)", {
  dictionary <- data.frame(
    table_name = c("p", "c"), attribute_name = "p1", key_type = c(".", "FK"),
    reference_table = c(NA, "p"), data_type = "character"
  )
  results <- preflight_checks(gpq_read_spec(dictionary))
  expect_equal(
    results$detail[results$rule_id == "dd_pk_missing"],
    c("Table p has no primary key.", "Table c has no primary key.")
  )
  expect_equal(
    results$detail[results$rule_id == "dd_fk_target_missing"],
    "c.p1 refers to \"p\", which has no primary key."
  )
})

test_that("a clash's finding gives both sides' cells, a then b (D12.33)", {
  dir <- withr::local_tempdir()
  write_csv <- function(name, lines) {
    path <- file.path(dir, name)
    writeLines(lines, path)
    path
  }
  dictionary <- write_csv("dictionary.csv", c(
    "table_name,attribute_name,key_type,data_type", "plots,plot_id,PK,character",
    "visits,visit_id,PK,character", "visits,src_visit_id,.,character"
  ))
  precedence <- data.frame(
    sheet = "ds", key_col = "id", attribute_name = "name", winner = "datasets",
    blank_rule = "wins"
  )
  read_with <- function(datasets) {
    gpq_read_spec(
      dictionary,
      code_lists = list(ds = write_csv("ds.csv", c("id,name", "1,b"))),
      non_code_sheets = "ds", datasets = datasets, precedence = precedence,
      lineage_spec = data.frame(
        contributor_label = c("BC", "ON"), table_name = "plots",
        attribute_name = "src_visit_id", spec_type = "id", source_text = "tbl.key",
        note = NA, source_cell = c("A2!D11", "A2!F11")
      )
    )
  }
  cells <- function(spec) {
    results <- preflight_checks(spec)
    results$source_cell[results$rule_id == "spec_clash_resolved"]
  }
  from_files <- read_with(write_csv("datasets.csv", c("id,name", "1,a")))
  # Row order isn't part of the contract: compared sorted (D12.30).
  expect_equal(sort(cells(from_files)), sort(c(
    "datasets.csv:2; ds.csv:2", "A2!D11, A2!F11; dictionary.csv:4"
  )))
  in_memory <- read_with(data.frame(id = "1", name = "a"))
  expect_true("; ds.csv:2" %in% cells(in_memory))
  expect_true(all(is.na(cells(fx_planted_spec()))))
})

test_that("lineage_id_unflagged runs without a lineage spec; the other three don't (D12.33)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("t_id", "src_t_id"), key_type = c("PK", "."),
    data_type = "character", appendix = NA
  )
  spec <- gpq_read_spec(
    dictionary,
    id_pattern = "^src_.*_id$",
    column_map = gpq_column_map(lineage_flag = c(column = "appendix", value = "A2"))
  )
  results <- preflight_checks(spec)
  expect_equal(
    results$detail[results$rule_id == "lineage_id_unflagged"],
    "t.src_t_id matches the ID pattern but the dictionary doesn't flag it for the lineage spec."
  )
  reasons <- results$not_run_reason
  names(reasons) <- results$rule_id
  reading_lineage <- c(
    "lineage_spec_unparseable", "lineage_name_unknown", "lineage_spec_row_unflagged"
  )
  for (rule in reading_lineage) {
    expect_equal(reasons[[rule]], "no_input", info = rule)
  }
})
