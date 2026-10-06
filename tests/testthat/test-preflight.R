# Tests for the pre-flight checks (plan 4.2, 9.5; D8.16, D12.15 to D12.25, D12.54, D12.64).

summarise_checks <- function(results) {
  first <- results[!duplicated(results$rule_id), ]
  summary <- paste(first$outcome, first$n_findings)
  names(summary) <- first$rule_id
  summary
}

test_that("the checks run in 4.2's order with the approved outcomes", {
  rules <- preflight_rules()
  expect_named(preflight_check_functions(), rules$rule_id)
  on_failure <- rules$on_failure
  names(on_failure) <- rules$rule_id
  expect_equal(on_failure, c(
    dd_duplicate_attribute = "stop", dd_type_unknown = "stop", dd_type_column_ambiguous = "stop",
    dd_pk_missing = "stop", dd_fk_target_missing = "stop", code_list_missing = "stop",
    code_column_missing = "stop", code_list_duplicate_code = "warn", code_list_blank_row = "warn",
    code_list_unreferenced = "stop", code_list_empty_column = "warn", spec_clash_resolved = "warn",
    spec_clash_unresolved = "stop", datasets_row_missing = "warn", spec_encoding_invalid = "stop",
    spec_csv_malformed = "stop", site_id_range_invalid = "stop", lineage_spec_unparseable = "stop",
    lineage_name_unknown = "warn", lineage_spec_row_unflagged = "warn",
    lineage_id_unflagged = "warn", crosswalk_unreadable = "stop"
  ))
})

test_that("each planted defect gives its check's findings", {
  results <- preflight_checks(fx_planted_spec())
  expect_equal(vapply(results, class, ""), preflight_columns())
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
  # The toy specs give no findings, but they lack inputs, so the checks of those inputs
  # don't run on them; the clean spec with every input, below, runs all 22 (D12.31).
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

test_that("a clean spec with every input runs all 22 checks, each without a finding (D12.31)", {
  dictionary <- data.frame(
    table_name = c("plots", "plots", "plots", "trees", "trees", "trees"),
    attribute_name = c("plot_id", "kind", "src_plot_id", "tree_id", "plot_id", "status"),
    key_type = c("PK", ".", ".", "PK", "FK", "."),
    reference_table = c(NA, NA, NA, NA, "plots", NA),
    lookup_table = c(NA, "Y", NA, NA, NA, "cond"),
    appendix = c(NA, NA, "A2", NA, NA, NA),
    data_type = "character"
  )
  spec <- gpq_read_spec(
    dictionary,
    code_lists = list(
      kind = data.frame(kind = c("A", "B"), description = c("a", "b")),
      ranges = data.frame(
        contributor = c("AA", "BB"), v2_start = c("1", "101"), v2_end = c("100", "200")
      ),
      contributor = data.frame(name = c("AA", "BB")),
      ds = data.frame(id = c("1", "2"), name = c("a", "b"))
    ),
    non_code_sheets = c("ranges", "contributor", "ds"),
    datasets = data.frame(id = c("1", "2"), name = c("a", "b")),
    lineage_spec = data.frame(
      contributor_label = "BC", table_name = "plots", attribute_name = "src_plot_id",
      spec_type = "id", source_text = "tbl.key", note = NA, source_cell = "A2!D5"
    ),
    crosswalks = list(cond = data.frame(status = c("L", "D"))),
    id_pattern = "^src_.*_id$",
    id_bands = list(
      sheet = "ranges", label_col = "contributor", start_col = "v2_start", end_col = "v2_end",
      labels_from = c(sheet = "contributor", column = "name"), reserved_pattern = NULL
    ),
    precedence = data.frame(
      sheet = "ds", key_col = "id", attribute_name = "name", winner = "datasets",
      blank_rule = "wins"
    ),
    column_map = gpq_column_map(lineage_flag = c(column = "appendix", value = "A2"))
  )
  results <- preflight_checks(spec)
  # One row per check, none not_run, stop or warn: every check met clean input.
  expect_equal(results$rule_id, preflight_rules()$rule_id)
  expect_false(any(results$outcome %in% c("not_run", "stop", "warn")))
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
    "Code \"B\" appears 2 times in column kind of sheet pairs: rows 2, 3 as R counts rows."
  )
})

test_that("blank rows and repeated codes are placed as each input form counts rows (D12.64)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind"), key_type = c("PK", "."),
    lookup_table = c(NA, "Y"), data_type = "character"
  )
  details <- function(code_lists, origins = NULL) {
    spec <- gpq_read_spec(dictionary, code_lists = code_lists, origins = origins)
    results <- preflight_checks(spec)
    results$detail[results$rule_id %in% c("code_list_duplicate_code", "code_list_blank_row")]
  }
  # A CSV by its lines: the header is line 1, and a cell over two lines counts both.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("kind,description", "A,\"first", "second\"", "B,b", ",", "B,again"), path)
  expect_equal(details(list(kind = path)), c(
    "Code \"B\" appears 2 times in column kind of sheet kind: lines 4, 6.",
    "Sheet kind has a blank row: line 5."
  ))
  # A data.frame by its rows as R counts them, df[N, ].
  frame <- data.frame(kind = c("A", "B", NA, "B"), description = c("a", "b", NA, "again"))
  expect_equal(details(list(kind = frame)), c(
    "Code \"B\" appears 2 times in column kind of sheet kind: rows 2, 4 as R counts rows.",
    "Sheet kind has a blank row: row 3 as R counts rows."
  ))
  # A data.frame from a workbook by the rows its origin gives, as Excel shows them.
  workbook <- testthat::test_path("fixtures", "blank_cells.xlsx")
  origin <- list(`code_lists:kind` = list(path = workbook, sheet = "Sheet1", rows = 10L))
  expect_equal(details(list(kind = frame), origin), c(
    "Code \"B\" appears 2 times in column kind of sheet kind: rows 11, 13.",
    "Sheet kind has a blank row: row 12."
  ))
  # A workbook by its rows as Excel shows them, the header row 1.
  raw <- data.table::data.table(
    V1 = c("kind", "A", "B", NA, "B"), V2 = c("description", "a", "b", NA, "again")
  )
  testthat::local_mocked_bindings(
    workbook_sheets = function(path) "kind", read_xlsx_raw = function(...) data.table::copy(raw)
  )
  expect_equal(details(testthat::test_path("fixtures", "blank_cells.xlsx")), c(
    "Code \"B\" appears 2 times in column kind of sheet kind: rows 3, 5.",
    "Sheet kind has a blank row: row 4."
  ))
})

test_that("a blank header is headerless in every input form; a header V2 isn't (D12.64, D12.65)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind"), key_type = c("PK", "."),
    lookup_table = c(NA, "Y"), data_type = "character"
  )
  # Each form holds sheet kind whose column 2 has a blank header and no values.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("kind,,description", "A,,a", "B,,b"), path)
  frame <- data.frame(kind = c("A", "B"), x = NA, description = c("a", "b"))
  names(frame)[[2L]] <- ""
  raw <- data.table::data.table(
    V1 = c("kind", "A", "B"), V2 = NA_character_, V3 = c("description", "a", "b")
  )
  testthat::local_mocked_bindings(
    workbook_sheets = function(path) "kind", read_xlsx_raw = function(...) data.table::copy(raw)
  )
  forms <- list(
    csv = list(kind = path), workbook = testthat::test_path("fixtures", "blank_cells.xlsx"),
    data.frame = list(kind = frame)
  )
  for (form in names(forms)) {
    spec <- gpq_read_spec(dictionary, code_lists = forms[[form]])
    header <- spec$code_lists[spec$code_lists$source_row == 1L, ]
    expect_equal(header$sheet_column, c("kind", "V2", "description"), info = form)
    expect_equal(header$value, c("kind", NA, "description"), info = form)
    results <- preflight_checks(spec)
    expect_equal(results$outcome[results$rule_id == "code_list_empty_column"], "pass", info = form)
  }
  # A header written V2 is a header, whatever fread() names a blank one.
  writeLines(c("kind,V2,description", "A,,a", "B,,b"), path)
  results <- preflight_checks(gpq_read_spec(dictionary, code_lists = list(kind = path)))
  expect_equal(
    results$detail[results$rule_id == "code_list_empty_column"],
    "Column V2 of sheet kind has a header but no values."
  )
})

test_that("code_list_empty_column skips a column without its header cell, never stops", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind"), key_type = c("PK", "."),
    lookup_table = c(NA, "Y"), data_type = "character"
  )
  spec <- gpq_read_spec(dictionary, code_lists = list(kind = data.frame(kind = "A", note = NA)))
  # The reader always writes a header cell; a spec without one is made by hand here.
  cells <- spec$code_lists
  spec$code_lists <- cells[!(cells$sheet_column == "note" & cells$source_row == 1L), ]
  empty_column <- preflight_check_functions()$code_list_empty_column
  expect_equal(empty_column(spec), no_findings())
})

test_that("code_list_empty_column checks every referenced sheet but non-code ones (D12.65)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "kind", "remark"), key_type = c("PK", ".", "."),
    lookup_table = c(NA, "Y", "memo"), data_type = "character"
  )
  # Sheet kind has no kind column, so its code column isn't found (code_column_missing);
  # memo, though referenced, is declared as not a code list, so its empty spare is skipped.
  spec <- gpq_read_spec(
    dictionary,
    code_lists = list(
      kind = data.frame(code = c("A", "B"), note = NA), memo = data.frame(text = "x", spare = NA)
    ),
    non_code_sheets = "memo"
  )
  results <- preflight_checks(spec)
  expect_equal(
    results$detail[results$rule_id == "code_list_empty_column"],
    "Column note of sheet kind has a header but no values."
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
  # No cell-level check, code_list_blank_row in particular, fires on these sheets, except
  # code_list_empty_column on the referenced header-only sheet (D12.65).
  expect_equal(
    sort(unique(fired$rule_id)),
    c("code_column_missing", "code_list_empty_column", "code_list_unreferenced")
  )
  expect_equal(
    fired$detail[fired$rule_id == "code_list_empty_column"],
    "Column code of sheet palette has a header but no values."
  )
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

test_that("lineage_spec_unparseable runs when no flagged attribute has a lineage row (R110)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("t_id", "src_t_id"), key_type = c("PK", "."),
    data_type = "character", appendix = c(NA, "A2")
  )
  lineage_row <- data.frame(
    contributor_label = "BC", table_name = "t", attribute_name = "src_t_id", spec_type = "id",
    source_text = "tbl.key", note = NA, source_cell = "A2!D5"
  )
  unparseable <- function(dictionary, lineage_spec) {
    spec <- gpq_read_spec(
      dictionary,
      lineage_spec = lineage_spec,
      column_map = gpq_column_map(lineage_flag = c(column = "appendix", value = "A2"))
    )
    results <- preflight_checks(spec)
    results[results$rule_id == "lineage_spec_unparseable", ]
  }
  # A lineage spec with no rows beside a flagged attribute: that attribute's finding.
  empty <- unparseable(dictionary, lineage_row[0L, ])
  expect_equal(
    empty$detail, "t.src_t_id is flagged for the lineage spec but has no row in it."
  )
  # A flag column that flags nothing beside one lineage-spec row: nothing to cover.
  dictionary$appendix <- NA
  expect_equal(unparseable(dictionary, lineage_row)$outcome, "pass")
})

test_that("gpq_preflight stops with a classed error carrying the table", {
  condition <- tryCatch(gpq_preflight(fx_planted_spec()), gpq_preflight_error = function(e) e)
  expect_s3_class(condition, "gpq_preflight_error")
  expect_equal(nrow(condition$preflight), nrow(preflight_checks(fx_planted_spec())))
  expect_match(conditionMessage(condition), "dd_duplicate_attribute:", fixed = TRUE)
})

test_that("a type map's malformed line or invalid byte stops pre-flight (D12.57)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"
  )
  ragged <- withr::local_tempfile(fileext = ".csv")
  lines <- c("data_type,r_class,date_format", "character,character,", "x,character,,extra")
  writeLines(lines, ragged)
  bad_byte <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(
    charToRaw("data_type,r_class,date_format\ncharacter,character,\nte"), as.raw(0x97),
    charToRaw("xt,character,\n")
  ), bad_byte)
  for (case in list(c(ragged, "spec_csv_malformed"), c(bad_byte, "spec_encoding_invalid"))) {
    spec <- gpq_read_spec(dictionary, type_map = gpq_type_map(case[[1L]]))
    condition <- tryCatch(gpq_preflight(spec), gpq_preflight_error = function(e) e)
    expect_s3_class(condition, "gpq_preflight_error")
    rows <- condition$preflight
    expect_true(any(rows$rule_id == case[[2L]] & rows$outcome == "stop"))
  }
})

test_that("gpq_preflight warns once on warnings only, and is quiet on a clean spec", {
  spec <- fx_fish_gear_twice_spec()
  gear <- spec$code_lists[spec$code_lists$sheet == "gear", ]
  expect_equal(gear$value, c("gear", "GN", "GN"))
  # One warning in all, with and without files written, of the right class, with the count
  # and the table (D12.16).
  for (output_dir in list(NULL, withr::local_tempdir())) {
    warned <- list()
    returned <- withCallingHandlers(
      gpq_preflight(spec, output_dir = output_dir),
      warning = function(w) {
        warned[[length(warned) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    )
    expect_length(warned, 1L)
    expect_s3_class(warned[[1L]], "gpq_preflight_warning")
    expect_equal(sum(returned$outcome == "warn"), 1L)
    expect_match(conditionMessage(warned[[1L]]), "1 warning(s)", fixed = TRUE)
    expect_identical(warned[[1L]]$preflight, returned)
  }
  expect_no_condition(results <- gpq_preflight(fx_fish_spec()))
  expect_true(all(results$outcome %in% c("pass", "not_run")))
})

test_that("pre-flight leaves the caller's spec untouched (plan 16.2, D12.27)", {
  dir <- withr::local_tempdir()
  # A spec that stops and one that only warns, each with its files written.
  for (spec in list(fx_planted_spec(), fx_fish_gear_twice_spec())) {
    before <- data.table::copy(spec)
    tryCatch(
      suppressWarnings(gpq_preflight(spec, output_dir = dir)),
      gpq_preflight_error = function(e) NULL
    )
    # Base identical(): any change to the caller's object, indices included, fails.
    expect_true(identical(spec, before))
  }
})

test_that("gpq_preflight refuses an edited spec and an output_dir that isn't a folder", {
  spec <- fx_fish_spec()
  dropped <- structure(unclass(spec)[-1L], class = "gpq_spec")
  auto_index <- getOption("datatable.auto.index")
  expect_error(gpq_preflight(dropped), "components")
  for (output_dir in list(NA_character_, "", c("a", "b"), 1)) {
    expect_error(gpq_preflight(spec, output_dir = output_dir), "output_dir")
  }
  file <- withr::local_tempfile()
  writeLines("x", file)
  expect_error(gpq_preflight(spec, output_dir = file), "output_dir")
  # A refusal leaves data.table's option as the caller had it.
  expect_identical(getOption("datatable.auto.index"), auto_index)
})
