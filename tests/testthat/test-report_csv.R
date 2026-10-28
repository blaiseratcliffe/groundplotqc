# The provider issues CSV (plan 11.6; D8.18, D20.31 to D20.33, D20.35, D20.36): the key's parts
# as columns, the formula guard, the columns and their order, the message, the rows' order
# and a round trip through the file, on fx_report_results() (D20.58) and small tables made
# here. Every test runs on CRAN (D20.3).

test_that("split_provider_key splits decoded and partial keys, every other key empty (D20.31)", {
  keys <- c(
    "src_station.code=S1; src_haul.no=2", "jurisdiction=AB; src_catch.no=", "S1; 2", NA, "C-77",
    "4021_X_17", NA
  )
  # A key of two tables' parts; a derived part labelled with its bare attribute name and a
  # blank part (partial); a part value that held "; ", so the decoder gave the ID as delivered
  # (raw, D20.35); fallback_pk; no_lineage_spec; another raw ID; a column-level issue's none.
  status <- c("decoded", "partial", "raw", "fallback_pk", "no_lineage_spec", "raw", NA)
  parts <- split_provider_key(keys, status)
  # Labels by their first part position, then in C order.
  expect_named(parts, c(
    "key_jurisdiction", "key_src_station.code", "key_src_catch.no", "key_src_haul.no"
  ))
  empty <- rep(NA_character_, 5L)
  expect_identical(parts$key_src_station.code, c("S1", NA, empty))
  expect_identical(parts$key_src_haul.no, c("2", NA, empty))
  expect_identical(parts$key_jurisdiction, c(NA, "AB", empty))
  # A blank part is a blank cell, not a missing one.
  expect_identical(parts$key_src_catch.no, c(NA, "", empty))
  # A key marked decoded that still doesn't split exactly is left empty, never guessed
  # (D20.31 (a)): a part without "=", a value holding "=", a label given twice, a part with no
  # label.
  odd <- c(
    "src_station.code=S1; 2", "src_station.code=S=1", "a.b=1; a.b=2", "a.b=3", "=x; a.b=4"
  )
  parts <- split_provider_key(odd, rep("decoded", 5L))
  expect_named(parts, "key_a.b")
  expect_identical(parts$key_a.b, c(NA, NA, NA, "3", NA))
  # A latin1-marked key splits as its UTF-8 text does.
  latin <- iconv("a.b=été; c=-1", "UTF-8", "latin1")
  expect_identical(Encoding(latin), "latin1")
  parts <- split_provider_key(c(latin, NA), c("decoded", NA))
  expect_named(parts, c("key_a.b", "key_c"))
  expect_identical(parts$key_a.b, c("été", NA))
  expect_identical(parts$key_c, c("-1", NA))
  # No key that splits: no columns.
  expect_identical(ncol(split_provider_key(c("C-77", NA), c("raw", "fallback_pk"))), 0L)
})

test_that("guard_csv_text guards a cell a spreadsheet would read as a formula, no other (D20.32)", {
  # The last cell is marked latin1, matched as its UTF-8 text is.
  latin <- iconv("-café", "UTF-8", "latin1")
  expect_identical(Encoding(latin), "latin1")
  cells <- c(
    "=1+1", "@SUM(A1)", "-5", "-x", "+1", "\tx", "\rx", "x=1", NA, "", "-", "-1.5e3", "-.5",
    "-Inf", "-5 ", " =1", latin
  )
  dt <- data.table::data.table(text = cells, n = seq_along(cells), flag = TRUE)
  before <- data.table::copy(dt)
  guarded <- guard_csv_text(dt)
  expect_identical(guarded$text, c(
    "'=1+1", "'@SUM(A1)", "-5", "'-x", "'+1", "'\tx", "'\rx", "x=1", NA, "", "'-", "-1.5e3",
    "-.5", "'-Inf", "'-5 ", " =1", "'-café"
  ))
  expect_identical(guarded$n, before$n)
  expect_identical(guarded$flag, before$flag)
  # The table given is guarded in place, with no second copy (D20.92): the result is it.
  expect_identical(data.table::address(guarded), data.table::address(dt))
  expect_identical(dt$text, guarded$text)
  # A table with no cell to guard is left as it was.
  plain <- data.table::data.table(n = 1:2, text = c("a", "-1"))
  expect_identical(guard_csv_text(data.table::copy(plain)), plain)
})

test_that("the CSV has 11.6's columns, key parts by table then part, no mark (D20.31, D20.33)", {
  local_results_registry()
  x <- fx_report_results()
  issues <- route_unit(x, "100", c("100", "110"))$issues
  csv <- provider_csv_table(issues, c("unit", "area"), "plot")
  # D20.74: the key column after the group columns; D20.75: the parts, then the whole key and
  # its status; D20.71: catches' labels (by part, then C order), then hauls' new one.
  expect_named(csv, c(
    "unit", "area", "plot", "table_name", "column_name", "rule_id", "severity",
    "issue_class", "message", "key_jurisdiction", "key_src_station.code", "key_src_catch.no",
    "key_src_haul.no", "provider_key", "provider_key_status", "record_pk", "value",
    "expected", "proposed_value", "easy_fix", "status", "change", "fingerprint", "n_records"
  ))
  expect_false("reaches_other_unit" %in% names(csv))
  expect_identical(class(csv$n_records), "integer")
  expect_identical(class(csv$easy_fix), "logical")
  # A unit with no issues: the header alone, no key columns; no group or key column without
  # them (D20.29).
  empty <- provider_csv_table(issues[0L], c("unit", "area"), "plot")
  expect_identical(nrow(empty), 0L)
  expect_false(any(startsWith(names(empty), "key_")))
  expect_identical(names(provider_csv_table(issues[0L], character(), NULL))[[1L]], "table_name")
  # A table's labels come before a later table's, whatever their part positions.
  y <- data.table::copy(issues[issues$issue_id %in% c(7L, 11L)])
  data.table::set(y, j = "table_name", value = c("b_table", "a_table"))
  data.table::set(y, j = "provider_key", value = c("c.d=3", "m=1; n.o=2"))
  data.table::set(y, j = "provider_key_status", value = "decoded")
  expect_identical(
    grep("^key_", names(provider_csv_table(y, "unit", NULL)), value = TRUE),
    c("key_m", "key_n.o", "key_c.d")
  )
})

test_that("the CSV holds every routed issue, sorted as the report sorts (D20.32, D20.42)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  issues <- route_unit(x, "100", units)$issues
  csv <- provider_csv_table(issues, c("unit", "area"), "plot")
  expect_identical(nrow(csv), nrow(issues))
  expect_true("not_evaluated" %in% csv$status)
  # Errors, then warnings, each by rule in C order; value_above_max's two by provider key, the
  # raw "-ST-100-3" before "src_station.code=...".
  expect_identical(csv$rule_id, c(
    "blank_string", "class_mismatch", "code_not_in_list", "fk_orphan", "raw_na",
    "table_missing", "value_above_max", "value_above_max", "decimals_exceeded",
    "undeclared_code_set"
  ))
  expect_identical(csv$record_pk[csv$rule_id == "value_above_max"], c("C3", "C1"))
  # A flag comes after errors and warnings.
  csv <- provider_csv_table(route_unit(x, "110", units)$issues, c("unit", "area"), "plot")
  expect_identical(csv$severity, c("error", "error", "error", "warning", "flag"))
  # Ties after the provider key go by record ID, then issue ID, as the "Each issue" table
  # sorts them (R65): with both keys alike, C1 comes before C3, though its issue comes later.
  tied <- data.table::copy(issues)
  data.table::set(tied, i = which(tied$issue_id == 10L), j = "provider_key", value = "-ST-100-3")
  csv <- provider_csv_table(tied, c("unit", "area"), "plot")
  expect_identical(csv$record_pk[csv$rule_id == "value_above_max"], c("C1", "C3"))
  # Keys of one tie group marked differently sort in C order as UTF-8 text (D20.42): issue
  # 10's "é" marked latin1 before issue 9's "ü" marked UTF-8, though the latin1 byte is the
  # higher.
  mixed <- data.table::copy(issues)
  pair <- match(c(9L, 10L), mixed$issue_id)
  data.table::set(mixed, i = pair, j = "provider_key", value = c(
    "ü", iconv("é", "UTF-8", "latin1")
  ))
  data.table::set(mixed, i = pair, j = "provider_key_status", value = "raw")
  expect_identical(Encoding(mixed$provider_key[pair]), c("UTF-8", "latin1"))
  csv <- provider_csv_table(mixed, c("unit", "area"), "plot")
  expect_identical(csv$record_pk[csv$rule_id == "value_above_max"], c("C1", "C3"))
  # An issue with no provider key comes after those with one.
  at <- which(issues$issue_id == 9L)
  data.table::set(issues, i = at, j = "provider_key", value = NA_character_)
  csv <- provider_csv_table(issues, c("unit", "area"), "plot")
  expect_identical(csv$record_pk[csv$rule_id == "value_above_max"], c("C1", "C3"))
})

test_that("a unit rule's issue has no n_records in any unit's CSV (D16.8, D20.122)", {
  local_results_registry()
  x <- fx_report_results()
  # whitespace_padding's column-level issue of 110's, unmarked here, so it reaches 110.
  data.table::set(x$issues, i = 17L, j = "reaches_other_unit", value = FALSE)
  units <- c("100", "110")
  # The registry's unit rules among the fixture's: their "*" issues reach both units, and
  # undeclared_code_set's issue of each unit its own (R98: in every run).
  unit_rules <- c("table_missing", "class_mismatch", "undeclared_code_set")
  for (unit in units) {
    csv <- provider_csv_table(route_unit(x, unit, units)$issues, c("unit", "area"), "plot")
    expect_true("class_mismatch" %in% csv$rule_id, info = unit)
    expect_true(all(is.na(csv$n_records[csv$rule_id %in% unit_rules])), info = unit)
  }
  # The record rule's column-level issue keeps its count.
  expect_identical(csv$n_records[csv$rule_id == "whitespace_padding"], 4L)
  # A one-unit run's CSV too.
  csv <- provider_csv_table(route_unit(x, "100", "100")$issues, c("unit", "area"), "plot")
  expect_true(all(is.na(csv$n_records[csv$rule_id %in% unit_rules])))
  expect_identical(csv$n_records[csv$rule_id == "blank_string"], 3L)
})

test_that("message is the rule's text, and proposals are empty until M11 (11.6; D20.32)", {
  local_results_registry()
  x <- fx_report_results()
  issues <- route_unit(x, "100", c("100", "110"))$issues
  csv <- provider_csv_table(issues, c("unit", "area"), "plot")
  expect_identical(
    csv$message, vapply(csv$rule_id, report_text, character(1), USE.NAMES = FALSE)
  )
  expect_identical(
    csv$message[csv$rule_id == "fk_orphan"], "A foreign key names no record of its parent table."
  )
  text <- data.table::data.table(text_id = "fk_orphan", lang = "en", text = "Your own words.")
  csv <- provider_csv_table(issues, c("unit", "area"), "plot", text = text)
  expect_identical(csv$message[csv$rule_id == "fk_orphan"], "Your own words.")
  # A language with no row of its own falls back to English (D14.8).
  csv <- provider_csv_table(issues, c("unit", "area"), "plot", lang = "fr")
  expect_identical(
    csv$message[csv$rule_id == "fk_orphan"], "A foreign key names no record of its parent table."
  )
  # Issue 10 has a correction proposing a value, but proposals come at M11.
  expect_true(all(is.na(csv$proposed_value)))
  expect_true(all(is.na(csv$easy_fix)))
})

test_that("every text column of the CSV is guarded (D20.36)", {
  # A registry and a text of the rule's own, so every text cell can start with "=": the rule's
  # ID and message too. A decoded key's parts can't hold "=" (D20.35), so its parts start with
  # "+" and "@", which the guard covers as well.
  testthat::local_mocked_bindings(rule_registry = function() {
    data.table::data.table(rule_id = "=rule", message_id = "=message")
  })
  text <- data.table::data.table(text_id = "=message", lang = "en", text = "=Text.")
  issues <- data.table::data.table(
    issue_id = 1:2, unit = "=100", plot = "=P1", table_name = "=t", column_name = "=c",
    rule_id = "=rule",
    severity = "=error", issue_class = "=source", provider_key = c("=raw", "@a=+1; @b=@2"),
    provider_key_status = c("=raw", "decoded"), record_pk = "=pk", value = "=v",
    expected = "=e", status = "=open", change = "=new", fingerprint = "=f",
    n_records = c(1L, 2L), reaches_other_unit = FALSE
  )
  before <- data.table::copy(issues)
  csv <- provider_csv_table(issues, "unit", "plot", text = text)
  # The guard works in place on the CSV's own table, never on the routed issues (D20.92).
  expect_identical(issues, before)
  text_columns <- names(csv)[vapply(csv, is.character, logical(1))]
  expect_true(all(c("key_@a", "key_@b") %in% text_columns))
  for (column in setdiff(text_columns, "proposed_value")) {
    cells <- csv[[column]]
    # The only cell not starting with a mark the guard covers: the decoded key's status.
    cells <- cells[!is.na(cells) & cells != "decoded"]
    expect_true(length(cells) > 0L && all(startsWith(cells, "'")), info = column)
  }
  expect_identical(csv[["key_@a"]], c(NA, "'+1"))
})

test_that("the CSV reads back cell for cell from fwrite(bom = TRUE), after its BOM (D8.18)", {
  local_results_registry()
  x <- fx_report_results()
  issues <- data.table::copy(x$issues)
  # A value marked latin1, as delivered text can be.
  latin <- "caf\xe9"
  Encoding(latin) <- "latin1"
  data.table::set(issues, i = 1L, j = "value", value = latin)
  csv <- provider_csv_table(issues, c("unit", "area"), "plot")
  path <- withr::local_tempfile(fileext = ".csv")
  # As gpq_build_reports() writes it (task 11): UTF-8 whatever a cell's mark, LF line ends.
  data.table::fwrite(csv, path, bom = TRUE, encoding = "UTF-8", eol = "\n")
  expect_identical(readBin(path, "raw", 3L), as.raw(c(0xef, 0xbb, 0xbf)))
  back <- data.table::fread(
    path,
    colClasses = "character", na.strings = "", strip.white = FALSE, encoding = "UTF-8"
  )
  # fread() keeps a quoted cell's doubled quotes, as gpq_read_results() undoes them.
  undo_doubled_quotes(back)
  expect_identical(names(back), names(csv))
  for (column in names(csv)) {
    expect_identical(back[[column]], as.character(csv[[column]]), info = column)
  }
  expect_true("café" %in% back$value)
})
