# Tests for the provider report's sections (plan 11.3; D8.18, D14.7, D20.17, D20.23, D20.28,
# D20.30, D20.39 to D20.47, D20.53 to D20.56). Each builder is tested alone on a small table of
# results and issues made inside its test with new_results() and new_issues(), a stand-in for
# html_table() or html_bar() catching what it is given where the test reads a table's content;
# where a test reads the HTML, the real builders make it.

test_that("section_header lists the run's facts, then its files and settings folded (D20.40)", {
  schema <- metadata_schema()
  metadata <- lapply(schema, empty_table)
  metadata$run <- complete_table(list(
    run_id = "20261026T120000Z_abc123", run_start = "2026-10-26T12:00:00Z",
    run_end = "2026-10-26T12:00:05Z", package_version = "0.0.0.9024",
    git_commit = NA_character_, r_version = "4.5.2", platform = "x86_64-w64-mingw32",
    data_table_version = "1.18.0", report_units = "u<1>; u2", group_cols = "unit; area",
    outcome = "completed"
  ), schema$run, "run")
  metadata$spec_manifest <- complete_table(list(
    input = c("dictionary", "code_lists", "non_code_sheets"),
    file = c("20261005_dd.csv", "lists.xlsx", "in memory"), file_date = c("20261005", NA, NA),
    sha256 = c(strrep("a", 64L), strrep("b", 64L), NA)
  ), schema$spec_manifest, "spec_manifest")
  # A CSV file, a table given in memory and one read from a connection (9.5, D9.23).
  metadata$inputs <- complete_table(list(
    input = c("tables:catches", "tables:hauls", "tables:stations"),
    file = c("catches <&>.csv", "in memory", "DBI connection, not hashed"),
    sha256 = c(strrep("c", 64L), NA, NA),
    detail = c("3 rows; separator comma", "2 rows", "4 rows; table st; driver SQLiteConnection")
  ), schema$inputs, "inputs")
  metadata$rules <- complete_table(
    list(input = "rules", name = "Fish <rules>", version = "1.0", sha256 = strrep("d", 64L)),
    schema$rules, "rules"
  )
  # A row the registry doesn't define, named like the unit's CSV name in `settings`, which
  # section 1 never shows as a setting (D20.68).
  metadata$settings <- complete_table(list(
    setting = c("lang", "example_cap", "severity:raw_na", "csv_name"),
    value = c("en", "5", "warning", "other_issues.csv"),
    tier = c("built-in", "rule set", "argument", "argument")
  ), schema$settings, "settings")
  metadata$self_test <- complete_table(
    list(rule_id = "pk_missing", stage = "conformance", layer = 4L), schema$self_test, "self_test"
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u%3C1%3E_issues.csv"
  )
  caught <- list()
  testthat::local_mocked_bindings(
    html_facts = function(values) {
      caught$facts <<- values
      "<facts>"
    },
    html_details = function(summary, body) {
      caught$details <<- c(caught$details, summary)
      body
    },
    html_table = function(dt, caption, ...) {
      caught[[caption]] <<- data.table::copy(dt)
      "<table>"
    }
  )
  tables <- list(metadata = metadata, unit = "u<1>", group_cols = c("unit", "area"))
  section <- section_header(tables, NULL, settings)
  expect_equal(attr(section, "gpq_section")[["id"]], "header")
  expect_equal(attr(section, "gpq_section")[["title"]], "About this report")
  expect_equal(caught$facts, c(
    "Report unit (unit)" = "u<1>", "Run ID" = "20261026T120000Z_abc123",
    "Started (UTC)" = "2026-10-26 12:00:00", "Finished (UTC)" = "2026-10-26 12:00:05",
    "Table: catches" = paste0("catches <&>.csv, SHA-256 ", strrep("c", 64L)),
    "Table: hauls" = "in memory; 2 rows",
    "Table: stations" = "DBI connection, not hashed; 4 rows; table st; driver SQLiteConnection",
    # D20.88: the rule set's name as written, not quoted.
    "Rule set" = "Fish <rules>, version 1.0", "Package version" = "0.0.0.9024",
    "Git commit" = "none (an installed package)", "R version" = "4.5.2"
  ))
  # The files read from a file only, their dates from their names; the settings with the tier
  # each came from, in words.
  expect_equal(caught$details, c("Specification files (2)", "Settings (3)"))
  expect_equal(
    caught[["Specification files"]],
    data.table::data.table(File = c("20261005_dd.csv", "lists.xlsx"), Date = c("2026-10-05", NA))
  )
  expect_equal(caught[["Settings"]], data.table::data.table(
    Setting = c("lang", "example_cap", "severity:raw_na"), Value = c("en", "5", "warning"),
    `Set by` = c("built-in", "rule set", "argument")
  ))
  # Never the run's list of units, the files' SHA-256 or the self-test (D20.40).
  everything <- c(names(caught$facts), caught$facts, unlist(caught[-1L]), as.character(section))
  expect_false(any(grepl("u2", everything, fixed = TRUE)))
  expect_false(any(grepl(strrep("a", 64L), everything, fixed = TRUE)))
  expect_false(any(grepl("pk_missing", everything, fixed = TRUE)))
  # Nor the unit's CSV name, from `settings` or a row the registry doesn't define (D20.68).
  expect_false(any(grepl("csv_name|issues.csv", everything)))
  # A run with no group columns has no unit row (D20.29); without a rule set, no rule-set
  # row; a git checkout's commit as recorded.
  data.table::set(metadata$run, j = "git_commit", value = strrep("e", 40L))
  metadata$rules <- empty_table(schema$rules)
  section_header(list(metadata = metadata, unit = NULL, group_cols = character()), NULL, settings)
  expect_equal(names(caught$facts)[1:2], c("Run ID", "Started (UTC)"))
  expect_false("Rule set" %in% names(caught$facts))
  expect_equal(caught$facts[["Git commit"]], strrep("e", 40L))
})

test_that("section_glance counts each open issue once, by class, layer and value (D20.41)", {
  run_id <- "20261026T120000Z_abc123"
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:6, run_id = run_id, run_scope = "package",
    rule_id = c(
      "code_not_in_list", "code_not_in_list", "raw_na", "column_missing", "value_above_max",
      "value_above_max"
    ),
    rule_fingerprint = "f", stage = "conformance", layer = c(3L, 3L, 2L, 0L, 3L, 3L),
    severity = c("error", "error", "warning", "error", "warning", "warning"), reason_code = "*",
    table_name = c("catches", "catches", "hauls", "hauls", "catches", "catches"),
    column_name = c("species", "species", "minutes", "gear", "weight", "weight"),
    record_pk = c("c1", "c2", "h1", "*", "c3", "c4"), row_number = c(1L, 2L, 1L, NA, 3L, 4L),
    pk_positional = c(FALSE, FALSE, FALSE, NA, FALSE, FALSE),
    level = c("record", "record", "record", "column", "record", "record"),
    n_records = c(NA, NA, NA, 0L, NA, NA), unit = c("u1", "u1", "u1", "*", "u1", "u1"),
    area = c("<a&1>", strrep("z", 101L), "a2", "*", "<a&1>", "<a&1>"),
    plot = c("p1", "p2", "p3", "*", "*", "p4"), root_issue_id = c(NA, NA, NA, NA, NA, 5L),
    status = c(rep("open", 5L), "not_evaluated"),
    issue_class = c(
      "source", "source", "harmonization", "harmonization", "unclassified", "unclassified"
    )
  ), group_cols = c("unit", "area"), key_col = "plot")
  # A Layer 4 results row with no open issue still gives its layer a row.
  results <- new_results(list(
    run_id = run_id, run_scope = "package",
    rule_id = c("column_missing", "raw_na", "code_not_in_list", "pk_duplicate"),
    rule_fingerprint = "f", stage = "conformance", layer = c(0L, 2L, 3L, 4L),
    severity = c("error", "warning", "error", "error"),
    table_name = c("hauls", "hauls", "catches", "catches"),
    column_name = c("gear", "minutes", "species", "catch_id"),
    unit = c("*", "u1", "u1", "u1"), area = c("*", "a2", "<a&1>", "<a&1>"),
    n_checked = c(1L, 3L, 4L, 4L), n_fail = c(1L, 1L, 2L, 0L), n_not_evaluated = 0L,
    n_unchecked = c(NA, 0L, 0L, 0L), reason_code = "*",
    n_distinct_fail_values = c(1L, 1L, 2L, 0L), n_easy_fix = 0L,
    status = c("fail", "fail", "fail", "pass"),
    message_id = c("column_missing", "raw_na", "code_not_in_list", "pk_duplicate")
  ), group_cols = c("unit", "area"))
  tables <- list(
    results = results, issues = issues, unit = "u1", group_cols = c("unit", "area"),
    key_col = "plot"
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- list()
  bars <- list()
  testthat::local_mocked_bindings(
    html_table = function(dt, caption, row_cap = NULL, csv_name = NULL, row_class = NULL, ...) {
      caught[[caption]] <<- list(dt = data.table::copy(dt), row_class = row_class)
      "<table>"
    },
    html_bar = function(value, max, label, bar_class = NULL) {
      bars[[length(bars) + 1L]] <<- list(
        value = value, max = max, label = label, bar_class = bar_class
      )
      "<bars>"
    }
  )
  section <- section_glance(tables, NULL, settings)
  html <- as.character(section)
  expect_equal(attr(section, "gpq_section")[["id"]], "glance")
  expect_match(
    html,
    paste(
      "Open issues: 5. For you to act on: 2. Being fixed in harmonization: 2.",
      "Under review: 1."
    ),
    fixed = TRUE
  )
  # Severity bars: every severity, in order, coloured by class.
  expect_equal(bars[[1L]], list(
    value = c(3L, 2L, 0L, 0L), max = 3L, label = c("Error", "Warning", "Flag", "Information"),
    bar_class = c("sev-error", "sev-warning", "sev-flag", "sev-info")
  ))
  expect_equal(caught[["Open issues by layer and severity"]]$dt, data.table::data.table(
    Layer = c(
      "Layer 0: structure", "Layer 2: missing-value codes", "Layer 3: value domains",
      "Layer 4: keys and relationships", "Total"
    ),
    Error = c(1L, 0L, 2L, 0L, 3L), Warning = c(0L, 1L, 1L, 0L, 2L), Flag = rep(0L, 5L),
    Information = rep(0L, 5L), Total = c(1L, 1L, 3L, 0L, 5L)
  ))
  expect_equal(caught[["Open issues by layer and severity"]]$row_class, c(rep(NA, 4L), "total"))
  # Three classes (D20.55).
  expect_equal(caught[["Open issues by class"]]$dt, data.table::data.table(
    Class = c("For you to act on", "Being fixed in harmonization", "Under review"),
    Issues = c(2L, 2L, 1L)
  ))
  # By the breakdown column from the issues: most issues first, then by value, "*" after the
  # values with as many and shown as W4; a long value cut (D20.39); then by table.
  expect_match(html, "<h3>Open issues by area</h3>", fixed = TRUE)
  expect_equal(bars[[2L]], list(
    value = c(2L, 1L, 1L, 1L), max = 2L,
    label = c("<a&1>", "a2", paste0(strrep("z", 100L), "…"), "(not linked)"), bar_class = NULL
  ))
  expect_equal(bars[[3L]], list(
    value = c(3L, 2L), max = 3L, label = c("catches", "hauls"), bar_class = NULL
  ))
  # D20.90: a chart of more values than row_cap shows the first ones, and says the CSV has them
  # all.
  bars <- list()
  settings$row_cap <- 2L
  html <- as.character(section_glance(tables, NULL, settings))
  expect_equal(bars[[2L]]$label, c("<a&1>", "a2"))
  expect_match(
    html,
    paste(
      "Showing the 2 values with the most open issues. The issues CSV, u1_issues.csv, has",
      "them all."
    ),
    fixed = TRUE
  )
  expect_equal(bars[[3L]]$label, c("catches", "hauls"))
  # No open issue: zeros, and no chart by value.
  bars <- list()
  tables$issues <- issues[issues$status == "not_evaluated"]
  html <- as.character(section_glance(tables, NULL, settings))
  expect_match(
    html,
    paste(
      "Open issues: 0. For you to act on: 0. Being fixed in harmonization: 0.",
      "Under review: 0."
    ),
    fixed = TRUE
  )
  expect_length(bars, 1L)
  expect_equal(caught[["Open issues by class"]]$dt$Issues, c(0L, 0L, 0L))
})

test_that("section_guide shows a paragraph only where the report has what it explains (D20.47)", {
  run_id <- "20261026T120000Z_abc123"
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 7L,
    csv_name = "u1_issues.csv"
  )
  # Layers 0, 2 and 3 ran; a check didn't run; an open record-level source issue with a key
  # status and an open column-level issue.
  results <- new_results(list(
    run_id = run_id, run_scope = "package",
    rule_id = c("column_missing", "raw_na", "code_not_in_list", "value_above_max"),
    rule_fingerprint = "f", stage = "conformance", layer = c(0L, 2L, 3L, 3L),
    severity = c("error", "warning", "error", "warning"),
    table_name = c("hauls", "hauls", "catches", "catches"),
    column_name = c("gear", "minutes", "species", "weight"), unit = "u1", area = "a1",
    n_checked = c(1L, 3L, 4L, 0L), n_fail = c(1L, 0L, 1L, 0L), n_not_evaluated = 0L,
    n_unchecked = c(NA, 0L, 0L, 0L), reason_code = "*",
    n_distinct_fail_values = c(1L, 0L, 1L, 0L), n_easy_fix = 0L,
    status = c("fail", "pass", "fail", "not_run"), not_run_reason = c(NA, NA, NA, "no_sentinels"),
    message_id = c("column_missing", "raw_na", "code_not_in_list", "value_above_max")
  ), group_cols = c("unit", "area"))
  data.table::set(results, j = "n_not_evaluated_own", value = 0L)
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:2, run_id = run_id, run_scope = "package",
    rule_id = c("code_not_in_list", "column_missing"), rule_fingerprint = "f",
    stage = "conformance", layer = c(3L, 0L), severity = "error", reason_code = "*",
    table_name = c("catches", "hauls"), column_name = c("species", "gear"),
    record_pk = c("c1", "*"), row_number = c(1L, NA), pk_positional = c(FALSE, NA),
    level = c("record", "column"), n_records = c(NA, 0L), provider_key = c("plot=1", NA),
    provider_key_status = c("decoded", NA), unit = c("u1", "*"), area = c("a1", "*"),
    plot = c("p1", "*"), status = "open", issue_class = c("source", "harmonization")
  ), group_cols = c("unit", "area"), key_col = "plot")
  tables <- list(results = results, issues = issues, unit = "u1", group_cols = c("unit", "area"))
  html <- as.character(section_guide(tables, NULL, settings))
  shows <- function(text_id, ...) {
    grepl(html_escape(report_text(text_id, ...)), html, fixed = TRUE)
  }
  expect_equal(attr(section_guide(tables, NULL, settings), "gpq_section")[["id"]], "guide")
  expect_true(shows("report_guide_about", csv_name = "u1_issues.csv"))
  expect_true(shows("report_guide_layers"))
  for (layer in c(0L, 2L, 3L)) {
    expect_true(shows(paste0("report_layer_", layer)), info = layer)
    expect_true(shows(paste0("report_guide_layer_", layer)), info = layer)
  }
  expect_false(shows("report_guide_layer_1"))
  expect_false(shows("report_guide_layer_4"))
  for (text_id in c(
    "report_guide_severities", "report_guide_classes", "report_guide_sentinels",
    "report_guide_keys", "report_guide_not_evaluated"
  )) {
    expect_true(shows(text_id), info = text_id)
  }
  expect_true(shows("report_guide_column_level", min_n = 7L))
  expect_match(html, "at least 7, the report shows", fixed = TRUE)
  expect_true(shows("report_guide_caps", row_cap = 500L, example_cap = 5L, cell_cap = 100L))
  expect_match(html, "<div class=\"guide\">", fixed = TRUE)
  # A report with nothing to explain but the run: what it is, and Layer 0's line.
  results <- results[1L]
  tables <- list(results = results, issues = issues[0L], unit = "u1", group_cols = "unit")
  html <- as.character(section_guide(tables, NULL, settings))
  expect_true(shows("report_guide_about", csv_name = "u1_issues.csv"))
  expect_true(shows("report_guide_layer_0"))
  for (text_id in c(
    "report_guide_layer_2", "report_guide_severities", "report_guide_classes",
    "report_guide_sentinels", "report_guide_keys", "report_guide_not_evaluated"
  )) {
    expect_false(shows(text_id), info = text_id)
  }
  expect_false(shows("report_guide_caps", row_cap = 500L, example_cap = 5L, cell_cap = 100L))
  expect_false(shows("report_guide_column_level", min_n = 7L))
})

test_that("sections 1, 2 and 14 escape every value they show, every slot filled (11.1)", {
  run_id <- "20261026T120000Z_abc123"
  schema <- metadata_schema()
  # Each value a marker <mN> holding markup, so one left unescaped shows as "<m".
  metadata <- lapply(schema, empty_table)
  metadata$run <- complete_table(list(
    run_id = run_id, run_start = "2026-10-26T12:00:00Z", run_end = "2026-10-26T12:00:05Z",
    package_version = "<m2>", git_commit = "<m3>", r_version = "4.5.2",
    platform = "x86_64-w64-mingw32", data_table_version = "1.18.0", outcome = "completed"
  ), schema$run, "run")
  metadata$inputs <- complete_table(
    list(input = "tables:<m4>", file = "<m5>.csv", sha256 = "<m6>", detail = "1 row"),
    schema$inputs, "inputs"
  )
  metadata$spec_manifest <- complete_table(
    list(input = "dictionary", file = "<m7>.csv", file_date = "<m8>", sha256 = "x"),
    schema$spec_manifest, "spec_manifest"
  )
  metadata$rules <- complete_table(
    list(input = "rules", name = "<m9>&", version = "'1'", sha256 = "x"), schema$rules, "rules"
  )
  # A severity entry, the one kind of setting row whose name holds caller text (D20.68).
  metadata$settings <- complete_table(
    list(setting = "severity:<m11>", value = "<m12>&\"", tier = "option"), schema$settings,
    "settings"
  )
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1L, run_id = run_id, run_scope = "package", rule_id = "code_not_in_list",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    reason_code = "*", table_name = "catches", column_name = "species", record_pk = "c1",
    row_number = 1L, pk_positional = FALSE, level = "record", value = "x", unit = "<m1>&",
    area = "<m13>&", plot = "p1", status = "open", issue_class = "source"
  ), group_cols = c("unit", "area"), key_col = "plot")
  results <- new_results(list(
    run_id = run_id, run_scope = "package", rule_id = "code_not_in_list",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    table_name = "catches", column_name = "species", unit = "<m1>&", area = "<m13>&",
    n_checked = 1L, n_fail = 1L, n_not_evaluated = 0L, n_unchecked = 0L, reason_code = "*",
    n_distinct_fail_values = 1L, n_easy_fix = 0L, status = "fail",
    message_id = "code_not_in_list"
  ), group_cols = c("unit", "area"))
  data.table::set(results, j = "n_not_evaluated_own", value = 0L)
  tables <- list(
    results = results, issues = issues, metadata = metadata, unit = "<m1>&",
    group_cols = c("unit", "area"), key_col = "plot"
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "<m14>_issues.csv"
  )
  html <- c(
    header = as.character(section_header(tables, NULL, settings)),
    glance = as.character(section_glance(tables, NULL, settings)),
    guide = as.character(section_guide(tables, NULL, settings))
  )
  expect_false(any(grepl("<m[0-9]", html)))
  expect_match(html[["header"]], "&lt;m1&gt;&amp;", fixed = TRUE)
  expect_match(html[["header"]], "&lt;m9&gt;&amp;, version &#39;1&#39;", fixed = TRUE)
  expect_match(html[["header"]], "&lt;m12&gt;&amp;&quot;", fixed = TRUE)
  expect_match(html[["glance"]], "&lt;m13&gt;&amp;", fixed = TRUE)
  expect_match(html[["guide"]], "&lt;m14&gt;_issues.csv", fixed = TRUE)
  expect_false(any(grepl("\\{[a-z0-9_]+\\}", html)))
})

test_that("each layer of a registered data rule has its name and its guide line (D20.41, D20.47)", {
  registry <- rule_registry()
  layers <- sort(unique(registry$layer[registry$check_type != "preflight"]))
  expect_true(all(0:4 %in% layers))
  for (layer in layers) {
    expect_no_error(report_text(paste0("report_layer_", layer)))
    expect_no_error(report_text(paste0("report_guide_layer_", layer)))
  }
})
