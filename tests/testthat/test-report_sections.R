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
  # Each tier is looked up as a word, so a caller's word takes its place (D14.7).
  tier_text <- validate_text_table(data.frame(
    text_id = "report_tier_built_in", lang = "en", text = "default"
  ))
  section_header(tables, tier_text, settings)
  expect_equal(caught[["Settings"]]$`Set by`, c("default", "rule set", "argument"))
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
  # Below the cap, no capped line.
  expect_no_match(html, "with the most open issues", fixed = TRUE)
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
  # The line once, for the chart by area only: the chart by table has just 2 values.
  capped_line <- "with the most open issues"
  expect_equal(lengths(regmatches(html, gregexpr(capped_line, html, fixed = TRUE))), 1L)
  # row_cap 0: no bar in either chart, and each chart's line once.
  bars <- list()
  settings$row_cap <- 0L
  html <- as.character(section_glance(tables, NULL, settings))
  expect_length(bars, 1L)
  none_shown <- paste("Showing the 0 values", capped_line)
  expect_equal(lengths(regmatches(html, gregexpr(none_shown, html, fixed = TRUE))), 2L)
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
  # Each condition alone (D20.47), from a Layer 0 row with nothing to explain: which of W178
  # to W184 show.
  paragraphs <- c(
    severities = report_text("report_guide_severities"),
    classes = report_text("report_guide_classes"),
    sentinels = report_text("report_guide_sentinels"), keys = report_text("report_guide_keys"),
    not_evaluated = report_text("report_guide_not_evaluated"),
    column_level = report_text("report_guide_column_level", min_n = 7L),
    caps = report_text("report_guide_caps", row_cap = 500L, example_cap = 5L, cell_cap = 100L)
  )
  showing <- function(rows, found = issues[0L]) {
    built <- as.character(section_guide(
      list(results = rows, issues = found, unit = "u1", group_cols = "unit"), NULL, settings
    ))
    names(paragraphs)[vapply(
      paragraphs, function(x) grepl(html_escape(x), built, fixed = TRUE), TRUE
    )]
  }
  base <- results[1L]
  expect_equal(showing(base), character())
  # A Layer 2 row: the missing-value codes only.
  expect_equal(showing(results[1:2]), "sentinels")
  # Each kind of section 10 row alone (a check not run, records not reached, records a check
  # couldn't judge, a not-evaluated issue) brings W182 and the caps, nothing else.
  unchecked <- results[c(1L, 3L)]
  data.table::set(unchecked, i = 2L, j = "n_unchecked", value = 2L)
  unjudged <- results[c(1L, 3L)]
  data.table::set(unjudged, i = 2L, j = "n_not_evaluated_own", value = 1L)
  waiting <- issues[1L]
  data.table::set(waiting, j = "status", value = "not_evaluated")
  expect_equal(showing(results[c(1L, 4L)]), c("not_evaluated", "caps"))
  expect_equal(showing(unchecked), c("not_evaluated", "caps"))
  expect_equal(showing(unjudged), c("not_evaluated", "caps"))
  expect_equal(showing(base, waiting), c("not_evaluated", "caps"))
  # An open issue brings the severities, the classes and the caps; the keys only for an open
  # record-level source issue with a key status, and the column-level paragraph only for an
  # open column-level issue.
  harmonized <- issues[1L]
  data.table::set(harmonized, j = "issue_class", value = "harmonization")
  unkeyed <- issues[1L]
  data.table::set(unkeyed, j = "provider_key_status", value = NA_character_)
  expect_equal(showing(base, issues[1L]), c("severities", "classes", "keys", "caps"))
  expect_equal(showing(base, harmonized), c("severities", "classes", "caps"))
  expect_equal(showing(base, unkeyed), c("severities", "classes", "caps"))
  expect_equal(showing(base, issues[2L]), c("severities", "classes", "column_level", "caps"))
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
  # Each value, and each group column's name, a marker <mN> holding markup, so one left
  # unescaped shows as "<m".
  groups <- c("unit<m15>&", "area<m16>&")
  metadata <- lapply(schema, empty_table)
  metadata$run <- complete_table(list(
    run_id = run_id, run_start = "2026-10-26T12:00:00Z", run_end = "2026-10-26T12:00:05Z",
    package_version = "<m2>", git_commit = "<m3>", r_version = "4.5.2",
    platform = "x86_64-w64-mingw32", data_table_version = "1.18.0", outcome = "completed"
  ), schema$run, "run")
  # A file with its SHA-256, and a table given in memory, shown with its detail (W29).
  metadata$inputs <- complete_table(list(
    input = c("tables:<m4>", "tables:<m17>"), file = c("<m5>.csv", "in memory"),
    sha256 = c("<m6>", NA), detail = c("1 row", "<m18>& rows")
  ), schema$inputs, "inputs")
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
    row_number = 1L, pk_positional = FALSE, level = "record", value = "x",
    "unit<m15>&" = "<m1>&", "area<m16>&" = "<m13>&", plot = "p1", status = "open",
    issue_class = "source"
  ), group_cols = groups, key_col = "plot")
  results <- new_results(list(
    run_id = run_id, run_scope = "package", rule_id = "code_not_in_list",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    table_name = "catches", column_name = "species", "unit<m15>&" = "<m1>&",
    "area<m16>&" = "<m13>&", n_checked = 1L, n_fail = 1L, n_not_evaluated = 0L,
    n_unchecked = 0L, reason_code = "*", n_distinct_fail_values = 1L, n_easy_fix = 0L,
    status = "fail", message_id = "code_not_in_list"
  ), group_cols = groups)
  data.table::set(results, j = "n_not_evaluated_own", value = 0L)
  tables <- list(
    results = results, issues = issues, metadata = metadata, unit = "<m1>&",
    group_cols = groups, key_col = "plot"
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "<m14>_issues.csv"
  )
  # At row_cap 0 each chart shows its capped line, which names the unit's CSV (D20.90).
  capped <- settings
  capped$row_cap <- 0L
  html <- c(
    header = as.character(section_header(tables, NULL, settings)),
    glance = as.character(section_glance(tables, NULL, settings)),
    capped = as.character(section_glance(tables, NULL, capped)),
    guide = as.character(section_guide(tables, NULL, settings))
  )
  expect_false(any(grepl("<m[0-9]", html)))
  expect_match(html[["header"]], "&lt;m1&gt;&amp;", fixed = TRUE)
  expect_match(html[["header"]], "Report unit (unit&lt;m15&gt;&amp;)", fixed = TRUE)
  expect_match(html[["header"]], "in memory; &lt;m18&gt;&amp; rows", fixed = TRUE)
  expect_match(html[["header"]], "&lt;m9&gt;&amp;, version &#39;1&#39;", fixed = TRUE)
  expect_match(html[["header"]], "&lt;m12&gt;&amp;&quot;", fixed = TRUE)
  expect_match(html[["glance"]], "&lt;m13&gt;&amp;", fixed = TRUE)
  expect_match(html[["glance"]], "Open issues by area&lt;m16&gt;&amp;", fixed = TRUE)
  expect_match(
    html[["capped"]], "The issues CSV, &lt;m14&gt;_issues.csv, has them all.",
    fixed = TRUE
  )
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

test_that("issue_summary: a row per check, sorted, its examples cut as one cell (D20.39, D20.42)", {
  run_id <- "20261026T120000Z_abc123"
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:11, run_id = run_id, run_scope = "package",
    rule_id = c(rep("code_not_in_list", 5L), "raw_na", rep("value_above_max", 5L)),
    rule_fingerprint = "f", stage = "conformance", layer = c(rep(3L, 5L), 2L, rep(3L, 5L)),
    severity = c(rep("warning", 5L), "error", rep("warning", 5L)), reason_code = "*",
    table_name = c(rep("catches", 5L), "hauls", rep("catches", 5L)),
    column_name = c(rep("species", 5L), "minutes", "weight", "weight", rep("length", 3L)),
    record_pk = paste0("r", 1:11), row_number = 1:11, pk_positional = FALSE, level = "record",
    value = c("b", "a", "b", NA, "c", NA, strrep("p", 60L), strrep("q", 60L), "x", "y", "z"),
    unit = "u1", area = "a1", plot = "p1", status = "open", issue_class = "harmonization"
  ), group_cols = c("unit", "area"), key_col = "plot")
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 2L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- list()
  testthat::local_mocked_bindings(
    html_table = function(dt, caption, row_cap = NULL, csv_name = NULL, row_class = NULL, ...) {
      caught <<- list(
        dt = data.table::copy(dt), caption = caption, row_cap = row_cap, csv_name = csv_name,
        row_class = row_class
      )
      "<table>"
    }
  )
  # The rows in another order than their issue IDs: the examples still follow the IDs.
  issue_summary(issues[11:1], NULL, settings)
  expect_equal(caught$caption, "By check")
  expect_equal(caught$dt, data.table::data.table(
    Severity = c("Error", "Warning", "Warning", "Warning"),
    Check = c("raw_na", "code_not_in_list", "value_above_max", "value_above_max"),
    `What it means` = c(
      report_text("raw_na"), report_text("code_not_in_list"), report_text("value_above_max"),
      report_text("value_above_max")
    ),
    Table = c("hauls", "catches", "catches", "catches"),
    Column = c("minutes", "species", "length", "weight"), Issues = c(1L, 5L, 3L, 2L),
    # Distinct values, NA left out, the first two by issue ID; the two long ones joined and
    # cut as one cell at 100 characters.
    Examples = c(
      NA, "b; a", "x; y", paste0(strrep("p", 60L), "; ", strrep("q", 38L), "…")
    )
  ))
  expect_equal(caught$row_cap, 500L)
  expect_equal(caught$csv_name, "u1_issues.csv") # D20.68
  expect_equal(caught$row_class, c("sev-error", rep("sev-warning", 3L)))
  # A caller's text in the run's language, the package's English where it has none (D14.7).
  text <- validate_text_table(data.frame(
    text_id = "report_summary_caption", lang = "fr", text = "Par controle"
  ))
  settings$lang <- "fr"
  issue_summary(issues, text, settings)
  expect_equal(caught$caption, "Par controle")
  expect_equal(names(caught$dt)[[1L]], "Severity")
})

test_that("issue_records: keys, raw values, breakdowns, no empty column (D20.17, D20.42, D20.56)", {
  run_id <- "20261026T120000Z_abc123"
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:3, run_id = run_id, run_scope = "package",
    rule_id = c("code_not_in_list", "code_not_in_list", "whitespace_padding"),
    rule_fingerprint = "f", stage = "conformance", layer = c(3L, 3L, 2L),
    severity = c("error", "error", "warning"), reason_code = "*", table_name = "catches",
    column_name = "species", record_pk = c("c2", "c1", "c3"), row_number = 1:3,
    pk_positional = FALSE, level = "record", value = c("q", strrep("v", 101L), "x"),
    provider_key = c("plot=2", "plot=1", NA), provider_key_status = c("decoded", "partial", NA),
    unit = "u1", area = c("a1", "*", "a2"), plot = c("p2", "*", "p1"), status = "open",
    issue_class = "source"
  ), group_cols = c("unit", "area"), key_col = "plot")
  columns <- c(
    "issue_id", "severity", "rule_id", "table_name", "column_name", "provider_key",
    "provider_key_status", "record_pk", "value", "expected"
  )
  # As section_actions() hands them: the breakdown column and the key column after the fixed
  # ones, and each cell's raw value apart (D20.123).
  shown <- issues[, c(columns, "area", "plot"), with = FALSE]
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- list()
  testthat::local_mocked_bindings(
    html_table = function(dt, caption, row_cap = NULL, csv_name = NULL, row_class = NULL, ...) {
      caught <<- list(
        dt = data.table::copy(dt), caption = caption, row_cap = row_cap, csv_name = csv_name,
        row_class = row_class
      )
      "<table>"
    }
  )
  issue_records(shown, c("Q", NA, NA), NULL, settings)
  expect_equal(caught$caption, "Each issue")
  # Sorted by severity, check, table, column and key; no expected value anywhere, so no column
  # for it (D20.42); "*" shown as W4; the long value cut (D20.39).
  expect_equal(caught$dt, data.table::data.table(
    Severity = c("Error", "Error", "Warning"),
    Check = c("code_not_in_list", "code_not_in_list", "whitespace_padding"),
    Table = "catches", Column = "species", `Your key` = c("plot=1", "plot=2", NA),
    `Key status` = c("partly decoded", "decoded", NA), `Record ID` = c("c1", "c2", "c3"),
    Value = c(paste0(strrep("v", 100L), "…"), "q", "x"),
    `Your value before harmonization` = c(NA, "Q", NA),
    area = c("(not linked)", "a1", "a2"), plot = c("(not linked)", "p2", "p1")
  ))
  expect_equal(caught$row_class, c("sev-error", "sev-error", "sev-warning"))
  expect_equal(caught$row_cap, 500L)
  expect_equal(caught$csv_name, "u1_issues.csv") # D20.68
  # No lineage and no key: their columns go; the CSV keeps them all.
  bare <- issues[, columns, with = FALSE]
  data.table::set(bare, j = c("provider_key", "provider_key_status"), value = NA_character_)
  issue_records(bare, rep(NA_character_, 3L), NULL, settings)
  expect_named(caught$dt, c("Severity", "Check", "Table", "Column", "Record ID", "Value"))
})

test_that("sections 3 to 5 hold their class's record issues, pointing to section 6 (D20.28)", {
  run_id <- "20261026T120000Z_abc123"
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:7, run_id = run_id, run_scope = "package",
    rule_id = c(
      "code_not_in_list", "fk_orphan", "raw_na", "column_missing", "value_above_max",
      "code_not_in_list", "code_not_in_list"
    ),
    rule_fingerprint = "f", stage = "conformance", layer = c(3L, 4L, 2L, 0L, 3L, 3L, 3L),
    severity = c("error", "error", "warning", "error", "warning", "error", "error"),
    reason_code = "*",
    table_name = c("catches", "catches", "hauls", "hauls", "catches", "catches", "catches"),
    column_name = c("species", "haul_id", "minutes", "gear", "weight", "species", "species"),
    record_pk = c("c1", "*", "h1", "*", "c3", "c4", "c9"),
    row_number = c(1L, NA, 1L, NA, 3L, 4L, 9L),
    pk_positional = c(FALSE, NA, FALSE, NA, FALSE, FALSE, FALSE),
    level = c("record", "column", "record", "column", "record", "record", "record"),
    n_records = c(NA, 4L, NA, 0L, NA, NA, NA),
    value = c("<s1>", "<s2>", "<h1>", NA, "<r1>", "<s4>", "<s9>"),
    unit = c("u1", "u1", "u1", "*", "u1", "u1", "u2"),
    area = c("a1", "a1", "a2", "*", "*", "a1", "b1"),
    plot = c("p1", "*", "p2", "*", "*", "p4", "p9"),
    root_issue_id = c(NA, NA, NA, NA, NA, 1L, NA),
    status = c(rep("open", 5L), "not_evaluated", "open"),
    issue_class = c(
      "source", "source", "harmonization", "harmonization", "unclassified", "source", "source"
    )
  ), group_cols = c("unit", "area"), key_col = "plot")
  # A raw value for the unit's cell and one for another unit's (D20.17).
  lineage <- data.table::data.table(
    table_name = "catches", record_pk = c("c1", "c9"), column_name = "species",
    raw_value = c("<raw1>&\"'", "<raw9>"), raw_source = "x"
  )
  tables <- list(
    issues = issues[issues$unit != "u2"], unit = "u1", group_cols = c("unit", "area"),
    key_col = "plot", lineage = lineage
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- list()
  testthat::local_mocked_bindings(
    html_table = function(dt, caption, ...) {
      caught[[caption]] <<- data.table::copy(dt)
      paste0("<table ", caption, ">")
    }
  )
  pointer <- paste0(
    "<p>Issues of this class that cover a whole column: 1, listed in ",
    "<a href=\"#column-level\">Issues that cover a whole column</a>.</p>"
  )
  actions <- as.character(section_actions(tables, NULL, settings))
  expect_equal(attr(section_actions(tables, NULL, settings), "gpq_section")[["id"]], "actions")
  expect_match(actions, html_escape(report_text("report_actions_intro")), fixed = TRUE)
  # The column-level line first, then "By check" and "Each issue" (D20.28, D20.42).
  at <- vapply(
    c(pointer, "<table By check>", "<table Each issue>"),
    function(piece) regexpr(piece, actions, fixed = TRUE), integer(1)
  )
  expect_true(all(at > 0L))
  expect_false(is.unsorted(at))
  expect_equal(caught[["By check"]]$Examples, "<s1>")
  expect_equal(caught[["Each issue"]]$`Record ID`, "c1")
  # Only the unit's cell meets a lineage row.
  expect_equal(caught[["Each issue"]]$`Your value before harmonization`, "<raw1>&\"'")
  expect_equal(caught[["Each issue"]]$area, "a1")
  expect_equal(caught[["Each issue"]]$plot, "p1")
  caught <- list()
  harmonization <- as.character(section_harmonization(tables, NULL, settings))
  expect_match(
    harmonization,
    paste0("<p class=\"note\">", html_escape(report_text("report_harmonization_intro")), "</p>"),
    fixed = TRUE
  )
  # The summary only (D20.43), then the line pointing to section 6.
  expect_equal(names(caught), "By check")
  expect_equal(caught[["By check"]]$Examples, "<h1>")
  expect_gt(
    regexpr(pointer, harmonization, fixed = TRUE),
    regexpr("<table By check>", harmonization, fixed = TRUE)
  )
  caught <- list()
  review <- as.character(section_under_review(tables, NULL, settings))
  expect_match(review, html_escape(report_text("report_under_review_intro")), fixed = TRUE)
  expect_equal(caught[["By check"]]$Examples, "<r1>")
  expect_false(grepl("listed in", review, fixed = TRUE))
  # Column-level source issues only: the line, no table.
  tables$issues <- issues[2L]
  actions <- as.character(section_actions(tables, NULL, settings))
  expect_match(actions, pointer, fixed = TRUE)
  expect_false(grepl("<table", actions, fixed = TRUE))
  # None: each section's line alone.
  tables$issues <- issues[0L]
  for (built in list(
    list(section_actions, "report_actions_empty"),
    list(section_harmonization, "report_harmonization_empty"),
    list(section_under_review, "report_under_review_empty")
  )) {
    html <- as.character(built[[1L]](tables, NULL, settings))
    expect_match(html, paste0("<p>", html_escape(report_text(built[[2L]])), "</p>"), fixed = TRUE)
    expect_false(grepl("<table", html, fixed = TRUE))
  }
})

test_that("section_column_level lists each open column-level issue once, sorted (D20.45)", {
  run_id <- "20261026T120000Z_abc123"
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:6, run_id = run_id, run_scope = "package",
    rule_id = c(
      "coercion_loss", "column_missing", "undeclared_code_set", "fk_orphan", "code_not_in_list",
      "value_above_max"
    ),
    rule_fingerprint = "f", stage = "conformance", layer = c(1L, 0L, 3L, 4L, 3L, 3L),
    severity = c("error", "error", "warning", "error", "error", "warning"), reason_code = "*",
    table_name = c("catches", "hauls", "catches", "catches", "catches", "catches"),
    column_name = c("length", "gear", "species", "haul_id", "species", "length"),
    record_pk = c("*", "*", "*", "*", "c1", "*"), row_number = c(NA, NA, NA, NA, 1L, NA),
    pk_positional = c(NA, NA, NA, NA, FALSE, NA),
    level = c("column", "column", "column", "column", "record", "column"),
    n_records = c(5L, 0L, 640L, 3L, NA, 5L), value = c(NA, NA, "A; B", "<h9>", "x", NA),
    expected = c("numeric", NA, NA, NA, NA, NA),
    unit = c("u1", "*", "u1", "u1", "u1", "u1"), area = c("a1", "*", "*", "a2", "a1", "a1"),
    plot = "*", root_issue_id = c(NA, NA, NA, NA, NA, 1L),
    status = c(rep("open", 5L), "not_evaluated"),
    issue_class = c(
      "unclassified", "harmonization", "harmonization", "source", "source", "unclassified"
    )
  ), group_cols = c("unit", "area"), key_col = "plot")
  tables <- list(issues = issues, unit = "u1", group_cols = c("unit", "area"), key_col = "plot")
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- list()
  testthat::local_mocked_bindings(
    html_table = function(dt, caption, row_cap = NULL, csv_name = NULL, row_class = NULL, ...) {
      caught <<- list(dt = data.table::copy(dt), row_class = row_class, row_cap = row_cap)
      "<table>"
    }
  )
  section <- section_column_level(tables, NULL, settings)
  expect_equal(attr(section, "gpq_section")[["id"]], "column-level")
  expect_match(
    as.character(section), html_escape(report_text("report_column_level_intro")),
    fixed = TRUE
  )
  # By severity, then class (source, harmonization, unclassified), table and column; the
  # record-level and the not-evaluated issues are elsewhere. A unit rule's "*" issue,
  # column_missing's, shows no record count (D16.8, D20.122); the unit's own
  # undeclared_code_set issue keeps its count (D20.124).
  expect_equal(caught$dt, data.table::data.table(
    Severity = c("Error", "Error", "Error", "Warning"),
    Class = c(
      "For you to act on", "Being fixed in harmonization", "Under review",
      "Being fixed in harmonization"
    ),
    Check = c("fk_orphan", "column_missing", "coercion_loss", "undeclared_code_set"),
    `What it means` = c(
      report_text("fk_orphan"), report_text("column_missing"), report_text("coercion_loss"),
      report_text("undeclared_code_set")
    ),
    Table = c("catches", "hauls", "catches", "catches"),
    Column = c("haul_id", "gear", "length", "species"), Records = c(3L, NA, 5L, 640L),
    Value = c("<h9>", NA, NA, "A; B"), Expected = c(NA, NA, "numeric", NA),
    area = c("a2", "(not linked)", "a1", "(not linked)")
  ))
  expect_equal(caught$row_class, c("sev-error", "sev-error", "sev-error", "sev-warning"))
  expect_equal(caught$row_cap, 500L)
  # A column empty in every row goes (D20.44).
  tables$issues <- issues[-1L]
  section_column_level(tables, NULL, settings)
  expect_false("Expected" %in% names(caught$dt))
  # None: W133 alone.
  tables$issues <- issues[5:6]
  html <- as.character(section_column_level(tables, NULL, settings))
  expect_match(html, html_escape(report_text("report_column_level_empty")), fixed = TRUE)
})

test_that("section_not_evaluated sums records by check and reason over the groups (D20.46)", {
  run_id <- "20261026T120000Z_abc123"
  results <- new_results(list(
    run_id = run_id, run_scope = "package",
    rule_id = c(
      "value_above_max", "value_above_max", "key_incoherent", "column_missing", "pk_duplicate",
      "fk_orphan"
    ),
    rule_fingerprint = "f", stage = "conformance", layer = c(3L, 3L, 4L, 0L, 4L, 4L),
    severity = "error",
    table_name = c("catches", "catches", "catches", "hauls", "hauls", "catches"),
    column_name = c("weight", "weight", "haul_id", "gear", "haul_id", "haul_id"),
    unit = c("u1", "u1", "u1", "*", "u1", "u1"), area = c("a1", "a2", "*", "*", "a1", "a1"),
    n_checked = c(4L, 3L, 0L, 0L, 0L, 5L), n_fail = c(0L, 0L, 0L, 0L, 0L, 1L),
    n_not_evaluated = c(2L, 0L, 0L, 1L, 0L, 1L), n_unchecked = c(1L, 2L, 0L, NA, 3L, 0L),
    reason_code = "*", n_distinct_fail_values = c(0L, 0L, 0L, 0L, 0L, 1L), n_easy_fix = 0L,
    status = c("pass", "pass", "not_run", "not_run", "not_run", "fail"),
    not_run_reason = c(NA, NA, "no_input", "no_table", "none_checked", NA),
    message_id = c(
      "value_above_max", "value_above_max", "key_incoherent", "column_missing", "pk_duplicate",
      "fk_orphan"
    )
  ), group_cols = c("unit", "area"))
  # As route_unit() works it out (D20.62): the records not evaluated that no issue holds.
  data.table::set(results, j = "n_not_evaluated_own", value = c(1L, 0L, 0L, 1L, 0L, 1L))
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:4, run_id = run_id, run_scope = "package",
    rule_id = c("coercion_loss", "value_above_max", "coercion_loss", "value_above_max"),
    rule_fingerprint = "f", stage = "conformance", layer = c(1L, 3L, 1L, 3L),
    severity = "error", reason_code = "*", table_name = "catches",
    column_name = c("weight", "weight", "length", "length"),
    record_pk = c("c1", "c1", "*", "*"), row_number = c(1L, 1L, NA, NA),
    pk_positional = c(FALSE, FALSE, NA, NA), level = c("record", "record", "column", "column"),
    n_records = c(NA, NA, 5L, 5L), unit = "u1", area = "a1", plot = "p1",
    root_issue_id = c(NA, 1L, NA, 3L), status = c("open", "not_evaluated", "open", "not_evaluated"),
    issue_class = "unclassified"
  ), group_cols = c("unit", "area"), key_col = "plot")
  metadata <- lapply(metadata_schema(), empty_table)
  metadata$self_test <- complete_table(list(
    rule_id = c("design_geometry_inconsistent", "subplot_label_ambiguous"),
    stage = "conformance", layer = 4L
  ), metadata_schema()$self_test, "self_test")
  tables <- list(results = results, issues = issues, metadata = metadata, unit = "u1")
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- list()
  testthat::local_mocked_bindings(
    html_table = function(dt, caption, row_cap = NULL, csv_name = NULL, ...) {
      caught <<- list(dt = data.table::copy(dt), caption = caption, row_cap = row_cap)
      "<table>"
    }
  )
  section <- section_not_evaluated(tables, NULL, settings)
  html <- as.character(section)
  expect_equal(attr(section, "gpq_section")[["id"]], "not-evaluated")
  expect_equal(
    attr(section, "gpq_section")[["title"]],
    "Checks not run, and records not evaluated or not reached"
  )
  expect_match(
    html,
    paste(
      "This run is incomplete. These checks reported nothing:",
      "design_geometry_inconsistent, subplot_label_ambiguous."
    ),
    fixed = TRUE
  )
  expect_equal(caught$caption, "By check and reason")
  expect_equal(caught$dt, data.table::data.table(
    Table = c(rep("catches", 6L), "hauls", "hauls"),
    Column = c("haul_id", "haul_id", "length", "weight", "weight", "weight", "gear", "haul_id"),
    Check = c(
      "fk_orphan", "key_incoherent", "value_above_max", "value_above_max", "value_above_max",
      "value_above_max", "column_missing", "pk_duplicate"
    ),
    Reason = c(
      "Not evaluated: this check can't judge the value",
      "Not run: a setting or input it needs wasn't given",
      "Not evaluated: coercion_loss failed on the same value first",
      "Not evaluated: coercion_loss failed on the same value first",
      "Not evaluated: this check can't judge the value",
      "Not reached through the keys",
      "Not run: a table it needs wasn't delivered",
      "Not run: no record could be checked"
    ),
    # Summed over the unit's groups (D20.23); a unit rule counts no records (D16.8).
    Records = c(1L, 0L, 5L, 1L, 1L, 3L, NA, 3L)
  ))
  # D20.91: capped by the section itself, its line naming no file. The rows with the most
  # records stay, shown in the section's order: 5, 3 and 3, not the first three rows.
  settings$row_cap <- 3L
  html <- as.character(section_not_evaluated(tables, NULL, settings))
  expect_equal(caught$dt$Check, c("value_above_max", "value_above_max", "pk_duplicate"))
  expect_equal(caught$dt$Records, c(5L, 3L, 3L))
  expect_null(caught$row_cap)
  expect_match(html, "<p>Showing 3 of 8 rows.</p>", fixed = TRUE)
  # A unit rule's row, which counts no records, ranks as 0, and rows of as many records rank in
  # the section's order (D20.94, R73): the 0 row comes first, so it stays.
  settings$row_cap <- 7L
  section_not_evaluated(tables, NULL, settings)
  expect_false("column_missing" %in% caught$dt$Check)
  expect_equal(caught$dt$Records, c(1L, 0L, 5L, 1L, 1L, 3L, 3L))
  # Nothing to list: W159, unless the self-test lists rules.
  tables$results <- results[results$status != "not_run"]
  data.table::set(tables$results, j = c("n_unchecked", "n_not_evaluated_own"), value = 0L)
  tables$issues <- issues[issues$status == "open"]
  html <- as.character(section_not_evaluated(tables, NULL, settings))
  expect_match(html, "This run is incomplete.", fixed = TRUE)
  expect_false(grepl("Every check ran", html, fixed = TRUE))
  tables$metadata$self_test <- empty_table(metadata_schema()$self_test)
  html <- as.character(section_not_evaluated(tables, NULL, settings))
  expect_match(html, "<p>Every check ran and evaluated every record.</p>", fixed = TRUE)
})

test_that("every section builds for a unit with no results row and no issue", {
  schema <- metadata_schema()
  metadata <- lapply(schema, empty_table)
  metadata$run <- complete_table(list(
    run_id = "20261026T120000Z_abc123", run_start = "2026-10-26T12:00:00Z",
    run_end = "2026-10-26T12:00:05Z", package_version = "0.0.0.9024", r_version = "4.5.2",
    platform = "x86_64-w64-mingw32", data_table_version = "1.18.0", outcome = "completed"
  ), schema$run, "run")
  results <- new_results(group_cols = c("unit", "area"))
  data.table::set(results, j = "n_not_evaluated_own", value = integer())
  tables <- list(
    results = results, issues = new_issues(group_cols = c("unit", "area"), key_col = "plot"),
    corrections = new_corrections(), metadata = metadata, unit = "u1",
    group_cols = c("unit", "area"), key_col = "plot", lineage = NULL
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  builders <- list(
    section_header, section_glance, section_actions, section_harmonization,
    section_under_review, section_column_level, section_not_evaluated, section_guide
  )
  html <- vapply(builders, function(build) as.character(build(tables, NULL, settings)), "")
  expect_match(html[[2L]], "Open issues: 0.", fixed = TRUE)
  expect_match(html[[3L]], "No issues for you to act on.", fixed = TRUE)
  expect_match(html[[7L]], "Every check ran and evaluated every record.", fixed = TRUE)
  expect_false(any(grepl("\\{[a-z0-9_]+\\}", html)))
})

test_that("every not-run reason the results schema allows has a section 10 text (D20.53)", {
  # The reasons as validate_schema() lists them, read from its code, so a reason added there
  # without a text fails here.
  code <- paste(deparse(body(validate_schema)), collapse = " ")
  listed <- regmatches(code, regexpr("not_run_reason = c\\([^)]*\\)", code))
  allowed <- eval(parse(text = sub("^not_run_reason = ", "", listed)))
  expect_gte(length(allowed), 13L)
  results <- new_results(list(
    run_id = "20261026T120000Z_abc123", run_scope = "package", rule_id = "value_above_max",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    table_name = "catches", column_name = paste0("c", seq_along(allowed)), unit = "u1",
    n_checked = 0L, n_fail = 0L, n_not_evaluated = 0L, n_unchecked = 0L, reason_code = "*",
    n_distinct_fail_values = 0L, n_easy_fix = 0L, status = "not_run",
    not_run_reason = allowed, message_id = "value_above_max"
  ), group_cols = "unit")
  data.table::set(results, j = "n_not_evaluated_own", value = 0L)
  tables <- list(
    results = results,
    issues = new_issues(group_cols = "unit"),
    metadata = lapply(metadata_schema(), empty_table), unit = "u1"
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- NULL
  testthat::local_mocked_bindings(html_table = function(dt, caption, ...) {
    caught <<- data.table::copy(dt)
    "<table>"
  })
  section_not_evaluated(tables, NULL, settings)
  expect_equal(nrow(caught), length(allowed))
  expect_setequal(
    caught$Reason,
    vapply(paste0("report_reason_", allowed), report_text, "", USE.NAMES = FALSE)
  )
  expect_true(all(startsWith(caught$Reason, "Not run: ")))
})

test_that("two units' sections: fixed ids, one section per issue, escaped, each its own (D20.28)", {
  run_id <- "20261026T120000Z_abc123"
  hostile <- fx_hostile_text()
  # Each issue's value is its own marker, holding markup.
  values <- sprintf("<v%d>&\"", 1:9)
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:9, run_id = run_id, run_scope = "package",
    rule_id = c(
      "code_not_in_list", "code_not_in_list", "fk_orphan", "raw_na", "column_missing",
      "value_above_max", "coercion_loss", "value_above_max", "code_not_in_list"
    ),
    rule_fingerprint = "f", stage = "conformance", layer = c(3L, 3L, 4L, 2L, 0L, 3L, 1L, 3L, 3L),
    severity = c(
      "error", "error", "error", "warning", "error", "warning", "error", "warning", "error"
    ),
    reason_code = "*",
    table_name = c(
      "catches", "catches", "catches", "hauls", "hauls", "catches", "catches", "catches",
      "catches"
    ),
    column_name = c(
      "species", "species", "haul_id", "minutes", "gear", "weight", "length", "length",
      "species"
    ),
    record_pk = c("c<1>", "c2", "*", "h1", "*", "c3", "*", "c4", "c9"),
    row_number = c(1L, 2L, NA, 1L, NA, 3L, NA, 4L, 9L),
    pk_positional = c(FALSE, FALSE, NA, FALSE, NA, FALSE, NA, FALSE, FALSE),
    level = c(
      "record", "record", "column", "record", "column", "record", "column", "record", "record"
    ),
    n_records = c(NA, NA, 12L, NA, 0L, NA, 5L, NA, NA), value = values,
    expected = c(hostile[[3L]], hostile[[4L]], rep(NA, 7L)),
    provider_key = c("<script>&", "plot=2", rep(NA, 7L)),
    provider_key_status = c("raw", "decoded", rep(NA, 7L)),
    unit = c("u1", "u1", "u1", "u1", "*", "u1", "u1", "u1", "u2"),
    area = c(hostile[[5L]], "a2", "a1", "a2", "*", "*", "a1", "a1", "b1"),
    plot = c("<p&1>", "p2", "*", "p3", "*", "*", "*", "p4", "p9"),
    root_issue_id = c(rep(NA, 7L), 7L, NA),
    status = c(rep("open", 7L), "not_evaluated", "open"),
    issue_class = c(
      "source", "source", "source", "harmonization", "harmonization", "unclassified",
      "unclassified", "unclassified", "source"
    )
  ), group_cols = c("unit", "area"), key_col = "plot")
  # A results row's examples and a group value of another unit, never shown (D20.23).
  results <- new_results(list(
    run_id = run_id, run_scope = "package",
    rule_id = c(
      "column_missing", "coercion_loss", "raw_na", "code_not_in_list", "value_above_max",
      "value_above_max", "fk_orphan", "key_incoherent", "code_not_in_list"
    ),
    rule_fingerprint = "f", stage = "conformance", layer = c(0L, 1L, 2L, 3L, 3L, 3L, 4L, 4L, 3L),
    severity = c(
      "error", "error", "warning", "error", "warning", "warning", "error", "error", "error"
    ),
    table_name = c(
      "hauls", "catches", "hauls", "catches", "catches", "catches", "catches", "catches",
      "catches"
    ),
    column_name = c(
      "gear", "length", "minutes", "species", "weight", "length", "haul_id", "haul_id",
      "species"
    ),
    unit = c("*", "u1", "u1", "u1", "u1", "u1", "u1", "*", "u2"),
    area = c("*", "a1", "a2", "zz-other", "*", "a1", "a1", "*", "b1"),
    n_checked = c(1L, 5L, 3L, 4L, 4L, 4L, 12L, 0L, 3L),
    n_fail = c(1L, 5L, 1L, 2L, 1L, 0L, 12L, 0L, 1L),
    n_not_evaluated = c(0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L),
    n_unchecked = c(NA, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L), reason_code = "*",
    n_distinct_fail_values = c(1L, 1L, 1L, 2L, 1L, 0L, 1L, 0L, 1L), n_easy_fix = 0L,
    examples = c(NA, NA, NA, "secret-example", NA, NA, NA, NA, values[[9L]]),
    status = c("fail", "fail", "fail", "fail", "fail", "pass", "fail", "not_run", "fail"),
    not_run_reason = c(rep(NA, 7L), "no_input", NA),
    message_id = c(
      "column_missing", "coercion_loss", "raw_na", "code_not_in_list", "value_above_max",
      "value_above_max", "fk_orphan", "key_incoherent", "code_not_in_list"
    )
  ), group_cols = c("unit", "area"))
  schema <- metadata_schema()
  metadata <- lapply(schema, empty_table)
  metadata$run <- complete_table(list(
    run_id = run_id, run_start = "2026-10-26T12:00:00Z", run_end = "2026-10-26T12:00:05Z",
    package_version = "0.0.0.9024", r_version = "4.5.2", platform = "x86_64-w64-mingw32",
    data_table_version = "1.18.0", group_cols = "unit; area", outcome = "completed"
  ), schema$run, "run")
  metadata$inputs <- complete_table(
    list(input = "tables:catches", file = "in memory", detail = "9 rows"), schema$inputs, "inputs"
  )
  run <- list(
    results = results, issues = issues, corrections = new_corrections(), metadata = metadata
  )
  lineage <- data.table::data.table(
    table_name = "catches", record_pk = c("c<1>", "c9"), column_name = "species",
    raw_value = c("<raw1>&\"'", "<raw9>&"), raw_source = "x"
  )
  builders <- list(
    section_header, section_glance, section_actions, section_harmonization,
    section_under_review, section_column_level, section_not_evaluated, section_guide
  )
  # One unit's sections as gpq_build_reports() builds them from route_unit()'s tables (task 7),
  # the lineage rows filtered there (D20.61, D20.65).
  report <- function(unit, text = NULL, lang = "en") {
    tables <- c(route_unit(run, unit, c("u1", "u2"), lineage), list(
      metadata = metadata, unit = unit, group_cols = c("unit", "area"), key_col = "plot"
    ))
    settings <- list(
      lang = lang, row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
      csv_name = paste0(unit, "_issues.csv")
    )
    lapply(builders, function(build) build(tables, text, settings))
  }
  mine <- report("u1")
  # D20.30: each builder's id, fixed and unique.
  ids <- vapply(mine, function(section) attr(section, "gpq_section")[["id"]], "")
  expect_equal(ids, c(
    "header", "glance", "actions", "harmonization", "under-review", "column-level",
    "not-evaluated", "guide"
  ))
  html <- vapply(mine, as.character, "")
  # D20.28: each open issue of the unit in exactly one section; the not-evaluated one counted
  # in section 10, its value in none.
  holding <- vapply(values, function(value) sum(grepl(html_escape(value), html, fixed = TRUE)), 1L)
  expect_equal(unname(holding), c(rep(1L, 7L), 0L, 0L))
  expect_match(
    html[[7L]], "Not evaluated: coercion_loss failed on the same value first",
    fixed = TRUE
  )
  # 11.1: nothing reaches the page unescaped; the hostile text shows escaped.
  expect_false(any(grepl("<v[0-9]|<script>|<raw|<p&1>|c<1>", html)))
  expect_match(html[[3L]], html_escape("<raw1>&\"'"), fixed = TRUE)
  expect_match(html[[3L]], "&lt;script&gt;&amp;", fixed = TRUE)
  expect_match(html[[3L]], html_escape(hostile[[3L]]), fixed = TRUE)
  expect_match(html[[2L]], hostile[[5L]], fixed = TRUE)
  # Every slot filled (the text rows' slots).
  expect_false(any(grepl("\\{[a-z0-9_]+\\}", html)))
  # D20.23: no results row's examples or group values.
  expect_false(any(grepl("secret-example|zz-other", html)))
  # D20.17: a raw value only in its own unit's report.
  theirs <- vapply(report("u2"), as.character, "")
  expect_false(any(grepl("raw1", theirs, fixed = TRUE)))
  expect_match(theirs[[3L]], html_escape("<raw9>&"), fixed = TRUE)
  expect_false(any(grepl("raw9", html, fixed = TRUE)))
  expect_false(any(grepl(html_escape(values[[1L]]), theirs, fixed = TRUE)))
  # The caller's text in the run's language reaches the sections (D14.7, D14.30).
  text <- validate_text_table(data.frame(
    text_id = "report_section_guide", lang = "fr", text = "Guide <b>"
  ))
  french <- report("u1", text, "fr")
  expect_equal(attr(french[[8L]], "gpq_section")[["title"]], "Guide <b>")
  expect_match(as.character(french[[8L]]), "<h2>Guide &lt;b&gt;</h2>", fixed = TRUE)
})

test_that("the tables and charts sort text marked latin1 as its UTF-8 text, in C order (D20.42)", {
  run_id <- "20261026T120000Z_abc123"
  # One text marked UTF-8 and one marked latin1: read as UTF-8, "é" comes before "ü" in C
  # order, though its latin1 byte is the higher, so a sort on the bytes as marked puts it last.
  marked <- c("ü", iconv("é", "UTF-8", "latin1"))
  expect_identical(Encoding(marked), c("UTF-8", "latin1"))
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- NULL
  testthat::local_mocked_bindings(html_table = function(dt, caption, ...) {
    caught <<- data.table::copy(dt)
    "<table>"
  })
  # Record-level issues alike but for their columns, provider keys or record IDs.
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:6, run_id = run_id, run_scope = "package", rule_id = "code_not_in_list",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    reason_code = "*", table_name = "catches", column_name = c(marked, rep("species", 4L)),
    record_pk = c("r1", "r2", "r3", "r4", marked), row_number = 1:6, pk_positional = FALSE,
    level = "record", value = "x", provider_key = c("k", "k", marked, NA, NA),
    provider_key_status = c("raw", "raw", "raw", "raw", NA, NA), unit = "u1", status = "open",
    issue_class = "source"
  ), group_cols = "unit")
  # "By check": the four on species first, as the most issues, then "é" before "ü".
  issue_summary(issues, NULL, settings)
  expect_equal(caught$Column, c("species", "é", "ü"))
  # "Each issue": by column, then provider key, then record ID, each in C order.
  issue_records(issues, rep(NA_character_, 6L), NULL, settings)
  expect_equal(caught$`Record ID`, c("r4", "r3", "é", "ü", "r2", "r1"))
  # Section 6: column-level issues alike but for their columns.
  whole <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:2, run_id = run_id, run_scope = "package", rule_id = "value_above_max",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    reason_code = "*", table_name = "catches", column_name = marked, record_pk = "*",
    row_number = NA_integer_, pk_positional = NA, level = "column", n_records = 12L,
    unit = "u1", status = "open", issue_class = "source"
  ), group_cols = "unit")
  section_column_level(list(issues = whole, group_cols = "unit"), NULL, settings)
  expect_equal(caught$Column, c("é", "ü"))
  # Section 10: checks not run alike but for their columns.
  results <- new_results(list(
    run_id = run_id, run_scope = "package", rule_id = "value_above_max",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    table_name = "catches", column_name = marked, unit = "u1", n_checked = 0L, n_fail = 0L,
    n_not_evaluated = 0L, n_unchecked = 2L, reason_code = "*", n_distinct_fail_values = 0L,
    n_easy_fix = 0L, status = "not_run", not_run_reason = "no_input",
    message_id = "value_above_max"
  ), group_cols = "unit")
  data.table::set(results, j = "n_not_evaluated_own", value = 0L)
  tables <- list(
    results = results, issues = new_issues(group_cols = "unit"),
    metadata = lapply(metadata_schema(), empty_table), unit = "u1"
  )
  # With data.table's optimisation off, so the sort is base R's order() and not data.table's
  # own, which reads text as UTF-8 by itself.
  withr::with_options(
    list(datatable.optimize = 0L),
    section_not_evaluated(tables, NULL, settings)
  )
  expect_equal(caught$Column, c("é", "ü"))
  # Section 2's chart by table, its values of as many issues by value, the same way.
  bars <- NULL
  testthat::local_mocked_bindings(html_bar = function(value, max, label, bar_class = NULL) {
    bars <<- label
    "<bars>"
  })
  data.table::set(issues, j = "table_name", value = c(marked, rep("catches", 4L)))
  glance <- list(results = new_results(group_cols = "unit"), issues = issues, group_cols = "unit")
  withr::with_options(list(datatable.optimize = 0L), section_glance(glance, NULL, settings))
  expect_equal(bars, c("catches", "é", "ü"))
})

test_that("section 3's raw values: one-record keys only, a column raw_value kept (D20.123)", {
  run_id <- "20261026T120000Z_abc123"
  # A group column named raw_value, as a run accepts; the third record's key is positional.
  issues <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1:3, run_id = run_id, run_scope = "package", rule_id = "code_not_in_list",
    rule_fingerprint = "f", stage = "conformance", layer = 3L, severity = "error",
    reason_code = "*", table_name = "catches", column_name = "species",
    record_pk = c("c1", "c2", "c3"), row_number = 1:3, pk_positional = c(FALSE, FALSE, TRUE),
    level = "record", value = c("x", "y", "z"), unit = "u1", raw_value = c("g1", "g2", "g3"),
    plot = c("p1", "p2", "p3"), status = "open", issue_class = "source"
  ), group_cols = c("unit", "raw_value"), key_col = "plot")
  # A lineage row on the first record's cell and one on the positional record's (D20.117).
  lineage <- data.table::data.table(
    table_name = "catches", record_pk = c("c1", "c3"), column_name = "species",
    raw_value = c("X1", "X3"), raw_source = "x"
  )
  tables <- list(
    issues = issues, unit = "u1", group_cols = c("unit", "raw_value"), key_col = "plot",
    lineage = lineage
  )
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u1_issues.csv"
  )
  caught <- list()
  testthat::local_mocked_bindings(html_table = function(dt, caption, ...) {
    caught[[caption]] <<- data.table::copy(dt)
    "<table>"
  })
  section_actions(tables, NULL, settings)
  shown <- caught[["Each issue"]]
  # The raw value only on the record whose key names one; the group column in its own place.
  expect_named(shown, c(
    "Severity", "Check", "Table", "Column", "Record ID", "Value",
    "Your value before harmonization", "raw_value", "plot"
  ))
  expect_equal(shown$`Your value before harmonization`, c("X1", NA, NA))
  expect_equal(shown$raw_value, c("g1", "g2", "g3"))
  # With no lineage, no raw value to show, and the group column still there.
  tables$lineage <- NULL
  section_actions(tables, NULL, settings)
  shown <- caught[["Each issue"]]
  expect_named(shown, c(
    "Severity", "Check", "Table", "Column", "Record ID", "Value", "raw_value", "plot"
  ))
  expect_equal(shown$raw_value, c("g1", "g2", "g3"))
})

test_that("section 6 shows no count for a unit rule's \"*\" issue, any unit (D20.122, D20.124)", {
  local_results_registry()
  x <- fx_report_results()
  # whitespace_padding's column-level issue of 110's, unmarked here, so it reaches 110.
  data.table::set(x$issues, i = 17L, j = "reaches_other_unit", value = FALSE)
  units <- c("100", "110")
  settings <- list(
    lang = "en", row_cap = 500L, example_cap = 5L, cell_cap = 100L, column_level_min_n = 10L,
    csv_name = "u_issues.csv"
  )
  caught <- NULL
  testthat::local_mocked_bindings(html_table = function(dt, caption, ...) {
    caught <<- data.table::copy(dt)
    "<table>"
  })
  shown <- lapply(units, function(unit) {
    tables <- c(
      route_unit(x, unit, units),
      list(unit = unit, group_cols = c("unit", "area"), key_col = "plot")
    )
    section_column_level(tables, NULL, settings)
    caught
  })
  # In each unit, the unit rules' "*" issues (table_missing's and class_mismatch's) with none;
  # the unit's own undeclared_code_set issue (D20.124) and a record rule's column-level issue
  # with their counts: 2 and blank_string's 3 in 100, 1 and whitespace_padding's 4 in 110.
  star <- c("table_missing", "class_mismatch")
  counts <- list(
    c(undeclared_code_set = 2L, blank_string = 3L),
    c(undeclared_code_set = 1L, whitespace_padding = 4L)
  )
  for (k in 1:2) {
    expect_true("class_mismatch" %in% shown[[k]]$Check)
    expect_true(all(is.na(shown[[k]]$Records[shown[[k]]$Check %in% star])))
    expect_equal(
      shown[[k]]$Records[match(names(counts[[k]]), shown[[k]]$Check)], unname(counts[[k]])
    )
  }
  # A run with no group columns has no "*" unit: a unit rule's issue keeps its count.
  alone <- new_issues(list(
    reaches_other_unit = FALSE,
    issue_id = 1L, run_id = "20261026T120000Z_abc123", run_scope = "package",
    rule_id = "class_mismatch", rule_fingerprint = "f", stage = "conformance", layer = 1L,
    severity = "error", reason_code = "*", table_name = "catches", column_name = "fish_count",
    record_pk = "*", row_number = NA_integer_, pk_positional = NA, level = "column",
    n_records = 9L, status = "open", issue_class = "harmonization"
  ))
  section_column_level(list(issues = alone, group_cols = character()), NULL, settings)
  expect_equal(caught$Records, 9L)
})
