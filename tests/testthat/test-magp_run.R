# Tests for magp_run_qc(), MAGPlot's run end to end; and plan 13's confidentiality test end to
# end, on a planted copy of magp_example run from its path (plan 3.2, 3.4, 9.5, 11.9, 13; D7.34,
# D18.89, D21.3 to D21.5, D21.11 to D21.15, D21.18, D21.41, D21.42, D21.69). Synthetic data only:
# magp_example and copies of it, saved as a delivery's .Rdata by fx_magp_rdata() where a test
# gives a path. A run on the whole of magp_example costs about 3 s, so one delivery run carries
# most of the checks, the confidentiality test's among them (D20.3, D21.8, D21.37).

# An inputs row's detail for each table of a list: its rows, as the engine writes a table's.
rows_text <- function(tables) {
  n <- vapply(tables, nrow, integer(1), USE.NAMES = FALSE)
  paste(n, ifelse(n == 1L, "row", "rows"))
}

test_that("magp_run_qc() stops on a caller's error before it loads or writes anything (D21.11)", {
  output <- withr::local_tempdir()
  path <- fx_magp_rdata()
  # A path is loaded only once the arguments pass.
  testthat::local_mocked_bindings(magp_read_delivery = function(path) stop("loaded"))
  # CSV paths, as a list's elements or as a vector of paths, are refused (D21.11).
  csv <- "holds more than one path, or a list with a path or other text in place of a table"
  expect_error(magp_run_qc(list(magp_plots = "magp_plots.csv"), output), csv)
  expect_error(
    magp_run_qc(c(magp_plots = "magp_plots.csv", magp_trees = "magp_trees.csv"), output), csv
  )
  # magp_meta given as text isn't taken for a CSV path (D21.12), nor, as text or a list, for an
  # element that isn't a data.frame (M7b m8): the settings stop the run next.
  for (meta in list("built 2026-10-01", list(note = "made up"))) {
    expect_error(
      magp_run_qc(list(magp_meta = meta), output, settings = list(no_such = 1)),
      "settings the package doesn't have"
    )
  }
  shape <- "`tables` must be magp_read_delivery\\(\\)'s result"
  # A list of tables with no names is refused too, before pre-flight writes anything.
  for (bad in list(42, magp_example$magp_plots, character(), NULL, unname(magp_example))) {
    expect_error(magp_run_qc(bad, output), shape)
  }
  # So is a list holding an element that isn't a data.frame, but for magp_meta (M7b m8): a
  # number beside a table, a table's columns as a plain list, and NULL.
  not_frames <- list(
    list(magp_plots = magp_example$magp_plots, magp_sites = 42),
    list(magp_plots = as.list(magp_example$magp_plots)),
    list(magp_plots = NULL)
  )
  for (bad in not_frames) {
    expect_error(magp_run_qc(bad, output), shape)
  }
  a_file <- withr::local_tempfile(lines = "not a folder")
  for (bad in list(c(output, output), NA_character_, " ", a_file, 1)) {
    expect_error(magp_run_qc(path, bad), "`output_dir` must be one folder, not an existing file")
  }
  for (bad in list(NA, "yes", c(TRUE, FALSE), 1)) {
    expect_error(magp_run_qc(path, output, report = bad), "`report` must be TRUE or FALSE")
  }
  # The settings, in the engine's words, before the delivery is loaded.
  expect_error(magp_run_qc(path, output, settings = "en"), "`settings` must be a list")
  expect_error(
    magp_run_qc(path, output, settings = list(no_such = 1)), "settings the package doesn't have"
  )
  expect_error(magp_run_qc(path, output, settings = list(row_cap = -1)), "row_cap")
  # overrides, lineage and pipeline_fill: one that is neither a data.frame nor the path of an
  # existing CSV file stops before the delivery is loaded, in the words pre-flight gives it (M7b
  # m7).
  words <- list()
  for (input in c("overrides", "lineage", "pipeline_fill")) {
    words[[input]] <- tryCatch(
      gpq_preflight(magp_spec(), magp_rules(), inputs = structure(list(42), names = input)),
      error = conditionMessage
    )
    given <- list(path, output)
    given[[input]] <- 42
    expect_error(do.call(magp_run_qc, given), words[[input]], fixed = TRUE, info = input)
  }
  expect_match(
    words$overrides, "`overrides` must be a data.frame or the path of an existing .csv file",
    fixed = TRUE
  )
  # A path is taken only as an existing CSV file's: a file that isn't a CSV, a CSV path with no
  # file.
  for (bad in list(a_file, file.path(output, "absent.csv"))) {
    expect_error(magp_run_qc(path, output, overrides = bad), words$overrides, fixed = TRUE)
  }
  expect_equal(list.files(output, recursive = TRUE, all.files = TRUE, no.. = TRUE), character())
})

test_that("a run pre-flight stops writes its run, with its tables' contributors (D21.18 Q3)", {
  tables <- fx_mark_fixture(lapply(magp_example, data.table::copy))
  # The three tables holding the contributor: a new contributor, 120, beside what a run leaves
  # out of its units: a blank, NA, "*" and the codes "X" and "Z" (D21.16, D21.20).
  data.table::set(tables$magp_contributors, i = 2L, j = "magp_contributor_id", value = "X")
  data.table::set(tables$magp_datasets, i = 1L, j = "magp_contributor_id", value = " ")
  data.table::set(
    tables$magp_designs,
    j = "magp_contributor_id", value = c("120", NA, "*", "Z")
  )
  path <- fx_magp_rdata(tables)
  output <- withr::local_tempdir()
  # A severity naming no registered rule stops pre-flight (rule_id_unknown).
  stopped <- tryCatch(
    magp_run_qc(path, output, settings = list(severity = c(no_such_rule = "error"))),
    gpq_preflight_error = function(e) e
  )
  expect_s3_class(stopped, "gpq_preflight_error")
  expect_true("rule_id_unknown" %in% stopped$preflight$rule_id[stopped$preflight$outcome == "stop"])
  run_dir <- list.dirs(output, recursive = FALSE)
  expect_length(run_dir, 1L)
  # Only pre-flight's two files and the stopped run's two (11.9; D7.11).
  expect_equal(
    sort(list.files(run_dir, recursive = TRUE), method = "radix"),
    c(
      "metadata/preflight.csv", "metadata/run.csv", "metadata/spec_manifest.csv",
      "reports/preflight.html"
    )
  )
  run <- read_run_csv(file.path(run_dir, "metadata", "run.csv"), metadata_schema()$run)
  expect_equal(run$run_id, basename(run_dir))
  expect_equal(run$outcome, "stopped_preflight")
  expect_equal(run$report_units, "100; 110; 120")
  expect_equal(run$group_cols, "magp_contributor_id; jurisdiction; magp_dataset_id")
  # The run ID names the run's start (D21.14).
  expect_equal(gsub("[-:]", "", run$run_start), sub("_.*$", "", run$run_id))
  expect_true(run$run_start <= run$run_end)
  manifest <- file.path(run_dir, "metadata", "spec_manifest.csv")
  expect_identical(
    readBin(manifest, "raw", file.size(manifest)), compiled_bytes(magp_spec()$manifest)
  )
})

test_that("a delivery's run: its files, each contributor's kept to its own records (13; D21.13)", {
  # One run serves the delivery's checks (its folder, inputs, times, keys, classes and reports)
  # and plan 13's confidentiality test, both tests' plants and inputs on one copy (D21.37).
  tables <- fx_mark_fixture(lapply(magp_example, data.table::copy))
  # The delivery's open issues: an unparseable date in a column the pipeline computes, on 100's
  # site, and a code no list holds, on 110's plot measurement.
  data.table::set(tables$magp_sites, i = 1L, j = "estab_date", value = "not a date")
  data.table::set(tables$magp_plot_meas, i = 20L, j = "canopy_structure", value = "QQ")
  # Plan 13's test end to end (D16.16, D18.89, D20.2 (d), D21.18 Q7): magp_example's two
  # contributors, 100 and 110, with these plants in a copy saved as separate objects, run from
  # its path. No coordinate is planted: no rule of Layers 0 to 4 carries one, so D8.19 (2)
  # rests on test-report_build.R's hand-made test.
  spec <- magp_spec()
  pk_of <- function(table) {
    spec$keys$attribute_name[spec$keys$table_name == table & spec$keys$key_type == "PK"]
  }
  plant <- function(table, key, column, value) {
    at <- which(tables[[table]][[pk_of(table)]] == key)
    data.table::set(tables[[table]], i = at, j = column, value = value)
  }
  # D18.89's (A): a tree of 100's whose subplot is 110's, filed under 100 with sides read from
  # 110's site and plot.
  plant("magp_trees", "magp_trees_1", "magp_subplot_id", "magp_subplots_17")
  # (B): a plot measurement of 100's given 110's design, filed under 110 with 100's dataset and
  # plot. Rules fire on it as it stands, so no defect is planted on it.
  plant("magp_plot_meas", "magp_plot_meas_1", "magp_design_id", "magp_designs_3")
  # An issue of each unit's own: a slope over the dictionary's maximum on 100's plot 7, an
  # aspect over it on 110's plot 14.
  plant("magp_plots", "magp_plots_7", "plot_slope", 1234.5)
  plant("magp_plots", "magp_plots_14", "plot_aspect", 456.5)
  # A code no code list holds, on 100.
  plant("magp_plot_meas", "magp_plot_meas_12", "canopy_structure", "QX9")
  # A species code of 110's that no list holds, its raw value given in `lineage`, classed
  # source by an override scoped to its rule, table, column and contributor alone: 110's own
  # issue above is another rule's.
  plant("magp_tree_meas", "magp_tree_meas_100", "species", "QZ7")
  # A record with no unit: a tree condition naming a measurement no record holds.
  plant("magp_tree_cond", "magp_tree_cond_200", "magp_treemeas_id", "magp_tree_meas_999")
  # A made-up magp_meta (D21.12): recorded in the inputs, never checked. `$<-` keeps the
  # list's fixture marker, which c() would drop.
  meta <- data.frame(built = "2026-10-01T00:00:00Z", commit = "0123abc")
  tables$magp_meta <- meta
  path <- fx_magp_rdata(tables)
  delivery <- magp_read_delivery(path)
  output <- withr::local_tempdir()
  # What the run hands on to decoding, the classes and the reports (D21.3, D21.5, D21.18 Q5
  # and Q6).
  caught <- list()
  decode <- gpq_decode_lineage
  classify <- gpq_classify_issues
  build <- gpq_build_reports
  testthat::local_mocked_bindings(
    gpq_decode_lineage = function(issues, tables, spec, key_col, table_map = NULL,
                                  flags = NULL) {
      caught$decoded <<- list(key_col = key_col, flags = flags, tables = names(tables))
      decode(issues, tables, spec, key_col, table_map, flags)
    },
    gpq_classify_issues = function(issues, rules, evidence = list(), overrides = NULL,
                                   settings = list()) {
      # The run's own reference to the tables is gone by now (D21.18 Q5).
      caught$tables_held <<- exists("tables", envir = parent.frame(), inherits = FALSE)
      caught$evidence <<- evidence
      caught$overrides <<- overrides
      caught$settings <<- settings
      on.exit(caught$classified <<- Sys.time())
      classify(issues, rules, evidence, overrides, settings)
    },
    gpq_build_reports = function(results, output_dir, ...) {
      caught$build <<- list(...)
      build(results, output_dir, ...)
    }
  )
  settings <- list(lang = "en", row_cap = 7L, example_cap = 2L, cell_cap = 60L)
  # Your inputs (D21.18 Q6): an override classing 110's code issue source, that code's raw value
  # from the provider, and a cell the pipeline's fill wrote; beside them, the confidentiality
  # test's override and raw value of 110's species code. Each override is scoped to contributor
  # 110, so the first doesn't class the code planted in the same column on 100 (D21.37).
  override <- data.frame(
    fingerprint = NA, rule_id = "code_not_in_list",
    table_name = c("magp_plot_meas", "magp_tree_meas"),
    column_name = c("canopy_structure", "species"), magp_contributor_id = "110",
    issue_class = "source", who = "A. Reviewer", when = "2026-10-29",
    why = c("Checked with the provider.", "Planted for the test.")
  )
  raw <- data.frame(
    table_name = c("magp_plot_meas", "magp_tree_meas"),
    record_pk = c("magp_plot_meas_20", "magp_tree_meas_100"),
    column_name = c("canopy_structure", "species"), raw_value = c("q-raw-110", "RAW-SP-110"),
    raw_source = c("plot_meas.csv", "synthetic")
  )
  fill <- data.frame(
    table_name = "magp_plots", record_pk = "magp_plots_1", column_name = "plot_elevation",
    written_value = "-1"
  )
  started <- Sys.time()
  warned <- list()
  results <- withCallingHandlers(
    magp_run_qc(
      path, output,
      overrides = override, lineage = raw, pipeline_fill = fill, settings = settings
    ),
    gpq_preflight_warning = function(w) {
      warned[[length(warned) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  ended <- Sys.time()
  run <- results$metadata$run
  run_dir <- file.path(output, run$run_id)
  # Pre-flight's warning reaches the caller once, naming the run folder's page and holding the
  # table (D21.30); the checks' own pre-flight's never does.
  expect_length(warned, 1L)
  expect_identical(
    conditionMessage(warned[[1L]]),
    sprintf(
      "Pre-flight found %d warning(s); see %s.", sum(results$metadata$preflight$outcome == "warn"),
      file.path(run_dir, "reports", "preflight.html")
    )
  )
  expect_identical(warned[[1L]]$preflight, results$metadata$preflight)
  # The run folder (11.9): pre-flight's files, the results and metadata, and each
  # contributor's report and issues CSV.
  expect_equal(
    sort(list.files(run_dir, recursive = TRUE), method = "radix"),
    c(
      paste0("metadata/", c(
        "clashes", "inputs", "preflight", "rules", "run", "self_test", "settings",
        "spec_manifest"
      ), ".csv"),
      "reports/100.html", "reports/110.html", "reports/preflight.html",
      "tables/100_issues.csv", "tables/110_issues.csv", "tables/corrections.csv",
      "tables/issues.csv", "tables/results.csv"
    )
  )
  expect_equal(run$outcome, "completed")
  expect_equal(run$report_units, "100; 110")
  # Decoding by contributor, with Ontario's age trees, on the tables without magp_meta (D21.3,
  # D21.12); every issue given its key status.
  expect_equal(caught$decoded$key_col, "magp_contributor_id")
  expect_equal(caught$decoded$flags, age_tree_flags(delivery))
  expect_setequal(caught$decoded$tables, setdiff(names(delivery), "magp_meta"))
  expect_false(anyNA(results$issues$provider_key_status))
  # Classes from your overrides, then MAGPlot's evidence (D21.5) and yours, pipeline_fill as
  # `filled` (D21.18 Q6), the tables let go first (D21.18 Q5).
  expect_false(caught$tables_held)
  expect_named(caught$evidence, c("computed", "crosswalk", "filled", "lineage"))
  expect_identical(caught$evidence$filled, fill)
  expect_identical(caught$evidence$lineage, raw)
  expect_identical(caught$overrides, override)
  expect_equal(caught$evidence$computed, computed_evidence(spec = magp_spec()))
  expect_equal(caught$evidence$crosswalk, crosswalk_evidence(spec = magp_spec()))
  # The settings reach the classes, and the checks, whose settings table records the cap given
  # as set by the argument (D21.18 Q6).
  expect_identical(caught$settings, settings)
  resolved <- results$metadata$settings
  expect_equal(
    resolved[resolved$setting == "row_cap", c("value", "tier")],
    data.table::data.table(value = "7", tier = "argument")
  )
  # The open issues and their classes, in the checks' order: the delivery's two (the date on
  # 100's site, the code on 110's plot measurement) among the confidentiality test's twelve (the
  # code on 100, 110's species code, the slope and aspect, the orphan, (B)'s three and (A)'s
  # four). The overrides class only 110's two codes (D21.37).
  open <- results$issues[results$issues$status == "open"]
  expect_equal(
    open[, c("record_pk", "rule_id", "magp_contributor_id", "issue_class", "class_basis")],
    data.table::data.table(
      record_pk = c(
        "350002", "magp_plot_meas_12", "magp_plot_meas_20", "magp_tree_meas_100", "magp_plots_7",
        "magp_plots_14", "magp_tree_cond_200", "magp_plot_meas_1", "magp_subplot_meas_1",
        "magp_subplot_meas_2", "magp_trees_1", "magp_trees_1", "magp_tree_meas_1",
        "magp_tree_meas_9"
      ),
      rule_id = c(
        "date_unparseable", rep("code_not_in_list", 3L), rep("value_above_max", 2L), "fk_orphan",
        rep("key_incoherent", 7L)
      ),
      magp_contributor_id = c(
        "100", "100", "110", "110", "100", "110", "*", rep("110", 3L), rep("100", 4L)
      ),
      issue_class = c(
        "harmonization", "unclassified", "source", "source", "unclassified", "unclassified",
        rep("harmonization", 8L)
      ),
      class_basis = c("computed", "default", "override", "override", rep("default", 10L))
    )
  )
  # The inputs rows (D21.13): each table's with the delivery's file and hash and the engine's
  # detail, then magp_meta's, with no detail. Then a row for each run input classification was
  # given (D21.41, D21.42), the three given as data.frames and MAGPlot's two evidence tables
  # (as classification got them) in memory, each with the rows classification read from it.
  inputs <- results$metadata$inputs
  given <- delivery[names(delivery) != "magp_meta"]
  run_inputs <- list(
    overrides = override, filled = fill, lineage = raw,
    computed = caught$evidence$computed, crosswalk = caught$evidence$crosswalk
  )
  # The delivery's file and hash on each table's row and magp_meta's, none on the run inputs'.
  from_delivery <- length(given) + 1L
  expect_equal(inputs, data.table::data.table(
    input = c(paste0("tables:", names(given)), "magp_meta", names(run_inputs)),
    file = c(rep(attr(delivery, "source_file"), from_delivery), rep("in memory", 5L)),
    sha256 = c(rep(attr(delivery, "sha256"), from_delivery), rep(NA_character_, 5L)),
    detail = c(rows_text(given), NA, rows_text(run_inputs))
  ))
  expect_no_error(validate_gpq_results(results))
  # The run's own pre-flight, run with MAGPlot's text, is the one kept (D21.18 Q3).
  expect_equal(
    results$metadata$preflight$outcome[results$metadata$preflight$rule_id == "text_slot_unknown"],
    "pass"
  )
  # Returned as written (D21.18 Q4).
  expect_identical(gpq_read_results(run_dir), results)
  # run_start the call's own, which the run ID names; run_end after the classes, before the
  # write (D21.14).
  expect_equal(gsub("[-:]", "", run$run_start), sub("_.*$", "", run$run_id))
  expect_true(utc_text(started) <= run$run_start)
  expect_true(utc_text(caught$classified) <= run$run_end)
  expect_true(run$run_end <= utc_text(ended))
  # The settings' language and caps reach the reports as arguments, with MAGPlot's text and
  # your lineage input (D21.18 Q6).
  expect_equal(caught$build[c("lang", "row_cap", "example_cap", "cell_cap")], settings)
  expect_identical(caught$build$text, magp_texts())
  expect_identical(caught$build$lineage, raw)
  # Each open issue's class in its own contributor's CSV, but for those marked
  # reaches_other_unit, which are in neither contributor's (D18.89). The CSV sorts as the report
  # does, so both sides are put in the records' order.
  for (unit in c("100", "110")) {
    csv <- data.table::fread(
      file.path(run_dir, "tables", paste0(unit, "_issues.csv")),
      encoding = "UTF-8", colClasses = "character"
    )
    shown <- csv[csv$status == "open", c("record_pk", "rule_id", "issue_class")]
    filed <- open[
      open$magp_contributor_id == unit & !open$reaches_other_unit,
      c("record_pk", "rule_id", "issue_class")
    ]
    expect_equal(
      shown[order(shown$record_pk, shown$rule_id)], filed[order(filed$record_pk, filed$rule_id)],
      info = unit
    )
  }
  # Section 1 of each contributor's report names the delivery's file and hash, and lists only
  # the tables, never magp_meta or a run input's row (D21.13, D21.41). The raw value is in its
  # own contributor's report only, beside its issue (D20.17).
  shown_as_table <- function(name) {
    report_text("report_header_table", "en", table = name, text = magp_texts())
  }
  for (unit in c("100", "110")) {
    html <- readLines(
      file.path(run_dir, "reports", paste0(unit, ".html")),
      encoding = "UTF-8", warn = FALSE
    )
    expect_true(any(grepl(attr(delivery, "sha256"), html, fixed = TRUE)), info = unit)
    expect_false(any(grepl("magp_meta", html, fixed = TRUE)), info = unit)
    # A table's row shows as a table, so the same words for a run input would show too.
    expect_true(any(grepl(shown_as_table("magp_plots"), html, fixed = TRUE)), info = unit)
    for (name in names(run_inputs)) {
      expect_false(any(grepl(shown_as_table(name), html, fixed = TRUE)), info = paste(unit, name))
    }
    expect_equal(any(grepl("q-raw-110", html, fixed = TRUE)), unit == "110", info = unit)
  }
  # Plan 13's confidentiality test, on the same run's files (D21.18 Q7, D21.37).
  units <- c("100", "110")
  expect_identical(split_units(results$metadata$run$report_units), units)
  # Each unit's surrogate keys: every record's primary key in the clean example, its unit read
  # through its chain of parents. The unit values themselves are checked apart, below.
  parent <- c(
    magp_design_frames = "magp_designs", magp_sites = "magp_datasets",
    magp_site_dist = "magp_sites", magp_site_treat = "magp_sites",
    magp_site_visits = "magp_sites", magp_plots = "magp_sites", magp_subplots = "magp_plots",
    magp_plot_meas = "magp_plots", magp_subplot_meas = "magp_subplots",
    magp_trees = "magp_subplots", magp_tree_meas = "magp_trees",
    magp_tree_cond = "magp_tree_meas", magp_tree_age = "magp_tree_meas",
    magp_tally_trees = "magp_subplot_meas"
  )
  owner <- function(table) {
    rows <- magp_example[[table]]
    if (!table %in% names(parent)) {
      return(rows[["magp_contributor_id"]])
    }
    up <- parent[[table]]
    key <- pk_of(up)
    owner(up)[match(rows[[key]], magp_example[[up]][[key]])]
  }
  keyed <- c("magp_datasets", "magp_designs", names(parent))
  expect_setequal(c(keyed, "magp_contributors"), names(magp_example))
  key <- unlist(lapply(keyed, function(table) {
    as.character(magp_example[[table]][[pk_of(table)]])
  }))
  key_unit <- unlist(lapply(keyed, owner))
  expect_false(anyNA(key_unit))
  keys <- split(key, key_unit)
  # Whole tokens: runs of letters, digits, "_", "." and "-", split at markup, spaces, "=", ";"
  # and every other character, a "." ending a sentence dropped. A unit's files hold the tokens
  # of its HTML, the whole cells of its CSV and each cell's tokens, so a key inside a cell such
  # as "magp_plot_id=magp_plots_9" counts, and magp_plots_1 never matches inside magp_plots_10
  # (D21.18 Q7).
  tokens <- function(text) {
    found <- unlist(strsplit(text, "[^A-Za-z0-9_.-]+"), use.names = FALSE)
    found <- gsub("^[.]+|[.]+$", "", found)
    unique(found[nzchar(found)])
  }
  seen <- lapply(structure(units, names = units), function(unit) {
    page <- paste(
      readLines(file.path(run_dir, "reports", paste0(unit, ".html")), encoding = "UTF-8"),
      collapse = "\n"
    )
    csv <- data.table::fread(
      file.path(run_dir, "tables", paste0(unit, "_issues.csv")),
      colClasses = "character", encoding = "UTF-8"
    )
    cells <- unlist(csv, use.names = FALSE)
    list(page = page, cells = cells, held = unique(c(tokens(page), tokens(cells), cells)))
  })
  run_issues <- data.table::fread(
    file.path(run_dir, "tables", "issues.csv"),
    colClasses = "character", encoding = "UTF-8"
  )
  # Presence: each own issue's strings are whole cells of its unit's CSV, the unit value among
  # them, and 110's raw value a token of its section 3.
  own <- list(
    "100" = c("magp_plots_7", "magp_plot_meas_12"),
    "110" = c("magp_plots_14", "magp_tree_meas_100")
  )
  carried <- c(
    "record_pk", "value", "expected", "provider_key", "magp_contributor_id", "jurisdiction",
    "magp_dataset_id", "magp_plot_id", "fingerprint"
  )
  for (unit in units) {
    rows <- run_issues[run_issues$record_pk %chin% own[[unit]]]
    expect_setequal(rows$record_pk, own[[unit]])
    expect_identical(unique(rows$magp_contributor_id), unit)
    expect_identical(unique(rows$reaches_other_unit), "FALSE")
    strings <- unlist(rows[, carried, with = FALSE], use.names = FALSE)
    strings <- strings[!is.na(strings) & nzchar(strings)]
    expect_identical(setdiff(strings, seen[[unit]]$cells), character(), info = unit)
  }
  actions <- regmatches(
    seen[["110"]]$page,
    regexpr("(?s)<section id=\"actions\">.*?</section>", seen[["110"]]$page, perl = TRUE)
  )
  expect_true("RAW-SP-110" %chin% tokens(actions))
  # Absence: neither of a unit's files holds the other unit's surrogate keys or planted values
  # (codes, slope or aspect, date text, raw value); the unit values as whole CSV cells only,
  # since a count in the HTML can read as one. The keys and values are looked for as whole HTML
  # tokens, whole CSV cells and CSV cells' tokens. The tokens split at spaces, so a value with
  # spaces ("not a date") is also looked for as a fixed substring of the HTML text and of each
  # CSV cell (D21.50).
  planted <- list(
    "100" = c("QX9", "1234.5", "not a date"), "110" = c("QZ7", "RAW-SP-110", "QQ", "456.5")
  )
  for (unit in units) {
    other <- setdiff(units, unit)
    expect_identical(
      intersect(c(keys[[other]], planted[[other]]), seen[[unit]]$held), character(),
      info = unit
    )
    for (phrase in planted[[other]][grepl(" ", planted[[other]], fixed = TRUE)]) {
      expect_false(grepl(phrase, seen[[unit]]$page, fixed = TRUE), info = paste(unit, phrase))
      expect_false(any(grepl(phrase, seen[[unit]]$cells, fixed = TRUE)), info = paste(unit, phrase))
    }
    expect_true(unit %chin% seen[[unit]]$cells)
    expect_false(other %chin% seen[[unit]]$cells)
  }
  # (A)'s and (B)'s issues, their records' measurements' among them, and the "*" record's are
  # in the run's own issues table, (A)'s filed under 100 and (B)'s under 110, both marked
  # reaches_other_unit, the "*" record's under "*"; none of them is in either unit's files.
  measured <- tables$magp_tree_meas$magp_treemeas_id[
    tables$magp_tree_meas$magp_tree_id == "magp_trees_1"
  ]
  parts <- tables$magp_subplot_meas$magp_subpmeas_id[
    tables$magp_subplot_meas$magp_plotmeas_id == "magp_plot_meas_1"
  ]
  a_rows <- run_issues$record_pk %chin% c("magp_trees_1", measured)
  b_rows <- run_issues$record_pk %chin% c("magp_plot_meas_1", parts)
  star_rows <- run_issues$record_pk == "magp_tree_cond_200"
  expect_true(all(c("magp_trees_1", "magp_plot_meas_1") %chin% run_issues$record_pk))
  expect_identical(unique(run_issues$magp_contributor_id[a_rows]), "100")
  expect_identical(unique(run_issues$magp_contributor_id[b_rows]), "110")
  expect_identical(unique(run_issues$reaches_other_unit[a_rows | b_rows]), "TRUE")
  expect_identical(run_issues$magp_contributor_id[star_rows], "*")
  crossing <- a_rows | b_rows | star_rows
  for (unit in units) {
    expect_identical(
      intersect(c(run_issues$record_pk[crossing], "magp_tree_meas_999"), seen[[unit]]$held),
      character(),
      info = unit
    )
    expect_identical(
      intersect(run_issues$fingerprint[crossing], seen[[unit]]$cells), character(),
      info = unit
    )
  }
})

test_that("a plain list's run is recorded in memory; report = FALSE builds no report (D21.13)", {
  # Two tables and a magp_meta of another shape (D21.12); the other tables are missing, which
  # Layer 0 reports.
  tables <- list(
    magp_contributors = data.table::copy(magp_example$magp_contributors),
    magp_datasets = data.table::copy(magp_example$magp_datasets),
    magp_meta = list(note = "made up")
  )
  # Run inputs too (D21.41): two overrides in a CSV file, recorded by the file's name and
  # SHA-256, and a lineage input as a data.frame, in memory; no pipeline_fill, so no `filled`
  # row.
  override <- data.frame(
    fingerprint = NA, rule_id = c("code_not_in_list", NA),
    table_name = c("magp_plot_meas", "magp_plots"), column_name = c("canopy_structure", NA),
    issue_class = "source", who = "A. Reviewer", when = "2026-10-29",
    why = "Checked with the provider."
  )
  overrides_file <- file.path(withr::local_tempdir(), "overrides.csv")
  utils::write.csv(override, overrides_file, row.names = FALSE, na = "")
  raw <- data.frame(
    table_name = "magp_plot_meas", record_pk = "magp_plot_meas_20",
    column_name = "canopy_structure", raw_value = "q", raw_source = "plot_meas.csv"
  )
  output <- withr::local_tempdir()
  results <- withCallingHandlers(
    magp_run_qc(tables, output, overrides = overrides_file, lineage = raw, report = FALSE),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  run_dir <- file.path(output, results$metadata$run$run_id)
  # MAGPlot's own evidence tables are in memory too (D21.42); each row's detail counts the rows
  # of the table as given.
  run_inputs <- list(
    overrides = override, lineage = raw,
    computed = computed_evidence(spec = magp_spec()),
    crosswalk = crosswalk_evidence(spec = magp_spec())
  )
  expect_equal(results$metadata$inputs, data.table::data.table(
    input = c(
      "tables:magp_contributors", "tables:magp_datasets", "magp_meta", names(run_inputs)
    ),
    file = c(rep("in memory", 3L), "overrides.csv", rep("in memory", 3L)),
    sha256 = c(rep(NA_character_, 3L), unname(tools::sha256sum(overrides_file)), rep(NA, 3L)),
    detail = c("2 rows", "4 rows", NA, rows_text(run_inputs))
  ))
  expect_no_error(validate_gpq_results(results))
  expect_identical(gpq_read_results(run_dir), results)
  # The results and metadata only (D21.18 Q4).
  expect_equal(list.files(file.path(run_dir, "reports")), "preflight.html")
  expect_equal(
    sort(list.files(file.path(run_dir, "tables")), method = "radix"),
    c("corrections.csv", "issues.csv", "results.csv")
  )
  # Every issue classed, so gpq_build_reports() can build the saved run's reports later
  # (D21.18 Q4).
  expect_false(anyNA(results$issues$issue_class))
  expect_equal(results$metadata$run$report_units, "100; 110")
  # The run reads its inputs before any check with the issues' group columns as classification
  # reads them from the issues (M7b m31, D22.27): an override with an attribute that is neither
  # an override's column nor a group column gets W229, which names them, from classification on
  # this run's issues and the same from the run, before any check.
  scoped <- cbind(override, magp_site_id = NA)
  from_classes <- tryCatch(
    gpq_classify_issues(results$issues, magp_rules(), overrides = scoped),
    error = conditionMessage
  )
  expect_match(
    from_classes,
    "the issues' group columns (magp_contributor_id, jurisdiction, magp_dataset_id, magp_plot_id)",
    fixed = TRUE
  )
  expect_error(
    withCallingHandlers(
      magp_run_qc(tables, output, overrides = scoped, report = FALSE),
      gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
    ),
    from_classes,
    fixed = TRUE
  )
})

test_that("a run's age-tree flags read each column as the run matches it (D21.69)", {
  # The two tables the flags read, as a plain list: the other tables are missing, which Layer 0
  # reports, and the run is cheaper.
  tables <- lapply(magp_example[c("magp_trees", "magp_tree_meas")], data.table::copy)
  trees <- tables$magp_trees
  meas <- tables$magp_tree_meas
  # One tree is an age tree: every measurement of it has tree_type "A".
  age_tree <- trees$magp_tree_id[[1L]]
  data.table::set(meas, which(meas$magp_tree_id == age_tree), "tree_type", "A")
  # The flags of the tables under the dictionary's names, then the three columns the flags read
  # delivered under names that differ in letter case or spaces, as the checks and decoding
  # match them.
  want <- age_tree_flags(tables)
  data.table::setnames(trees, "magp_tree_id", "Magp_Tree_Id")
  data.table::setnames(meas, c("magp_tree_id", "tree_type"), c("magp_tree_id ", "TREE_TYPE"))
  # Decoding is mocked to take the flags the run hands it and stop the run there, so
  # classification and writing don't run.
  caught <- NULL
  testthat::local_mocked_bindings(
    gpq_decode_lineage = function(issues, tables, spec, key_col, table_map = NULL,
                                  flags = NULL) {
      caught <<- flags
      stop("decoding reached")
    }
  )
  output <- withr::local_tempdir()
  expect_error(
    withCallingHandlers(
      magp_run_qc(tables, output, report = FALSE),
      gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
    ),
    "decoding reached"
  )
  flagged <- caught$magp_trees
  expect_equal(flagged$magp_tree_id[flagged$age_tree == "Y"], age_tree)
  expect_equal(caught, want)
  # A second run: tree_type delivered under a name no attribute matches, even by letter case
  # and spaces, is left out, so no tree is flagged "Y", though the age tree's measurements are
  # all "A" as before (M7b m52); and magp_trees' key under its own name after a near name
  # holding other keys, the first column, the exact name taken (M7b m53).
  tables <- lapply(magp_example[c("magp_trees", "magp_tree_meas")], data.table::copy)
  meas <- tables$magp_tree_meas
  data.table::set(meas, which(meas$magp_tree_id == age_tree), "tree_type", "A")
  data.table::setnames(meas, "tree_type", "tree type")
  trees <- tables$magp_trees
  data.table::set(trees, j = "Magp_Tree_Id", value = paste0(trees$magp_tree_id, "_near"))
  data.table::setcolorder(trees, "Magp_Tree_Id")
  caught <- NULL
  local({
    # The flags read the tables alone, so this run's checks are mocked to give no issues: it
    # goes on to the flags at once, which keeps its CRAN-mode time small (D20.3).
    testthat::local_mocked_bindings(gpq_check_conformance = function(...) list(metadata = list()))
    expect_error(
      withCallingHandlers(
        magp_run_qc(tables, output, report = FALSE),
        gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
      ),
      "decoding reached"
    )
  })
  expect_equal(caught, list(magp_trees = data.table::data.table(
    magp_tree_id = want$magp_trees$magp_tree_id, age_tree = "N"
  )))
})

test_that("inputs that classification can't take stop the run before any check (D21.4)", {
  output <- withr::local_tempdir()
  # A plain list of the tables the inputs name, so nothing is loaded from a file: the stops come
  # before any check reads a table (D21.37).
  tables <- list(
    magp_plot_meas = data.table::copy(magp_example$magp_plot_meas),
    magp_plots = data.table::copy(magp_example$magp_plots)
  )
  # A run that reached the checks would stop with this, so a stop in other words came first.
  # The mock of the package's own load_bit64() records which tables each call is given (D18.86,
  # D19.80); with no integer64 column in these tables the real one would do nothing.
  seen <- list()
  testthat::local_mocked_bindings(
    gpq_check_conformance = function(...) stop("checks reached"),
    load_bit64 = function(tables) {
      seen[[length(seen) + 1L]] <<- names(tables)
      invisible(NULL)
    }
  )
  quiet <- function(expr) {
    withCallingHandlers(expr, gpq_preflight_warning = function(w) invokeRestart("muffleWarning"))
  }
  override <- data.frame(
    fingerprint = NA, rule_id = "code_not_in_list", table_name = "magp_plot_meas",
    column_name = "canopy_structure", issue_class = "source", who = "A. Reviewer",
    when = "2026-10-29", why = "Checked with the provider."
  )
  raw <- data.frame(
    table_name = "magp_plot_meas", record_pk = "magp_plot_meas_20",
    column_name = "canopy_structure", raw_value = "q", raw_source = "plot_meas.csv"
  )
  fill <- data.frame(
    table_name = "magp_plots", record_pk = "magp_plots_1", column_name = "plot_elevation",
    written_value = "-1"
  )
  # W229: an attribute of the specification, which pre-flight takes, that isn't one of the
  # issues' group columns.
  expect_error(
    quiet(magp_run_qc(tables, output, overrides = cbind(override, magp_site_id = NA))),
    "`overrides` must have the columns"
  )
  # The problem unit's key is one of them, as classification reads the issues, so this run, with
  # the two other inputs given too, reads them all and goes on to the checks.
  expect_error(
    quiet(magp_run_qc(
      tables, output,
      overrides = cbind(override, magp_plot_id = "magp_plots_9"), lineage = raw,
      pipeline_fill = fill
    )),
    "checks reached"
  )
  # W246: a cell listed twice, in lineage or in pipeline_fill, which classification names
  # `filled`.
  expect_error(
    quiet(magp_run_qc(tables, output, lineage = rbind(raw, raw))),
    "`lineage` lists the cell magp_plot_meas.canopy_structure of record magp_plot_meas_20 twice",
    fixed = TRUE
  )
  expect_error(
    quiet(magp_run_qc(tables, output, pipeline_fill = rbind(fill, fill))),
    "`filled` lists the cell magp_plots.plot_elevation of record magp_plots_1 twice",
    fixed = TRUE
  )
  # Each run asked load_bit64() about its tables, then about the data.frames among its inputs,
  # as classification does before it reads them: overrides first, then the evidence by name.
  expect_equal(seen, list(
    names(tables), "overrides",
    names(tables), c("overrides", "filled", "lineage"),
    names(tables), "lineage",
    names(tables), "filled"
  ))
  # Each run left pre-flight's files only (D21.15).
  runs <- list.dirs(output, recursive = FALSE)
  expect_length(runs, 4L)
  for (run_dir in runs) {
    expect_equal(
      sort(list.files(run_dir, recursive = TRUE), method = "radix"),
      c("metadata/preflight.csv", "reports/preflight.html")
    )
  }
})
