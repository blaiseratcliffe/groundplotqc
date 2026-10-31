# Tests for magp_run_qc(), MAGPlot's run end to end (plan 3.2, 3.4, 9.5, 11.9; D7.34, D21.3 to
# D21.5, D21.11 to D21.15, D21.18, D21.41, D21.42). Synthetic data only: magp_example and copies
# of it, saved as a delivery's .Rdata by fx_magp_rdata() where a test gives a path. A run on the
# whole of magp_example costs about 3 s, so one delivery run carries most of the checks (D20.3,
# D21.8).

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
  # magp_meta given as text isn't taken for a CSV path (D21.12): the settings stop the run
  # next.
  expect_error(
    magp_run_qc(list(magp_meta = "built 2026-10-01"), output, settings = list(no_such = 1)),
    "settings the package doesn't have"
  )
  shape <- "`tables` must be magp_read_delivery\\(\\)'s result"
  # A list of tables with no names is refused too, before pre-flight writes anything.
  for (bad in list(42, magp_example$magp_plots, character(), NULL, unname(magp_example))) {
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

test_that("a delivery's run: its folder, inputs, times, keys, classes and reports (D21.13)", {
  tables <- fx_mark_fixture(lapply(magp_example, data.table::copy))
  # One open issue for each contributor: an unparseable date in a column the pipeline computes,
  # on 100's site, and a code no list holds, on 110's plot measurement.
  data.table::set(tables$magp_sites, i = 1L, j = "estab_date", value = "not a date")
  data.table::set(tables$magp_plot_meas, i = 20L, j = "canopy_structure", value = "QQ")
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
  # from the provider, and a cell the pipeline's fill wrote.
  override <- data.frame(
    fingerprint = NA, rule_id = "code_not_in_list", table_name = "magp_plot_meas",
    column_name = "canopy_structure", issue_class = "source", who = "A. Reviewer",
    when = "2026-10-29", why = "Checked with the provider."
  )
  raw <- data.frame(
    table_name = "magp_plot_meas", record_pk = "magp_plot_meas_20",
    column_name = "canopy_structure", raw_value = "q-raw-110", raw_source = "plot_meas.csv"
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
  open <- results$issues[results$issues$status == "open"]
  expect_equal(
    open[, c("rule_id", "magp_contributor_id", "issue_class", "class_basis")],
    data.table::data.table(
      rule_id = c("date_unparseable", "code_not_in_list"), magp_contributor_id = c("100", "110"),
      issue_class = c("harmonization", "source"), class_basis = c("computed", "override")
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
  # Each open issue's class in its own contributor's CSV.
  for (unit in c("100", "110")) {
    csv <- data.table::fread(
      file.path(run_dir, "tables", paste0(unit, "_issues.csv")),
      encoding = "UTF-8", colClasses = "character"
    )
    expect_equal(
      csv[csv$status == "open", c("rule_id", "issue_class")],
      open[open$magp_contributor_id == unit, c("rule_id", "issue_class")]
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
