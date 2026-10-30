# Tests for magp_run_qc(), MAGPlot's run end to end (plan 3.2, 3.4, 9.5, 11.9; D7.34, D21.3,
# D21.5, D21.11 to D21.15, D21.18). Synthetic data only: magp_example and copies of it, saved
# as a delivery's .Rdata by fx_magp_rdata() where a test gives a path. A run on the whole of
# magp_example costs about 3 s, so one delivery run carries most of the checks (D20.3, D21.8).

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
  for (bad in list(42, magp_example$magp_plots, character(), NULL)) {
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
      on.exit(caught$classified <<- Sys.time())
      classify(issues, rules, evidence, overrides, settings)
    },
    gpq_build_reports = function(results, output_dir, ...) {
      caught$build <<- list(...)
      build(results, output_dir, ...)
    }
  )
  settings <- list(lang = "en", row_cap = 7L, example_cap = 2L, cell_cap = 60L)
  started <- Sys.time()
  warned <- list()
  results <- withCallingHandlers(
    magp_run_qc(path, output, settings = settings),
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
  expect_match(
    conditionMessage(warned[[1L]]), file.path(run_dir, "reports", "preflight.html"),
    fixed = TRUE
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
  # Classes from MAGPlot's evidence (D21.5), the tables let go first (D21.18 Q5).
  expect_false(caught$tables_held)
  expect_named(caught$evidence, c("computed", "crosswalk"))
  expect_equal(caught$evidence$computed, computed_evidence(spec = magp_spec()))
  expect_equal(caught$evidence$crosswalk, crosswalk_evidence(spec = magp_spec()))
  open <- results$issues[results$issues$status == "open"]
  expect_equal(
    open[, c("rule_id", "magp_contributor_id", "issue_class", "class_basis")],
    data.table::data.table(
      rule_id = c("date_unparseable", "code_not_in_list"), magp_contributor_id = c("100", "110"),
      issue_class = c("harmonization", "unclassified"), class_basis = c("computed", "default")
    )
  )
  # The inputs rows (D21.13): each table's with the delivery's file and hash and the engine's
  # detail, then magp_meta's, with no detail.
  inputs <- results$metadata$inputs
  given <- delivery[names(delivery) != "magp_meta"]
  rows <- vapply(given, nrow, integer(1), USE.NAMES = FALSE)
  expect_equal(inputs, data.table::data.table(
    input = c(paste0("tables:", names(given)), "magp_meta"),
    file = attr(delivery, "source_file"), sha256 = attr(delivery, "sha256"),
    detail = c(paste(rows, ifelse(rows == 1L, "row", "rows")), NA)
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
  # The settings' language and caps reach the reports as arguments, with MAGPlot's text
  # (D21.18 Q6).
  expect_equal(caught$build[c("lang", "row_cap", "example_cap", "cell_cap")], settings)
  expect_identical(caught$build$text, magp_texts())
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
  # Section 1 names the delivery's file and hash, and lists only the tables, never magp_meta
  # (D21.13).
  html <- readLines(file.path(run_dir, "reports", "100.html"), encoding = "UTF-8", warn = FALSE)
  expect_true(any(grepl(attr(delivery, "sha256"), html, fixed = TRUE)))
  expect_false(any(grepl("magp_meta", html, fixed = TRUE)))
})

test_that("a plain list's run is recorded in memory; report = FALSE builds no report (D21.13)", {
  # Two tables and a magp_meta of another shape (D21.12); the other tables are missing, which
  # Layer 0 reports.
  tables <- list(
    magp_contributors = data.table::copy(magp_example$magp_contributors),
    magp_datasets = data.table::copy(magp_example$magp_datasets),
    magp_meta = list(note = "made up")
  )
  output <- withr::local_tempdir()
  results <- withCallingHandlers(
    magp_run_qc(tables, output, report = FALSE),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  run_dir <- file.path(output, results$metadata$run$run_id)
  expect_equal(results$metadata$inputs, data.table::data.table(
    input = c("tables:magp_contributors", "tables:magp_datasets", "magp_meta"),
    file = "in memory", sha256 = NA_character_, detail = c("2 rows", "4 rows", NA)
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
