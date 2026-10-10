# Tests for a run's files: gpq_write_results() and gpq_read_results(), the round trip, the
# files' bytes, a stopped run's files and the refusals (plan 9.1, 9.5, 11.9, 18.4; D4.16,
# D7.11, D15.4, D15.16, D15.17).

test_that("a run written and read back is identical (18.4)", {
  local_results_registry()
  object <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(object, dir)
  expect_equal(run_dir, file.path(dir, "20261008T120000Z_abc123"))
  expect_identical(gpq_read_results(run_dir), object)
})

test_that("the run folder holds tables/ and metadata/, each file once (11.9; D7.11)", {
  local_results_registry()
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(fx_results(), dir)
  expect_setequal(list.files(file.path(run_dir, "tables")), c(
    "results.csv", "issues.csv", "corrections.csv"
  ))
  expect_setequal(
    list.files(file.path(run_dir, "metadata")), paste0(names(metadata_schema()), ".csv")
  )
  # UTF-8 with LF endings, every field quoted, a blank an empty field, "" kept apart from NA.
  bytes <- readBin(file.path(run_dir, "tables", "issues.csv"), "raw", 1e6)
  expect_false(any(bytes == as.raw(0x0D)))
  lines <- readLines(file.path(run_dir, "tables", "issues.csv"), encoding = "UTF-8")
  expect_match(lines[[2L]], "^1,\"20261008T120000Z_abc123\",")
  expect_match(lines[[2L]], ",\"\",", fixed = TRUE)
  expect_true(any(grepl("café ☃", lines, fixed = TRUE)))
})

test_that("a run folder's results are never written over; pre-flight files there stay", {
  local_results_registry()
  object <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- file.path(dir, object$metadata$run$run_id)
  # A run writes its pre-flight files into its folder first (D7.11); preflight.csv is then
  # left as it is, written once (D15.17).
  gpq_preflight(fx_fish_spec(), output_dir = run_dir)
  csv <- file.path(run_dir, "metadata", "preflight.csv")
  then <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(csv, then)
  gpq_write_results(object, dir)
  expect_true(file.exists(file.path(run_dir, "reports", "preflight.html")))
  expect_equal(as.numeric(file.mtime(csv)), as.numeric(then))
  expect_identical(gpq_read_results(run_dir), object)
  expect_error(gpq_write_results(object, dir), "already holds a run's results")
})

test_that("a preflight.csv in the folder that isn't the results' stops before any file", {
  local_results_registry()
  object <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- file.path(dir, object$metadata$run$run_id)
  tryCatch(
    suppressWarnings(gpq_preflight(fx_planted_spec(), output_dir = run_dir)),
    gpq_preflight_error = function(e) NULL
  )
  expect_error(
    gpq_write_results(object, dir),
    "preflight.csv holds a different pre-flight table from the results'",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(run_dir, "tables")))
  expect_setequal(list.files(file.path(run_dir, "metadata")), "preflight.csv")
})

test_that("a run stopped at pre-flight leaves four files and no results to read (4.2; D7.11)", {
  dir <- withr::local_tempdir()
  spec <- fx_planted_spec()
  run_id <- "20261008T120000Z_def456"
  run_dir <- file.path(dir, run_id)
  stopped <- tryCatch(
    suppressWarnings(gpq_preflight(spec, output_dir = run_dir)),
    gpq_preflight_error = function(e) e
  )
  expect_s3_class(stopped, "gpq_preflight_error")
  run <- run_metadata(
    run_id, Sys.time(), Sys.time(),
    report_units = "u1", outcome = "stopped_preflight"
  )
  write_run_metadata(list(run = run, spec_manifest = spec$manifest), run_dir)
  expect_setequal(
    list.files(run_dir, recursive = TRUE),
    c(
      "reports/preflight.html", "metadata/preflight.csv", "metadata/run.csv",
      "metadata/spec_manifest.csv"
    )
  )
  expect_error(gpq_read_results(run_dir), "stopped at pre-flight, so it has no results")
  expect_error(
    write_run_metadata(list(run = run, notes = run), run_dir),
    "Run metadata has the tables run, spec_manifest"
  )
})

test_that("reading stops on a missing file or one that doesn't read cleanly, naming it", {
  local_results_registry()
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(fx_results(), dir)
  corrections <- file.path(run_dir, "tables", "corrections.csv")
  lines <- readLines(corrections)
  writeLines(c(lines, "\"1\",\"2\""), corrections)
  expect_error(gpq_read_results(run_dir), "corrections.csv doesn't read cleanly")
  withr::local_options(warn = 2)
  expect_error(gpq_read_results(run_dir), "corrections.csv doesn't read cleanly")
  file.remove(corrections)
  expect_error(gpq_read_results(run_dir), "has no .*corrections.csv")
  expect_error(gpq_read_results(file.path(dir, "no_such_run")), "one run folder that exists")
})

test_that("writing refuses what isn't a gpq_results object or a folder", {
  local_results_registry()
  object <- fx_results()
  expect_error(gpq_write_results(unclass(object), tempdir()), "must be a gpq_results object")
  file <- withr::local_tempfile()
  writeLines("x", file)
  expect_error(gpq_write_results(object, file), "one folder, not an existing file")
  expect_error(gpq_write_results(object, NA_character_), "one folder")
})
