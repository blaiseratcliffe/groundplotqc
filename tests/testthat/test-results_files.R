# Tests for a run's files: gpq_write_results() and gpq_read_results(), the round trip, the
# files' bytes, a stopped run's files and the refusals (plan 9.1, 9.5, 11.9, 18.4; D4.16,
# D7.11, D15.4, D15.16, D15.17, D15.27).

test_that("a run written and read back is identical (18.4)", {
  local_results_registry()
  object <- fx_results()
  # Cells fx_results() leaves NA take the text "NA" beside NA, markup, an emoji and a very
  # long string, so the round trip also holds what 18.4 lists beyond fx_hostile_text()'s
  # first five.
  issues <- data.table::copy(object$issues)
  data.table::set(issues, i = 3:4, j = "expected", value = c("NA", "<script>&"))
  data.table::set(
    issues,
    i = 4:5, j = "value", value = c(intToUtf8(0x1F600), strrep("long text, ", 2000L))
  )
  object$issues <- issues
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(object, dir)
  expect_equal(run_dir, file.path(dir, "20261008T120000Z_abc123"))
  expect_identical(gpq_read_results(run_dir), object)
})

test_that("a completed run with no group columns and no key column round trips (9.1)", {
  local_results_registry()
  object <- fx_results()
  drop <- function(x, columns) data.table::set(data.table::copy(x), j = columns, value = NULL)
  metadata <- object$metadata
  run <- data.table::copy(metadata$run)
  data.table::set(run, j = "group_cols", value = NA_character_)
  metadata$run <- run
  bare <- new_gpq_results(
    drop(object$results[1L], c("unit", "area")),
    drop(object$issues[1:2], c("unit", "area", "plot")),
    object$corrections, metadata
  )
  run_dir <- gpq_write_results(bare, withr::local_tempdir())
  expect_identical(gpq_read_results(run_dir), bare)
})

test_that("an output folder that doesn't exist yet is made", {
  local_results_registry()
  object <- fx_results()
  out <- file.path(withr::local_tempdir(), "made", "twice")
  expect_false(dir.exists(out))
  run_dir <- gpq_write_results(object, out)
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
  # UTF-8 with LF endings, every text field quoted, NA an empty field, "" kept apart from NA.
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

test_that("a file the write would make that is already there stops it before any file (D15.27)", {
  local_results_registry()
  object <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- file.path(dir, object$metadata$run$run_id)
  # Every file in the run folder, by name, with its bytes.
  snapshot <- function() {
    files <- list.files(run_dir, recursive = TRUE)
    bytes <- lapply(file.path(run_dir, files), function(path) readBin(path, "raw", file.size(path)))
    names(bytes) <- files
    bytes
  }
  put <- function(...) {
    path <- file.path(run_dir, ...)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("kept", path)
    path
  }
  refused <- function(files) {
    expect_error(
      gpq_write_results(object, dir),
      paste0(
        run_dir, " already holds ", paste(files, collapse = ", "),
        ", and a run's files are never written over."
      ),
      fixed = TRUE
    )
  }
  # A tables file there with no results.csv: nothing is written, no folder is made.
  issues_csv <- put("tables", "issues.csv")
  before <- snapshot()
  refused(issues_csv)
  expect_identical(snapshot(), before)
  expect_false(dir.exists(file.path(run_dir, "metadata")))
  # A metadata file there.
  unlink(run_dir, recursive = TRUE)
  run_csv <- put("metadata", "run.csv")
  before <- snapshot()
  refused(run_csv)
  expect_identical(snapshot(), before)
  expect_false(dir.exists(file.path(run_dir, "tables")))
  # Both, named together, the tables' first.
  issues_csv <- put("tables", "issues.csv")
  before <- snapshot()
  refused(c(issues_csv, run_csv))
  expect_identical(snapshot(), before)
  # With neither there the same call writes the run.
  unlink(run_dir, recursive = TRUE)
  expect_no_error(gpq_write_results(object, dir))
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
  csv <- file.path(run_dir, "metadata", "preflight.csv")
  before <- readBin(csv, "raw", file.size(csv))
  expect_error(
    gpq_write_results(object, dir),
    "preflight.csv holds a different pre-flight table from the results'",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(run_dir, "tables")))
  expect_setequal(list.files(file.path(run_dir, "metadata")), "preflight.csv")
  expect_identical(readBin(csv, "raw", file.size(csv)), before)
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
  # Each of the four files is written once: a second write of the metadata is refused,
  # naming both files, and leaves every file as it was (D15.27).
  files <- list.files(run_dir, recursive = TRUE)
  before <- lapply(file.path(run_dir, files), function(path) readBin(path, "raw", file.size(path)))
  expect_error(
    write_run_metadata(list(run = run, spec_manifest = spec$manifest), run_dir),
    paste0(
      run_dir, " already holds ", file.path(run_dir, "metadata", "run.csv"), ", ",
      file.path(run_dir, "metadata", "spec_manifest.csv"),
      ", and a run's files are never written over."
    ),
    fixed = TRUE
  )
  after <- lapply(file.path(run_dir, files), function(path) readBin(path, "raw", file.size(path)))
  expect_identical(after, before)
})

test_that("reading stops on a missing file or one that doesn't read cleanly, naming it", {
  local_results_registry()
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(fx_results(), dir)
  corrections <- file.path(run_dir, "tables", "corrections.csv")
  lines <- readLines(corrections)
  # The control: the file rewritten by writeLines() alone still reads, so the stop below is
  # drawn by the ragged line.
  writeLines(lines, corrections)
  expect_no_error(gpq_read_results(run_dir))
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
