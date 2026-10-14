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
  # CRLF and a lone CR in values (they replace the first two rows' values, which the other
  # round trips of fx_results() still hold), and hostile text in keys, group cells and the
  # provider key, not in values alone (18.4).
  data.table::set(
    issues,
    i = 1:2, j = "value", value = c("crlf one\r\ntwo", "lone\rcr")
  )
  data.table::set(
    issues,
    i = 1:2, j = "record_pk", value = c("c,1 \"q\"", "c\r\n2")
  )
  data.table::set(issues, j = "unit", value = "Québec \"QC\", x")
  data.table::set(issues, i = 1:3, j = "plot", value = c("p\n1", "p\n1", " p 2 "))
  data.table::set(issues, i = 2L, j = "provider_key", value = "tbl.key=<7>&; tbl.no=\"2\"")
  object$issues <- issues
  results <- data.table::copy(object$results)
  data.table::set(results, i = 1L, j = "unit", value = "Québec \"QC\", x")
  object$results <- results
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(object, dir)
  expect_equal(run_dir, file.path(dir, "20261008T120000Z_abc123"))
  expect_identical(gpq_read_results(run_dir), object)
})

test_that("a completed run with no group columns and no key column round trips (9.1)", {
  local_results_registry()
  object <- fx_results()
  without <- function(x, columns) data.table::set(data.table::copy(x), j = columns, value = NULL)
  metadata <- object$metadata
  run <- data.table::copy(metadata$run)
  data.table::set(run, j = "group_cols", value = NA_character_)
  metadata$run <- run
  bare <- new_gpq_results(
    without(object$results[1L], c("unit", "area")),
    without(object$issues[1:2], c("unit", "area", "plot")),
    object$corrections, metadata
  )
  run_dir <- gpq_write_results(bare, withr::local_tempdir())
  expect_identical(gpq_read_results(run_dir), bare)
})

test_that("group and key columns named with quotes round trip (9.1, 18.4)", {
  local_results_registry()
  object <- fx_results()
  # A quote in a name is written doubled and read back doubled, as it is in a cell, so the
  # names are undone like the cells; area's name holds two quotes in a row.
  quoted <- c(unit = "un\"it", area = "a\"\"b", plot = "pl\"ot")
  rename <- function(x) {
    x <- data.table::copy(x)
    present <- intersect(names(quoted), names(x))
    data.table::setnames(x, present, unname(quoted[present]))
    x
  }
  metadata <- object$metadata
  run <- data.table::copy(metadata$run)
  data.table::set(run, j = "group_cols", value = "un\"it; a\"\"b")
  metadata$run <- run
  changed <- new_gpq_results(
    rename(object$results), rename(object$issues), object$corrections, metadata
  )
  expect_true(all(unname(quoted) %in% names(changed$issues)))
  run_dir <- gpq_write_results(changed, withr::local_tempdir())
  expect_identical(gpq_read_results(run_dir), changed)
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
  # `listed` is the text after "already holds": the files by their paths inside the run
  # folder, several joined with ", " and " and " before the last (R69).
  refused <- function(listed) {
    expect_error(
      gpq_write_results(object, dir),
      paste0(run_dir, " already holds ", listed, ", and a run's files are never written over."),
      fixed = TRUE
    )
  }
  # A tables file there with no results.csv: nothing is written, no folder is made.
  put("tables", "issues.csv")
  before <- snapshot()
  refused("tables/issues.csv")
  expect_identical(snapshot(), before)
  expect_false(dir.exists(file.path(run_dir, "metadata")))
  # A metadata file there.
  unlink(run_dir, recursive = TRUE)
  put("metadata", "run.csv")
  before <- snapshot()
  refused("metadata/run.csv")
  expect_identical(snapshot(), before)
  expect_false(dir.exists(file.path(run_dir, "tables")))
  # Both, named together, the tables' first.
  put("tables", "issues.csv")
  before <- snapshot()
  refused("tables/issues.csv and metadata/run.csv")
  expect_identical(snapshot(), before)
  # Three, the last two joined by " and ".
  put("tables", "corrections.csv")
  before <- snapshot()
  refused("tables/issues.csv, tables/corrections.csv and metadata/run.csv")
  expect_identical(snapshot(), before)
  # With neither there the same call writes the run.
  unlink(run_dir, recursive = TRUE)
  expect_no_error(gpq_write_results(object, dir))
})

test_that("a file in the way of metadata/ stops the write before any table file (D15.27, D16.5)", {
  local_results_registry()
  object <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- file.path(dir, object$metadata$run$run_id)
  dir.create(run_dir)
  in_the_way <- file.path(run_dir, "metadata")
  writeLines("kept", in_the_way)
  # A file named metadata is a file the run didn't write, so the used run folder is refused
  # before the folder is tried (D16.5).
  expect_error(
    gpq_write_results(object, dir),
    paste0(run_dir, " already holds metadata, and a run's files are never written over."),
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(run_dir, "tables")))
  expect_identical(readLines(in_the_way), "kept")
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
      run_dir, " already holds metadata/run.csv and metadata/spec_manifest.csv, and a run's ",
      "files are never written over."
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

test_that("a used run folder is refused before any file; its own pre-flight files aren't (D16.5)", {
  local_results_registry()
  object <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- file.path(dir, object$metadata$run$run_id)
  dir.create(file.path(run_dir, "reports"), recursive = TRUE)
  writeLines("another run's page", file.path(run_dir, "reports", "u1.html"))
  writeLines("kept", file.path(run_dir, "notes.txt"))
  expect_error(
    gpq_write_results(object, dir),
    paste0(
      run_dir, " already holds notes.txt and reports/u1.html, and a run's files are never ",
      "written over."
    ),
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(run_dir, "tables")))
  expect_false(dir.exists(file.path(run_dir, "metadata")))
  # The files the write would make come first, then the others in C order: "Z.txt" before
  # "notes.txt", which the locale's order wouldn't give.
  dir.create(file.path(run_dir, "tables"))
  writeLines("kept", file.path(run_dir, "tables", "results_old.csv"))
  writeLines("kept", file.path(run_dir, "tables", "corrections.csv"))
  writeLines("kept", file.path(run_dir, "Z.txt"))
  expect_error(
    gpq_write_results(object, dir),
    paste0(
      run_dir, " already holds tables/corrections.csv, Z.txt, notes.txt, reports/u1.html and ",
      "tables/results_old.csv, and a run's files are never written over."
    ),
    fixed = TRUE
  )
  # A folder holding only the run's own pre-flight files is written.
  unlink(run_dir, recursive = TRUE)
  gpq_preflight(fx_fish_spec(), output_dir = run_dir)
  expect_no_error(gpq_write_results(object, dir))
})

test_that("an empty folder named like a file the write makes is refused before any file (D15.27)", {
  local_results_registry()
  object <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- file.path(dir, object$metadata$run$run_id)
  dir.create(file.path(run_dir, "tables", "issues.csv"), recursive = TRUE)
  dir.create(file.path(run_dir, "metadata", "run.csv"), recursive = TRUE)
  expect_error(
    gpq_write_results(object, dir),
    paste0(
      run_dir, " already holds tables/issues.csv and metadata/run.csv, and a run's files are ",
      "never written over."
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file.path(run_dir, "tables", "results.csv")))
  expect_false(file.exists(file.path(run_dir, "metadata", "settings.csv")))
})

test_that("a key column named with \"; \" round trips (D15.40; M3b m22)", {
  local_results_registry()
  object <- fx_results()
  issues <- data.table::copy(object$issues)
  data.table::setnames(issues, "plot", "plot; 2")
  changed <- new_gpq_results(object$results, issues, object$corrections, object$metadata)
  run_dir <- gpq_write_results(changed, withr::local_tempdir())
  back <- gpq_read_results(run_dir)
  expect_true("plot; 2" %in% names(back$issues))
  expect_identical(back, changed)
})

test_that("a value with an invalid byte and a quote round trips, with no warning", {
  local_results_registry()
  object <- fx_results()
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97, 0x20, 0x22, 0x71, 0x22)))
  Encoding(bad) <- "UTF-8"
  issues <- data.table::copy(object$issues)
  data.table::set(issues, i = 1L, j = "value", value = bad)
  object$issues <- issues
  run_dir <- gpq_write_results(object, withr::local_tempdir())
  expect_no_warning(back <- gpq_read_results(run_dir))
  expect_identical(back, object)
})

test_that("that round trip keeps the value's UTF-8 mark in a session that isn't UTF-8 (R37)", {
  local_results_registry()
  before <- Sys.getlocale("LC_CTYPE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", before)))
  set <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  testthat::skip_if(
    !nzchar(set) || l10n_info()[["UTF-8"]], "LC_CTYPE can't be set to a locale that isn't UTF-8."
  )
  object <- fx_results()
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97, 0x20, 0x22, 0x71, 0x22)))
  Encoding(bad) <- "UTF-8"
  issues <- data.table::copy(object$issues)
  data.table::set(issues, i = 1L, j = "value", value = bad)
  object$issues <- issues
  run_dir <- gpq_write_results(object, withr::local_tempdir())
  expect_no_warning(back <- gpq_read_results(run_dir))
  expect_identical(Encoding(back$issues$value[[1L]]), "UTF-8")
  expect_identical(back, object)
})

test_that("gpq_write_results and gpq_read_results are exported (D15.4)", {
  exports <- getNamespaceExports("groundplotqc")
  expect_true(all(c("gpq_write_results", "gpq_read_results") %in% exports))
})

test_that("a conformance run on magp_example is written and read back identical (18.4, D15.34)", {
  # About 4 s a run until M4b cuts the run's fixed cost (D16.18).
  testthat::skip_on_cran()
  results <- withCallingHandlers(
    gpq_check_conformance(
      magp_example, magp_spec(),
      group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
      problem_unit = list("magp_plots", "magp_plot_id"), rules = magp_rules(), layers = 0:2
    ),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  run_dir <- gpq_write_results(results, withr::local_tempdir())
  expect_identical(gpq_read_results(run_dir), results)
})
