# Tests for the run metadata: run IDs, the run, settings and rules tables, the self-test and
# a run's metadata checked whole (plan 9.5, 9.7; D4.17, D7.11, D7.12, D8.16, D9.29, D15.2,
# D15.5, D15.8).

test_that("a run ID is the UTC start time and six hex characters, new for each run (D15.5)", {
  start <- as.POSIXct("2026-12-15 14:03:12.25", tz = "UTC")
  id <- new_run_id(start)
  expect_match(id, run_id_pattern)
  expect_true(startsWith(id, "20261215T140312Z_"))
  # The same start in another time zone is the same UTC time.
  eastern <- as.POSIXct("2026-12-15 09:03:12", tz = "EST")
  expect_true(startsWith(new_run_id(eastern), "20261215T140312Z_"))
  expect_false(identical(new_run_id(start), new_run_id(start + 1e-3)))
  # The caller's random-number stream is left as it was.
  set.seed(42)
  before <- .Random.seed
  new_run_id()
  expect_identical(.Random.seed, before)
})

test_that("utc_text() writes a time as UTC text", {
  expect_equal(utc_text(as.POSIXct("2026-10-08 12:00:05", tz = "UTC")), "2026-10-08T12:00:05Z")
})

test_that("the run table is one row with the versions, group columns and outcome (9.5)", {
  run <- run_metadata(
    "20261008T120000Z_abc123", as.POSIXct("2026-10-08 12:00:00", tz = "UTC"),
    as.POSIXct("2026-10-08 12:01:00", tz = "UTC"),
    group_cols = c("unit", "area"), report_units = c("u2", "u1", "u2")
  )
  expect_equal(names(run), names(metadata_schema()$run))
  expect_equal(nrow(run), 1L)
  expect_equal(run$run_start, "2026-10-08T12:00:00Z")
  expect_equal(run$group_cols, "unit; area")
  expect_equal(run$report_units, "u1; u2")
  expect_equal(run$outcome, "completed")
  expect_equal(run$package_version, as.character(utils::packageVersion("groundplotqc")))
  expect_equal(run$data_table_version, as.character(utils::packageVersion("data.table")))
  expect_equal(run$r_version, paste(R.version$major, R.version$minor, sep = "."))
  expect_true(is.na(run$git_commit) || grepl("^[0-9a-f]{40}$", run$git_commit))
  expect_no_error(validate_schema(run, "run", metadata_schema()$run))
  none <- run_metadata(
    "20261008T120000Z_abc123", Sys.time(), Sys.time(),
    outcome = "stopped_preflight"
  )
  expect_true(is.na(none$group_cols) && is.na(none$report_units))
  expect_equal(none$outcome, "stopped_preflight")
  # The units come in the C order, whatever the session's collation, so that the same input
  # gives the same run.csv on every machine. testthat sets the C collation; the machine's own
  # is put back for this call.
  withr::local_collate("")
  mixed <- run_metadata(
    "20261008T120000Z_abc123", Sys.time(), Sys.time(),
    report_units = c("b", "A", "a", "B")
  )
  expect_equal(mixed$report_units, "A; B; a; b")
})

test_that("join_units() escapes backslashes and semicolons inside each unit, in C order (D20.7)", {
  expect_identical(join_units(character()), NA_character_)
  # MAGPlot's IDs hold neither, so run.csv reads as it did.
  expect_identical(join_units(c("220", "100", "220.01", "100")), "100; 220; 220.01")
  # Inside each unit "\" is written "\\" and ";" is written "\;"; then the units are joined.
  expect_identical(join_units("a; b"), r"(a\; b)")
  expect_identical(join_units(c(";", "x")), r"(\;; x)")
  expect_identical(join_units(c(r"(a\)", "b")), r"(a\\; b)")
  expect_identical(join_units(r"(\)"), r"(\\)")
  expect_identical(join_units(r"(a\;b)"), r"(a\\\;b)")
  expect_identical(join_units(c(r"(x\\)", "y")), r"(x\\\\; y)")
})

test_that("split_units() splits where a \"; \" isn't escaped, then unescapes once (D20.48)", {
  expect_identical(split_units(NA_character_), character())
  expect_identical(split_units("100; 220; 220.01"), c("100", "220", "220.01"))
  # A ";" after one backslash is inside its unit; after two, or four, the unit before it ends
  # in a backslash, or two.
  expect_identical(split_units(r"(a\; b; c)"), c("a; b", "c"))
  expect_identical(split_units(r"(a\\; b)"), c(r"(a\)", "b"))
  expect_identical(split_units(r"(x\\\\; y)"), c(r"(x\\)", "y"))
  # One pass: "\\\;" is a backslash then a semicolon, neither read twice.
  expect_identical(split_units(r"(a\\\;b)"), r"(a\;b)")
  # D20.7's units each round-trip alone and together: "; ", ";" alone, a trailing backslash, a
  # lone one, a backslash then a semicolon, and two backslashes before the next unit.
  units <- c("a; b", ";", r"(c\)", r"(\)", r"(d\;e)", r"(f\\)", "g")
  for (unit in units) {
    expect_identical(split_units(join_units(unit)), unit)
  }
  expect_identical(split_units(join_units(units)), sort(units, method = "radix"))
})

test_that("report units round-trip through run.csv as written and read (D20.7)", {
  units <- c("a; b", ";", r"(c\)", r"(\)", r"(d\;e)", r"(f\\)", "g", "é; è")
  run <- run_metadata(
    "20261008T120000Z_abc123", Sys.time(), Sys.time(),
    group_cols = "unit", report_units = units
  )
  run_dir <- withr::local_tempdir()
  write_run_metadata(list(run = run), run_dir)
  path <- file.path(run_dir, "metadata", "run.csv")
  read <- read_run_csv(path, metadata_schema()$run)
  expect_identical(read$report_units, run$report_units)
  expect_identical(split_units(read$report_units), sort(units, method = "radix"))
  # The file holds the text as joined: the writer adds no backslash of its own.
  text <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(text) <- "UTF-8"
  expect_true(grepl(run$report_units, text, fixed = TRUE))
  # A field that ends in a backslash, just before its closing quote, is written and read back
  # as joined too.
  ends <- data.table::copy(run)
  data.table::set(ends, j = "report_units", value = join_units(c("100", r"(b\)")))
  ends_dir <- withr::local_tempdir()
  write_run_metadata(list(run = ends), ends_dir)
  ends_path <- file.path(ends_dir, "metadata", "run.csv")
  back <- read_run_csv(ends_path, metadata_schema()$run)
  expect_identical(back$report_units, r"(100; b\\)")
  expect_identical(split_units(back$report_units), c("100", r"(b\)"))
  ends_text <- rawToChar(readBin(ends_path, "raw", file.size(ends_path)))
  expect_true(grepl(r"("100; b\\",)", ends_text, fixed = TRUE))
})

test_that("a group value that is the text \"*\" is no report unit (D20.6)", {
  stations <- data.table::copy(fx_fish_clean()$stations)
  data.table::set(stations, 1L, "water_body", "*")
  results <- gpq_check_conformance(
    list(stations = stations), fx_fish_spec(),
    group_cols = "water_body", layers = 0
  )
  expect_identical(results$metadata$run$report_units, stations$water_body[[2L]])
  expect_false("*" %in% split_units(results$metadata$run$report_units))
})

test_that("the git commit is HEAD's in a checkout, NA outside one", {
  folder <- dirname(system.file("DESCRIPTION", package = "groundplotqc"))
  if (file.exists(file.path(folder, ".git"))) {
    # Loaded from a checkout, as devtools::test() runs, the commit is HEAD's. Then system.file()
    # is mocked to find no .git, as in an installed package; the mock is made only here, so an
    # installed run, as R CMD check and CRAN make, rebinds nothing (R145, D20.85).
    if (nzchar(Sys.which("git"))) {
      expect_match(package_git_commit(), "^[0-9a-f]{40}$")
    }
    imports <- parent.env(environment(package_git_commit))
    if (exists("system.file", envir = imports, inherits = FALSE)) {
      testthat::local_mocked_bindings(system.file = function(...) "")
    } else {
      testthat::local_mocked_bindings(system.file = function(...) "", .package = "base")
    }
  }
  # An installed package has no .git, so the commit is NA, with nothing mocked there.
  expect_true(is.na(package_git_commit()))
})

test_that("settings: each declared setting with its tier, then severity entries (D15.2)", {
  plain <- settings_metadata()
  expect_equal(names(plain), c("setting", "value", "tier"))
  declared <- c(
    "lang", "example_cap", "column_level_min_n", "raw_na_policy", "rogue_codes", "unknown_codes",
    "coordinate_attributes", "code_set_max_values", "code_set_skip_attributes",
    "code_set_skip_pattern", "design_geometry_table", "design_geometry_by",
    "design_geometry_columns", "subplot_label_table", "subplot_label_columns",
    "design_signature_table", "design_signature_by", "design_signature_columns",
    "fallback_class"
  )
  expect_equal(plain$setting, declared)
  expect_equal(
    plain$value,
    c("en", "5", "10", "when_sentinels", "-999; UNK; NA; N/A", NA, NA, "25", NA, NA, rep(NA, 9L))
  )
  expect_true(all(plain$tier == "built-in"))
  rule_set <- list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(setting = "lang", value = "fr", type = "character")
  )
  expect_equal(settings_metadata(rules = rule_set)[1L, c(value, tier)], c("fr", "rule set"))
  withr::local_options(groundplotqc.severity = c(fk_orphan = "warning", pk_missing = "error"))
  rows <- settings_metadata(list(lang = "en", severity = c(fk_orphan = "error")), rule_set)
  expect_equal(rows$setting, c(
    declared, "severity:fk_orphan", "severity:fk_orphan", "severity:pk_missing"
  ))
  later <- seq_along(declared)[-1L]
  expect_equal(rows$value[-later], c("en", "error", "warning", "error"))
  expect_equal(rows$tier[-later], c("argument", "argument", "option", "option"))
  expect_no_error(validate_schema(rows, "settings", metadata_schema()$settings))
  for (column in c("setting", "tier")) {
    blank <- data.table::copy(rows)
    data.table::set(blank, i = 1L, j = column, value = NA_character_)
    expect_error(
      validate_schema(blank, "settings", metadata_schema()$settings),
      paste("has blank cells in", column)
    )
  }
  # The rule set's severities stay in the rule set: no row for them.
  severe <- list(rules = data.frame(
    rule_id = "fk_orphan", table_name = "*", attribute_name = "*", severity = "warning",
    class = NA_character_, enabled = TRUE
  ))
  withr::local_options(groundplotqc.severity = NULL)
  expect_equal(settings_metadata(rules = severe)$setting, declared)
  expect_error(settings_metadata(list(colour = "red")), "settings the package doesn't have")
})

test_that("the rules table names the rule set and hashes its checked rows (D15.8)", {
  expect_equal(nrow(rules_metadata()), 0L)
  expect_equal(names(rules_metadata()), names(metadata_schema()$rules))
  from_files <- magp_rules()
  row <- rules_metadata(from_files)
  expect_equal(row$input, "rules")
  expect_equal(row$name, "MAGPlot 2.0")
  expect_equal(row$version, from_files$meta$version)
  expect_match(row$sha256, "^[0-9a-f]{64}$")
  # The same rows given in memory, as data.frames, give the same hash.
  in_memory <- lapply(from_files, as.data.frame)
  expect_equal(rules_metadata(in_memory)$sha256, row$sha256)
  # A changed cell changes it.
  changed <- in_memory
  changed$settings$value <- "fr"
  expect_false(rules_metadata(changed)$sha256 == row$sha256)
  # Without a meta row, no name or version.
  bare <- rules_metadata(fx_empty_rule_set())
  expect_true(is.na(bare$name) && is.na(bare$version))
})

test_that("the self-test lists the rules a run was to report that have no row (9.7)", {
  local_results_registry()
  results <- fx_results()$results
  expected <- data.table::data.table(
    rule_id = c("value_odd", "value_jump", "value_quiet"),
    stage = c("conformance", "plausibility", "conformance"), layer = c(2L, 7L, 3L)
  )
  missing <- run_self_test(results, expected)
  expect_equal(missing$rule_id, "value_quiet")
  expect_equal(names(missing), names(metadata_schema()$self_test))
  # A not_run row counts as reported.
  expect_equal(nrow(run_self_test(results, expected[1L])), 0L)
})

test_that("a run's metadata has 9.5's tables in order, checked", {
  local_results_registry()
  metadata <- fx_results()$metadata
  expect_named(metadata, names(metadata_schema()))
  expect_equal(metadata$spec_manifest, fx_fish_spec()$manifest)
  expect_true(all(metadata$clashes$rule_id == "spec_clash_resolved"))
  expect_equal(metadata$settings$setting, c(
    "lang", "example_cap", "column_level_min_n", "raw_na_policy", "rogue_codes", "unknown_codes",
    "coordinate_attributes", "code_set_max_values", "code_set_skip_attributes",
    "code_set_skip_pattern", "design_geometry_table", "design_geometry_by",
    "design_geometry_columns", "subplot_label_table", "subplot_label_columns",
    "design_signature_table", "design_signature_by", "design_signature_columns",
    "fallback_class", "severity:value_odd"
  ))
  bad_id <- data.table::copy(metadata)
  bad_id$run <- data.table::copy(metadata$run)
  data.table::set(bad_id$run, j = "run_id", value = "run 1")
  expect_error(validate_metadata(bad_id), "isn't a UTC start time")
  expect_error(validate_metadata(metadata[c(2L, 1L, 3:8)]), "in that order")
  bad_tier <- metadata
  bad_tier$settings <- data.table::copy(metadata$settings)
  data.table::set(bad_tier$settings, j = "tier", value = "default")
  expect_error(validate_metadata(bad_tier), "values default outside tier")
  two_runs <- metadata
  two_runs$run <- rbind(metadata$run, metadata$run)
  expect_error(validate_metadata(two_runs), "must have one row")
})

test_that("the clashes table keeps the clashes precedence resolved", {
  local_results_registry()
  spec <- fx_planted_spec()
  run <- run_metadata("20261008T120000Z_abc123", Sys.time(), Sys.time())
  preflight <- suppressWarnings(tryCatch(
    gpq_preflight(spec),
    gpq_preflight_error = function(e) e$preflight
  ))
  metadata <- new_metadata(run, spec, preflight)
  expect_equal(nrow(metadata$clashes), sum(spec$clashes$rule_id == "spec_clash_resolved"))
  expect_true(nrow(metadata$clashes) > 0L)
  expect_equal(metadata$preflight, preflight)
})

test_that("new_metadata() holds copies: a change by reference on either side stays there", {
  local_results_registry()
  spec <- fx_planted_spec()
  run <- run_metadata("20261008T120000Z_abc123", Sys.time(), Sys.time())
  preflight <- suppressWarnings(tryCatch(
    gpq_preflight(spec),
    gpq_preflight_error = function(e) e$preflight
  ))
  # A value in the first row that differs from the one there, so that a change shows.
  change <- function(table) {
    for (column in names(table)) {
      value <- table[[column]]
      data.table::set(table, i = 1L, j = column, value = switch(class(value)[[1L]],
        character = "changed",
        integer = -7L
      ))
    }
  }
  tables <- c("run", "spec_manifest", "preflight", "clashes")
  inputs <- function() {
    list(
      run = run, spec_manifest = spec$manifest, preflight = preflight, clashes = spec$clashes
    )
  }
  before <- lapply(inputs(), data.table::copy)
  metadata <- new_metadata(run, spec, preflight)
  for (name in tables) {
    expect_gt(nrow(metadata[[name]]), 0L)
  }
  # A change to the metadata's tables leaves the inputs as they were.
  for (name in tables) {
    change(metadata[[name]])
  }
  expect_identical(inputs(), before)
  # A change to the inputs leaves the metadata's tables as they were.
  metadata <- new_metadata(run, spec, preflight)
  kept <- lapply(metadata, data.table::copy)
  for (table in inputs()) {
    change(table)
  }
  expect_false(identical(inputs(), before))
  expect_identical(metadata, kept)
})
