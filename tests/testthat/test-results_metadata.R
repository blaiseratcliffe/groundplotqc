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
})

test_that("the git commit is HEAD's in a checkout, NA outside one", {
  # Loaded from a checkout, as devtools::test() runs, the commit is HEAD's.
  folder <- dirname(system.file("DESCRIPTION", package = "groundplotqc"))
  if (file.exists(file.path(folder, ".git")) && nzchar(Sys.which("git"))) {
    expect_match(package_git_commit(), "^[0-9a-f]{40}$")
  }
  # An installed package has no .git, so system.file() finds none.
  imports <- parent.env(environment(package_git_commit))
  if (exists("system.file", envir = imports, inherits = FALSE)) {
    testthat::local_mocked_bindings(system.file = function(...) "")
  } else {
    testthat::local_mocked_bindings(system.file = function(...) "", .package = "base")
  }
  expect_true(is.na(package_git_commit()))
})

test_that("settings: each declared setting with its tier, then severity entries (D15.2)", {
  plain <- settings_metadata()
  expect_equal(names(plain), c("setting", "value", "tier"))
  expect_equal(plain$setting, "lang")
  expect_equal(plain$tier, "built-in")
  rule_set <- list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(setting = "lang", value = "fr", type = "character")
  )
  expect_equal(settings_metadata(rules = rule_set)[, c(value, tier)], c("fr", "rule set"))
  withr::local_options(groundplotqc.severity = c(fk_orphan = "warning", pk_missing = "error"))
  rows <- settings_metadata(list(lang = "en", severity = c(fk_orphan = "error")), rule_set)
  expect_equal(rows$setting, c(
    "lang", "severity:fk_orphan", "severity:fk_orphan", "severity:pk_missing"
  ))
  expect_equal(rows$value, c("en", "error", "warning", "error"))
  expect_equal(rows$tier, c("argument", "argument", "option", "option"))
  expect_no_error(validate_schema(rows, "settings", metadata_schema()$settings))
  # The rule set's severities stay in the rule set: no row for them.
  severe <- list(rules = data.frame(
    rule_id = "fk_orphan", table_name = "*", attribute_name = "*", severity = "warning",
    class = NA_character_, enabled = TRUE
  ))
  withr::local_options(groundplotqc.severity = NULL)
  expect_equal(settings_metadata(rules = severe)$setting, "lang")
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
  expect_equal(metadata$settings$setting, c("lang", "severity:value_odd"))
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
