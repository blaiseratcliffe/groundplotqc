# A stand-in registry with two data rules, and a small results object of the fish survey
# built with it, holding the strings a CSV round trip is most likely to change (plan 9,
# 18.4; D15.2, D15.9). The engine registers no data rule before M4, so tests that build
# results put the stand-in in place first.

# The registry with a conformance rule (Layer 2) and a plausibility rule (Layer 7) added,
# in place of rule_registry() until the calling test ends. Returns it, invisibly.
local_results_registry <- function(env = parent.frame()) {
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = c("value_odd", "value_jump"), stage = c("conformance", "plausibility"),
    layer = c(2L, 7L), check_type = "test", default_severity = c("error", "flag"),
    default_class = c("harmonization", "depends"),
    inputs = c("attributes; sentinels; settings:lang; settings:severity", "codes"),
    strategy = "none", message_id = "preflight_title", rule_version = 1L, unit = FALSE
  ))
  testthat::local_mocked_bindings(rule_registry = function() registry, .env = env)
  invisible(registry)
}

# Text a CSV round trip could change: an empty string, padding, quotes and a comma, a line
# break, accents and a symbol, the letters NA, markup.
fx_hostile_text <- function() {
  c(
    "", "  padded ", "say \"hi\", twice", "line one\nline two", "café ☃", "NA",
    "<script>&"
  )
}

# A run's results on the fish survey: group columns unit and area, the problem unit's key
# column plot; a fail, a not_run and a plausibility row; record issues with an empty value,
# hostile text, a missing PK and a provider key; a not_evaluated issue and a column-level
# one; one correction. Needs local_results_registry() in force.
fx_results <- function() {
  spec <- fx_fish_spec()
  run_id <- "20261008T120000Z_abc123"
  hostile <- fx_hostile_text()
  run <- run_metadata(
    run_id, as.POSIXct("2026-10-08 12:00:00", tz = "UTC"),
    as.POSIXct("2026-10-08 12:00:05", tz = "UTC"),
    group_cols = c("unit", "area"), report_units = c("u2", "u1", "u2")
  )
  metadata <- new_metadata(
    run, spec, gpq_preflight(spec),
    rules = fx_empty_rule_set(), settings = list(severity = c(value_odd = "warning"))
  )
  results <- new_results(list(
    run_id = run_id, run_scope = "package",
    rule_id = c("value_odd", "value_odd", "value_jump"), rule_fingerprint = "f1",
    stage = c("conformance", "conformance", "plausibility"), layer = c(2L, 2L, 7L),
    severity = c("warning", "warning", "flag"), table_name = "catches",
    column_name = c("weight", "fish_count", "weight"),
    unit = c("u1", "u1", "*"), area = c("a1", "*", "*"),
    n_checked = c(8L, 0L, 8L), n_fail = c(3L, 0L, 1L), n_not_evaluated = c(1L, 8L, 0L),
    reason_code = c("*", "*", "value_jump"), n_distinct_fail_values = c(3L, 0L, 1L),
    n_easy_fix = 0L, examples = c(paste(hostile[1:3], collapse = "; "), NA, hostile[[4L]]),
    status = c("fail", "not_run", "fail"), not_run_reason = c(NA, "no_sentinels", NA),
    message_id = "preflight_title"
  ), group_cols = c("unit", "area"))
  issues <- new_issues(list(
    issue_id = 1:5, run_id = run_id, run_scope = "package",
    rule_id = c("value_odd", "value_odd", "value_odd", "value_jump", "value_odd"),
    rule_fingerprint = "f1",
    stage = c("conformance", "conformance", "conformance", "plausibility", "conformance"),
    layer = c(2L, 2L, 2L, 7L, 2L),
    severity = c("warning", "warning", "warning", "flag", "warning"),
    reason_code = c("*", "*", "*", "value_jump", "*"), table_name = "catches",
    column_name = c("weight", "weight", "weight", "weight", "fish_count"),
    record_pk = c("c1", "c2", NA, "c1", "*"), row_number = c(1L, 2L, 3L, 1L, NA),
    pk_positional = c(FALSE, FALSE, TRUE, FALSE, NA),
    level = c("record", "record", "record", "record", "column"),
    n_records = c(NA, NA, NA, NA, 12L),
    value = c(hostile[[1L]], hostile[[3L]], hostile[[2L]], NA, NA),
    expected = c("a weight", hostile[[5L]], NA, NA, NA),
    provider_key = c(NA, "tbl.key=7; tbl.no=2", NA, NA, NA),
    provider_key_status = c(NA, "decoded", NA, NA, NA),
    unit = "u1", area = c("a1", "a1", "a1", "a1", "*"), plot = c("p1", "p1", "p2", "p1", "*"),
    root_issue_id = c(NA, NA, NA, 1L, NA),
    status = c("open", "open", "open", "not_evaluated", "open")
  ), group_cols = c("unit", "area"), key_col = "plot")
  fingerprints <- issue_fingerprint(issues)
  data.table::set(issues, j = "fingerprint", value = fingerprints$fingerprint)
  data.table::set(issues, j = "fingerprint_pk", value = fingerprints$fingerprint_pk)
  corrections <- new_corrections(list(
    correction_id = 1L, run_id = run_id, issue_id = 1L, table_name = "catches",
    record_pk = "c1", column_name = "weight", current_value = hostile[[1L]],
    proposed_value = "-99", strategy = "sentinel_fix", tie = FALSE, support = "1 of 1",
    support_n = 1L, support_d = 1L, fixed_value_passes = TRUE, easy_fix = FALSE,
    easy_fix_blocked_by = "2; problem_plot"
  ))
  new_gpq_results(results, issues, corrections, metadata)
}
