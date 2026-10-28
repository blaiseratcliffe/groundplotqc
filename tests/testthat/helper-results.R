# A stand-in registry with two data rules, and a small results object of the fish survey
# built with it, holding the strings a CSV round trip is most likely to change (plan 9,
# 18.4; D15.2, D15.9). The engine registers no data rule before M4, so tests that build
# results put the stand-in in place first. Also a hand-made run of two report units for the
# report tests, with every routing case and the values the confidentiality tests plant
# (M7a; plan 13).

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
    n_checked = c(8L, 0L, 8L), n_fail = c(3L, 0L, 1L), n_not_evaluated = c(1L, 0L, 0L),
    n_unchecked = c(0L, 8L, 0L),
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
    reaches_other_unit = FALSE, root_issue_id = c(NA, NA, NA, 1L, NA),
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

# A run of two report units, "100" and "110", for the report tests (M7a tasks 7 to 11):
# group columns unit and area, the problem unit's key column plot. The rows are made by hand
# on the fish survey's tables, not what a run of it writes; gear_log stands for a table the
# delivery lacks. They hold each routing case (D16.16, D18.60, D18.89, D20.10, D20.11, D20.14,
# D20.24): unit rules' "*" rows and issues; a record rule's "*" row and issue; a not_run row
# for a table not given and one counting unreachable records; D18.89's (A) and (B), a record
# reaching a parent whose unit is "*" and a collapsed row, each marked; undeclared_code_set's
# row per unit and its marked "*" row; fk_orphan's not-evaluated records with no issue, and
# not-evaluated issues with their roots, one of them held back. Also every class, the
# severities error, warning and flag (an info rule writes results rows only, D20.55),
# provider keys of each status, hostile text and two corrections.
# Planted values, for the confidentiality tests (13): 100's own "AB-secret", "A-100-1",
# "P-100-1", "P-100-2" and "ST-100-1"; 110's own "BC-secret", "A-110-1", "P-110-1" and
# "ST-110-2"; and, held only by "*" rows, marked issues and a results row's examples, so in
# no unit's files of the two-unit run, "ZZ-secret", "S-110-secret", "P-110-9", "C-100-5",
# "P-100-3", "A-ZZ-1", "P-110-2", "ST-110-3" and "C-ZZ-13". The flag is pattern_mismatch's, a
# conformance rule given the severity a rule set may give any rule (R12), so every rule is the
# package's own and the sections have each layer's text.
fx_report_results <- function() {
  spec <- fx_fish_spec()
  run_id <- "20261026T120000Z_7a0007"
  hostile <- fx_hostile_text()
  run <- run_metadata(
    run_id, as.POSIXct("2026-10-26 12:00:00", tz = "UTC"),
    as.POSIXct("2026-10-26 12:00:09", tz = "UTC"),
    group_cols = c("unit", "area"), report_units = c("100", "110")
  )
  metadata <- new_metadata(run, spec, gpq_preflight(spec), rules = fx_empty_rule_set())
  registry <- rule_registry()
  # Each rule's stage, layer, severity and reason code, as a run gives them; pattern_mismatch
  # with the severity flag, as a rule set may give it (R12).
  by_rule <- function(rule_id) {
    at <- match(rule_id, registry$rule_id)
    stage <- registry$stage[at]
    list(
      stage = stage, layer = registry$layer[at],
      severity = ifelse(rule_id == "pattern_mismatch", "flag", registry$default_severity[at]),
      reason_code = ifelse(stage == "plausibility", rule_id, "*")
    )
  }
  rule_id <- c(
    "table_missing", "pk_duplicate", "class_mismatch", "undeclared_code_set", "blank_string",
    "code_not_in_list", "raw_na", "value_above_max", "decimals_exceeded", "fk_orphan",
    "key_incoherent", "raw_na", "value_above_max", "fk_unassigned", "whitespace_padding",
    "code_not_in_list", "pattern_mismatch", "value_above_max", "parent_without_children",
    "code_not_in_list", "value_below_min"
  )
  n_fail <- c(1L, 0L, 1L, 1L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 4L, 1L, 1L, 0L, 0L, 1L, 0L)
  results <- new_results(c(
    list(run_id = run_id, run_scope = "package", rule_id = rule_id, rule_fingerprint = "f1"),
    by_rule(rule_id),
    list(
      table_name = c(
        "gear_log", "gear_log", "catches", "stations", "stations", "hauls", "catches",
        "catches", "catches", "catches", "catches", "catches", "catches", "hauls", "hauls",
        "catches", "catches", "catches", "hauls", "catches", "catches"
      ),
      column_name = c(
        "*", "log_id", "fish_count", "station_name", "station_name", "gear", "weight",
        "weight", "weight", "haul_id", "haul_id", "weight", "weight", "station_id", "gear",
        "species", "weight", "weight", "haul_id", "species", "weight"
      ),
      unit = c(rep("*", 4L), rep("100", 7L), rep("110", 8L), "*", "*"),
      # 110's rows in A-100-1 and A-ZZ-1: groups that D18.89's (B) and a "*"-unit parent mix.
      area = c(
        rep("*", 4L), rep("A-100-1", 7L), "A-100-1", "A-100-1", "A-ZZ-1", rep("A-110-1", 5L),
        "*", "*"
      ),
      n_checked = c(
        1L, 0L, 1L, 1L, 5L, 4L, 6L, 5L, 5L, 4L, 5L, 1L, 0L, 1L, 6L, 4L, 4L, 4L, 3L, 1L, 0L
      ),
      n_fail = n_fail,
      # fk_orphan's 3 are records holding a missing-value code a foreign key may hold, which
      # no issue holds (D18.71). value_above_max's are one record of 100's and one of 110's in
      # A-100-1, each a not-evaluated issue's (110's held back, D18.89's (B)), and two of
      # 110's in A-110-1 that no issue holds.
      n_not_evaluated = c(
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 3L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L
      ),
      n_unchecked = c(
        NA, 0L, NA, NA, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L
      ),
      n_distinct_fail_values = pmin(n_fail, 1L), n_easy_fix = 0L,
      examples = c(
        NA, NA, "character", "AB-secret; Pond; BC-secret; ZZ-secret", hostile[[1L]], "=1+1",
        NA, "1500", "1.23456", hostile[[6L]], "H5", NA, NA, "?", " NET", hostile[[3L]],
        hostile[[7L]], NA, NA, hostile[[4L]], NA
      ),
      status = ifelse(
        seq_along(rule_id) %in% c(2L, 13L, 21L), "not_run", ifelse(n_fail > 0L, "fail", "pass")
      ),
      not_run_reason = c(
        NA, "no_table", rep(NA, 10L), "none_checked", rep(NA, 7L), "none_checked"
      ),
      message_id = registry$message_id[match(rule_id, registry$rule_id)]
    )
  ), group_cols = c("unit", "area"))
  rule_id <- c(
    "table_missing", "class_mismatch", "undeclared_code_set", "undeclared_code_set",
    "undeclared_code_set", "blank_string", "code_not_in_list", "raw_na", "value_above_max",
    "value_above_max", "decimals_exceeded", "fk_orphan", "key_incoherent", "raw_na",
    "value_above_max", "fk_unassigned", "whitespace_padding", "code_not_in_list",
    "pattern_mismatch", "code_not_in_list"
  )
  level <- c(rep("column", 6L), rep("record", 10L), "column", rep("record", 3L))
  issues <- new_issues(c(
    list(
      issue_id = seq_along(rule_id), run_id = run_id, run_scope = "package", rule_id = rule_id,
      rule_fingerprint = "f1"
    ),
    by_rule(rule_id),
    list(
      table_name = c(
        "gear_log", "catches", "stations", "stations", "stations", "stations", "hauls",
        "catches", "catches", "catches", "catches", "catches", "catches", "catches", "catches",
        "hauls", "hauls", "catches", "catches", "catches"
      ),
      column_name = c(
        "*", "fish_count", "station_name", "station_name", "station_name", "station_name",
        "gear", "weight", "weight", "weight", "weight", "haul_id", "haul_id", "weight", "weight",
        "station_id", "gear", "species", "weight", "species"
      ),
      record_pk = c(
        rep("*", 6L), "H1", "C3", "C3", "C1", "C2", "C4", "C5", "C9", "C9", "H7", "*", "C11",
        "C12", "C13"
      ),
      row_number = c(rep(NA, 6L), 1L, 3L, 3L, 1L, 2L, 4L, 5L, 9L, 9L, 7L, NA, 11L, 12L, 13L),
      pk_positional = ifelse(level == "record", FALSE, NA),
      level = level,
      n_records = c(0L, 9L, 2L, 1L, 1L, 3L, rep(NA, 10L), 4L, NA, NA, NA),
      value = c(
        NA, "character", "AB-secret; Pond", "BC-secret", "ZZ-secret", hostile[[1L]], "=1+1",
        NA, NA, "1500", "1.23456", hostile[[6L]], "H5", NA, NA, "?", " NET", hostile[[3L]],
        hostile[[7L]], hostile[[4L]]
      ),
      expected = c(
        NA, "integer", NA, NA, NA, NA, "gear", NA, "1000", hostile[[2L]], "3", "hauls",
        "station_id=S-110-secret", NA, "1000", NA, "NET", "species", hostile[[5L]], "species"
      ),
      provider_key = c(
        rep(NA, 6L), "src_station.code=ST-100-1; src_haul.no=1", "-ST-100-3", "-ST-100-3",
        "src_station.code=ST-100-1; src_catch.no=", "jurisdiction=AB; src_catch.no=2", NA,
        "C-100-5", NA, NA, "src_station.code=ST-110-3; src_haul.no=7", NA,
        "src_station.code=ST-110-2; src_catch.no=11", "C-110-12", "C-ZZ-13"
      ),
      provider_key_status = c(
        rep(NA, 6L), "decoded", "raw", "raw", "partial", "decoded", "fallback_pk",
        "no_lineage_spec", "fallback_pk", "fallback_pk", "decoded", NA, "decoded",
        "no_lineage_spec", "no_lineage_spec"
      ),
      unit = c("*", "*", "100", "110", "*", rep("100", 8L), rep("110", 6L), "*"),
      area = c(
        rep("*", 5L), rep("A-100-1", 10L), "A-ZZ-1", rep("A-110-1", 3L), "*"
      ),
      plot = c(
        rep("*", 6L), rep("P-100-1", 5L), "P-100-2", "P-110-9", "P-100-3", "P-100-3",
        "P-110-2", "*", "P-110-1", "P-110-1", "*"
      ),
      # Marked (D20.10, D20.11, D20.14): undeclared_code_set's "*" row; D18.89's (A), a
      # key_incoherent side read from 110's site, filed under 100; (B), a record of 100's filed
      # under 110 with 100's area, and its not-evaluated issue; a record of 110's whose area
      # came from a parent whose unit is "*"; a collapsed row one of whose records is marked.
      reaches_other_unit = seq_along(rule_id) %in% c(5L, 13L, 14L, 15L, 16L, 17L),
      root_issue_id = c(rep(NA, 8L), 8L, rep(NA, 5L), 14L, rep(NA, 5L)),
      status = ifelse(seq_along(rule_id) %in% c(9L, 15L), "not_evaluated", "open"),
      issue_class = c(
        "harmonization", "harmonization", rep("unclassified", 3L), "harmonization", "source",
        rep("harmonization", 3L), "source", rep("harmonization", 4L), "source",
        "harmonization", "source", "unclassified", "harmonization"
      )
    )
  ), group_cols = c("unit", "area"), key_col = "plot")
  data.table::set(issues, j = "class_basis", value = data.table::fifelse(
    issues$issue_class == "source", "override", "default"
  ))
  fingerprints <- issue_fingerprint(issues)
  data.table::set(issues, j = "fingerprint", value = fingerprints$fingerprint)
  data.table::set(issues, j = "fingerprint_pk", value = fingerprints$fingerprint_pk)
  # One correction for an issue of 100's, one for D18.89's (B), which reaches no unit.
  corrections <- new_corrections(list(
    correction_id = 1:2, run_id = run_id, issue_id = c(10L, 14L), table_name = "catches",
    record_pk = c("C1", "C9"), column_name = "weight", current_value = c("1500", NA),
    proposed_value = c("150", "-99"), strategy = c("none", "sentinel_fix"), tie = FALSE,
    fixed_value_passes = TRUE, easy_fix = c(FALSE, TRUE)
  ))
  new_gpq_results(results, issues, corrections, metadata)
}
