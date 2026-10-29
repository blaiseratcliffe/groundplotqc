# Fixtures for issue classes (plan 10.3; D19.28, D19.54, D19.58): a hand-made issues table on
# the fish survey's tables, checked as a run's, with the group columns water_body and
# station_id, carrying the fixture marker, and a builder of overrides.
#   1: code_not_in_list (depends), catches.species, C1, "XXX", LK, S1
#   2: code_not_in_list (depends), catches.species, C2, "PW", RV, S2
#   3: value_above_max (depends), hauls.minutes, H2, "900", LK, S1
#   4: pk_duplicate (harmonization), hauls.haul_id, H3, "H3", RV, S2
#   5: date_unparseable (depends), hauls.haul_date, H1, "2020-13-01", LK, S1
#   6: code_not_in_list (depends), catches.species, column-level (12 records), LK, S1
#   7: code_not_in_list (depends), catches.species, C3, "ZZ", LK, S1
#   8: code_not_in_list (depends), hauls.gear, H4, "QQ", LK, S1
fx_class_issues <- function() {
  issues <- new_issues(list(
    issue_id = 1:8, run_id = "20261022T120000Z_abc123", run_scope = "package",
    rule_id = c(
      "code_not_in_list", "code_not_in_list", "value_above_max", "pk_duplicate",
      "date_unparseable", "code_not_in_list", "code_not_in_list", "code_not_in_list"
    ),
    rule_fingerprint = "f1", stage = "conformance", layer = c(3L, 3L, 3L, 4L, 1L, 3L, 3L, 3L),
    severity = "error", reason_code = "*",
    table_name = c("catches", "catches", "hauls", "hauls", "hauls", "catches", "catches", "hauls"),
    column_name = c(
      "species", "species", "minutes", "haul_id", "haul_date", "species", "species", "gear"
    ),
    record_pk = c("C1", "C2", "H2", "H3", "H1", "*", "C3", "H4"),
    row_number = c(1L, 2L, 2L, 3L, 1L, NA, 3L, 4L),
    pk_positional = c(FALSE, FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE),
    level = c(rep("record", 5L), "column", "record", "record"),
    n_records = c(rep(NA, 5L), 12L, NA, NA),
    value = c("XXX", "PW", "900", "H3", "2020-13-01", NA, "ZZ", "QQ"),
    water_body = c("LK", "RV", "LK", "RV", "LK", "LK", "LK", "LK"),
    station_id = c("S1", "S2", "S1", "S2", "S1", "S1", "S1", "S1"),
    reaches_other_unit = FALSE, status = "open"
  ), group_cols = c("water_body", "station_id"))
  fingerprints <- issue_fingerprint(issues)
  data.table::set(issues, j = "fingerprint", value = fingerprints$fingerprint)
  data.table::set(issues, j = "fingerprint_pk", value = fingerprints$fingerprint_pk)
  data.table::setattr(issues, "gpq_fixture", TRUE)
  issues
}

# Overrides, one row per element of the arguments; a scope cell not given is blank.
fx_class_overrides <- function(fingerprint = NA, rule_id = NA, table_name = NA,
                               column_name = NA, issue_class = "source", ...) {
  data.frame(
    fingerprint = fingerprint, rule_id = rule_id, table_name = table_name,
    column_name = column_name, issue_class = issue_class, who = "A. Reviewer",
    when = "2026-10-22", why = "Checked.", ...
  )
}
