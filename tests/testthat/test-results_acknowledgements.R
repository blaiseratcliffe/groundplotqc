# Tests for the acknowledgements register (plan 3.4, 11.8; D4.2, D4.8, D8.16, D23.13, D23.21,
# D23.26, D23.34): an entry holding on its value or lapsing, matched as the comparison matches
# (fingerprint, then pk: form; run scope; a column-level entry's group columns; the NA skip; the
# tables not matched; the rule ID map), only open issues acknowledged, the register as a
# data.frame or a CSV file, and the caller's errors.

# Issues as a run's issues table holds them, one per element, with a group column unit; each
# fingerprint made from the record and provider key.
issues_of <- function(record_pk = "k1", value = "900", provider_key = NA, level = "record",
                      unit = "u1", table_name = "hauls", status = "open",
                      run_scope = "package", rule_id = "value_above_max") {
  issues <- data.table::data.table(
    run_id = "20261108T120000Z_bbbbbb", run_scope = run_scope, rule_id = rule_id,
    table_name = table_name, column_name = "minutes", record_pk = record_pk, level = level,
    value = value, provider_key = provider_key, unit = unit, status = status
  )
  prints <- issue_fingerprint(issues)
  data.table::set(issues, j = "fingerprint", value = prints$fingerprint)
  data.table::set(issues, j = "fingerprint_pk", value = prints$fingerprint_pk)
  issues
}

# A register entry for each of these issues, acknowledging `acknowledged_value`.
entries_of <- function(issues, acknowledged_value = issues$value, decision = "verified") {
  data.frame(
    fingerprint = issues$fingerprint, fingerprint_pk = issues$fingerprint_pk,
    rule_id = issues$rule_id, table_name = issues$table_name,
    column_name = issues$column_name, provider_key = issues$provider_key,
    acknowledged_value = acknowledged_value, decision = decision, who = "A. Reviewer",
    when = "2026-11-17", why = "Checked.", run_scope = issues$run_scope, unit = issues$unit
  )
}

test_that("an entry holds on its value; one whose value changed lapses (D4.2, D4.8)", {
  # k1's entry holds; k2 was acknowledged at 800 and now holds 900; k9's issue is gone.
  issues <- issues_of(record_pk = c("k1", "k2", "k3"))
  register <- entries_of(
    issues_of(record_pk = c("k1", "k2", "k9")),
    acknowledged_value = c("900", "800", "1")
  )
  out <- gpq_apply_acknowledgements(issues, register)
  expect_identical(out$issues, issues)
  expect_equal(issues$status, c("acknowledged", "open", "open"))
  expect_equal(out$lapsed$fingerprint, register$fingerprint[[2L]])
  expect_equal(out$lapsed$acknowledged_value, "800")
  expect_equal(out$lapsed$current_value, "900")
  # The register's columns as read, then current_value.
  expect_named(out$lapsed, c(names(register), "current_value"))
  # Applied again, the acknowledged issue stays so and the entry still lapses.
  again <- gpq_apply_acknowledgements(issues, register)
  expect_equal(issues$status, c("acknowledged", "open", "open"))
  expect_equal(nrow(again$lapsed), 1L)
})

test_that("an entry still matches after its provider key's decoding changes (D9.22)", {
  register <- entries_of(issues_of(record_pk = "k1", provider_key = "site=1; tree=2"))
  issues <- issues_of(record_pk = "k1", provider_key = "plot=1; tree=2")
  expect_false(issues$fingerprint == register$fingerprint)
  gpq_apply_acknowledgements(issues, register)
  expect_equal(issues$status, "acknowledged")
  # An entry giving only the pk: form matches too.
  issues <- issues_of(record_pk = "k1", provider_key = "plot=1; tree=2")
  register$fingerprint <- NA
  gpq_apply_acknowledgements(issues, register)
  expect_equal(issues$status, "acknowledged")
})

test_that("a blank value equals a blank one; a column-level entry matches on its group", {
  # A column-level issue has no value; k2's value is spaces, which the register reads as blank.
  issues <- rbind(
    issues_of(record_pk = "*", value = NA, level = "column", unit = c("u1", "u2")),
    issues_of(record_pk = "k2", value = "  ")
  )
  register <- entries_of(issues[c(1L, 3L)], acknowledged_value = NA)
  gpq_apply_acknowledgements(issues, register)
  # u1's column-level issue is acknowledged, u2's, of the same fingerprint, isn't.
  expect_equal(issues$status, c("acknowledged", "open", "acknowledged"))
  # A register without the group column matches the column-level entry across its values (D23.100).
  ungrouped <- register
  ungrouped$unit <- NULL
  gpq_apply_acknowledgements(issues, ungrouped)
  expect_equal(issues$status, c("acknowledged", "acknowledged", "acknowledged"))
})

test_that("run scope, a missing fingerprint and the tables not matched (D8.16, D15.9, D23.13)", {
  issues <- issues_of(record_pk = c("k1", NA, "d1"), table_name = c("hauls", "hauls", "designs"))
  # A merged run's entry doesn't acknowledge a package run's issue.
  merged <- entries_of(issues_of(record_pk = "k1", run_scope = "merged"))
  gpq_apply_acknowledgements(issues, merged)
  expect_equal(issues$status, rep("open", 3L))
  # An issue with no fingerprint can't be acknowledged, nor one of a table not matched.
  register <- entries_of(issues)
  out <- gpq_apply_acknowledgements(issues, register, unmatched_tables = "designs")
  expect_equal(issues$status, c("acknowledged", "open", "open"))
  expect_equal(nrow(out$lapsed), 0L)
  gpq_apply_acknowledgements(issues, register)
  expect_equal(issues$status, c("acknowledged", "open", "acknowledged"))
})

test_that("an issue that wasn't evaluated keeps its status, its entry not lapsed", {
  # Drafted as T1-Q3's recommendation; the schema refuses a root on any other status.
  issues <- issues_of(record_pk = c("k1", "k2"), status = c("not_evaluated", "not_evaluated"))
  register <- entries_of(issues, acknowledged_value = c("900", "800"))
  out <- gpq_apply_acknowledgements(issues, register)
  expect_equal(issues$status, c("not_evaluated", "not_evaluated"))
  expect_equal(nrow(out$lapsed), 0L)
})

test_that("an entry of a renamed rule matches the new rule's issue (D4.2)", {
  testthat::local_mocked_bindings(rule_id_map = function() {
    data.table::data.table(
      old_rule_id = "minutes_high", new_rule_id = "value_above_max", change = "rename",
      package_version = "0.0.0.9038"
    )
  })
  issues <- issues_of()
  gpq_apply_acknowledgements(issues, entries_of(issues_of(rule_id = "minutes_high")))
  expect_equal(issues$status, "acknowledged")
})

test_that("the register is read from a CSV file as from a data.frame", {
  issues <- issues_of(record_pk = c("k1", "k2"))
  register <- entries_of(issues, acknowledged_value = c("900", "800"))
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(register, path)
  out <- gpq_apply_acknowledgements(issues, path)
  expect_equal(issues$status, c("acknowledged", "open"))
  expect_equal(out$lapsed$current_value, "900")
  # A bad decision is named at its line.
  register$decision[[2L]] <- "fine"
  data.table::fwrite(register, path)
  expect_error(
    gpq_apply_acknowledgements(issues, path),
    sprintf(
      paste(
        "The acknowledgement at line 3 of %s gives the decision \"fine\"; an",
        "acknowledgement's decision is verified or rejected_proposal."
      ),
      basename(path)
    ),
    fixed = TRUE
  )
})

test_that("a wrong issues table, register or unmatched_tables is a caller's error", {
  issues <- issues_of()
  register <- entries_of(issues)
  expect_error(
    gpq_apply_acknowledgements(as.data.frame(issues), register),
    "`issues` must be the issues table of a run's results",
    fixed = TRUE
  )
  expect_error(
    gpq_apply_acknowledgements(issues, register, unmatched_tables = 1),
    "`unmatched_tables` must be NULL or a character vector of table names.",
    fixed = TRUE
  )
  # A group column of the issues that isn't text, then two (D23.98).
  typed <- data.table::copy(issues)
  data.table::set(typed, j = "unit", value = 1L)
  expect_error(
    gpq_apply_acknowledgements(typed, register),
    "The group columns unit of `issues` must be text, as gpq_check_conformance() returns them.",
    fixed = TRUE
  )
  data.table::set(typed, j = "plot", value = 2)
  expect_error(
    gpq_apply_acknowledgements(typed, register),
    paste(
      "The group columns unit, plot of `issues` must be text, as gpq_check_conformance()",
      "returns them."
    ),
    fixed = TRUE
  )
  # A value or status column that isn't text, then both (D23.101).
  numeric_value <- data.table::copy(issues)
  data.table::set(numeric_value, j = "value", value = 900)
  expect_error(
    gpq_apply_acknowledgements(numeric_value, register),
    "The columns value of `issues` must be text, as gpq_check_conformance() returns them.",
    fixed = TRUE
  )
  factor_status <- data.table::copy(issues)
  data.table::set(factor_status, j = "status", value = factor("open"))
  expect_error(
    gpq_apply_acknowledgements(factor_status, register),
    "The columns status of `issues` must be text, as gpq_check_conformance() returns them.",
    fixed = TRUE
  )
  both <- data.table::copy(numeric_value)
  data.table::set(both, j = "status", value = factor("open"))
  expect_error(
    gpq_apply_acknowledgements(both, register),
    paste(
      "The columns value, status of `issues` must be text, as gpq_check_conformance()",
      "returns them."
    ),
    fixed = TRUE
  )
  # A column of its own missing, and one that isn't a group column of the issues.
  wrong <- register
  wrong$why <- NULL
  wrong$colour <- "red"
  expect_error(
    gpq_apply_acknowledgements(issues, wrong),
    paste(
      "`acknowledgements` must have the columns fingerprint, fingerprint_pk, rule_id,",
      "table_name, column_name, provider_key, acknowledged_value, decision, who, when, why,",
      "run_scope, and may have the issues' group columns (unit); missing: why; not taken:",
      "colour."
    ),
    fixed = TRUE
  )
  blank <- register
  blank$run_scope <- NA
  expect_error(
    gpq_apply_acknowledgements(issues, blank),
    paste(
      "The acknowledgement at row 1 of `acknowledgements` gives the run_scope (blank); an",
      "acknowledgement's run_scope is package or merged."
    ),
    fixed = TRUE
  )
  expect_error(
    gpq_apply_acknowledgements(issues, "no_such_file.csv"),
    "`acknowledgements` must be a data.frame or the path of an existing .csv file.",
    fixed = TRUE
  )
})
