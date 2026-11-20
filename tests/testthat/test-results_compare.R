# Tests for comparing a run with an earlier one (plan 3.4, 9.6, 11.8; D4.2, D4.7, D9.22, D15.9,
# D23.13, D23.21, D23.30 to D23.33): new, persisting and resolved issues, the data or the rules
# as their cause, matching by fingerprint then pk: form, on run scope and, for a column-level
# issue, on the group columns, the NA skip, the tables not matched, the rule ID map, and the
# earlier run as a folder, an issues file or a data.frame.

# Issues as a run's issues table holds them, one per element: the columns the comparison reads
# and a group column, unit; each fingerprint made from the record and provider key.
issues_of <- function(rule_id = "value_above_max", record_pk = "k1", provider_key = NA,
                      run_id = "20261101T120000Z_aaaaaa", rule_fingerprint = "f1",
                      table_name = "hauls", level = "record", unit = "u1",
                      run_scope = "package", first_seen_run = NA_character_) {
  issues <- data.table::data.table(
    run_id = run_id, run_scope = run_scope, rule_id = rule_id,
    rule_fingerprint = rule_fingerprint, table_name = table_name, column_name = "minutes",
    record_pk = record_pk, level = level, value = "900", provider_key = provider_key,
    unit = unit, status = "open", first_seen_run = first_seen_run,
    change = NA_character_, change_cause = NA_character_
  )
  prints <- issue_fingerprint(issues)
  data.table::set(issues, j = "fingerprint", value = prints$fingerprint)
  data.table::set(issues, j = "fingerprint_pk", value = prints$fingerprint_pk)
  issues
}

earlier <- "20261101T120000Z_aaaaaa"
later <- "20261108T120000Z_bbbbbb"

test_that("new, persisting and resolved issues, each new or resolved one's cause (D4.7, D23.31)", {
  # k1 persists, first seen at an earlier run still; k2 persists, first seen in `earlier`; k3
  # is new, its rule unchanged, and k4 new, its rule's fingerprint changed; k5 is resolved,
  # its rule unchanged, and k6 resolved, a rule with no issue now.
  before <- rbind(
    issues_of(record_pk = "k1", first_seen_run = "20261020T120000Z_cccccc"),
    issues_of(record_pk = "k2"),
    issues_of(rule_id = "raw_na", record_pk = "k4"),
    issues_of(record_pk = "k5"),
    issues_of(rule_id = "code_not_in_list", record_pk = "k6")
  )
  now <- rbind(
    issues_of(record_pk = c("k1", "k2", "k3"), run_id = later),
    issues_of(rule_id = "raw_na", record_pk = "k9", rule_fingerprint = "f2", run_id = later)
  )
  before_df <- as.data.frame(before)
  given_df <- data.table::copy(before_df)
  given <- data.table::copy(before)
  out <- gpq_compare_runs(now, before_df)
  # The current issues are changed by reference and returned.
  expect_identical(out$issues, now)
  expect_equal(now$change, c("persisting", "persisting", "new", "new"))
  expect_equal(now$first_seen_run, c("20261020T120000Z_cccccc", earlier, later, later))
  expect_equal(now$change_cause, c(NA, NA, "data", "rules_or_spec"))
  expect_equal(out$resolved$record_pk, c("k4", "k5", "k6"))
  expect_equal(out$resolved$change, rep("resolved", 3L))
  # k4's rule changed; k5's didn't; k6's rule has no issue now, so an issues table can't say it
  # didn't run: unchanged.
  expect_equal(out$resolved$change_cause, c("rules_or_spec", "data", "data"))
  expect_equal(out$resolved$first_seen_run, rep(earlier, 3L))
  # The earlier issues, given as a data.frame, are read where they are, never changed.
  expect_identical(before_df, given_df)
  # Comparing again, with the earlier issues as a data.table, gives the same and leaves them
  # as they were too.
  again <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("persisting", "persisting", "new", "new"))
  expect_equal(again$resolved$record_pk, c("k4", "k5", "k6"))
  expect_identical(before, given)
  # Records sharing a key, as pk_duplicate finds them, give issues sharing a fingerprint in both
  # runs: each current one persists, first seen when the earliest earlier one was (R15), and
  # each earlier one of a key no current issue has is resolved, one row each.
  before <- rbind(
    issues_of(record_pk = "d1", first_seen_run = c(NA, "20261020T120000Z_cccccc", NA)),
    issues_of(record_pk = c("d2", "d2"))
  )
  now <- issues_of(record_pk = c("d1", "d1", "d1"), run_id = later)
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, rep("persisting", 3L))
  expect_equal(now$first_seen_run, rep("20261020T120000Z_cccccc", 3L))
  expect_equal(now$change_cause, rep(NA_character_, 3L))
  expect_equal(out$resolved$record_pk, c("d2", "d2"))
  expect_equal(out$resolved$first_seen_run, rep(earlier, 2L))
  expect_equal(out$resolved$change_cause, rep("data", 2L))
})

test_that("an issue still persists after its provider key's decoding changes (D9.22)", {
  before <- issues_of(record_pk = c("k1", "k2"), provider_key = c("site=1; tree=2", NA))
  # k1's key decoded anew, k2's decoded now for the first time: each fingerprint differs, each
  # pk: form is the same.
  now <- issues_of(
    record_pk = c("k1", "k2"), provider_key = c("plot=1; tree=2", "site=1; tree=3"),
    run_id = later
  )
  expect_false(any(now$fingerprint %in% before$fingerprint))
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("persisting", "persisting"))
  expect_equal(nrow(out$resolved), 0L)
})

test_that("issues sharing a pk: form match by their fingerprints alone (D9.22, D23.148)", {
  # Two records of hauls share the PK k1, as a duplicated PK gives, each with its own provider
  # key: their fingerprints differ, their pk: form is the same. The earlier run holds the
  # first's issue; the second's is new, though its pk: form is the first's.
  before <- issues_of(record_pk = "k1", provider_key = "site=1; tree=2")
  now <- issues_of(
    record_pk = c("k1", "k1"), provider_key = c("site=1; tree=2", "site=1; tree=3"),
    run_id = later
  )
  expect_equal(length(unique(now$fingerprint)), 2L)
  expect_equal(length(unique(c(before$fingerprint_pk, now$fingerprint_pk))), 1L)
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("persisting", "new"))
  expect_equal(nrow(out$resolved), 0L)
  # The first's issue gone, two others sharing its PK: both new, the first's resolved.
  now <- issues_of(
    record_pk = c("k1", "k1"), provider_key = c("site=1; tree=3", "site=1; tree=4"),
    run_id = later
  )
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("new", "new"))
  expect_equal(out$resolved$fingerprint, before$fingerprint)
  # The earlier run's issues are counted too (D23.149): it holds two records sharing k1, the
  # later run one whose fingerprint is neither's, its pk: form its own in the later run. It is
  # new, and both earlier ones resolved.
  before <- issues_of(
    record_pk = c("k1", "k1"), provider_key = c("site=1; tree=2", "site=1; tree=3")
  )
  now <- issues_of(record_pk = "k1", provider_key = "site=1; tree=4", run_id = later)
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, "new")
  expect_equal(out$resolved$fingerprint, before$fingerprint)
  # A PK one issue alone holds still matches by its pk: form when its fingerprint changes: the
  # test "an issue still persists after its provider key's decoding changes (D9.22)" above.
})

test_that("matching needs the same run scope, and a column-level issue the same groups (D23.21)", {
  # A merged run's issue never matches a package run's of the same fingerprint (D8.16).
  before <- issues_of(record_pk = "k1", run_scope = "merged")
  now <- issues_of(record_pk = "k1", run_id = later)
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, "new")
  expect_equal(out$resolved$record_pk, "k1")
  # A column-level issue's fingerprint names no record: u1's persists, u2's is resolved, u3's
  # is new, though all three share it.
  before <- issues_of(record_pk = "*", level = "column", unit = c("u1", "u2"))
  now <- issues_of(record_pk = "*", level = "column", unit = c("u1", "u3"), run_id = later)
  expect_equal(length(unique(c(before$fingerprint, now$fingerprint))), 1L)
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("persisting", "new"))
  expect_equal(out$resolved$unit, "u2")
  # So it does where the earlier issue gives only its pk: form, as a register row may: the
  # fingerprint, then the pk: form, for a column-level issue too (D9.22, R51).
  data.table::set(before, j = "fingerprint", value = NA_character_)
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("persisting", "new"))
  expect_equal(out$resolved$unit, "u2")
  # A record-level issue moved to another group still persists.
  before <- issues_of(record_pk = "k1", unit = "u1")
  now <- issues_of(record_pk = "k1", unit = "u2", run_id = later)
  gpq_compare_runs(now, before)
  expect_equal(now$change, "persisting")
})

test_that("an issue with no fingerprint never matches, nor is resolved (D15.9)", {
  # k1's PK is missing on both sides, with no provider key: no fingerprint, no pk: form.
  before <- issues_of(record_pk = c(NA, "k2"))
  now <- issues_of(record_pk = c(NA, "k2"), run_id = later)
  expect_true(is.na(now$fingerprint[[1L]]) && is.na(now$fingerprint_pk[[1L]]))
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("new", "persisting"))
  expect_equal(now$first_seen_run, c(later, earlier))
  expect_equal(nrow(out$resolved), 0L)
})

test_that("the tables in unmatched_tables are new each run, never resolved (D12.75, D23.13)", {
  before <- issues_of(
    record_pk = c("d1", "k1", "d9"), table_name = c("designs", "hauls", "designs")
  )
  now <- issues_of(record_pk = c("d1", "k1"), table_name = c("designs", "hauls"), run_id = later)
  out <- gpq_compare_runs(now, before, unmatched_tables = "designs")
  expect_equal(now$change, c("new", "persisting"))
  expect_equal(nrow(out$resolved), 0L)
  # Without it, the design's issue persists and d9's is resolved.
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("persisting", "persisting"))
  expect_equal(out$resolved$record_pk, "d9")
})

test_that("an earlier issue of a renamed or split rule matches the new rules' (D4.2)", {
  testthat::local_mocked_bindings(rule_id_map = function() {
    data.table::data.table(
      old_rule_id = c("minutes_high", "code_bad", "code_bad"),
      new_rule_id = c("value_above_max", "code_not_in_list", "pattern_mismatch"),
      change = c("rename", "split", "split"), package_version = "0.0.0.9037"
    )
  })
  # The old rules' fingerprints differ from the new ones', as a renamed rule's registry row does.
  before <- issues_of(
    rule_id = c("minutes_high", "code_bad", "code_bad"), record_pk = c("k1", "k2", "k3"),
    rule_fingerprint = "f0"
  )
  now <- rbind(
    issues_of(record_pk = "k1", run_id = later),
    issues_of(rule_id = "pattern_mismatch", record_pk = "k2", run_id = later),
    issues_of(rule_id = "code_not_in_list", record_pk = "k9", run_id = later)
  )
  out <- gpq_compare_runs(now, before)
  expect_equal(now$change, c("persisting", "persisting", "new"))
  # The old rule is known by its new IDs with its own fingerprint, so a new issue of a new rule
  # reads as changed by the rules, and so does a resolved issue of the old rule.
  expect_equal(now$change_cause, c(NA, NA, "rules_or_spec"))
  expect_equal(out$resolved$record_pk, "k3")
  expect_equal(out$resolved$rule_id, "code_bad")
  expect_equal(out$resolved$change_cause, "rules_or_spec")
})

test_that("a run folder or an issues file is read for its issues and rules (D23.33)", {
  local_results_registry()
  first <- fx_results()
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(first, dir)
  # The current run: fx_results() again, its first issue gone, a new one of the rule
  # value_jump, whose fingerprint has changed, and its rule value_odd's unchanged.
  now <- fx_results()
  issues <- now$issues
  data.table::set(issues, i = 2L, j = "record_pk", value = "c7")
  data.table::set(issues, i = 2L, j = "fingerprint", value = "value_odd|catches|weight|pk:c7")
  data.table::set(issues, i = 2L, j = "fingerprint_pk", value = "value_odd|catches|weight|pk:c7")
  data.table::set(issues, i = 4L, j = "record_pk", value = "c8")
  data.table::set(issues, i = 4L, j = "fingerprint", value = "value_jump|catches|weight|pk:c8")
  data.table::set(issues, i = 4L, j = "fingerprint_pk", value = "value_jump|catches|weight|pk:c8")
  data.table::set(issues, i = 4L, j = "rule_fingerprint", value = "f2")
  out <- gpq_compare_runs(now, run_dir)
  expect_identical(out$issues, now$issues)
  # Issue 3's PK is missing, so it has no fingerprint: new, as 2 and 4 are; 1 and the
  # column-level 5 persist.
  expect_equal(now$issues$change, c("persisting", "new", "new", "new", "persisting"))
  expect_equal(now$issues$change_cause, c(NA, "data", "data", "rules_or_spec", NA))
  # The earlier run's issues 2 and 4 are resolved, each of a rule whose fingerprint the current
  # run's results give as before (f1).
  expect_equal(out$resolved$issue_id, c(2L, 4L))
  expect_equal(out$resolved$change_cause, c("data", "data"))
  # With the issues file alone, the rules are read from its issues.
  issues_csv <- file.path(run_dir, "tables", "issues.csv")
  from_file <- gpq_compare_runs(now$issues, issues_csv)
  expect_equal(now$issues$change, c("persisting", "new", "new", "new", "persisting"))
  expect_equal(from_file$resolved$issue_id, c(2L, 4L))
  # The issues file read as a caller may, with utils::read.csv() as the help says or with
  # fread(), each reading an empty cell as "": a blank is missing, so the earlier issue 3, with
  # no fingerprint, isn't resolved, and the persisting issues were first seen in the earlier run
  # (D15.9).
  as_read <- list(
    utils::read.csv(issues_csv, colClasses = "character"), data.table::fread(issues_csv)
  )
  for (earlier_issues in as_read) {
    given <- data.table::copy(earlier_issues)
    from_read <- gpq_compare_runs(now$issues, earlier_issues)
    expect_equal(as.integer(from_read$resolved$issue_id), c(2L, 4L))
    expect_equal(now$issues$first_seen_run[c(1L, 5L)], rep(first$issues$run_id[[1L]], 2L))
    # The caller's frame, its blanks read anew in a table of the comparison's own, is unchanged.
    expect_identical(earlier_issues, given)
  }
  # So with the current issues read back with fread() too, issue 3's "" on each side: new.
  read_back <- data.table::fread(issues_csv, colClasses = "character")
  gpq_compare_runs(read_back, as_read[[2L]])
  expect_equal(read_back$change, c("persisting", "persisting", "new", "persisting", "persisting"))
  # A rule that ran in only one run is a change of rules: the current run's results lack
  # value_jump.
  ran <- now$results
  without <- now
  without$results <- ran[ran$rule_id != "value_jump"]
  out <- gpq_compare_runs(without, run_dir)
  expect_equal(out$resolved$change_cause, c("data", "rules_or_spec"))
})

test_that("a wrong current, previous or unmatched_tables is a caller's error", {
  local_results_registry()
  now <- issues_of(run_id = later)
  before <- issues_of()
  current_words <- paste(
    "`current` must be a gpq_results object, or the issues table of a run's results, a",
    "data.table, as gpq_check_conformance() returns them."
  )
  expect_error(gpq_compare_runs(as.data.frame(now), before), current_words, fixed = TRUE)
  expect_error(gpq_compare_runs(now[, !"level"], before), current_words, fixed = TRUE)
  expect_error(
    gpq_compare_runs(now, before, unmatched_tables = c("designs", NA)),
    "`unmatched_tables` must be NULL or a character vector of table names.",
    fixed = TRUE
  )
  # A group column that isn't text, as fread() reads digits: an issues table's, and a run's
  # results' two (D23.95).
  numbered <- data.table::copy(now)
  data.table::set(numbered, j = "unit", value = 1L)
  expect_error(
    gpq_compare_runs(numbered, before),
    "`current`'s group columns unit must be text, as gpq_check_conformance() returns them.",
    fixed = TRUE
  )
  numbered_run <- fx_results()
  run_issues <- data.table::copy(numbered_run$issues)
  n <- nrow(run_issues)
  data.table::set(run_issues, j = c("unit", "area"), value = list(seq_len(n), rep(2, n)))
  numbered_run$issues <- run_issues
  expect_error(
    gpq_compare_runs(numbered_run, before),
    "`current`'s group columns unit, area must be text, as gpq_check_conformance() returns them.",
    fixed = TRUE
  )
  form <- "`previous` must be a run folder, a data.frame or the path of an existing .csv file."
  dir <- withr::local_tempdir()
  for (bad in list(42, file.path(dir, "absent.csv"), NA_character_, list(before))) {
    expect_error(gpq_compare_runs(now, bad), form, fixed = TRUE)
  }
  expect_error(
    gpq_compare_runs(now, before[, !c("first_seen_run", "rule_fingerprint")]),
    "`previous` lacks the issues columns rule_fingerprint, first_seen_run.",
    fixed = TRUE
  )
  # A folder that isn't a completed run's: no run.csv; a run stopped at pre-flight.
  expect_error(gpq_compare_runs(now, dir), "has no", fixed = TRUE)
  run <- fx_results()$metadata$run
  data.table::set(run, j = "outcome", value = "stopped_preflight")
  write_run_metadata(list(run = run), dir)
  expect_error(
    gpq_compare_runs(now, dir),
    paste("The run in", dir, "stopped at pre-flight, so it has no results to read."),
    fixed = TRUE
  )
})
