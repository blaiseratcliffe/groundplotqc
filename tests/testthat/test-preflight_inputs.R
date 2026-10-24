# Tests for pre-flight's run inputs (plan 3.6, 4.2; D19.51): read as the specification's
# inputs are, and input_unparseable, override_scope_conflict and override_unknown_name.

# Overrides of the fish survey, one row per element of the arguments, every column given.
override_rows <- function(rule_id = "value_above_max", table_name = "hauls",
                          column_name = "minutes", issue_class = "source", ...) {
  data.frame(
    fingerprint = NA, rule_id = rule_id, table_name = table_name, column_name = column_name,
    issue_class = issue_class, who = "A. Reviewer", when = "2026-10-22", why = "Checked.", ...
  )
}

# The fish survey's spec, read once for the file's tests, which never change it.
fish_spec <- fx_fish_spec()

# One check of the inputs on the fish survey, with these inputs.
input_check <- function(name, inputs) {
  preflight_check_functions()[[name]](fish_spec, preflight_context(inputs = inputs))
}

# Where a data.frame's row is, as a finding's detail starts.
input_row <- function(row) report_text("input_position_memory", row = row, input = "overrides")

test_that("the inputs' checks don't run without them; each input is read as text (D19.51)", {
  checks <- c("input_unparseable", "override_scope_conflict", "override_unknown_name")
  for (name in checks) {
    expect_equal(input_check(name, list()), "no_input", info = name)
  }
  lineage <- data.frame(
    table_name = "hauls", record_pk = "H1", column_name = "gear", raw_value = "gn",
    raw_source = "net.code"
  )
  expect_equal(nrow(input_check("input_unparseable", list(lineage = lineage))), 0L)
  expect_equal(input_check("override_scope_conflict", list(lineage = lineage)), "no_input")
  # A number is read as its text and a blank as NA, as a specification's input is.
  fill <- data.frame(
    table_name = "hauls", record_pk = c("H1", "H2"), column_name = "minutes",
    written_value = c(-99, NA)
  )
  before <- data.table::copy(fill)
  read <- read_run_inputs(list(pipeline_fill = fill), "inputs", names(run_input_schema()))
  expect_identical(read$pipeline_fill$data$written_value, c("-99", NA))
  # The caller's table, its number column included, is as it was.
  expect_identical(fill, before)
  # Overrides with no rows, in memory or as a header-only file, give no finding and no error.
  empty <- override_rows()[0, ]
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(empty, path)
  for (name in checks) {
    expect_equal(nrow(input_check(name, list(overrides = empty))), 0L, info = name)
    expect_equal(nrow(input_check(name, list(overrides = path))), 0L, info = name)
  }
})

test_that("a wrong inputs list or element is a caller's error (D19.51)", {
  spec <- fish_spec
  shape <- "`inputs` must be a list of data.frames or CSV paths, each named once."
  expect_error(gpq_preflight(spec, inputs = override_rows()), shape, fixed = TRUE)
  expect_error(gpq_preflight(spec, inputs = NULL), shape, fixed = TRUE)
  expect_error(gpq_preflight(spec, inputs = list(override_rows())), shape, fixed = TRUE)
  # A name given twice is the list's shape; a name outside the list is the other error.
  expect_error(
    gpq_preflight(spec, inputs = list(overrides = override_rows(), overrides = override_rows())),
    shape,
    fixed = TRUE
  )
  expect_error(
    gpq_preflight(spec, inputs = list(previous = override_rows())),
    paste(
      "`inputs` names tables it doesn't take: previous.",
      "It takes overrides, lineage and pipeline_fill."
    ),
    fixed = TRUE
  )
  expect_error(
    gpq_preflight(spec, inputs = list(overrides = "no_such_file.csv")),
    "`overrides` must be a data.frame or the path of an existing .csv file.",
    fixed = TRUE
  )
})

test_that("input_unparseable names missing and extra columns, an override's group taken (D19.51)", {
  # water_body is an attribute of the survey, so a group column of the overrides; colour isn't.
  overrides <- override_rows(water_body = "LK", colour = "red")
  overrides$why <- NULL
  found <- input_check("input_unparseable", list(overrides = overrides))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_both",
    input = "overrides", missing = "why", extra = "colour"
  ))
  lineage <- data.frame(
    table_name = "hauls", record_pk = "H1", column_name = "gear", raw_value = "gn"
  )
  found <- input_check("input_unparseable", list(lineage = lineage))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_missing",
    input = "lineage", missing = "raw_source"
  ))
  # Only the overrides take group columns.
  fill <- data.frame(
    table_name = "hauls", record_pk = "H1", column_name = "gear", written_value = "?",
    water_body = "LK"
  )
  found <- input_check("input_unparseable", list(pipeline_fill = fill))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_extra",
    input = "pipeline_fill", extra = "water_body"
  ))
  # Without a column of its own, an override's class, scope and names wait (R28): a class
  # that isn't one, two rows of one scope with the two classes and an unknown rule give
  # only the missing column.
  waiting <- override_rows(
    rule_id = c("no_such_rule", "no_such_rule", "value_above_max"),
    issue_class = c("source", "harmonization", "Neither")
  )
  waiting$why <- NULL
  found <- input_check("input_unparseable", list(overrides = waiting))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_missing",
    input = "overrides", missing = "why"
  ))
  for (name in c("override_scope_conflict", "override_unknown_name")) {
    expect_equal(nrow(input_check(name, list(overrides = waiting))), 0L, info = name)
  }
})

test_that("input_unparseable names an override's class and one with no scope (D19.51)", {
  overrides <- rbind(
    override_rows(issue_class = c("source", "Source")),
    override_rows(rule_id = NA, table_name = NA, column_name = NA)
  )
  found <- input_check("input_unparseable", list(overrides = overrides))
  expect_equal(found$detail, c(
    report_text(
      "preflight_detail_input_unparseable_class",
      position = input_row(2L), value = "Source"
    ),
    report_text("preflight_detail_input_unparseable_scope", position = input_row(3L))
  ))
  expect_true(all(found$file == "in memory" & is.na(found$source_cell)))
  # From a CSV file, each at its line.
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(overrides, path)
  found <- input_check("input_unparseable", list(overrides = path))
  expect_equal(found$file, rep(basename(path), 2L))
  expect_equal(found$source_cell, paste0(basename(path), c(":3", ":4")))
  expect_match(found$detail[[1L]], paste("Line 3 of", basename(path)), fixed = TRUE)
})

test_that("input_unparseable reports a run input's file that doesn't read cleanly", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "table_name,record_pk,column_name,raw_value,raw_source",
      "hauls,H1,gear,gn,net.code",
      "hauls,H2,gear,gn,net.code,again"
    ),
    path
  )
  found <- input_check("input_unparseable", list(lineage = path))
  expect_equal(nrow(found), 1L)
  expect_equal(found$source_cell, paste0(basename(path), ":3"))
})

test_that("override_scope_conflict names each row repeating a scope with the other class (D9.27)", {
  # 2 repeats 1 with its class; 3 repeats 1 with the other; 5's group differs from 4's; 7
  # repeats 6, whose blank rule ID is equal to its own.
  overrides <- override_rows(
    rule_id = c(rep("value_above_max", 5L), NA, NA),
    issue_class = c(
      "source", "source", "harmonization", "source", "harmonization", "harmonization", "source"
    ),
    water_body = c(NA, NA, NA, "LK", "RV", "LK", "LK")
  )
  found <- input_check("override_scope_conflict", list(overrides = overrides))
  short <- function(row) report_text("rule_set_position_memory_short", row = row)
  expect_equal(found$detail, c(
    report_text(
      "preflight_detail_override_scope_conflict",
      position = input_row(3L), first = short(1L), class = "harmonization",
      other_class = "source"
    ),
    report_text(
      "preflight_detail_override_scope_conflict",
      position = input_row(7L), first = short(6L), class = "source",
      other_class = "harmonization"
    )
  ))
  # A row input_unparseable stops on is left to it.
  stopped <- override_rows(issue_class = c("source", "neither"))
  expect_equal(nrow(input_check("override_scope_conflict", list(overrides = stopped))), 0L)
  # So are rows with no scope cell, whatever their classes (R30).
  blank <- override_rows(
    rule_id = NA, table_name = NA, column_name = NA, issue_class = c("source", "harmonization")
  )
  expect_equal(nrow(input_check("override_scope_conflict", list(overrides = blank))), 0L)
  # From a CSV file, the warning is at the line of the row it flags and names the first row
  # by its line (R32).
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(override_rows(issue_class = c("source", "harmonization")), path)
  found <- input_check("override_scope_conflict", list(overrides = path))
  expect_equal(found$file, basename(path))
  expect_equal(found$source_cell, paste0(basename(path), ":3"))
  expect_match(found$detail, paste("Line 3 of", basename(path)), fixed = TRUE)
  expect_match(found$detail, "repeats the scope of line 2 ", fixed = TRUE)
  # A rule ID is compared as written, before the rule ID map reads it: a renamed rule and its
  # new name are two scopes (R30).
  testthat::local_mocked_bindings(rule_id_map = function() {
    data.table::data.table(
      old_rule_id = "value_too_high", new_rule_id = "value_above_max", change = "rename",
      package_version = "0.0.0.9014"
    )
  })
  renamed <- override_rows(
    rule_id = c("value_too_high", "value_above_max"), issue_class = c("source", "harmonization")
  )
  expect_equal(nrow(input_check("override_scope_conflict", list(overrides = renamed))), 0L)
})

test_that("override_scope_conflict groups by scope whatever the group columns are called", {
  # Group columns named row and group, as the grouping's own columns are (16.2): rows 1 and 2
  # hold the same text in them but name two rules, so they are two scopes; rows 3 and 4 are
  # one scope with the two classes.
  stub <- list(attributes = data.table::data.table(attribute_name = c("group", "row")))
  overrides <- override_rows(
    rule_id = c("rule_a", "rule_b", "rule_c", "rule_c"),
    issue_class = c("source", "harmonization", "source", "harmonization"),
    group = c("x", "x", "z", "z"), row = c("x", "x", "z", "z")
  )
  read <- read_run_inputs(list(overrides = overrides), "inputs", names(run_input_schema()))
  found <- override_conflicts(stub, read$overrides)
  expect_equal(found$detail, report_text(
    "preflight_detail_override_scope_conflict",
    position = input_row(4L), first = report_text("rule_set_position_memory_short", row = 3L),
    class = "harmonization", other_class = "source"
  ))
})

test_that("override_unknown_name warns of each unknown name but those issues hold (D19.72)", {
  testthat::local_mocked_bindings(rule_id_map = function() {
    data.table::data.table(
      old_rule_id = "value_too_high", new_rule_id = "value_above_max", change = "rename",
      package_version = "0.0.0.9014"
    )
  })
  # The rows are numbered from 1. Rows 1, 3, 4, 5 and 9 are findings: 1 names no registered
  # rule; 3 an unknown table, whose column isn't then looked up; 4 a column its table lacks;
  # 5 a column no table has, the row giving no table; 9 a column the survey has, but in another
  # table than the row's (R31). The other rows pass: 2 names a renamed rule; 6 and 7 name the
  # column "*", which issues on a whole table hold; 7, 8, 12 and 13 are overrides of
  # table_unexpected or column_unexpected, whose issues hold the delivery's own names, so any
  # table and column goes with them; 10 names a real column with no table; 11 a pre-flight
  # check's rule ID; 14 has no rule and 15 no table and no column, which a scope may leave out
  # (D19.54).
  overrides <- override_rows(
    rule_id = c(
      "no_such_rule", "value_too_high", rep("value_above_max", 4L), "table_unexpected",
      "column_unexpected", "value_above_max", "value_above_max", "dd_pk_missing",
      "column_unexpected", "table_unexpected", NA, "value_above_max"
    ),
    table_name = c(
      "hauls", "hauls", "nets", "hauls", NA, "hauls", "nets", "hauls", "hauls", NA, "hauls",
      "nets", "nets", "hauls", NA
    ),
    column_name = c(
      "minutes", "minutes", "mesh", "mesh", "mesh", "*", "*", "mesh", "water_body", "minutes",
      "minutes", "mesh", "mesh", "minutes", NA
    )
  )
  found <- input_check("override_unknown_name", list(overrides = overrides))
  detail <- function(case, ...) {
    report_text(paste0("preflight_detail_override_unknown_name_", case), ...)
  }
  expect_equal(found$detail, c(
    detail("rule", position = input_row(1L), rule_id = "no_such_rule"),
    detail("table", position = input_row(3L), table_name = "nets"),
    detail(
      "attribute",
      position = input_row(c(4L, 9L)), table_name = "hauls",
      attribute_name = c("mesh", "water_body")
    ),
    detail("any", position = input_row(5L), attribute_name = "mesh")
  ))
  # From a CSV file, a finding is at the line of its row (R32).
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(override_rows(rule_id = "no_such_rule"), path)
  found <- input_check("override_unknown_name", list(overrides = path))
  expect_equal(found$file, basename(path))
  expect_equal(found$source_cell, paste0(basename(path), ":2"))
  expect_match(found$detail, paste("Line 2 of", basename(path)), fixed = TRUE)
})

test_that("gpq_preflight warns on overrides, stops on one it can't use, and keeps them (16.2)", {
  spec <- fish_spec
  overrides <- override_rows(issue_class = c("source", "harmonization"))
  before <- data.table::copy(overrides)
  expect_warning(
    results <- gpq_preflight(spec, inputs = list(overrides = overrides)),
    class = "gpq_preflight_warning"
  )
  expect_equal(results$outcome[results$rule_id == "override_scope_conflict"], "warn")
  expect_equal(results$outcome[results$rule_id == "input_unparseable"], "pass")
  expect_identical(overrides, before)
  expect_error(
    gpq_preflight(spec, inputs = list(overrides = override_rows(issue_class = "neither"))),
    class = "gpq_preflight_error"
  )
})
