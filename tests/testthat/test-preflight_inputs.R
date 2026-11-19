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
  # A name outside the list: thresholds, which joins at M13. The names taken are listed as the
  # schema gives them, the first three M6's.
  taken <- names(run_input_schema())
  expect_identical(taken[1:3], c("overrides", "lineage", "pipeline_fill"))
  n <- length(taken)
  expect_error(
    gpq_preflight(spec, inputs = list(thresholds = override_rows())),
    sprintf(
      "`inputs` names tables it doesn't take: thresholds. It takes %s and %s.",
      paste(taken[-n], collapse = ", "), taken[[n]]
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
  # From a CSV file, the finding is at its header, line 1.
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(lineage, path)
  found <- input_check("input_unparseable", list(lineage = path))
  expect_equal(c(found$file, found$source_cell), c(basename(path), paste0(basename(path), ":1")))
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
  # A blank class is named as blank.
  found <- input_check("input_unparseable", list(overrides = override_rows(issue_class = NA)))
  expect_equal(nrow(found), 1L)
  expect_match(found$detail, "(blank)", fixed = TRUE)
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

test_that("input_unparseable reads previous for its form, folder and columns only (D23.33)", {
  # A run of the fish survey, its group column water_body, written to its folder.
  run <- gpq_check_conformance(fx_fish_clean(), fish_spec, group_cols = "water_body", layers = 0)
  dir <- withr::local_tempdir()
  run_dir <- gpq_write_results(run, dir)
  issues_csv <- file.path(run_dir, "tables", "issues.csv")
  # The folder, its issues file and its issues in memory pass.
  for (previous in list(run_dir, issues_csv, run$issues)) {
    expect_equal(nrow(input_check("input_unparseable", list(previous = previous))), 0L)
  }
  # Only the issues' columns are read: a file whose rows don't read cleanly passes here, and
  # stops the comparison, which reads them (D23.46).
  broken <- file.path(dir, "broken.csv")
  writeLines(c(readLines(issues_csv, n = 1L), "\"1\",\"2\",\"3\""), broken)
  expect_equal(nrow(input_check("input_unparseable", list(previous = broken))), 0L)
  expect_error(gpq_compare_runs(run$issues, broken))
  # Columns: one the comparison reads missing, one no issues table has, and a group column and
  # an issues column of the earlier run's own taken.
  issues <- as.data.frame(run$issues)
  issues$first_seen_run <- NULL
  issues$colour <- character()
  found <- input_check("input_unparseable", list(previous = issues))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_both",
    input = "previous", missing = "first_seen_run", extra = "colour"
  ))
  # A folder that isn't a completed run's: none of a run's files, a stopped run's, a run's with
  # no results file; each named once, at the folder.
  empty <- file.path(dir, "not_a_run")
  dir.create(empty)
  found <- input_check("input_unparseable", list(previous = empty))
  expect_equal(c(found$file, found$detail), c("not_a_run", report_text(
    "preflight_detail_input_unparseable_folder",
    input = "previous", folder = "not_a_run", file = "metadata/run.csv"
  )))
  stopped <- file.path(dir, "stopped")
  stopped_run <- data.table::copy(run$metadata$run)
  data.table::set(stopped_run, j = "outcome", value = "stopped_preflight")
  write_run_metadata(list(run = stopped_run), stopped)
  dir.create(file.path(stopped, "tables"))
  file.copy(issues_csv, file.path(stopped, "tables"))
  file.copy(file.path(run_dir, "tables", "results.csv"), file.path(stopped, "tables"))
  found <- input_check("input_unparseable", list(previous = stopped))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_stopped",
    input = "previous", folder = "stopped"
  ))
  unlink(file.path(run_dir, "tables", "results.csv"))
  found <- input_check("input_unparseable", list(previous = run_dir))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_folder",
    input = "previous", folder = basename(run_dir), file = "tables/results.csv"
  ))
  # Neither a folder, a data.frame nor a CSV file: a caller's error.
  form <- "`previous` must be a run folder, a data.frame or the path of an existing .csv file."
  for (bad in list(42, file.path(dir, "absent.csv"), list(issues))) {
    expect_error(input_check("input_unparseable", list(previous = bad)), form, fixed = TRUE)
  }
})

test_that("input_unparseable names an acknowledgement's columns, decision and run scope (D23.26)", {
  entries <- function(decision = "verified", run_scope = "package", ...) {
    data.frame(
      fingerprint = "value_above_max|hauls|minutes|pk:H2",
      fingerprint_pk = "value_above_max|hauls|minutes|pk:H2", rule_id = "value_above_max",
      table_name = "hauls", column_name = "minutes", provider_key = NA,
      acknowledged_value = "900", decision = decision, who = "A. Reviewer",
      when = "2026-11-17", why = "Checked.", run_scope = run_scope, ...
    )
  }
  expect_equal(nrow(input_check("input_unparseable", list(acknowledgements = entries()))), 0L)
  # water_body, an attribute of the survey, is a group column; colour isn't.
  wrong <- entries(water_body = "LK", colour = "red")
  wrong$why <- NULL
  found <- input_check("input_unparseable", list(acknowledgements = wrong))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_both",
    input = "acknowledgements", missing = "why", extra = "colour"
  ))
  # A decision and a run scope outside their values, each by its row, a blank one named so.
  register <- entries(
    decision = c("verified", "fine", "verified"), run_scope = c("package", "package", NA)
  )
  found <- input_check("input_unparseable", list(acknowledgements = register))
  at_row <- function(row) {
    report_text("input_position_memory", row = row, input = "acknowledgements")
  }
  expect_equal(found$detail, c(
    report_text(
      "preflight_detail_input_unparseable_decision",
      position = at_row(2L), value = "fine"
    ),
    report_text(
      "preflight_detail_input_unparseable_run_scope",
      position = at_row(3L), value = NA
    )
  ))
  expect_match(found$detail[[2L]], "(blank)", fixed = TRUE)
  # From a CSV file, each at its line.
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(register, path)
  found <- input_check("input_unparseable", list(acknowledgements = path))
  expect_equal(found$source_cell, paste0(basename(path), c(":3", ":4")))
  # Without a column of its own, the decision and run scope wait.
  register$why <- NULL
  found <- input_check("input_unparseable", list(acknowledgements = register))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_missing",
    input = "acknowledgements", missing = "why"
  ))
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
  # (D19.54). 16 to 18 give no table and a column known without one: "*" (16), and the
  # delivery's own names of a column_unexpected (17) and a table_unexpected (18) override.
  overrides <- override_rows(
    rule_id = c(
      "no_such_rule", "value_too_high", rep("value_above_max", 4L), "table_unexpected",
      "column_unexpected", "value_above_max", "value_above_max", "dd_pk_missing",
      "column_unexpected", "table_unexpected", NA, "value_above_max", "value_above_max",
      "column_unexpected", "table_unexpected"
    ),
    table_name = c(
      "hauls", "hauls", "nets", "hauls", NA, "hauls", "nets", "hauls", "hauls", NA, "hauls",
      "nets", "nets", "hauls", NA, NA, NA, NA
    ),
    column_name = c(
      "minutes", "minutes", "mesh", "mesh", "mesh", "*", "*", "mesh", "water_body", "minutes",
      "minutes", "mesh", "mesh", "minutes", NA, "*", "mesh", "*"
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

test_that("override_unknown_name looks a column up in its row's table as a pair (M6 m59)", {
  # Table and column are matched together, not as text joined by a separator: here table "a"
  # and column "b\rc" would read as table "a\rb"'s column "c" if the two were pasted with "\r".
  # Row 2 names that column in its own table and row 3 a column of "a"; neither is a finding.
  stub <- list(attributes = data.table::data.table(
    table_name = c("a", "a\rb"), attribute_name = c("x", "c")
  ))
  overrides <- override_rows(table_name = c("a", "a\rb", "a"), column_name = c("b\rc", "c", "x"))
  found <- preflight_check_functions()[["override_unknown_name"]](
    stub, preflight_context(inputs = list(overrides = overrides))
  )
  expect_equal(found$detail, report_text(
    "preflight_detail_override_unknown_name_attribute",
    position = input_row(1L), table_name = "a", attribute_name = "b\rc"
  ))
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

test_that("input_unparseable checks the attribution map's columns, its key taken (D23.27)", {
  # water_body is an attribute of the survey, so the map's key column; colour isn't.
  map <- data.frame(
    water_body = c("LK", "LK", "*"), table_name = c("hauls", "*", "*"),
    attribute_name = c("minutes", "*", "*"), script_file = "lake.R", script_block = NA,
    script_commit = NA, note = NA
  )
  expect_equal(nrow(input_check("input_unparseable", list(attribution_map = map))), 0L)
  odd <- map
  odd$colour <- "red"
  odd$note <- NULL
  found <- input_check("input_unparseable", list(attribution_map = odd))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_both",
    input = "attribution_map", missing = "note", extra = "colour"
  ))
  # Without a column of its own, the rows wait: a blank key cell gives only the missing column.
  odd$colour <- NULL
  odd$table_name[[1L]] <- NA
  found <- input_check("input_unparseable", list(attribution_map = odd))
  expect_equal(found$detail, report_text(
    "preflight_detail_input_unparseable_missing",
    input = "attribution_map", missing = "note"
  ))
  # A map with no rows, in memory or as a header-only file, gives no finding.
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(map[0, ], path)
  for (empty in list(map[0, ], path)) {
    expect_equal(nrow(input_check("input_unparseable", list(attribution_map = empty))), 0L)
  }
})

test_that("input_unparseable names a map row with a blank key cell or a repeated key (D23.39)", {
  # Rows 2 and 4 leave a cell of their key blank; 5 repeats 1's key and 6 repeats 3's; 7 has
  # 1's table and attribute for another key value, so it repeats nothing.
  map <- data.frame(
    water_body = c("LK", "LK", "*", NA, "LK", "*", "RV"),
    table_name = c("hauls", NA, "*", "hauls", "hauls", "*", "hauls"),
    attribute_name = c("minutes", "gear", "*", "gear", "minutes", "*", "minutes"),
    script_file = "lake.R", script_block = NA, script_commit = NA, note = NA
  )
  found <- input_check("input_unparseable", list(attribution_map = map))
  row <- function(r) report_text("input_position_memory", row = r, input = "attribution_map")
  short <- function(r) report_text("rule_set_position_memory_short", row = r)
  columns <- "table_name, attribute_name, water_body"
  expect_equal(found$detail, c(
    report_text(
      "preflight_detail_input_unparseable_map_blank",
      position = row(c(2L, 4L)), columns = columns
    ),
    report_text(
      "preflight_detail_input_unparseable_map_repeat",
      position = row(c(5L, 6L)), first = short(c(1L, 3L)), columns = columns
    )
  ))
  # From a CSV file, each at its line; gpq_preflight() stops on them.
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(map, path)
  found <- input_check("input_unparseable", list(attribution_map = path))
  expect_equal(found$source_cell, paste0(basename(path), c(":3", ":5", ":6", ":7")))
  expect_match(found$detail[[3L]], paste("Line 6 of", basename(path)), fixed = TRUE)
  expect_error(
    gpq_preflight(fish_spec, inputs = list(attribution_map = path)),
    class = "gpq_preflight_error"
  )
  expect_no_error(gpq_preflight(fish_spec, inputs = list(attribution_map = map[c(1L, 3L, 7L), ])))
})
