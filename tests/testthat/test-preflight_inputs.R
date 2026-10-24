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
  for (name in c("input_unparseable", "override_scope_conflict", "override_unknown_name")) {
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
  read <- read_run_inputs(list(pipeline_fill = fill), "inputs", names(run_input_schema()))
  expect_identical(read$pipeline_fill$data$written_value, c("-99", NA))
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
})

test_that("override_unknown_name warns of each unknown name but those issues hold (D19.72)", {
  testthat::local_mocked_bindings(rule_id_map = function() {
    data.table::data.table(
      old_rule_id = "value_too_high", new_rule_id = "value_above_max", change = "rename",
      package_version = "0.0.0.9014"
    )
  })
  # 1 names no registered rule; 2 a renamed one; 3 an unknown table, whose column isn't then
  # looked up; 4 a column its table lacks; 5 a column no table has. Issues hold the names of
  # 6 to 8, which pass: a column "*", and the delivery's own names in an override of
  # table_unexpected or column_unexpected.
  overrides <- override_rows(
    rule_id = c(
      "no_such_rule", "value_too_high", rep("value_above_max", 4L), "table_unexpected",
      "column_unexpected"
    ),
    table_name = c("hauls", "hauls", "nets", "hauls", NA, "hauls", "nets", "hauls"),
    column_name = c("minutes", "minutes", "mesh", "mesh", "mesh", "*", "*", "mesh")
  )
  found <- input_check("override_unknown_name", list(overrides = overrides))
  detail <- function(case, ...) {
    report_text(paste0("preflight_detail_override_unknown_name_", case), ...)
  }
  expect_equal(found$detail, c(
    detail("rule", position = input_row(1L), rule_id = "no_such_rule"),
    detail("table", position = input_row(3L), table_name = "nets"),
    detail("attribute", position = input_row(4L), table_name = "hauls", attribute_name = "mesh"),
    detail("any", position = input_row(5L), attribute_name = "mesh")
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
