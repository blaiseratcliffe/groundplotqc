# Tests for a conformance run (plan 3.2, 6.1 to 6.3, 9.1 to 9.7; D2.10, D4.1, D7.3, D9.24,
# D16.3 to D16.17). The assembly of record outcomes is tested here with two stand-in rules
# (Layers 1 and 2), so the cascade and the column-level issues are pinned before the real
# Layer 1 and 2 rules; their own tests run through the families.

# The registry with two stand-in conformance rules, test_cast (Layer 1) and test_spell
# (Layer 2), in place of rule_registry() until the calling test ends.
local_stand_in_rules <- function(env = parent.frame()) {
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = c("test_cast", "test_spell"), stage = "conformance", layer = c(1L, 2L),
    check_type = "test", default_severity = "error", default_class = "harmonization",
    inputs = "attributes", strategy = "none", message_id = "preflight_title", rule_version = 1L
  ))
  testthat::local_mocked_bindings(rule_registry = function() registry, .env = env)
  invisible(registry)
}

# A fish run's input, context and state, for record_layer().
assembly <- function(settings = list(), group_cols = character(), tables = fx_fish_clean()) {
  spec <- fx_fish_spec()
  list(
    input = conformance_input(tables, spec, group_cols),
    context = conformance_context(spec, NULL, settings, 0:2),
    state = new_run_state()
  )
}

recorded <- function(state) {
  list(
    results = data.table::rbindlist(state$results, use.names = TRUE),
    issues = data.table::rbindlist(state$issues, use.names = TRUE)
  )
}

run_id <- "20261012T120000Z_abc123"

test_that("gpq_check_conformance refuses arguments it can't run on", {
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  expect_error(gpq_check_conformance(fish, list()), "must be a specification")
  expect_error(gpq_check_conformance(fish$stations, spec), "must be a list of data.frames")
  for (layers in list(6, NA, "0", numeric(), 0.5, matrix(0))) {
    expect_error(gpq_check_conformance(fish, spec, layers = layers), "whole numbers from 0 to 5")
  }
  expect_error(
    gpq_check_conformance(fish, spec, group_cols = "region"),
    "No table of the specification has the group columns region"
  )
  expect_error(
    gpq_check_conformance(fish, spec, group_cols = "status"),
    "have the names of columns the results already have"
  )
  expect_error(
    gpq_check_conformance(fish, spec, problem_unit = list("stations")), "list(table, key_col)",
    fixed = TRUE
  )
  expect_error(
    gpq_check_conformance(fish, spec, problem_unit = list("stations", "gear")),
    "names stations.gear, which the specification doesn't have"
  )
  for (run_id in list("today", c(run_id, run_id), NA_character_, 20261012)) {
    expect_error(gpq_check_conformance(fish, spec, run_id = run_id), "`run_id` must be NULL")
  }
})

test_that("pre-flight runs first: a stop stops the run, a warning reaches the caller (D16.3)", {
  fish <- fx_fish_clean()
  expect_error(
    gpq_check_conformance(fish, fx_planted_spec()),
    class = "gpq_preflight_error"
  )
  expect_warning(
    results <- gpq_check_conformance(fish, fx_fish_gear_twice_spec(), layers = 0),
    class = "gpq_preflight_warning"
  )
  expect_equal(
    results$metadata$preflight,
    suppressWarnings(gpq_preflight(fx_fish_gear_twice_spec()))
  )
})

test_that("a clean run passes Layer 0, one ungrouped row per table or column (D16.8)", {
  fish <- fx_fish_clean()
  results <- gpq_check_conformance(
    fish, fx_fish_spec(),
    group_cols = c("water_body", "gear"), layers = 0, run_id = run_id
  )
  expect_s3_class(results, "gpq_results")
  rows <- results$results
  expect_true(all(rows$status == "pass"))
  expect_equal(nrow(results$issues), 0L)
  expect_equal(as.vector(table(rows$rule_id)[c(
    "table_missing", "table_unexpected", "column_missing", "column_unexpected",
    "column_name_mismatch"
  )]), c(3L, 3L, 15L, 15L, 15L))
  expect_true(all(rows$n_checked == 1L & rows$water_body == "*" & rows$gear == "*"))
  expect_true(all(rows$run_id == run_id))
  expect_equal(results$metadata$run$group_cols, "water_body; gear")
  expect_equal(
    results$metadata$run$report_units,
    paste(sort(unique(fish$stations$water_body), method = "radix"), collapse = "; ")
  )
  expect_equal(results$metadata$inputs$input, paste0("tables:", names(fish)))
  expect_true(all(results$metadata$inputs$file == "in memory"))
  expect_equal(results$metadata$inputs$detail, paste(vapply(fish, nrow, 1L), "rows"))
  expect_equal(nrow(results$metadata$self_test), 0L)
})

test_that("Layer 0 finds missing, extra and misnamed tables and columns (D16.8, D16.14)", {
  fish <- fx_fish_clean()
  fish$stations <- NULL
  fish$notes <- data.frame(note = c("a", "b"))
  hauls <- data.table::copy(fish$hauls)
  data.table::setnames(hauls, "gear", "GEAR ")
  hauls$minutes <- NULL
  hauls$colour <- "red"
  fish$hauls <- hauls
  results <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0)
  open <- results$issues
  expect_true(all(open$level == "column" & open$record_pk == "*" & open$status == "open"))
  found <- paste(open$rule_id, open$table_name, open$column_name)
  expect_setequal(found, c(
    "table_missing stations *", "table_unexpected notes *", "column_missing hauls minutes",
    "column_unexpected hauls colour", "column_name_mismatch hauls gear"
  ))
  expect_equal(open$n_records[open$rule_id == "table_missing"], 0L)
  expect_equal(open$n_records[open$rule_id == "table_unexpected"], 2L)
  expect_equal(open$n_records[open$rule_id == "column_missing"], nrow(hauls))
  expect_equal(open$value[open$rule_id == "column_name_mismatch"], "GEAR ")
  expect_equal(open$expected[open$rule_id == "column_name_mismatch"], "gear")
  # The column rules can't run on a table that wasn't given (D9.24).
  rows <- results$results
  absent <- rows[rows$table_name == "stations" & rows$rule_id != "table_missing"]
  expect_true(all(absent$status == "not_run" & absent$not_run_reason == "no_table"))
  # column_missing and column_name_mismatch for each of its five attributes; a table not
  # given has no columns for column_unexpected to check.
  expect_equal(nrow(absent), 2L * 5L)
  expect_true(all(rows$n_fail <= rows$n_checked))
})

test_that("a unit rule's not_run row is one row of \"*\" counting the table or column (D16.29)", {
  fish <- fx_fish_clean()
  fish$stations <- NULL
  hauls <- data.table::copy(fish$hauls)
  hauls$minutes <- NULL
  fish$hauls <- hauls
  results <- gpq_check_conformance(
    fish, fx_fish_spec(),
    group_cols = c("water_body", "gear"), layers = 0
  )
  rows <- results$results
  not_run <- rows[rows$status == "not_run"]
  # Whatever the records' groups: one row for each rule, table and column, groups "*", and the
  # one table or column it couldn't check counted.
  expect_equal(anyDuplicated(not_run, by = c("rule_id", "table_name", "column_name")), 0L)
  expect_true(all(not_run$water_body == "*" & not_run$gear == "*"))
  expect_true(all(not_run$n_not_evaluated == 1L))
  expect_true(all(not_run$n_checked == 0L & not_run$n_fail == 0L))
  # The column that wasn't found, for the rule that reads its name, and each attribute of the
  # table that wasn't given, for the rules that read its columns.
  expect_equal(
    paste(not_run$rule_id, not_run$table_name, not_run$column_name, not_run$not_run_reason)[
      not_run$table_name == "hauls"
    ],
    "column_name_mismatch hauls minutes no_column"
  )
  absent <- not_run[not_run$table_name == "stations"]
  expect_equal(nrow(absent), 2L * 5L)
  expect_true(all(absent$not_run_reason == "no_table"))
  expect_no_error(new_results(rows, c("water_body", "gear"), rule_registry()))
})

test_that("a rule with unit outcomes records its not_run outcomes as units, others by group", {
  local_stand_in_rules()
  run <- assembly(group_cols = "gear")
  record_layer(list(test_cast = list(
    unit_outcome("test_cast", "catches", "weight", failed = FALSE, n_records = 5L),
    not_run_outcome("test_cast", "hauls", "gear", "no_column"),
    not_run_outcome("test_cast", "stations", "water_body", "no_table")
  )), 1L, run$input, run$context, run$state, run_id)
  record_layer(list(test_spell = list(
    record_outcome("test_spell", "catches", "weight", integer(), character()),
    not_run_outcome("test_spell", "hauls", "gear", "no_column")
  )), 2L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  unit <- done$results[done$results$rule_id == "test_cast" & done$results$status == "not_run"]
  expect_equal(unit$gear, c("*", "*"))
  expect_equal(unit$n_not_evaluated, c(1L, 1L))
  expect_equal(unit$not_run_reason, c("no_column", "no_table"))
  grouped <- done$results[done$results$rule_id == "test_spell" & done$results$status == "not_run"]
  expect_equal(grouped$n_not_evaluated, as.vector(table(run$input$groups$hauls$row_group)))
  expect_no_error(new_results(done$results, "gear", rule_registry()))
})

test_that("a family's outcomes are checked when made and when recorded", {
  expect_error(record_outcome("test_cast", "catches", "weight", 1:2, "a"), "one text for each")
  # Not row numbers: logical, below 1, fractional, missing, text, infinite, beyond the integer
  # range, or a row twice.
  for (fail in list(c(TRUE, FALSE), 0L, 1.5, NA_integer_, "1", Inf, 3e9, c(2L, 2L))) {
    expect_error(
      record_outcome("test_cast", "catches", "weight", fail, rep("a", length(fail))),
      "must be row numbers"
    )
  }
  expect_no_error(record_outcome("test_cast", "catches", "weight", integer(), character()))
  local_stand_in_rules()
  run <- assembly()
  mystery <- list(kind = "mystery", table_name = "catches", column_name = "weight")
  expect_error(
    record_layer(
      list(test_cast = list(mystery)), 1L, run$input, run$context, run$state, run_id
    ),
    "test_cast returned an outcome of kind \"mystery\""
  )
})

test_that("rule-set rows give severity and enabled, the most specific winning (D16.6, D16.7)", {
  withr::local_options(groundplotqc.severity = NULL)
  fish <- fx_fish_clean()
  fish$hauls$colour <- "red"
  fish$catches$size <- 1
  fish$notes <- data.frame(note = "a")
  rules <- list(rules = data.frame(
    rule_id = c("column_unexpected", "column_unexpected", "table_unexpected"),
    table_name = c("*", "catches", "*"), attribute_name = "*",
    severity = c("error", NA, NA), class = NA_character_, enabled = c(TRUE, TRUE, FALSE)
  ))
  results <- gpq_check_conformance(fish, fx_fish_spec(), rules = rules, layers = 0)
  extra <- results$issues[results$issues$rule_id == "column_unexpected"]
  # The catches row gives no severity, so the "*" row's error holds there too.
  expect_equal(extra$severity, c("error", "error"))
  expect_false("table_unexpected" %in% results$results$rule_id)
  expect_false("table_unexpected" %in% results$metadata$self_test$rule_id)
  # A specific row's severity wins over the "*" row's; the option is below the rule set and
  # the argument above it.
  rules$rules$severity[[2L]] <- "warning"
  results <- gpq_check_conformance(fish, fx_fish_spec(), rules = rules, layers = 0)
  extra <- results$issues[results$issues$rule_id == "column_unexpected"]
  expect_equal(extra$severity[extra$table_name == "catches"], "warning")
  expect_equal(extra$severity[extra$table_name == "hauls"], "error")
  withr::local_options(groundplotqc.severity = c(column_unexpected = "warning"))
  results <- gpq_check_conformance(fish, fx_fish_spec(), rules = rules, layers = 0)
  expect_equal(
    results$issues$severity[results$issues$rule_id == "column_unexpected"], c("error", "warning")
  )
  results <- gpq_check_conformance(
    fish, fx_fish_spec(),
    rules = rules, layers = 0, settings = list(severity = c(column_unexpected = "error"))
  )
  expect_equal(
    results$issues$severity[results$issues$rule_id == "column_unexpected"], c("error", "error")
  )
})

test_that("the self-test lists a registered rule its family didn't report (9.7, D16.17)", {
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = "test_forgotten", stage = "conformance", layer = 0L, check_type = "test",
    default_severity = "error", default_class = "harmonization", inputs = "attributes",
    strategy = "none", message_id = "preflight_title", rule_version = 1L
  ))
  testthat::local_mocked_bindings(rule_registry = function() registry)
  results <- gpq_check_conformance(fx_fish_clean(), fx_fish_spec(), layers = 0)
  expect_equal(results$metadata$self_test$rule_id, "test_forgotten")
  expect_equal(results$metadata$self_test$layer, 0L)
})

test_that("group columns named like the engine's own variables are read as columns", {
  for (name in c("whole", "rows", "row_group")) {
    spec <- fx_fish_spec()
    attributes <- data.table::copy(spec$attributes)
    renamed <- which(attributes$attribute_name == "station_name")
    data.table::set(attributes, i = renamed, j = "attribute_name", value = name)
    spec$attributes <- attributes
    fish <- fx_fish_clean()
    stations <- data.table::copy(fish$stations)
    data.table::setnames(stations, "station_name", name)
    fish$stations <- stations
    # Masked records, one per record and a whole column's collapsed (D16.10).
    catches <- data.table::copy(fish$catches)
    catches$weight <- c("abc", "def", as.character(catches$weight[-(1:2)]))
    catches$fish_count <- rep("zz", nrow(catches))
    fish$catches <- catches
    results <- gpq_check_conformance(
      fish, spec,
      group_cols = name, settings = list(column_level_min_n = 3L)
    )
    masked <- results$issues[results$issues$status == "not_evaluated"]
    expect_setequal(masked[[name]], fish$stations[[name]])
  }
})

test_that("a layer with no registered rule runs nothing", {
  results <- gpq_check_conformance(fx_fish_clean(), fx_fish_spec(), layers = 3:5)
  expect_equal(nrow(results$results), 0L)
  expect_equal(nrow(results$metadata$self_test), 0L)
})

test_that("a given run ID is kept, and the run changes none of its tables (16.2)", {
  fish <- fx_fish_clean()
  before <- lapply(fish, data.table::copy)
  results <- gpq_check_conformance(fish, fx_fish_spec(), run_id = run_id)
  expect_equal(results$metadata$run$run_id, run_id)
  for (table in names(fish)) {
    expect_identical(fish[[table]], before[[table]], info = table)
    expect_null(data.table::indices(fish[[table]]), info = table)
  }
  made <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0)
  expect_match(made$metadata$run$run_id, run_id_pattern)
})

test_that("failures are record issues, or one column-level issue when every record fails (6.3)", {
  local_stand_in_rules()
  catches <- fx_fish_clean()$catches
  run <- assembly(settings = list(column_level_min_n = 3L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", 1:3, c("a", "b", "b"), "double"
  ))), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  expect_equal(done$results$n_checked, nrow(catches))
  expect_equal(done$results$n_fail, 3L)
  expect_equal(done$results$n_distinct_fail_values, 2L)
  expect_equal(done$results$examples, "a; b")
  expect_equal(done$results$status, "fail")
  expect_equal(done$issues$level, rep("record", 3L))
  expect_equal(done$issues$row_number, 1:3)
  expect_equal(done$issues$record_pk, catches$catch_id[1:3])
  expect_equal(done$issues$pk_positional, rep(FALSE, 3L))
  expect_equal(done$issues$issue_id, 1:3)
  # Every record failing, at least column_level_min_n of them: one column-level issue.
  run <- assembly(settings = list(column_level_min_n = 3L))
  all_rows <- seq_len(nrow(catches))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", all_rows, as.character(all_rows)
  ))), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  expect_equal(done$issues$level, "column")
  expect_equal(done$issues$record_pk, "*")
  expect_equal(done$issues$n_records, nrow(catches))
  expect_equal(done$results$n_fail, nrow(catches))
  # Fewer than column_level_min_n: one issue per record still.
  run <- assembly(settings = list(column_level_min_n = 10L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", all_rows, as.character(all_rows)
  ))), 1L, run$input, run$context, run$state, run_id)
  expect_equal(nrow(recorded(run$state)$issues), nrow(catches))
})

test_that("examples are capped, distinct and in row order; NA isn't one", {
  local_stand_in_rules()
  run <- assembly(settings = list(example_cap = 2L, column_level_min_n = 100L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", 1:5, c("c", NA, "a", "c", "b")
  ))), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  expect_equal(done$results$examples, "c; a")
  expect_equal(done$results$n_distinct_fail_values, 4L)
})

test_that("a value that failed an earlier layer is not evaluated, with its root (6.2)", {
  local_stand_in_rules()
  run <- assembly(settings = list(column_level_min_n = 100L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", 1:2, c("x", "y")
  ))), 1L, run$input, run$context, run$state, run_id)
  record_layer(list(test_spell = list(
    record_outcome("test_spell", "catches", "weight", 2:4, c("y", "z", "z")),
    record_outcome("test_spell", "catches", "species", 1L, "q")
  )), 2L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  # Rows 1 and 2 failed Layer 1, so the Layer 2 check evaluates neither, failing or not.
  spell <- done$results[done$results$rule_id == "test_spell" & done$results$column_name == "weight"]
  expect_equal(spell$n_checked, nrow(fx_fish_clean()$catches) - 2L)
  expect_equal(spell$n_fail, 2L)
  expect_equal(spell$n_not_evaluated, 2L)
  masked <- done$issues[done$issues$status == "not_evaluated"]
  expect_equal(masked$row_number, 1:2)
  expect_equal(masked$root_issue_id, done$issues$issue_id[done$issues$rule_id == "test_cast"])
  expect_equal(masked$value, value_text(fx_fish_clean()$catches$weight[1:2]))
  # Another column of the same record isn't masked.
  other <- done$issues[done$issues$column_name == "species"]
  expect_equal(other$status, "open")
  # The assembled tables pass the results model's checks.
  expect_no_error(new_results(done$results, registry = rule_registry()))
  expect_no_error(new_issues(done$issues, registry = rule_registry()))
})

test_that("masked records sharing one root collapse into one column-level issue (D16.10)", {
  local_stand_in_rules()
  n <- nrow(fx_fish_clean()$catches)
  run <- assembly(settings = list(column_level_min_n = 3L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", seq_len(n), rep("x", n)
  ))), 1L, run$input, run$context, run$state, run_id)
  record_layer(list(test_spell = list(record_outcome(
    "test_spell", "catches", "weight", 1L, "x"
  ))), 2L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  masked <- done$issues[done$issues$status == "not_evaluated"]
  expect_equal(masked$level, "column")
  expect_equal(masked$n_records, n)
  expect_equal(masked$root_issue_id, done$issues$issue_id[done$issues$rule_id == "test_cast"])
  spell <- done$results[done$results$rule_id == "test_spell"]
  expect_equal(c(spell$n_checked, spell$n_fail, spell$n_not_evaluated), c(0L, 0L, n))
  # Fewer masked records than column_level_min_n stay one issue each.
  run <- assembly(settings = list(column_level_min_n = 3L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", 1:2, c("x", "y")
  ))), 1L, run$input, run$context, run$state, run_id)
  record_layer(list(test_spell = list(record_outcome(
    "test_spell", "catches", "weight", integer(), character()
  ))), 2L, run$input, run$context, run$state, run_id)
  masked <- recorded(run$state)$issues
  masked <- masked[masked$status == "not_evaluated"]
  expect_equal(masked$level, c("record", "record"))
  expect_equal(masked$root_issue_id, 1:2)
})

test_that("a failure of the same layer masks nothing (6.2)", {
  local_stand_in_rules()
  run <- assembly(settings = list(column_level_min_n = 100L))
  record_layer(list(
    test_spell = list(record_outcome("test_spell", "catches", "weight", 1L, "x")),
    blank_string = list(record_outcome("blank_string", "catches", "weight", 1L, "x"))
  ), 2L, run$input, run$context, run$state, run_id)
  expect_equal(recorded(run$state)$issues$status, c("open", "open"))
})

test_that("column-level failures and results are per group (6.3, D7.5)", {
  local_stand_in_rules()
  fish <- fx_fish_clean()
  run <- assembly(settings = list(column_level_min_n = 2L), group_cols = "gear", tables = fish)
  grouping <- run$input$groups$catches
  first_group <- which(grouping$row_group == 1L)
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", first_group, rep("x", length(first_group))
  ))), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  expect_equal(done$results$gear, grouping$values$gear)
  expect_equal(done$results$n_fail, c(length(first_group), rep(0L, nrow(grouping$values) - 1L)))
  expect_equal(done$issues$level, "column")
  expect_equal(done$issues$gear, grouping$values$gear[[1L]])
  expect_no_error(new_issues(done$issues, group_cols = "gear", registry = rule_registry()))
})

test_that("an empty table checks nothing in one row; not_run counts by group (D16.17, D9.24)", {
  local_stand_in_rules()
  fish <- fx_fish_clean()
  fish$catches <- fish$catches[0L]
  run <- assembly(group_cols = "gear", tables = fish)
  record_layer(list(test_cast = list(
    record_outcome("test_cast", "catches", "weight", integer(), character()),
    not_run_outcome("test_cast", "hauls", "gear", "no_column")
  )), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  empty <- done$results[done$results$table_name == "catches"]
  expect_equal(c(empty$gear, empty$status), c("*", "pass"))
  expect_equal(empty$n_checked, 0L)
  not_run <- done$results[done$results$table_name == "hauls"]
  hauls <- run$input$groups$hauls
  expect_equal(not_run$n_not_evaluated, as.vector(table(hauls$row_group)))
  expect_true(all(not_run$status == "not_run" & not_run$not_run_reason == "no_column"))
})

test_that("a rule with nothing in scope or off everywhere: no rows, not expected (D16.7, D16.17)", {
  local_stand_in_rules()
  run <- assembly()
  record_layer(list(test_cast = list()), 1L, run$input, run$context, run$state, run_id)
  expect_equal(run$state$not_expected, "test_cast")
  rules <- validate_rule_set(list(rules = data.frame(
    rule_id = "test_spell", table_name = "*", attribute_name = "*", severity = NA,
    class = NA, enabled = FALSE
  )))
  spec <- fx_fish_spec()
  run$context <- conformance_context(spec, rules, list(), 0:2)
  record_layer(list(test_spell = list(record_outcome(
    "test_spell", "catches", "weight", 1L, "x"
  ))), 2L, run$input, run$context, run$state, run_id)
  expect_equal(run$state$not_expected, c("test_cast", "test_spell"))
  expect_length(run$state$results, 0L)
})
