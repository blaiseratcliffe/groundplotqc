# Tests for the results model: the three tables' schemas, the constructors, the checks of
# each table and the gpq_results object (plan 9.1 to 9.4, 9.7; D4.1, D7.3, D7.5, D8.16,
# D9.21, D9.24, D15.9).

# The schema of one of fx_results()'s tables: group columns unit and area, key column plot.
fish_schema <- function(name) {
  results_schema(c("unit", "area"), "plot")[[name]]
}

test_that("results_schema() puts the caller's columns in their places, as text (9.2, 9.3)", {
  schema <- results_schema(c("unit", "area"), "plot")
  results <- names(schema$results)
  expect_equal(results[match("column_name", results) + 1:2], c("unit", "area"))
  issues <- names(schema$issues)
  expect_equal(issues[match("provider_key_status", issues) + 1:3], c("unit", "area", "plot"))
  # D20.18's columns: n_unchecked beside the other counts, reaches_other_unit after the
  # caller's columns (9.2, 9.3).
  expect_equal(results[match("n_not_evaluated", results) + 1L], "n_unchecked")
  expect_equal(
    issues[match("plot", issues) + 0:2], c("plot", "reaches_other_unit", "root_issue_id")
  )
  expect_equal(results_schema()$issues[["reaches_other_unit"]], "logical")
  expect_equal(unname(schema$issues[c("unit", "area", "plot")]), rep("character", 3L))
  expect_false(any(c("unit", "area", "plot") %in% names(schema$corrections)))
  # A key column that is also a group column comes once.
  expect_equal(sum(names(results_schema("plot", "plot")$issues) == "plot"), 1L)
  # No caller columns: the fixed columns only.
  expect_equal(results_schema()$results[["layer"]], "integer")
  expect_equal(results_schema()$issues[["pk_positional"]], "logical")
})

test_that("a caller's column named like a fixed one, repeated or blank is refused", {
  expect_error(results_schema("status"), "have the names of columns the results already have")
  expect_error(results_schema(key_col = "fingerprint"), "have the names")
  # Nor a name a report adds: the routed results' remainder, the provider CSV's message, or
  # one starting with key_, the CSV's key parts (D20.76), which has its own error (D20.95).
  expect_error(results_schema("message"), "have the names")
  expect_error(results_schema(key_col = "n_not_evaluated_own"), "have the names")
  expect_error(results_schema("key_jurisdiction"), "start with \"key_\"", fixed = TRUE)
  expect_error(results_schema(key_col = "key_plot"), "key_plot start with", fixed = TRUE)
  # A key column that is also a group column is named once.
  expect_error(
    results_schema(c("unit", "key_site"), "key_site"), "columns key_site start with",
    fixed = TRUE
  )
  expect_no_error(results_schema(c("plot_key", "keyed")))
  expect_error(results_schema(c("unit", "unit")), "each given once")
  expect_error(results_schema(c("unit", " ")), "each given once")
  expect_error(results_schema(NA_character_), "each given once")
  expect_error(results_schema(key_col = c("a", "b")), "one column name")
})

test_that("a group column named with \"; \" is refused, a key column isn't (D15.39, D15.40)", {
  # run.csv's group_cols joins the names with "; " (R11, D9.29), so a name holding it could
  # not be told from two names when the run is read back.
  expect_error(results_schema("a; b"), "have \"; \" in their names")
  expect_error(
    results_schema(c("unit", "x; y", "z; w")), "\"x; y\", \"z; w\" have \"; \" in their names",
    fixed = TRUE
  )
  expect_error(new_results(NULL, group_cols = "a; b"), "have \"; \" in their names")
  # The key column isn't recorded in group_cols, so its name may hold the separator (D15.40).
  kept <- results_schema("unit", "plot; 2")
  expect_true("plot; 2" %in% names(kept$issues))
  # A semicolon without the space, or a space without the semicolon, is no separator.
  schema <- results_schema(c("a;b", "c ;d", "e ,f"), "g;h")
  expect_true(all(c("a;b", "c ;d", "e ,f", "g;h") %in% names(schema$issues)))
})

test_that("the constructors complete a table with NA", {
  local_results_registry()
  given <- data.table::data.table(
    run_id = "r", run_scope = "package", rule_id = "value_odd", rule_fingerprint = "f",
    stage = "conformance", layer = 2L, severity = "error", table_name = "t",
    column_name = "c", unit = "u", n_checked = 2L, n_fail = 0L, n_not_evaluated = 0L,
    n_unchecked = 0L, reason_code = "*", n_distinct_fail_values = 0L, n_easy_fix = 0L,
    status = "pass",
    message_id = "m"
  )
  table <- new_results(given, group_cols = "unit")
  expect_equal(names(table), names(results_schema("unit")$results))
  expect_true(is.na(table$examples) && is.na(table$not_run_reason))
  expect_equal(nrow(new_results()), 0L)
  expect_equal(names(new_issues(key_col = "plot")), names(results_schema(key_col = "plot")$issues))
  expect_equal(nrow(new_corrections()), 0L)
  expect_error(new_corrections(list(extra = 1L)), "has no columns extra")
  # A list with values and no names has no columns to complete: refused, not read as empty.
  expect_error(new_corrections(list(1L)), "The corrections table's columns must be named")
  expect_error(new_issues(list("a", "b")), "The issues table's columns must be named")
  expect_equal(nrow(new_corrections(list())), 0L)
})

test_that("the constructors share no column with the input, in whatever form it is given", {
  local_results_registry()
  object <- fx_results()
  groups <- c("unit", "area")
  builders <- list(
    results = list(table = object$results, build = function(x) new_results(x, groups)),
    issues = list(table = object$issues, build = function(x) new_issues(x, groups, "plot")),
    corrections = list(table = object$corrections, build = new_corrections)
  )
  # A value in the first row that differs from the one there, so that a change shows.
  change <- function(table) {
    for (column in names(table)) {
      value <- table[[column]]
      data.table::set(table, i = 1L, j = column, value = switch(class(value)[[1L]],
        character = "changed",
        integer = -7L,
        logical = !(value[[1L]] %in% TRUE)
      ))
    }
  }
  for (name in names(builders)) {
    for (form in c("list", "data.frame", "data.table")) {
      given <- data.table::copy(builders[[name]]$table)
      given <- switch(form,
        list = as.list(given),
        data.frame = as.data.frame(given),
        data.table = given
      )
      before <- data.table::copy(given)
      table <- builders[[name]]$build(given)
      # No column of the result is the vector of the caller's column.
      for (column in intersect(names(table), names(given))) {
        expect_false(data.table::address(table[[column]]) == data.table::address(given[[column]]))
      }
      # A change by reference to the result leaves the input as it was.
      change(table)
      expect_identical(given, before)
      # A change by reference to the input leaves the result as it was.
      table <- builders[[name]]$build(given)
      kept <- data.table::copy(table)
      change(data.table::setDT(given))
      expect_identical(table, kept)
    }
  }
})

test_that("a table of the wrong columns or classes, or with a blank required cell, is refused", {
  local_results_registry()
  object <- fx_results()
  results <- object$results
  expect_error(
    validate_schema(as.data.frame(results), "results", fish_schema("results")),
    "must be a data.table"
  )
  expect_error(
    validate_schema(results[, -1L], "results", fish_schema("results")),
    "has the columns run_id, run_scope"
  )
  wrong <- data.table::copy(results)
  data.table::set(wrong, j = "layer", value = as.double(wrong$layer))
  expect_error(validate_schema(wrong, "results", fish_schema("results")), "wrong class: layer")
  for (column in c("run_id", "unit", "n_fail")) {
    blank <- data.table::copy(results)
    data.table::set(blank, i = 1L, j = column, value = NA)
    expect_error(
      validate_schema(blank, "results", fish_schema("results")),
      paste("has blank cells in", column)
    )
  }
  empty <- data.table::copy(results)
  data.table::set(empty, i = 1L, j = "table_name", value = "")
  expect_error(validate_schema(empty, "results", fish_schema("results")), "blank cells in table")
  # A value column may hold an empty string: blank_string's failing value is one.
  expect_equal(object$issues$value[[1L]], "")
})

test_that("coded columns hold their values only (9.2, 9.3; D8.12, D9.24)", {
  local_results_registry()
  object <- fx_results()
  cases <- list(
    list("results", "severity", "fatal"), list("results", "status", "skipped"),
    list("results", "not_run_reason", "no_time"), list("issues", "level", "dataset"),
    list("issues", "status", "closed"), list("issues", "issue_class", "both"),
    list("issues", "provider_key_status", "guessed"), list("issues", "change_cause", "luck")
  )
  for (case in cases) {
    table <- data.table::copy(object[[case[[1L]]]])
    data.table::set(table, i = 2L, j = case[[2L]], value = case[[3L]])
    expect_error(
      validate_schema(table, case[[1L]], fish_schema(case[[1L]])),
      paste0("values ", case[[3L]], " outside ", case[[2L]])
    )
  }
})

test_that("results and issues name only declared rules that aren't pre-flight checks (9.7)", {
  local_results_registry()
  object <- fx_results()
  for (name in c("results", "issues")) {
    table <- data.table::copy(object[[name]])
    data.table::set(table, i = 1L, j = "rule_id", value = "no_such_rule")
    expect_error(validate_schema(table, name, fish_schema(name)), "doesn't declare: no_such_rule")
    data.table::set(table, i = 1L, j = "rule_id", value = "dd_pk_missing")
    expect_error(validate_schema(table, name, fish_schema(name)), "doesn't declare: dd_pk_missing")
    table <- data.table::copy(object[[name]])
    data.table::set(table, i = 1L, j = "layer", value = 3L)
    expect_error(validate_schema(table, name, fish_schema(name)), "stage or layer")
  }
})

test_that("results rows agree with their status and are one per rule, place, group and reason", {
  local_results_registry()
  results <- fx_results()$results
  check <- function(i, column, value) {
    table <- data.table::copy(results)
    data.table::set(table, i = i, j = column, value = value)
    validate_schema(table, "results", fish_schema("results"))
  }
  expect_error(check(1L, "not_run_reason", "no_table"), "gives a not-run reason")
  expect_error(check(2L, "not_run_reason", NA_character_), "gives a not-run reason")
  expect_error(check(2L, "n_checked", 3L), "counts checked or failing records on a not_run row")
  # A fail without failures has no distinct failing values either, or the bound stops it.
  expect_error(
    check(1L, c("n_fail", "n_distinct_fail_values"), list(0L, 0L)),
    "a pass with failures or a fail without one"
  )
  expect_error(check(1L, "n_easy_fix", -1L), "negative counts")
  # The bounds between the counts (m24, D16.8).
  expect_error(check(1L, "n_fail", 9L), "more failing records than records checked")
  expect_error(check(1L, "n_easy_fix", 4L), "more easy fixes or distinct failing values")
  expect_error(check(1L, "n_distinct_fail_values", 4L), "more easy fixes or distinct failing")
  expect_no_error(check(1L, "n_easy_fix", 3L))
  # A conformance rule's reason code is "*" (R3); a plausibility rule's is its own.
  expect_error(check(1L, "reason_code", "value_jump"), "conformance row a reason code")
  expect_no_error(check(3L, "reason_code", "other_reason"))
  twice <- rbind(results, results[1L])
  expect_error(validate_schema(twice, "results", fish_schema("results")), "more than one row")
  # The same rule and place in another group is another row.
  other <- results[1L]
  data.table::set(other, j = "area", value = "a2")
  expect_no_error(validate_schema(rbind(results, other), "results", fish_schema("results")))
})

test_that("issues: IDs once, record and column levels, positional PKs, roots (6.2, 6.3; D15.9)", {
  local_results_registry()
  issues <- fx_results()$issues
  check <- function(i, column, value) {
    table <- data.table::copy(issues)
    data.table::set(table, i = i, j = column, value = value)
    validate_schema(table, "issues", fish_schema("issues"))
  }
  expect_error(check(2L, "issue_id", 1L), "issue ID below 1 or used twice")
  expect_error(check(1L, "reason_code", "value_jump"), "conformance row a reason code")
  expect_error(check(1L, "row_number", NA_integer_), "without a row number")
  expect_error(check(1L, "pk_positional", NA), "without a row number or a positional flag")
  expect_error(check(3L, "pk_positional", FALSE), "without a PK that aren't marked positional")
  expect_error(check(5L, "record_pk", "c9"), "column-level issues without record_pk")
  expect_error(check(5L, "n_records", NA_integer_), "column-level issues without record_pk")
  expect_error(check(5L, "n_records", -1L), "column-level issues without record_pk")
  # A table rule's issue on a missing table counts no records (D16.8).
  expect_no_error(check(5L, "n_records", 0L))
  expect_error(check(5L, "row_number", 4L), "or with a row number")
  expect_error(check(1L, "root_issue_id", 2L), "has a root on an issue that isn't not_evaluated")
  expect_error(check(4L, "root_issue_id", NA_integer_), "a not_evaluated one without")
  expect_error(check(4L, "root_issue_id", 9L), "a root that isn't an issue of the same table")
  expect_error(check(1L, "table_name", "hauls"), "a root that isn't an issue of the same table")
})

test_that("corrections: IDs once, support counts never negative and in order (9.4)", {
  local_results_registry()
  corrections <- fx_results()$corrections
  twice <- rbind(corrections, corrections)
  expect_error(validate_schema(twice, "corrections", fish_schema("corrections")), "used twice")
  table <- data.table::copy(corrections)
  data.table::set(table, j = "support_n", value = 2L)
  expect_error(
    validate_schema(table, "corrections", fish_schema("corrections")),
    "support counts out of order"
  )
  # Each count is checked on its own, the other missing.
  for (column in c("support_n", "support_d")) {
    alone <- data.table::copy(corrections)
    data.table::set(alone, j = setdiff(c("support_n", "support_d"), column), value = NA_integer_)
    expect_no_error(validate_schema(alone, "corrections", fish_schema("corrections")))
    data.table::set(alone, j = column, value = -1L)
    expect_error(
      validate_schema(alone, "corrections", fish_schema("corrections")),
      "negative support counts"
    )
  }
})

test_that("new_gpq_results() checks the tables together", {
  local_results_registry()
  object <- fx_results()
  expect_s3_class(object, "gpq_results")
  expect_named(object, c("results", "issues", "corrections", "metadata"))
  args <- unclass(object)
  other_run <- data.table::copy(args$issues)
  data.table::set(other_run, j = "run_id", value = "20261008T130000Z_000000")
  expect_error(
    new_gpq_results(args$results, other_run, args$corrections, args$metadata),
    "issues table holds rows of another run"
  )
  stray <- data.table::copy(args$corrections)
  data.table::set(stray, j = "issue_id", value = 99L)
  expect_error(
    new_gpq_results(args$results, args$issues, stray, args$metadata),
    "names issues the issues table doesn't have"
  )
  extra <- data.table::copy(args$issues)
  data.table::set(extra, j = "stand", value = "s1")
  expect_error(
    new_gpq_results(args$results, extra, args$corrections, args$metadata),
    "columns no schema has: plot, stand"
  )
})

test_that("bind_results() appends one run's results to another's, renumbering IDs", {
  local_results_registry()
  object <- fx_results()
  second <- object
  second$results <- data.table::copy(object$results)
  data.table::set(second$results, j = "area", value = "a9")
  both <- bind_results(object, second)
  expect_equal(nrow(both$results), 2L * nrow(object$results))
  expect_equal(both$issues$issue_id, 1:10)
  expect_equal(both$issues$root_issue_id[[9L]], 6L)
  expect_equal(both$corrections$correction_id, 1:2)
  expect_equal(both$corrections$issue_id, c(1L, 6L))
  # The first object is left as it was.
  expect_equal(object$issues$issue_id, 1:5)
  # The issue IDs written in corrections' text cells move by the same offset (R16, D15.26):
  # whole digit parts only, "problem_plot" and other parts as written, NA as NA.
  expect_equal(both$corrections$easy_fix_blocked_by, c("2; problem_plot", "7; problem_plot"))
  expect_equal(object$corrections$easy_fix_blocked_by, "2; problem_plot")
  third <- object
  third$results <- data.table::copy(object$results)
  data.table::set(third$results, j = "area", value = "a8")
  third$corrections <- data.table::copy(object$corrections)
  data.table::set(third$corrections, j = "related_issue_ids", value = "1; 3; p2; 12x; 40")
  data.table::set(third$corrections, j = "easy_fix_blocked_by", value = NA_character_)
  more <- bind_results(object, third)
  expect_equal(more$corrections$related_issue_ids, c(NA, "6; 8; p2; 12x; 45"))
  expect_equal(more$corrections$easy_fix_blocked_by, c("2; problem_plot", NA))
  expect_equal(third$corrections$related_issue_ids, "1; 3; p2; 12x; 40")
  # An all-digit part too long for an integer (more than nine digits) is no issue ID of this
  # run and stays as written, with no warning; nine digits still move.
  long <- third
  long$corrections <- data.table::copy(third$corrections)
  data.table::set(
    long$corrections,
    j = "related_issue_ids", value = "1; 9999999999; 99999999999999999999; 999999999"
  )
  more <- expect_no_warning(bind_results(object, long))
  expect_equal(
    more$corrections$related_issue_ids,
    c(NA, "6; 9999999999; 99999999999999999999; 1000000004")
  )
  # Either object may have no corrections.
  none <- third
  none$corrections <- new_corrections()
  expect_equal(bind_results(object, none)$corrections$correction_id, 1L)
  expect_equal(bind_results(none, object)$corrections$issue_id, 6L)
  # The same run ID and issues columns, but other group columns: not bound.
  grouped <- object
  grouped$metadata$run <- data.table::copy(object$metadata$run)
  data.table::set(grouped$metadata$run, j = "group_cols", value = "unit; area; plot")
  grouped$results <- data.table::copy(object$results)
  data.table::set(grouped$results, j = "plot", value = "p1")
  data.table::setcolorder(
    grouped$results, names(results_schema(c("unit", "area", "plot"))$results)
  )
  expect_no_error(validate_gpq_results(grouped))
  expect_error(bind_results(object, grouped), "Only results of one run")
  # Another run's results, every table carrying its run ID, aren't bound.
  other <- object
  other$metadata$run <- data.table::copy(object$metadata$run)
  for (name in c("results", "issues", "corrections")) {
    other[[name]] <- data.table::copy(object[[name]])
    data.table::set(other[[name]], j = "run_id", value = "20261008T130000Z_000000")
  }
  data.table::set(other$metadata$run, j = "run_id", value = "20261008T130000Z_000000")
  expect_error(bind_results(object, other), "Only results of one run")
})

# M7a task 3 (D20.10, D20.15, D20.18)
test_that("n_unchecked is NA only on a unit rule's row; reaches_other_unit is always given", {
  local_results_registry()
  object <- fx_results()
  check <- function(table, name, column, value) {
    changed <- data.table::copy(table)
    data.table::set(changed, i = 1L, j = column, value = value)
    validate_schema(changed, name, fish_schema(name))
  }
  expect_error(check(object$results, "results", "n_unchecked", -1L), "negative counts")
  expect_error(
    check(object$results, "results", "n_unchecked", NA_integer_),
    paste(
      "gives n_unchecked to the row of a rule that checks whole tables or columns, or leaves it",
      "NA on another rule's row"
    )
  )
  expect_error(
    check(object$issues, "issues", "reaches_other_unit", NA), "blank cells in reaches_other_unit"
  )
  # A unit rule's row counts no records (D16.8): its n_unchecked is NA.
  unit_row <- data.table::copy(object$results[1L])
  data.table::set(
    unit_row,
    j = c("rule_id", "layer", "n_fail", "n_distinct_fail_values", "status", "n_unchecked"),
    value = list("table_missing", 0L, 0L, 0L, "pass", NA_integer_)
  )
  expect_no_error(validate_schema(unit_row, "results", fish_schema("results")))
  data.table::set(unit_row, j = "n_unchecked", value = 0L)
  expect_error(
    validate_schema(unit_row, "results", fish_schema("results")),
    "gives n_unchecked to the row of a rule that checks whole tables or columns"
  )
})
