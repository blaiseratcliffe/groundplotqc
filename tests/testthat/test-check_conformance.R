# Tests for a conformance run (plan 3.2, 6.1 to 6.3, 9.1 to 9.7; D2.10, D4.1, D7.3, D9.24,
# D16.3 to D16.17). The assembly of record outcomes is tested here with two stand-in rules
# (Layers 1 and 2), so the cascade and the column-level issues are pinned before the real
# Layer 1 and 2 rules; their own tests run through the families.

# The registry with two stand-in conformance rules, test_cast (Layer 1) and test_spell
# (Layer 2), in place of rule_registry() until the calling test ends; the rules named in
# `unit` are unit rules (D17.15).
local_stand_in_rules <- function(env = parent.frame(), unit = character()) {
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = c("test_cast", "test_spell"), stage = "conformance", layer = c(1L, 2L),
    check_type = "test", default_severity = "error", default_class = "harmonization",
    inputs = "attributes", strategy = "none", message_id = "preflight_title", rule_version = 1L,
    unit = c("test_cast", "test_spell") %in% unit
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

test_that("a unit rule records its not_run outcomes as units, others by group (D17.15)", {
  local_stand_in_rules(unit = "test_cast")
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
  # A unit rule none of whose outcomes ran still writes one "*" row per column (D16.30 gone).
  only <- assembly(group_cols = "gear")
  record_layer(list(test_cast = list(
    not_run_outcome("test_cast", "hauls", "gear", "text_input"),
    not_run_outcome("test_cast", "catches", "weight", "text_input")
  )), 1L, only$input, only$context, only$state, run_id)
  rows <- recorded(only$state)$results
  expect_equal(rows$gear, c("*", "*"))
  expect_equal(rows$n_not_evaluated, c(1L, 1L))
  expect_equal(rows$not_run_reason, c("text_input", "text_input"))
})

test_that("a family's outcomes are checked when made and when recorded", {
  expect_error(record_outcome("test_cast", "catches", "weight", 1:2, "a"), "one text for each")
  # Not row numbers: logical, below 1, fractional, missing, text, infinite, beyond the integer
  # range, a row twice, or a matrix of whole numbers.
  for (fail in list(
    c(TRUE, FALSE), 0L, 1.5, NA_integer_, "1", Inf, 3e9, c(2L, 2L), matrix(1:2, nrow = 1L)
  )) {
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
    strategy = "none", message_id = "preflight_title", rule_version = 1L, unit = FALSE
  ))
  testthat::local_mocked_bindings(rule_registry = function() registry)
  results <- gpq_check_conformance(fx_fish_clean(), fx_fish_spec(), layers = 0)
  expect_equal(results$metadata$self_test$rule_id, "test_forgotten")
  expect_equal(results$metadata$self_test$layer, 0L)
})

test_that("group columns named like the engine's own variables are read as columns", {
  testthat::skip_on_cran()
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
  results <- gpq_check_conformance(fx_fish_clean(), fx_fish_spec(), layers = 5)
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

# the minors pass's fix round (D18.86)
test_that("a table holding integer64 has bit64 loaded for it, or the run stops without it", {
  # The test traces base requireNamespace(), which no CRAN check may change (R145).
  testthat::skip_on_cran()
  # A column of class integer64 made from its 64-bit patterns, as readRDS() returns one in a
  # session where bit64 isn't loaded: little-endian words, NA as integer64's NA.
  bits_of <- function(values) {
    high <- floor(values / 2^32)
    low <- values - high * 2^32
    low <- ifelse(low >= 2^31, low - 2^32, low)
    words <- as.integer(rbind(ifelse(is.na(values), 0, low), high))
    bits <- writeBin(words, raw(), endian = "little")
    structure(
      readBin(bits, "double", n = length(values), endian = "little"),
      class = "integer64"
    )
  }
  # A text-typed key whose codes read as numbers, as test-check_keys.R's integer64 test has it.
  dictionary <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "character"
  )
  spec <- gpq_read_spec(
    dictionary,
    sentinels = gpq_sentinels(character = c(missing = "-2", not_applicable = "99999999"))
  )
  tables_of <- function(stored) {
    list(
      p = data.table::data.table(p_id = c("1", "2")),
      k = data.table::data.table(
        k_id = stored(c(10, 11, 12, 13, -2, -2)), p_id = stored(c(1, -2, 99999999, 7, 1, 2))
      )
    )
  }
  # Each requireNamespace() call for bit64 counted; while `absent`, bit64 can't be loaded.
  asked <- new.env()
  asked$n <- 0L
  asked$absent <- TRUE
  suppressMessages(trace(
    "requireNamespace",
    tracer = bquote(if (identical(package, "bit64")) {
      assign("n", get("n", envir = .(asked)) + 1L, envir = .(asked))
      if (get("absent", envir = .(asked))) package <- "bit64.absent"
    }),
    where = baseenv(), print = FALSE
  ))
  withr::defer(suppressMessages(untrace("requireNamespace", where = baseenv())))
  # Without bit64 the run stops, naming the first such column, W135's text.
  expect_error(
    gpq_check_conformance(tables_of(bits_of), spec, layers = 4),
    paste(
      "Table k: the column k_id is stored as integer64, which needs the bit64 package to be",
      "read correctly. Install it with install.packages(\"bit64\") and run again."
    ),
    fixed = TRUE
  )
  expect_identical(asked$n, 1L)
  # The first table in the given order holding one is named (the minors pass's fix round 2,
  # fixtures N2).
  both <- tables_of(bits_of)
  both$p <- data.table::data.table(p_id = bits_of(c(1, 2)))
  expect_error(
    gpq_check_conformance(both, spec, layers = 4),
    "Table p: the column p_id is stored as integer64",
    fixed = TRUE
  )
  # A run with no integer64 column asks for nothing, its keys stored as text, doubles or
  # integers (fixtures N1).
  asked$absent <- FALSE
  asked$n <- 0L
  plain <- gpq_check_conformance(tables_of(as.character), spec, layers = 4)
  expect_identical(asked$n, 0L)
  for (stored in list(as.numeric, as.integer)) {
    gpq_check_conformance(tables_of(stored), spec, layers = 1)
    expect_identical(asked$n, 0L)
  }
  # With bit64, the run loads it and reads the column as the same keys stored as text. Found
  # installed without loading it, so where no earlier test has, the run is what loads it.
  testthat::skip_if(!nzchar(system.file(package = "bit64")), "bit64 isn't installed.")
  asked$n <- 0L
  wide <- gpq_check_conformance(tables_of(bits_of), spec, layers = 4)
  expect_identical(asked$n, 1L)
  expect_true(isNamespaceLoaded("bit64"))
  placed <- function(results) {
    open <- results$issues[results$issues$status == "open"]
    paste(open$rule_id, open$table_name, open$column_name, open$row_number, open$value)
  }
  expect_equal(placed(wide), c(
    "pk_missing k k_id 5 -2", "pk_missing k k_id 6 -2", "fk_orphan k p_id 4 7",
    "fk_unassigned k p_id 2 -2"
  ))
  expect_identical(placed(wide), placed(plain))
})

# the final fix wave (D18.103)
test_that("a unit rule's results examples are its value unless it gives its own", {
  expect_equal(unit_outcome("test_cast", "t", "c", TRUE, 1L, value = "v")$examples, "v")
  expect_equal(
    unit_outcome("test_cast", "t", "c", TRUE, 1L, value = "v", examples = "w")$examples, "w"
  )
  # A column stored as text, an integer column stored as doubles, and one named in other case.
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  data.table::set(catches, j = "weight", value = as.character(catches$weight))
  data.table::set(catches, j = "fish_count", value = as.numeric(catches$fish_count))
  fish$catches <- catches
  hauls <- data.table::copy(fish$hauls)
  data.table::setnames(hauls, "gear", "Gear")
  fish$hauls <- hauls
  results <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0:1)
  for (rule in c("class_mismatch", "integer_stored_as_double", "column_name_mismatch")) {
    # Worked out before the brackets: rule_id is also a column of the results.
    failed <- results$results$rule_id == rule & results$results$status == "fail"
    row <- results$results[failed]
    found <- results$issues[results$issues$rule_id == rule]
    expect_equal(nrow(row), 1L, info = rule)
    expect_equal(row$examples, found$value, info = rule)
  }
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
  expect_equal(c(spell$status, spell$not_run_reason), c("not_run", "none_checked"))
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

test_that("an empty table is one not_run row, no_records; not_run counts by group (D18.29)", {
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
  expect_equal(c(empty$gear, empty$status, empty$not_run_reason), c("*", "not_run", "no_records"))
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

test_that("the performance smoke test: a 0.1x MAGPlot run finishes in time, clean (18.4)", {
  # Skipped on CRAN (18.5, D7.14). The run took 3.3 to 4.4 s at M4b's planning (D17.18).
  testthat::skip_on_cran()
  tables <- fx_magp_scaled(0.1)
  elapsed <- system.time(results <- fx_caught(tables))[["elapsed"]]
  expect_lt(elapsed, 30)
  expect_equal(sum(results$issues$status == "open"), 0L)
  expect_equal(nrow(results$metadata$self_test), 0L)
})

test_that("record_outcome() refuses unchecked rows that aren't rows, or that also fail (D18.21)", {
  expect_error(
    record_outcome("r", "t", "c", 1:2, c("a", "b"), unchecked = 2:3),
    "Rule r: a row can't both fail and be unchecked.",
    fixed = TRUE
  )
  for (unchecked in list(c(2L, 2L), 0L, NA_integer_, 1.5, matrix(2L), "2")) {
    expect_error(
      record_outcome("r", "t", "c", 1L, "a", unchecked = unchecked),
      paste(
        "Rule r: `unchecked` must be row numbers, each once: whole numbers from 1 within the",
        "integer range."
      ),
      fixed = TRUE, info = deparse(unchecked)
    )
  }
  # None given is none: the outcome carries an empty vector of rows.
  expect_identical(record_outcome("r", "t", "c", 1L, "a")$unchecked, integer())
  expect_identical(record_outcome("r", "t", "c", 1L, "a", unchecked = c(3, 2))$unchecked, c(3L, 2L))
})

test_that("rows a rule didn't check count in neither n_checked nor n_fail (D18.21)", {
  local_stand_in_rules()
  n <- nrow(fx_fish_clean()$catches)
  run <- assembly(settings = list(column_level_min_n = 100L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", 1:2, c("x", "y")
  ))), 1L, run$input, run$context, run$state, run_id)
  record_layer(list(test_spell = list(record_outcome(
    "test_spell", "catches", "weight", 3L, "z",
    unchecked = c(2L, 4L, 5L)
  ))), 2L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  spell <- done$results[done$results$rule_id == "test_spell"]
  # Rows 1 and 2 are masked by Layer 1, row 2 counted once, as masked; rows 4 and 5 weren't
  # checked; row 3 fails.
  expect_equal(c(spell$n_checked, spell$n_fail, spell$n_not_evaluated), c(n - 4L, 1L, 2L))
  expect_equal(spell$status, "fail")
  expect_true(is.na(spell$not_run_reason))
  issues <- done$issues[done$issues$rule_id == "test_spell"]
  # A row the rule didn't check has no issue; a masked one keeps its not_evaluated issue. The
  # failures are numbered before the masked records, so the rows are read in their order.
  issues <- issues[order(issues$row_number)]
  expect_equal(issues$row_number, 1:3)
  expect_equal(issues$status, c("not_evaluated", "not_evaluated", "open"))
  expect_no_error(new_results(done$results, registry = rule_registry()))
  expect_no_error(new_issues(done$issues, registry = rule_registry()))
})

test_that("a group in which the rule checked nothing is not_run, none_checked (D18.22)", {
  local_stand_in_rules()
  run <- assembly(group_cols = "gear")
  grouping <- run$input$groups$catches
  first_group <- which(grouping$row_group == 1L)
  others <- nrow(grouping$values) - 1L
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", integer(), character(),
    unchecked = first_group
  ))), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  rows <- done$results
  expect_equal(rows$gear, grouping$values$gear)
  expect_equal(rows$status, c("not_run", rep("pass", others)))
  expect_equal(rows$not_run_reason, c("none_checked", rep(NA_character_, others)))
  # Its records are counted as not evaluated, and none is checked or fails.
  expect_equal(rows$n_checked[[1L]], 0L)
  expect_equal(rows$n_fail[[1L]], 0L)
  expect_equal(rows$n_not_evaluated[[1L]], length(first_group))
  expect_equal(rows$n_not_evaluated[-1L], rep(0L, others))
  expect_equal(nrow(done$issues), 0L)
  expect_no_error(new_results(rows, group_cols = "gear", registry = rule_registry()))
  # A group masked in part and unchecked in the rest checked nothing too: not_run, its
  # records counted once each.
  run <- assembly(group_cols = "gear")
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", first_group[[1L]], "x"
  ))), 1L, run$input, run$context, run$state, run_id)
  record_layer(list(test_spell = list(record_outcome(
    "test_spell", "catches", "weight", integer(), character(),
    unchecked = first_group
  ))), 2L, run$input, run$context, run$state, run_id)
  spell <- recorded(run$state)$results
  spell <- spell[spell$rule_id == "test_spell"]
  expect_equal(spell$status[[1L]], "not_run")
  expect_equal(spell$not_run_reason[[1L]], "none_checked")
  expect_equal(spell$n_not_evaluated[[1L]], length(first_group))
})

test_that("every checked row failing collapses into one issue, unchecked rows aside (D18.21)", {
  local_stand_in_rules()
  n <- nrow(fx_fish_clean()$catches)
  run <- assembly(settings = list(column_level_min_n = 2L))
  # Rows 1 and 2 aren't checked and every other row fails: as many failures as rows checked.
  failing <- 3:n
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", failing, rep("x", length(failing)),
    unchecked = 1:2
  ))), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  expect_equal(c(done$results$n_checked, done$results$n_fail), c(n - 2L, n - 2L))
  expect_equal(done$results$n_not_evaluated, 0L)
  expect_equal(done$results$status, "fail")
  expect_equal(done$issues$level, "column")
  expect_equal(done$issues$n_records, n - 2L)
  expect_no_error(new_issues(done$issues, registry = rule_registry()))
})

test_that("record_outcome() takes rows not evaluated and the rules masking it (D18.32)", {
  overlap <- "Rule r: a row can't be not evaluated and also fail or be unchecked."
  expect_error(record_outcome("r", "t", "c", 1L, "a", not_evaluated = 1L), overlap, fixed = TRUE)
  expect_error(
    record_outcome("r", "t", "c", 1L, "a", unchecked = 2L, not_evaluated = 2L), overlap,
    fixed = TRUE
  )
  for (rows in list(c(2L, 2L), 0L, NA_integer_, 1.5, matrix(2L), "2")) {
    expect_error(
      record_outcome("r", "t", "c", 1L, "a", not_evaluated = rows),
      paste(
        "Rule r: `not_evaluated` must be row numbers, each once: whole numbers from 1 within",
        "the integer range."
      ),
      fixed = TRUE, info = deparse(rows)
    )
  }
  for (bad in list(1L, NA_character_, list("q"))) {
    expect_error(
      record_outcome("r", "t", "c", 1L, "a", masked_by = bad),
      "Rule r: `masked_by` must be rule IDs.",
      fixed = TRUE, info = deparse(bad)
    )
  }
  outcome <- record_outcome("r", "t", "c", 1L, "a", not_evaluated = c(3, 2), masked_by = "q")
  expect_identical(outcome$not_evaluated, c(3L, 2L))
  expect_identical(outcome$masked_by, "q")
  # None given is none.
  plain <- record_outcome("r", "t", "c", 1L, "a")
  expect_identical(plain$not_evaluated, integer())
  expect_identical(plain$masked_by, character())
})

test_that("rows not evaluated count in n_not_evaluated, with no issue and no root (D18.32)", {
  local_stand_in_rules()
  n <- nrow(fx_fish_clean()$catches)
  run <- assembly(settings = list(column_level_min_n = 100L))
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", 1L, "x"
  ))), 1L, run$input, run$context, run$state, run_id)
  record_layer(list(test_spell = list(record_outcome(
    "test_spell", "catches", "weight", 2L, "y",
    not_evaluated = c(1L, 3L, 4L)
  ))), 2L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  spell <- done$results[done$results$rule_id == "test_spell"]
  # Row 1 is masked by Layer 1 and counted once, as masked; rows 3 and 4 aren't evaluated;
  # row 2 fails.
  expect_equal(c(spell$n_checked, spell$n_fail, spell$n_not_evaluated), c(n - 3L, 1L, 3L))
  issues <- done$issues[done$issues$rule_id == "test_spell"]
  # The masked row keeps its not_evaluated issue and root; rows 3 and 4 have none. The
  # failures are numbered before the masked records, so the rows are read in their order.
  issues <- issues[order(issues$row_number)]
  expect_equal(issues$row_number, 1:2)
  expect_equal(issues$status, c("not_evaluated", "open"))
  expect_no_error(new_results(done$results, registry = rule_registry()))
  # A group whose every row isn't evaluated checked nothing: not_run, none_checked (D18.22).
  run <- assembly(group_cols = "gear")
  grouping <- run$input$groups$catches
  first_group <- which(grouping$row_group == 1L)
  record_layer(list(test_cast = list(record_outcome(
    "test_cast", "catches", "weight", integer(), character(),
    not_evaluated = first_group
  ))), 1L, run$input, run$context, run$state, run_id)
  rows <- recorded(run$state)$results
  expect_equal(c(rows$status[[1L]], rows$not_run_reason[[1L]]), c("not_run", "none_checked"))
  expect_equal(rows$n_not_evaluated[[1L]], length(first_group))
  expect_equal(length(run$state$issues), 0L)
})

test_that("a rule masks the records an earlier rule of its layer failed, rooted there (D18.32)", {
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = c("test_root", "test_masked"), stage = "conformance", layer = 4L,
    check_type = "test", default_severity = "error", default_class = "harmonization",
    inputs = "attributes", strategy = "none", message_id = "preflight_title", rule_version = 1L,
    unit = FALSE
  ))
  testthat::local_mocked_bindings(rule_registry = function() registry)
  spec <- fx_fish_spec()
  settings <- list(column_level_min_n = 100L)
  run <- assembly(settings = settings)
  run$context <- conformance_context(spec, NULL, settings, 4L)
  record_layer(list(
    test_root = list(record_outcome("test_root", "catches", "catch_id", 2L, "catches_2")),
    test_masked = list(record_outcome(
      "test_masked", "catches", "catch_id", 2:3, c("catches_2", "catches_3"),
      masked_by = "test_root"
    ))
  ), 4L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  root <- done$issues[done$issues$rule_id == "test_root"]
  masked <- done$issues[done$issues$rule_id == "test_masked"]
  masked <- masked[order(masked$row_number)]
  expect_equal(masked$row_number, 2:3)
  expect_equal(masked$status, c("not_evaluated", "open"))
  expect_equal(masked$root_issue_id, c(root$issue_id, NA_integer_))
  counts <- done$results[done$results$rule_id == "test_masked"]
  expect_equal(c(counts$n_fail, counts$n_not_evaluated), c(1L, 1L))
  # Only the named rule masks: the same failure doesn't mask a rule that names none.
  run <- assembly(settings = settings)
  run$context <- conformance_context(spec, NULL, settings, 4L)
  record_layer(list(
    test_root = list(record_outcome("test_root", "catches", "catch_id", 2L, "catches_2")),
    test_masked = list(record_outcome("test_masked", "catches", "catch_id", 2L, "catches_2"))
  ), 4L, run$input, run$context, run$state, run_id)
  issues <- recorded(run$state)$issues
  expect_equal(issues$status, c("open", "open"))
  # A rule naming a later rule of its layer, or a rule of another layer, is a developer's
  # error (D18.32 (a)).
  for (named in list(
    list(rule = "test_root", by = "test_masked"), list(rule = "test_masked", by = "raw_na")
  )) {
    run <- assembly(settings = settings)
    run$context <- conformance_context(spec, NULL, settings, 4L)
    found <- list()
    found[[named$rule]] <- list(record_outcome(
      named$rule, "catches", "catch_id", 1L, "catches_1",
      masked_by = named$by
    ))
    expect_error(
      record_layer(found, 4L, run$input, run$context, run$state, run_id),
      sprintf(
        "Rule %s names %s in `masked_by`; a rule may be masked only by an earlier rule of Layer 4.",
        named$rule, named$by
      ),
      fixed = TRUE
    )
  }
})

# the finish's minors pass (m48)
test_that("a rule masks the records of a column-level failure of an earlier rule of its layer", {
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = c("test_root", "test_masked"), stage = "conformance", layer = 4L,
    check_type = "test", default_severity = "error", default_class = "harmonization",
    inputs = "attributes", strategy = "none", message_id = "preflight_title", rule_version = 1L,
    unit = FALSE
  ))
  testthat::local_mocked_bindings(rule_registry = function() registry)
  # Every record fails test_root, so its failures are one column-level issue (6.3), which
  # masks every record of test_masked, which names it, as one column-level issue (D16.10).
  ids <- fx_fish_clean()$catches$catch_id
  n <- length(ids)
  settings <- list(column_level_min_n = 3L)
  run <- assembly(settings = settings)
  run$context <- conformance_context(fx_fish_spec(), NULL, settings, 4L)
  record_layer(list(
    test_root = list(record_outcome("test_root", "catches", "catch_id", seq_len(n), ids)),
    test_masked = list(record_outcome(
      "test_masked", "catches", "catch_id", 1L, ids[[1L]],
      masked_by = "test_root"
    ))
  ), 4L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  root <- done$issues[done$issues$rule_id == "test_root"]
  expect_equal(c(root$level, root$status), c("column", "open"))
  masked <- done$issues[done$issues$rule_id == "test_masked"]
  expect_equal(c(masked$level, masked$status), c("column", "not_evaluated"))
  expect_equal(c(masked$n_records, masked$root_issue_id), c(n, root$issue_id))
  counts <- done$results[done$results$rule_id == "test_masked"]
  expect_equal(c(counts$n_checked, counts$n_fail, counts$n_not_evaluated), c(0L, 0L, n))
})

# the finish's minors pass (m47, m87)
test_that("the cascade binds no failure that can't mask, and copies none it reads whole", {
  # A Layer 4 failure masks no later Layer 4 rule that doesn't name it: nothing is bound, and
  # no table is built (D16.18).
  state <- new_run_state()
  key <- paste("catches", "catch_id", sep = "\r")
  state$failed[[key]] <- list(data.table::data.table(
    table_name = "catches", column_name = "catch_id", row_number = 2L, layer = 4L,
    issue_id = 1L, level = "record", status = "open", rule_id = "pk_missing"
  ))
  state$column_failed[[key]] <- list(data.table::data.table(
    group = 1L, layer = 4L, issue_id = 2L, rule_id = "pk_missing"
  ))
  expect_null(cascade_masks(state, "catches", "catch_id", 4L, rep(1L, 8L)))
  expect_equal(
    cascade_masks(state, "catches", "catch_id", 4L, rep(1L, 3L), "pk_missing")$row_number, 1:3
  )
  # A Layer 4 check of a key column reads the column's Layers 1 and 2 failures as they are
  # when none is Layer 3's, not a copy (D18.76).
  local_stand_in_rules()
  run <- assembly(settings = list(column_level_min_n = 100L))
  run$context <- conformance_context(fx_fish_spec(), NULL, list(column_level_min_n = 100L), 0:4)
  record_layer(list(test_spell = list(record_outcome(
    "test_spell", "catches", "catch_id", 1L, "x"
  ))), 2L, run$input, run$context, run$state, run_id)
  read <- list()
  real <- cascade_masks
  testthat::local_mocked_bindings(cascade_masks = function(state, ...) {
    read[[length(read) + 1L]] <<- state$failed[[key]]
    real(state, ...)
  })
  record_layer(list(pk_missing = list(record_outcome(
    "pk_missing", "catches", "catch_id", integer(), character()
  ))), 4L, run$input, run$context, run$state, run_id)
  expect_identical(
    data.table::address(read[[1L]][[1L]]), data.table::address(run$state$failed[[key]][[1L]])
  )
})

test_that("a count writes results rows in the counted table's groups, no issue (D18.32)", {
  local_stand_in_rules()
  run <- assembly(group_cols = "gear")
  hauls <- run$input$groups$hauls
  record_layer(list(test_cast = list(count_outcome(
    "test_cast", "catches", "haul_id", "hauls", 1L
  ))), 1L, run$input, run$context, run$state, run_id)
  done <- recorded(run$state)
  rows <- done$results
  # One row per group of the hauls, under the catches' haul_id.
  expect_equal(rows$table_name, rep("catches", nrow(hauls$values)))
  expect_equal(rows$column_name, rep("haul_id", nrow(hauls$values)))
  expect_equal(rows$gear, hauls$values$gear)
  expect_equal(rows$n_checked, tabulate(hauls$row_group, hauls$n))
  expect_equal(rows$n_fail, as.integer(seq_len(hauls$n) == hauls$row_group[[1L]]))
  expect_equal(rows$status, fifelse(rows$n_fail > 0L, "fail", "pass"))
  expect_equal(nrow(done$issues), 0L)
  expect_no_error(new_results(rows, group_cols = "gear", registry = rule_registry()))
  # A counted table with no rows: one not_run row, no_records (D18.29, D18.34).
  fish <- fx_fish_clean()
  fish$hauls <- fish$hauls[0L]
  run <- assembly(group_cols = "gear", tables = fish)
  record_layer(list(test_cast = list(count_outcome(
    "test_cast", "catches", "haul_id", "hauls", integer()
  ))), 1L, run$input, run$context, run$state, run_id)
  empty <- recorded(run$state)$results
  expect_equal(c(empty$status, empty$not_run_reason, empty$gear), c("not_run", "no_records", "*"))
  expect_error(
    count_outcome("r", "t", "c", "p", c(1L, 1L)),
    "Rule r: `fail` must be row numbers, each once: whole numbers from 1 within the integer range.",
    fixed = TRUE
  )
})
