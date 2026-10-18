# Tests for Layer 4, keys and relationships (plan 6.1, 6.2; D7.3, D7.9, D16.2, D18.1, D18.3,
# D18.4, D18.10, D18.14, D18.15, D18.33 to D18.43): primary and foreign keys, the redundant
# keys key_pairs names, repeated src_*_id values, the records no root reaches, childless
# parents and the three design candidates.
#
# Task cuts (M5): task 10 writes the file with the four helpers and the tests marked "task 10",
# its first test naming its six rules only; tasks 11 to 14 add the tests marked with their
# number and their rules to the first test. Each cut keeps the tests in this order.

# The helpers of test-check_sentinels.R, with the same meanings (R25, R37), the run's layers
# reaching Layer 4.
fish_run <- function(tables, settings = list(), layers = 0:4, rules = NULL, spec = fx_fish_spec()) {
  gpq_check_conformance(tables, spec, layers = layers, settings = settings, rules = rules)
}

opened <- function(results, rule_id = NULL) {
  issues <- results$issues
  issues <- issues[issues$status == "open"]
  if (is.null(rule_id)) {
    return(issues)
  }
  # Worked out before the brackets: rule_id is also a column of the issues.
  wanted <- issues$rule_id == rule_id
  issues[wanted]
}

placed_at <- function(issues) paste(issues$table_name, issues$column_name, issues$row_number)

edited <- function(table, column, rows, value, fish = fx_fish_clean()) {
  copy <- data.table::copy(fish[[table]])
  data.table::set(copy, i = rows, j = column, value = value)
  fish[[table]] <- copy
  fish
}

# ---- task 10 (the names grow with tasks 11 to 14) ----

test_that("check_keys returns an outcome list for each Layer 4 rule", {
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 4L)
  found <- check_keys(conformance_input(fish, spec), context)
  expect_named(found, c(
    "pk_duplicate", "pk_missing", "fk_orphan", "fk_unassigned", "fk_sentinel_unverified",
    "fk_not_applicable_mismatch"
  ))
  # Three tables of one key column each; two foreign keys, neither of a number type.
  expect_length(found$pk_duplicate, 3L)
  expect_length(found$pk_missing, 3L)
  expect_length(found$fk_orphan, 2L)
  expect_length(found$fk_unassigned, 2L)
  expect_length(found$fk_sentinel_unverified, 0L)
  expect_equal(
    vapply(found$fk_not_applicable_mismatch, `[[`, "", "reason"), c("no_matrix", "no_matrix")
  )
})

# ---- task 10 ----

test_that("pk_duplicate fails every holder of a repeated key, never a missing one (D18.10)", {
  nbsp <- intToUtf8(0xA0)
  fish <- edited("catches", "catch_id", 2:6, c("catches_1", NA, NA, nbsp, nbsp))
  results <- fish_run(fish)
  found <- opened(results, "pk_duplicate")
  # Every holder fails, the first included; two NAs or two blanks repeat nothing: they are
  # pk_missing's.
  expect_equal(placed_at(found), c("catches catch_id 1", "catches catch_id 2"))
  expect_equal(found$value, c("catches_1", "catches_1"))
  expect_equal(placed_at(opened(results, "pk_missing")), paste("catches catch_id", 3:6))
  rows <- results$results[results$results$rule_id == "pk_duplicate"]
  # The missing keys are left alone, checked and passing (R20).
  expect_equal(rows$n_checked[rows$table_name == "catches"], 8L)
})

test_that("a composite key is compared whole, on its first part; each part checked (D18.42)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("a", "b", "v"), key_type = c("PK", "PK", "."),
    data_type = "character"
  )
  spec <- gpq_read_spec(dictionary)
  tables <- list(t = data.table::data.table(
    a = c("1", "1", "1", NA), b = c("x", "y", "x", "x"), v = c("p", "q", "r", "s")
  ))
  results <- gpq_check_conformance(tables, spec, layers = 4)
  found <- opened(results, "pk_duplicate")
  expect_equal(placed_at(found), c("t a 1", "t a 3"))
  expect_equal(found$value, c("1|x", "1|x"))
  expect_equal(placed_at(opened(results, "pk_missing")), "t a 4")
})

test_that("pk_missing finds NA, blanks and a sentinel the key can't hold (D9.24, D18.15)", {
  nbsp <- intToUtf8(0xA0)
  fish <- edited("catches", "catch_id", 1:5, c(NA, "", nbsp, "?", "~"))
  results <- fish_run(fish)
  # Only pk_missing reports them: raw_na and blank_string leave keys alone (D7.9), and no
  # sentinel is allowed in a primary key here (3.5).
  expect_equal(unique(opened(results)$rule_id), "pk_missing")
  found <- opened(results, "pk_missing")
  expect_equal(placed_at(found), paste("catches catch_id", 1:5))
  expect_equal(found$value, c(NA, "", nbsp, "?", "~"))
})

test_that("fk_orphan and fk_unassigned split a key's failures, its open sentinel waits (D18.33)", {
  fish <- edited("catches", "haul_id", 1:5, c("hauls_9", NA, "", "?", "~"))
  results <- fish_run(fish)
  orphan <- opened(results, "fk_orphan")
  expect_equal(placed_at(orphan), "catches haul_id 1")
  expect_equal(c(orphan$value, orphan$expected), c("hauls_9", "hauls"))
  expect_equal(placed_at(opened(results, "fk_unassigned")), paste("catches haul_id", 2:4))
  # Nothing else opens: an NA or blank key isn't raw_na's or blank_string's (D7.9).
  expect_setequal(unique(opened(results)$rule_id), c("fk_orphan", "fk_unassigned"))
  # "~", allowed in a foreign key, is counted as not evaluated, with no issue (D7.3, D7.7).
  rows <- results$results
  row <- rows[rows$rule_id == "fk_orphan" & rows$table_name == "catches"]
  expect_equal(c(row$n_checked, row$n_fail, row$n_not_evaluated), c(7L, 1L, 1L))
  expect_false(any(results$issues$rule_id == "fk_orphan" & results$issues$row_number == 5L))
})

test_that("a padded foreign key is Layer 2's, so fk_orphan doesn't evaluate it (D2.10)", {
  fish <- edited("catches", "haul_id", 1L, "hauls_1 ")
  results <- fish_run(fish)
  expect_equal(opened(results)$rule_id, "whitespace_padding")
  masked <- results$issues[results$issues$rule_id == "fk_orphan"]
  expect_equal(masked$status, "not_evaluated")
  expect_equal(masked$root_issue_id, opened(results)$issue_id)
})

test_that("a foreign key's -9 is one fk_sentinel_unverified warning, no key error (D16.2)", {
  testthat::skip_on_cran()
  tables <- fx_magp_clean()
  tables$magp_trees <- data.table::copy(tables$magp_trees)
  data.table::set(tables$magp_trees, i = 1L, j = "magp_site_id", value = -9L)
  results <- fx_caught(tables)
  found <- opened(results)
  expect_equal(found$rule_id, "fk_sentinel_unverified")
  expect_equal(c(found$table_name, found$column_name, found$value), c(
    "magp_trees", "magp_site_id", "-9"
  ))
  expect_equal(found$severity, "warning")
  # Never looked up: fk_orphan counts it as not evaluated (D18.33).
  rows <- results$results
  at <- rows$rule_id == "fk_orphan" & rows$table_name == "magp_trees" &
    rows$column_name == "magp_site_id"
  expect_equal(sum(rows$n_not_evaluated[at]), 1L)
})

test_that("fk_not_applicable_mismatch is not_run, no_matrix, per foreign key and group (D7.3)", {
  fish <- fx_fish_clean()
  results <- gpq_check_conformance(fish, fx_fish_spec(), group_cols = "gear", layers = 4)
  rows <- results$results[results$results$rule_id == "fk_not_applicable_mismatch"]
  expect_setequal(paste(rows$table_name, rows$column_name), c(
    "hauls station_id", "catches haul_id"
  ))
  expect_true(all(rows$status == "not_run" & rows$not_run_reason == "no_matrix"))
  catches <- rows[rows$table_name == "catches"]
  expect_equal(sum(catches$n_not_evaluated), nrow(fish$catches))
})

# task 10's fix round (the spec review's I2)
test_that("a sentinel a primary key may hold repeats nothing: pk_duplicate leaves it alone", {
  # With allowed_in_pk TRUE, "~" is neither pk_missing's nor a key to compare (D18.10 (2)).
  spec <- fx_fish_spec()
  sentinels <- data.table::copy(spec$sentinels)
  allowed <- which(sentinels$data_type == "character" & sentinels$value == "~")
  data.table::set(sentinels, i = allowed, j = "allowed_in_pk", value = TRUE)
  spec$sentinels <- sentinels
  results <- fish_run(edited("catches", "catch_id", 1:2, c("~", "~")), spec = spec)
  expect_equal(nrow(opened(results)), 0L)
  rows <- results$results[results$results$rule_id == "pk_duplicate"]
  row <- rows[rows$table_name == "catches"]
  expect_equal(c(row$n_checked, row$n_fail), c(8L, 0L))
})

# task 10's fix round (the spec review's m1)
test_that("a composite key missing a part's column is not_run, no_column, on its first part", {
  # pk_duplicate compares nothing without every part (D9.24, D18.42).
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("a", "b", "v"), key_type = c("PK", "PK", "."),
    data_type = "character"
  )
  spec <- gpq_read_spec(dictionary)
  tables <- list(t = data.table::data.table(a = c("1", "1", "2"), v = c("p", "q", "r")))
  results <- gpq_check_conformance(tables, spec, layers = 4)
  row <- results$results[results$results$rule_id == "pk_duplicate"]
  expect_equal(c(row$column_name, row$status, row$not_run_reason), c("a", "not_run", "no_column"))
  expect_equal(nrow(opened(results)), 0L)
})

# task 10's fix round (the fixture review's Minor 4)
test_that("a repeated primary key fails each holder in its own group (D18.10 (2))", {
  fish <- fx_fish_clean()
  gear <- fish$hauls$gear[match(fish$catches$haul_id, fish$hauls$haul_id)]
  other <- which(gear != gear[[1L]])[[1L]]
  fish <- edited("catches", "catch_id", other, "catches_1", fish)
  results <- gpq_check_conformance(fish, fx_fish_spec(), group_cols = "gear", layers = 4)
  expect_equal(opened(results, "pk_duplicate")$row_number, c(1L, other))
  rows <- results$results[results$results$rule_id == "pk_duplicate"]
  rows <- rows[rows$table_name == "catches"]
  expect_setequal(rows$gear[rows$n_fail == 1L], c(gear[[1L]], gear[[other]]))
  expect_equal(sum(rows$n_fail), 2L)
})

# task 10's fix round (the spec review's m3 and the fixture review's Minor 4)
test_that("a number foreign key warns once per record holding its allowed code, as text too", {
  # haul_id a whole number, read from a CSV file as text: -88 (not applicable, allowed in a
  # foreign key) written "-88.0" and "-88", and -99 (missing, refused) written "-99.0", each
  # read as its code (D18.33, R29).
  spec <- fx_fish_spec()
  attributes <- data.table::copy(spec$attributes)
  is_haul <- which(attributes$attribute_name == "haul_id")
  data.table::set(attributes, i = is_haul, j = "data_type", value = "whole")
  data.table::set(attributes, i = is_haul, j = "r_class", value = "integer")
  spec$attributes <- attributes
  fish <- fx_clean_tables(spec)
  as_text <- function(fish, ids) {
    catches <- data.table::copy(fish$catches)
    data.table::set(catches, j = "haul_id", value = ids)
    fish$catches <- catches
    fish
  }
  ids <- as.character(fish$catches$haul_id)
  ids[1:3] <- c("-88.0", "-88", "-99.0")
  results <- fish_run(as_text(fish, ids), layers = 4, spec = spec)
  found <- opened(results, "fk_sentinel_unverified")
  expect_equal(placed_at(found), paste("catches haul_id", 1:2))
  expect_equal(found$value, c("-88.0", "-88"))
  expect_equal(unique(found$severity), "warning")
  expect_equal(placed_at(opened(results, "fk_unassigned")), "catches haul_id 3")
  expect_setequal(unique(opened(results)$rule_id), c("fk_sentinel_unverified", "fk_unassigned"))
  rows <- results$results
  row <- rows[rows$rule_id == "fk_orphan" & rows$table_name == "catches"]
  expect_equal(c(row$n_fail, row$n_not_evaluated), c(0L, 2L))
  # A column full of them collapses by column_level_min_n, as usual (D18.33).
  every <- rep("-88", nrow(fish$catches))
  full <- fish_run(
    as_text(fish, every),
    settings = list(column_level_min_n = 5L), layers = 4, spec = spec
  )
  collapsed <- opened(full, "fk_sentinel_unverified")
  expect_equal(collapsed$level, "column")
  expect_equal(collapsed$n_records, nrow(fish$catches))
})

# task 10's fix round (the spec review's m3)
test_that("a foreign key to a composite primary key isn't followed; fk_unassigned checks it", {
  # keys gives it no reference attribute, so fk_orphan has nothing to check (D18.42).
  dictionary <- data.frame(
    table_name = c("parent", "parent", "child", "child"),
    attribute_name = c("a", "b", "child_id", "a"), key_type = c("PK", "PK", "PK", "FK"),
    reference_table = c(NA, NA, NA, "parent"), data_type = "character"
  )
  spec <- gpq_read_spec(dictionary)
  expect_true(is.na(spec$keys$reference_attribute[spec$keys$key_type == "FK"]))
  tables <- list(
    parent = data.table::data.table(a = c("1", "1"), b = c("x", "y")),
    child = data.table::data.table(child_id = c("c1", "c2", "c3"), a = c("1", "9", NA))
  )
  results <- gpq_check_conformance(tables, spec, layers = 4)
  # "9" names no parent's a, but isn't looked up.
  expect_false(any(results$results$rule_id == "fk_orphan"))
  found <- opened(results)
  expect_equal(paste(found$rule_id, placed_at(found)), "fk_unassigned child a 3")
})

# task 10's fix round (the datatable review's F1 and the fixture review's Minor 4)
test_that("key_rows() writes stored numbers as text only near a code, its rows unchanged (16.2)", {
  # A date-typed key stored as plain numbers: a code holds where the number's text is the
  # code, so a real 0 isn't "00000000" (m39) and 99999999.00001, near 99999999, isn't
  # "99999999"; only the values near a code that reads as a number are written as text.
  row <- data.table::data.table(family = "date")
  sentinels <- data.table::data.table(
    data_type = "date", value = c("00000000", "99999999", "X"),
    allowed_in_pk = FALSE, allowed_in_fk = c(FALSE, TRUE, TRUE)
  )
  x <- c(0, 99999999, 99999999.00001, 20200101, NA, 99999999)
  real_text <- value_text
  written <- list()
  testthat::local_mocked_bindings(value_text = function(x) {
    written[[length(written) + 1L]] <<- x
    real_text(x)
  })
  found <- key_rows(x, row, sentinels, "allowed_in_fk")
  expect_equal(found, list(missing = 5L, open = c(2L, 6L)))
  # The rows the whole column written as text gives.
  text <- real_text(x)
  expect_equal(found$missing, sort(c(which(is.na(x)), which(text %in% "00000000"))))
  expect_equal(found$open, which(text %in% c("99999999", "X")))
  expect_equal(written, list(0, c(99999999, 99999999.00001, 99999999)))
})
