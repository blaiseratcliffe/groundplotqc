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
