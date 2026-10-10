# Tests for the rule ID map (plan 9.6, 17.4; D4.2, D5.2, D9.20).

test_that("the shipped rule ID map reads clean with its four columns", {
  map <- rule_id_map()
  expect_equal(names(map), c("old_rule_id", "new_rule_id", "change", "package_version"))
  expect_true(all(vapply(map, is.character, logical(1))))
})

test_that("a valid map passes: renames and splits to registered rules", {
  registry <- data.table::data.table(rule_id = c("fk_orphan", "pk_a", "pk_b"))
  map <- data.table::data.table(
    old_rule_id = c("fk_missing_parent", "pk_bad", "pk_bad"),
    new_rule_id = c("fk_orphan", "pk_a", "pk_b"), change = c("rename", "split", "split"),
    package_version = "0.0.0.9002"
  )
  expect_no_error(validate_rule_id_map(map, registry))
  expect_no_error(validate_rule_id_map(map[0L], registry))
})

test_that("a map that breaks a rule stops, naming the rows' old rule IDs (D5.2)", {
  registry <- data.table::data.table(rule_id = c("fk_orphan", "pk_a", "pk_b"))
  row <- function(old, new, change = "rename") {
    data.table::data.table(
      old_rule_id = old, new_rule_id = new, change = change, package_version = "0.0.0.9002"
    )
  }
  check <- function(map) validate_rule_id_map(map, registry)
  expect_error(check(row("pk_a", "pk_b")), "names registered rules as old rule IDs: pk_a")
  expect_error(check(row("gone", "also_gone")), "unregistered rules as new rule IDs for: gone")
  expect_error(check(row("gone", NA)), "blank cells in the rows of: gone")
  expect_error(check(row("gone", "pk_a", "merge")), "other than rename or split for: gone")
  expect_error(check(rbind(row("gone", "pk_a"), row("gone", "pk_a"))), "repeats a row for: gone")
  expect_error(
    check(rbind(row("gone", "pk_a"), row("gone", "pk_b"))),
    "more than one rename row for: gone"
  )
  expect_error(check(row("gone", "pk_a", "split")), "a split with one row for: gone")
  expect_error(
    check(rbind(row("gone", "pk_a"), row("gone", "pk_b", "split"))),
    "mixes rename and split for: gone"
  )
  expect_error(check(row("gone", "pk_a")[, -4L]), "has the columns old_rule_id")
})
