# Tests for conditions in D8.10's grammar (plan 5.5, 10.1; D8.10, D19.43; R3). The spec and
# tables are helper-lineage.R's.

test_that("the five forms parse, each on an attribute, a chain or a flag", {
  spec <- fx_lineage_spec()
  parse <- function(text) parse_condition(text, "trees", spec$keys, spec$attributes)
  equal <- parse("kind == \"A\"")
  expect_equal(equal$operator, "==")
  expect_equal(equal$operand$attribute, "kind")
  expect_equal(equal$values, "A")
  expect_equal(parse("kind != 'A'")$values, "A")
  expect_equal(parse("kind %in% c(\"A\", 'L', 3)")$values, c("A", "L", "3"))
  expect_equal(parse("is_present(kind)")$operator, "is_present")
  expect_equal(parse("is_sentinel(site_id > src_site_id)")$operand$table, "sites")
  expect_equal(parse("flag.age_tree == \"Y\"")$operand, list(flag = "age_tree"))
  # A literal may hold an operator or a comma.
  expect_equal(parse("kind == \"a == b, c\"")$values, "a == b, c")
})

test_that("anything else doesn't parse, never guessed (R3)", {
  spec <- fx_lineage_spec()
  parse <- function(text) parse_condition(text, "trees", spec$keys, spec$attributes)
  for (text in c(
    "kind==\"A\"", "kind == A", "kind == \"A\" & kind == \"L\"", "!is_present(kind)",
    "nowhere == \"A\"", "site_id > nowhere == \"A\"", "site_id>src_site_id == \"S9\"",
    "kind %in% \"A\"", "kind %in% c(\"A\",\"L\")", "kind == \"A\", \"L\"", " kind == \"A\"",
    "is_present( kind )", "flag. == \"Y\"", "kind > \"A\"", NA, ""
  )) {
    expect_null(parse(text), label = text)
  }
  expect_null(
    parse_condition(c("kind == \"A\"", "kind == \"L\""), "trees", spec$keys, spec$attributes)
  )
})

test_that("a condition holds where its value's text matches, never on NA (R3)", {
  spec <- fx_lineage_spec()
  tables <- fx_lineage_tables()
  tables$trees$kind[[3L]] <- NA
  tables$trees$kind[[6L]] <- "X"
  tables$trees$site_id[[2L]] <- "nowhere"
  input <- conformance_input(tables, spec)
  holds <- function(text, rows = 1:8) {
    condition <- parse_condition(text, "trees", spec$keys, spec$attributes)
    condition_rows(input, "trees", condition, rows)
  }
  expect_equal(holds("kind == \"A\""), c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(holds("kind != \"A\""), c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(holds("kind %in% c(\"A\", \"X\")")[5:6], c(TRUE, TRUE))
  expect_equal(holds("is_present(kind)")[c(3L, 5L, 6L)], c(FALSE, TRUE, FALSE))
  expect_equal(holds("is_sentinel(kind)")[c(3L, 5L, 6L)], c(FALSE, FALSE, TRUE))
  # A broken chain holds nothing, != included.
  expect_equal(holds("site_id > src_site_id != \"S9\"")[1:2], c(TRUE, FALSE))
  # A number literal is compared as written with the number's text.
  expect_equal(holds("height == -5")[[1L]], TRUE)
  expect_equal(holds("height == -5.0")[[1L]], FALSE)
  expect_equal(holds("kind == \"A\"", c(5L, NA)), c(TRUE, FALSE))
})
