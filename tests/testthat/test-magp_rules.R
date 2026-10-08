# Tests for MAGPlot 2.0's rule set (plan 3.5; D9.20, D14.2, D14.15).

test_that("magp_rules() gives the meta, rules and settings components, checked", {
  rules <- magp_rules()
  expect_named(rules, c("meta", "rules", "settings"))
  expect_equal(rules$meta$rule_set_name, "MAGPlot 2.0")
  expect_true(grepl("^[0-9]+$", rules$meta$version))
  expect_true(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", rules$meta$date))
  expect_identical(rules, validate_rule_set(rules))
  # Each component keeps the file it was read from and each row's line (D14.19).
  expect_equal(attr(rules$rules, "source_file"), "rules_rules.csv")
  expect_equal(attr(rules$settings, "source_lines"), 2L)
})

test_that("MAGPlot's rule set is written for the spec set magp_spec() compiles (D14.15)", {
  manifest <- magp_spec()$manifest
  expect_equal(
    magp_rules()$meta$spec_version, manifest$file_date[manifest$input == "dictionary"]
  )
})

test_that("MAGPlot's settings rows are the settings it restates: the language", {
  expect_equal(
    as.data.frame(magp_rules()$settings),
    data.frame(setting = "lang", value = "en", type = "character"),
    ignore_attr = c("source_file", "source_lines")
  )
})
