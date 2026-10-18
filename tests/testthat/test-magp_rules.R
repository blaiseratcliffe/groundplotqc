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
  expect_equal(attr(rules$settings, "source_lines"), 2:11)
})

test_that("MAGPlot's rule set is written for the spec set magp_spec() compiles (D14.15)", {
  manifest <- magp_spec()$manifest
  expect_equal(
    magp_rules()$meta$spec_version, manifest$file_date[manifest$input == "dictionary"]
  )
})

test_that("MAGPlot's settings rows restate the settings, with its own values (D1.8a, D16.12)", {
  expect_equal(
    as.data.frame(magp_rules()$settings),
    data.frame(
      setting = c(
        "lang", "example_cap", "column_level_min_n", "raw_na_policy", "rogue_codes",
        "unknown_codes", "coordinate_attributes", "code_set_max_values",
        "code_set_skip_attributes", "code_set_skip_pattern"
      ),
      value = c(
        "en", "5", "10", "always", "-999; UNK; NA; N/A", "species=UNKN.SPP",
        "latitude; longitude; utm_e; utm_n", "25",
        "meas_plots; meas_frames; temporal_extent; meas_cycle; subplot",
        "_id$|^comments$|name|_date$|^src_|^meas_subplots$"
      ),
      type = c(
        "character", "integer", "integer", "character", "character", "character", "character",
        "integer", "character", "character"
      )
    ),
    ignore_attr = c("source_file", "source_lines", "source_sha256")
  )
  rules <- magp_rules()
  withr::local_options(groundplotqc.raw_na_policy = NULL, groundplotqc.unknown_codes = NULL)
  expect_equal(
    resolve_setting("unknown_codes", rules = rules),
    list(value = c(species = "UNKN.SPP"), tier = "rule set")
  )
  expect_equal(resolve_setting("raw_na_policy", rules = rules)$value, "always")
  expect_identical(resolve_setting("column_level_min_n", rules = rules)$value, 10)
})
