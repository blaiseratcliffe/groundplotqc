# Tests for the pre-flight checks of the rule set, the settings and the report text (plan
# 4.2; D9.21, D14.5 to D14.8, D14.13, D14.19, D14.21).

# A registry of four rules beside the real pre-flight checks: a conformance rule, a
# plausibility rule, an information rule, and one whose text no file has.
test_registry <- function() {
  extra <- data.table::data.table(
    rule_id = c("conform", "plaus", "info_rule", "untexted"), stage = c(
      "conformance", "plausibility", "conformance", "conformance"
    ),
    layer = c(1L, 6L, 4L, 1L), check_type = "test", default_severity = c(
      "error", "flag", "info", "error"
    ),
    default_class = c("depends", "depends", "none", "source"), inputs = "attributes",
    strategy = "none",
    message_id = c("preflight_title", "preflight_title", "preflight_title", "no_text")
  )
  rbind(rule_registry(), extra)
}

# A context with the test registry, as preflight_context() makes it.
test_context <- function(rules = NULL, settings = list(), text = NULL) {
  context <- preflight_context(rules, settings, text)
  context$registry <- test_registry()
  context
}

rule_rows <- function(...) {
  list(rules = data.frame(..., stringsAsFactors = FALSE))
}

check <- function(name, context, spec = fx_fish_spec()) {
  preflight_check_functions()[[name]](spec, context)
}

# Where a row of a rule set given as R objects is, as a finding's detail starts (D14.19).
memory_position <- function(row) {
  paste0("Row ", row, " of the rule set's rules (as R counts rows)")
}

test_that("the rule set's checks don't run without a rule set or a severity setting", {
  withr::local_options(groundplotqc.severity = NULL)
  context <- preflight_context()
  for (name in c("rule_set_unknown_column", "rule_id_unknown", "rule_set_override_invalid")) {
    expect_equal(check(name, context), "no_input", info = name)
  }
  # A severity setting alone runs the two checks that read it.
  context <- preflight_context(settings = list(severity = c(nobody = "error")))
  expect_equal(check("rule_set_unknown_column", context), "no_input")
  expect_equal(nrow(check("rule_id_unknown", context)), 1L)
})

test_that("rule_set_unknown_column names a missing table, attribute or any-table attribute", {
  rules <- rule_rows(
    rule_id = "conform", table_name = c("stations", "*", "stands", "hauls", "*", NA),
    attribute_name = c("station_id", "station_id", "*", "nothing", "nothing", "*"),
    severity = NA, class = NA, enabled = TRUE
  )
  found <- check("rule_set_unknown_column", test_context(rules))
  expect_equal(found$detail, c(
    paste(memory_position(3L), "names table stands, which isn't in the dictionary."),
    paste(memory_position(6L), "names table (blank), which isn't in the dictionary."),
    paste(memory_position(4L), "names hauls.nothing, which isn't in the dictionary."),
    paste(
      memory_position(5L),
      "names attribute nothing for every table, but no table in the dictionary has it."
    )
  ))
  expect_true(all(is.na(found$file)))
  expect_true(all(is.na(found$source_cell)))
})

test_that("rule_id_unknown names rows, a blank rule ID and setting entries by tier", {
  withr::local_options(groundplotqc.severity = c(by_option = "error"))
  rules <- rule_rows(
    rule_id = c("conform", "nobody", NA), table_name = "*", attribute_name = "*",
    severity = NA, class = NA, enabled = TRUE
  )
  found <- check(
    "rule_id_unknown", test_context(rules, list(severity = c(by_argument = "error")))
  )
  expect_equal(found$detail, c(
    paste(memory_position(2L), "names rule nobody, which isn't registered."),
    paste(memory_position(3L), "names rule (blank), which isn't registered."),
    "The severity setting given as an argument names rule by_argument, which isn't registered.",
    paste(
      "The severity setting set as the option groundplotqc.severity names rule by_option,",
      "which isn't registered."
    )
  ))
})

test_that("rule_set_override_invalid holds each kind of rule to its choices (D9.21)", {
  withr::local_options(groundplotqc.severity = NULL)
  rules <- rule_rows(
    rule_id = c("conform", "conform", "plaus", "info_rule", "info_rule", "conform"),
    table_name = "*", attribute_name = "*",
    severity = c("warning", "flag", "error", "info", NA, NA),
    class = c("harmonization", NA, NA, "source", "none", "none"), enabled = TRUE
  )
  found <- check("rule_set_override_invalid", test_context(rules))
  expect_equal(found$detail, c(
    paste(
      memory_position(2L), "gives conform the severity \"flag\", which isn't allowed",
      "for it (allowed: error, warning)."
    ),
    paste(
      memory_position(3L), "gives plaus the severity \"error\", which isn't allowed",
      "for it (allowed: flag)."
    ),
    paste(
      memory_position(4L), "gives info_rule the class \"source\", which isn't",
      "allowed for it (allowed: none)."
    ),
    paste(
      memory_position(6L), "gives conform the class \"none\", which isn't allowed",
      "for it (allowed: source, harmonization, depends)."
    )
  ))
})

test_that("no rule-set row or severity entry may change a pre-flight check (D14.5)", {
  rules <- rule_rows(
    rule_id = "code_list_empty_column", table_name = "*", attribute_name = "*",
    severity = NA, class = NA, enabled = FALSE
  )
  withr::local_options(groundplotqc.severity = c(dd_pk_missing = "warning"))
  found <- check(
    "rule_set_override_invalid",
    test_context(rules, list(severity = c(plaus = "warning", conform = "warning")))
  )
  expect_equal(found$detail, c(
    paste(
      memory_position(1L), "names code_list_empty_column, a pre-flight check,",
      "which a rule set can't change."
    ),
    paste(
      "The severity setting set as the option groundplotqc.severity names dd_pk_missing, a",
      "pre-flight check, which a setting can't change."
    ),
    paste(
      "The severity setting given as an argument gives plaus the severity \"warning\", which",
      "isn't allowed for it (allowed: flag)."
    )
  ))
})

test_that("a finding on a rule set read from files names its file and line (D14.19)", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "rule_id,table_name,attribute_name,severity,class,enabled",
      "dd_pk_missing,*,*,,,TRUE",
      "nobody,stands,*,,,TRUE"
    ),
    file.path(dir, "rules_rules.csv")
  )
  context <- test_context(read_rule_set(dir))
  found <- check("rule_id_unknown", context)
  expect_equal(found$detail, "Line 3 of rules_rules.csv names rule nobody, which isn't registered.")
  expect_equal(found$file, "rules_rules.csv")
  expect_equal(found$source_cell, "rules_rules.csv:3")
  found <- check("rule_set_unknown_column", context)
  expect_equal(
    found$detail, "Line 3 of rules_rules.csv names table stands, which isn't in the dictionary."
  )
  expect_equal(found$source_cell, "rules_rules.csv:3")
  found <- check("rule_set_override_invalid", context)
  expect_equal(found$detail, paste(
    "Line 2 of rules_rules.csv names dd_pk_missing, a pre-flight check, which a rule set",
    "can't change."
  ))
  expect_equal(found$source_cell, "rules_rules.csv:2")
})

test_that("an empty rule set passes all three checks", {
  withr::local_options(groundplotqc.severity = NULL)
  context <- preflight_context(fx_empty_rule_set())
  for (name in c("rule_set_unknown_column", "rule_id_unknown", "rule_set_override_invalid")) {
    expect_equal(nrow(check(name, context)), 0L, info = name)
  }
})

test_that("gpq_preflight takes rules, settings and text, and stops on a caller's error", {
  spec <- fx_fish_spec()
  expect_error(gpq_preflight(spec, rules = list(meta = data.frame())), "rules component")
  expect_error(gpq_preflight(spec, settings = list(row_cap = 10)), "row_cap")
  expect_error(gpq_preflight(spec, settings = list(lang = "French")), "Setting lang (argument)",
    fixed = TRUE
  )
  expect_error(gpq_preflight(spec, text = data.frame(text_id = "a")), "exactly the columns")
  results <- gpq_preflight(spec, rules = fx_empty_rule_set())
  ran <- results[results$rule_id %in% c("rule_set_unknown_column", "rule_id_unknown"), ]
  expect_equal(ran$outcome, c("pass", "pass"))
})

test_that("MAGPlot's rule set passes its checks with MAGPlot's spec (D14.2)", {
  results <- suppressWarnings(gpq_preflight(magp_spec(), rules = magp_rules()))
  rule_set <- results[results$rule_id %in% c(
    "rule_set_unknown_column", "rule_id_unknown", "rule_set_override_invalid"
  ), ]
  expect_equal(rule_set$outcome, c("pass", "pass", "pass"))
})
