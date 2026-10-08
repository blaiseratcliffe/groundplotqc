# Tests for settings resolution (plan 3.5, 8.5; D3.16, D9.21, D14.6, D14.9).

lang_rule_set <- function(value = "fr", type = "character") {
  validate_rule_set(list(
    rules = data.frame(
      rule_id = character(), table_name = character(), attribute_name = character(),
      severity = character(), class = character(), enabled = logical()
    ),
    settings = data.frame(setting = "lang", value = value, type = type)
  ))
}

test_that("M3a declares lang and severity only (D14.9)", {
  expect_named(setting_definitions(), c("lang", "severity"))
})

test_that("lang resolves argument > rule set > option > built-in, with its tier", {
  withr::local_options(groundplotqc.lang = NULL)
  expect_equal(resolve_setting("lang"), list(value = "en", tier = "built-in"))
  withr::local_options(groundplotqc.lang = "de")
  expect_equal(resolve_setting("lang"), list(value = "de", tier = "option"))
  expect_equal(
    resolve_setting("lang", rules = lang_rule_set()), list(value = "fr", tier = "rule set")
  )
  expect_equal(
    resolve_setting("lang", list(lang = "es"), lang_rule_set()),
    list(value = "es", tier = "argument")
  )
  expect_equal(
    resolve_setting("lang", list(lang = NULL), lang_rule_set()),
    list(value = "fr", tier = "rule set")
  )
})

test_that("a bad language stops, naming the setting and where it came from", {
  withr::local_options(groundplotqc.lang = NULL)
  expect_error(resolve_setting("lang", list(lang = 3)), "Setting lang (argument)", fixed = TRUE)
  expect_error(
    resolve_setting("lang", list(lang = c("en", "fr"))), "Setting lang (argument)",
    fixed = TRUE
  )
  expect_error(
    resolve_setting("lang", list(lang = NA_character_)), "Setting lang (argument)",
    fixed = TRUE
  )
  expect_error(resolve_setting("lang", list(lang = "EN")), "lower-case")
  expect_error(resolve_setting("lang", rules = lang_rule_set(" fr")), "(rule set)", fixed = TRUE)
  expect_error(
    resolve_setting("lang", rules = lang_rule_set(NA_character_)), "Setting lang (rule set)",
    fixed = TRUE
  )
  withr::local_options(groundplotqc.lang = "")
  expect_error(resolve_setting("lang"), "(option)", fixed = TRUE)
  expect_error(resolve_setting("no_such"), "No setting is named no_such")
})

test_that("resolve_setting refuses severity and points to severity_entries (D14.29)", {
  withr::local_options(groundplotqc.severity = NULL)
  expect_error(
    resolve_setting("severity", list(), NULL),
    "The severity setting is resolved per rule by severity_entries(), not by resolve_setting().",
    fixed = TRUE
  )
  expect_error(
    resolve_setting("severity", list(severity = c(a = "warning")), lang_rule_set()),
    "resolved per rule by severity_entries()",
    fixed = TRUE
  )
  withr::local_options(groundplotqc.severity = c(a = "warning"))
  expect_error(resolve_setting("severity"), "severity_entries()", fixed = TRUE)
})

test_that("check_settings refuses an unnamed list, an unknown setting and bad rule-set rows", {
  expect_null(check_settings())
  expect_null(check_settings(list(lang = "fr")))
  expect_null(check_settings(list(lang = "fr", severity = c(a = "warning")), lang_rule_set()))
  expect_error(check_settings(list("en")), "each named once")
  expect_error(check_settings(list(lang = "en", lang = "fr")), "each named once")
  expect_error(check_settings(data.frame(lang = "en")), "each named once")
  expect_error(check_settings(list(row_cap = 10)), "doesn't have: row_cap")
  expect_error(check_settings(rules = lang_rule_set(type = "integer")), "wrong type for lang")
  expect_error(
    check_settings(rules = lang_rule_set(type = NA_character_)), "wrong type for lang"
  )
  blank_setting <- lang_rule_set()
  blank_setting$settings$setting <- NA_character_
  expect_error(check_settings(rules = blank_setting), "can't give there: (blank).", fixed = TRUE)
  twice <- lang_rule_set()
  twice$settings <- rbind(twice$settings, twice$settings)
  expect_error(check_settings(rules = twice), "give lang more than once")
  severity_row <- lang_rule_set()
  severity_row$settings$setting <- "severity"
  expect_error(check_settings(rules = severity_row), "can't give there: severity")
  unknown_row <- lang_rule_set()
  unknown_row$settings$setting <- "row_cap"
  expect_error(check_settings(rules = unknown_row), "can't give there: row_cap")
})

test_that("severity_entries lists the argument's and the option's entries with their tier", {
  withr::local_options(groundplotqc.severity = c(rule_b = "flag"))
  entries <- severity_entries(list(severity = c(rule_a = "warning")))
  expect_equal(as.data.frame(entries), data.frame(
    rule_id = c("rule_a", "rule_b"), severity = c("warning", "flag"),
    tier = c("argument", "option")
  ))
  same_rule <- severity_entries(list(severity = c(rule_b = "warning")))
  expect_equal(as.data.frame(same_rule), data.frame(
    rule_id = c("rule_b", "rule_b"), severity = c("warning", "flag"),
    tier = c("argument", "option")
  ))
  withr::local_options(groundplotqc.severity = NULL)
  expect_equal(nrow(severity_entries()), 0L)
  expect_named(severity_entries(), c("rule_id", "severity", "tier"))
  several <- severity_entries(list(severity = c(rule_a = "warning", rule_b = "error")))
  expect_equal(as.data.frame(several), data.frame(
    rule_id = c("rule_a", "rule_b"), severity = c("warning", "error"), tier = "argument"
  ))
  none <- severity_entries(list(severity = structure(character(), names = character())))
  expect_equal(nrow(none), 0L)
  expect_named(none, c("rule_id", "severity", "tier"))
})

test_that("a severity setting of the wrong shape stops, naming its tier", {
  withr::local_options(groundplotqc.severity = NULL)
  wrong <- list(
    "warning", c(a = NA_character_), c(a = "warning", a = "error"), c(a = 1),
    structure("warning", names = NA_character_), c(" " = "warning")
  )
  for (x in wrong) {
    expect_error(
      severity_entries(list(severity = x)), "Setting severity (argument)",
      fixed = TRUE, info = paste(deparse(x), collapse = " ")
    )
  }
  withr::local_options(groundplotqc.severity = "warning")
  expect_error(severity_entries(), "Setting severity (option)", fixed = TRUE)
})

test_that("an invalid byte or a matrix stops with the setting's own message", {
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  named_bad <- "error"
  names(named_bad) <- bad
  expect_error(
    severity_entries(list(severity = named_bad)), "Setting severity (argument)",
    fixed = TRUE
  )
  expect_error(
    severity_entries(list(severity = c(a = bad))), "Setting severity (argument)",
    fixed = TRUE
  )
  withr::local_options(groundplotqc.severity = named_bad)
  expect_error(severity_entries(), "Setting severity (option)", fixed = TRUE)
  withr::local_options(groundplotqc.severity = NULL)
  expect_no_warning(expect_error(
    resolve_setting("lang", list(lang = bad)), "Setting lang (argument)",
    fixed = TRUE
  ))
  expect_error(resolve_setting("lang", list(lang = matrix("fr"))), "Setting lang (argument)",
    fixed = TRUE
  )
  unknown <- list(1)
  names(unknown) <- bad
  expect_error(check_settings(unknown), "doesn't have: ok<97>.", fixed = TRUE)
})
