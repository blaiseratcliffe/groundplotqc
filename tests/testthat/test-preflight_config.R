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
  # A severity setting alone, as an argument or as the option, runs the two checks that
  # read it.
  context <- preflight_context(settings = list(severity = c(nobody = "error")))
  expect_equal(check("rule_set_unknown_column", context), "no_input")
  expect_equal(nrow(check("rule_id_unknown", context)), 1L)
  expect_equal(nrow(check("rule_set_override_invalid", context)), 0L)
  withr::local_options(groundplotqc.severity = c(nobody = "error"))
  context <- preflight_context()
  expect_equal(check("rule_set_unknown_column", context), "no_input")
  expect_equal(nrow(check("rule_id_unknown", context)), 1L)
  expect_equal(nrow(check("rule_set_override_invalid", context)), 0L)
})

test_that("rule_set_unknown_column names a missing table, attribute or any-table attribute", {
  # Rows 7 and 8: a table of the dictionary with "*" passes; an attribute of another table
  # (species is in catches) doesn't. Rows 9 and 10 have no attribute.
  rules <- rule_rows(
    rule_id = "conform",
    table_name = c(
      "stations", "*", "stands", "hauls", "*", NA, "stations", "hauls", "stations", "*"
    ),
    attribute_name = c(
      "station_id", "station_id", "*", "nothing", "nothing", "*", "*", "species", NA, NA
    ),
    severity = NA, class = NA, enabled = TRUE
  )
  found <- check("rule_set_unknown_column", test_context(rules))
  expect_equal(found$detail, c(
    paste(memory_position(3L), "names table stands, which isn't in the dictionary."),
    paste(memory_position(6L), "names table (blank), which isn't in the dictionary."),
    paste(memory_position(4L), "names hauls.nothing, which isn't in the dictionary."),
    paste(memory_position(8L), "names hauls.species, which isn't in the dictionary."),
    paste(memory_position(9L), "names stations.(blank), which isn't in the dictionary."),
    paste(
      memory_position(5L),
      "names attribute nothing for every table, but no table in the dictionary has it."
    ),
    paste(
      memory_position(10L),
      "names attribute (blank) for every table, but no table in the dictionary has it."
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

test_that("a blank table in a row is a finding even when the dictionary has one", {
  spec <- gpq_read_spec(data.frame(
    table_name = c("t", NA), attribute_name = c("id", "x"), key_type = c("PK", "."),
    data_type = "character"
  ))
  rules <- rule_rows(
    rule_id = "conform", table_name = NA, attribute_name = c("x", "*"), severity = NA,
    class = NA, enabled = TRUE
  )
  found <- check("rule_set_unknown_column", test_context(rules), spec)
  expect_equal(found$detail, paste(
    memory_position(1:2), "names table (blank), which isn't in the dictionary."
  ))
})

test_that("a disabled row is checked as any other", {
  rules <- rule_rows(
    rule_id = "nobody", table_name = "nowhere", attribute_name = "*", severity = NA,
    class = NA, enabled = FALSE
  )
  context <- test_context(rules)
  expect_equal(
    check("rule_id_unknown", context)$detail,
    paste(memory_position(1L), "names rule nobody, which isn't registered.")
  )
  expect_equal(
    check("rule_set_unknown_column", context)$detail,
    paste(memory_position(1L), "names table nowhere, which isn't in the dictionary.")
  )
})

test_that("rule_set_override_invalid holds each kind of rule to its choices (D9.21)", {
  withr::local_options(groundplotqc.severity = NULL)
  rules <- rule_rows(
    rule_id = c("conform", "conform", "plaus", "info_rule", "info_rule", "conform", "conform"),
    table_name = "*", attribute_name = "*",
    severity = c("warning", "flag", "error", "info", NA, NA, "Warning"),
    class = c("harmonization", NA, NA, "source", "none", "none", NA), enabled = TRUE
  )
  found <- check("rule_set_override_invalid", test_context(rules))
  # A severity is matched as written: "Warning" is not "warning" (D14.35).
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
      memory_position(7L), "gives conform the severity \"Warning\", which isn't allowed",
      "for it (allowed: error, warning)."
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
  # Row 2 gives a pre-flight check its registered severity and a class: one finding, the
  # row's, whatever it says.
  rules <- rule_rows(
    rule_id = c("code_list_empty_column", "dd_pk_missing"), table_name = "*",
    attribute_name = "*", severity = c(NA, "error"), class = c(NA, "source"),
    enabled = c(FALSE, TRUE)
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
      memory_position(2L), "names dd_pk_missing, a pre-flight check, which a rule set",
      "can't change."
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

test_that("no disabled row or severity entry changes a pre-flight check's outcome (D14.5)", {
  spec <- fx_fish_gear_twice_spec()
  rules <- rule_rows(
    rule_id = "code_list_duplicate_code", table_name = "*", attribute_name = "*",
    severity = "error", class = NA, enabled = FALSE
  )
  settings <- list(severity = c(code_list_duplicate_code = "error"))
  for (asked in list(list(rules, list()), list(NULL, settings))) {
    results <- preflight_checks(spec, preflight_context(asked[[1L]], asked[[2L]]))
    duplicate <- results[results$rule_id == "code_list_duplicate_code", ]
    expect_equal(duplicate$outcome, "warn")
    expect_equal(duplicate$n_findings, 1L)
    expect_equal(results$outcome[results$rule_id == "rule_set_override_invalid"], "stop")
  }
})

test_that("each tier's severity entry is checked, the same rule in both included (8.5)", {
  withr::local_options(groundplotqc.severity = c(conform = "flag", dd_pk_missing = "warning"))
  found <- check(
    "rule_set_override_invalid",
    test_context(settings = list(severity = c(conform = "warning", dd_pk_missing = "error")))
  )
  expect_equal(found$detail, c(
    paste(
      "The severity setting given as an argument names dd_pk_missing, a pre-flight check,",
      "which a setting can't change."
    ),
    paste(
      "The severity setting set as the option groundplotqc.severity names dd_pk_missing, a",
      "pre-flight check, which a setting can't change."
    ),
    paste(
      "The severity setting set as the option groundplotqc.severity gives conform the",
      "severity \"flag\", which isn't allowed for it (allowed: error, warning)."
    )
  ))
})

test_that("a finding on a rule set read from files names its file and line (D14.19)", {
  dir <- withr::local_tempdir()
  # Line 4's severity keeps its leading space, as the file has it; line 5's spaces are blank.
  writeLines(
    c(
      "rule_id,table_name,attribute_name,severity,class,enabled",
      "dd_pk_missing,*,*,,,TRUE",
      "nobody,stands,*,,,TRUE",
      "conform,*,*, warning,,TRUE",
      "conform,*,*,  ,,TRUE"
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
  expect_equal(found$detail, c(
    paste(
      "Line 2 of rules_rules.csv names dd_pk_missing, a pre-flight check, which a rule set",
      "can't change."
    ),
    paste(
      "Line 4 of rules_rules.csv gives conform the severity \" warning\", which isn't allowed",
      "for it (allowed: error, warning)."
    )
  ))
  expect_equal(found$source_cell, c("rules_rules.csv:2", "rules_rules.csv:4"))
})

test_that("a rule ID read from a file reaches the page escaped (D14.19)", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "rule_id,table_name,attribute_name,severity,class,enabled",
      "\"<b>&\"\"\",*,*,,,TRUE"
    ),
    file.path(dir, "rules_rules.csv")
  )
  spec <- fx_fish_spec()
  context <- preflight_context(read_rule_set(dir))
  page <- preflight_html(preflight_checks(spec, context), spec)
  expect_match(page, "names rule &lt;b&gt;&amp;&quot;, which isn&#39;t registered.", fixed = TRUE)
  expect_false(grepl("<b>&", page, fixed = TRUE))
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
  ran <- results[results$rule_id %in% c(
    "rule_set_unknown_column", "rule_id_unknown", "rule_set_override_invalid"
  ), ]
  expect_equal(ran$outcome, c("pass", "pass", "pass"))
})

test_that("pre-flight leaves the caller's rules, settings and text as they were (16.2)", {
  withr::local_options(groundplotqc.severity = NULL)
  spec <- fx_fish_spec()
  text <- data.table::data.table(text_id = "preflight_title", lang = "en", text = "A title")
  # copy() keeps a table's attributes, indices included, so base identical() sees any change
  # the checks make to the caller's object.
  copy_of <- data.table::copy
  stopping <- list(rules = data.table::data.table(
    rule_id = c("nobody", "conform"), table_name = c("stands", "*"),
    attribute_name = "*", severity = c(NA, "flag"), class = NA_character_, enabled = TRUE
  ))
  passing <- list(rules = data.table::data.table(
    rule_id = character(), table_name = character(), attribute_name = character(),
    severity = character(), class = character(), enabled = logical()
  ))
  settings <- list(severity = c(conform = "warning"))
  for (case in list(list(stopping, settings), list(passing, list(lang = "en")))) {
    rules <- case[[1L]]
    settings_given <- case[[2L]]
    before <- copy_of(list(rules, settings_given, text))
    tryCatch(
      gpq_preflight(spec, rules = rules, settings = settings_given, text = text),
      gpq_preflight_error = function(e) NULL
    )
    expect_true(identical(list(rules, settings_given, text), before))
  }
  # The three checks share one context, and none leaves its working columns in it.
  context <- test_context(stopping, settings, text)
  before <- copy_of(context$rules$rules)
  for (name in c("rule_set_unknown_column", "rule_id_unknown", "rule_set_override_invalid")) {
    check(name, context)
  }
  expect_true(identical(context$rules$rules, before))
  expect_named(context$rules$rules, names(rule_set_schema()$rules))
})

test_that("MAGPlot's rule set passes its checks with MAGPlot's spec (D14.2)", {
  results <- suppressWarnings(gpq_preflight(magp_spec(), rules = magp_rules()))
  rule_set <- results[results$rule_id %in% c(
    "rule_set_unknown_column", "rule_id_unknown", "rule_set_override_invalid"
  ), ]
  expect_equal(rule_set$outcome, c("pass", "pass", "pass"))
})
