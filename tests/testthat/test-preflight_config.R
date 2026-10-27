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
    message_id = c("preflight_title", "preflight_title", "preflight_title", "no_text"),
    rule_version = 1L, unit = FALSE
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
  # A stop check lowered by an entry stays a stop, beside the warn check raised above: the
  # planted spec's trees have no primary key (dd_pk_missing 1).
  results <- preflight_checks(
    fx_planted_spec(), preflight_context(NULL, list(severity = c(dd_pk_missing = "warning")))
  )
  expect_equal(results$outcome[results$rule_id == "dd_pk_missing"], "stop")
  expect_equal(results$n_findings[results$rule_id == "dd_pk_missing"], 1L)
  expect_equal(results$outcome[results$rule_id == "rule_set_override_invalid"], "stop")
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
  expect_error(gpq_preflight(spec, settings = list(colour = "red")), "colour")
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

# ---- task 6 ----

test_that("text_id_missing names a registered rule with no English text", {
  found <- check("text_id_missing", test_context())
  expect_equal(found$detail, "Rule untexted has no English report text no_text.")
  expect_equal(nrow(check("text_id_missing", preflight_context())), 0L)
  # A caller's table can supply the English row.
  text <- data.frame(text_id = "no_text", lang = "en", text = "Now there is.")
  expect_equal(nrow(check("text_id_missing", test_context(text = text))), 0L)
})

test_that("text_id_fallback lists each rule's text missing in the run language (D14.8)", {
  withr::local_options(groundplotqc.lang = NULL)
  expect_equal(nrow(check("text_id_fallback", preflight_context())), 0L)
  found <- check("text_id_fallback", preflight_context(settings = list(lang = "fr")))
  registry <- rule_registry()
  expect_equal(nrow(found), nrow(registry))
  expect_equal(
    found$detail[[1L]],
    paste(
      "Rule dd_duplicate_attribute's text dd_duplicate_attribute has no row in language fr,",
      "so English is shown."
    )
  )
  # A French row in the caller's table is found at the lookup's first step.
  text <- data.frame(text_id = "dd_pk_missing", lang = "fr", text = "Chaque table a une cle.")
  found <- check("text_id_fallback", preflight_context(settings = list(lang = "fr"), text = text))
  expect_equal(nrow(found), nrow(registry) - 1L)
  expect_false(any(grepl("dd_pk_missing", found$detail)))
})

test_that("a run language from the rule set or the option reaches text_id_fallback", {
  withr::local_options(groundplotqc.lang = "de")
  found <- check("text_id_fallback", preflight_context())
  # Without the count, grepl() over no findings would pass.
  expect_equal(nrow(found), nrow(rule_registry()))
  expect_true(all(grepl("language de", found$detail)))
  found <- check("text_id_fallback", preflight_context(fx_planted_rule_set()))
  expect_equal(nrow(found), nrow(rule_registry()))
  expect_true(all(grepl("language fr", found$detail)))
})

test_that("text_slot_unknown names each slot a caller's text adds to the package's (D14.21)", {
  text <- data.frame(
    text_id = c(
      "preflight_detail_dd_pk_missing", "preflight_detail_dd_pk_missing", "preflight_title",
      "no_such_text", "preflight_title"
    ),
    lang = c("en", "fr", "en", "en", "fr"),
    text = c(
      "{table_name} lacks a key {nobody}.", "Pas de cle.", "Title {page}", "{anything}",
      "Titre {nom}"
    )
  )
  found <- check("text_slot_unknown", preflight_context(text = text))
  # The first French row uses fewer slots, which is fine; no_such_text isn't the engine's, so
  # it is skipped; the second French row adds a slot, which is a finding in its language.
  expect_equal(found$detail, c(
    paste(
      "Text preflight_detail_dd_pk_missing in language en uses the slot {nobody}, which the",
      "package's row of that text doesn't have."
    ),
    paste(
      "Text preflight_title in language en uses the slot {page}, which the package's row of",
      "that text doesn't have."
    ),
    paste(
      "Text preflight_title in language fr uses the slot {nom}, which the package's row of",
      "that text doesn't have."
    )
  ))
  expect_true(all(is.na(found$file)))
  expect_true(all(is.na(found$source_cell)))
  stopped <- tryCatch(
    gpq_preflight(fx_fish_spec(), text = text),
    gpq_preflight_error = function(e) e$preflight
  )
  expect_equal(unique(stopped$outcome[stopped$rule_id == "text_slot_unknown"]), "stop")
})

test_that("text_slot_unknown passes with no text table or only known slots", {
  expect_equal(nrow(check("text_slot_unknown", preflight_context())), 0L)
  text <- data.frame(
    text_id = "preflight_detail_dd_pk_missing", lang = "fr", text = "Table {table_name} sans cle."
  )
  expect_equal(nrow(check("text_slot_unknown", preflight_context(text = text))), 0L)
  # A table with no rows, a row with no slots, and a table of texts the package lacks.
  tables <- list(
    data.frame(text_id = character(), lang = character(), text = character()),
    data.frame(text_id = "preflight_title", lang = "fr", text = "Titre"),
    data.frame(text_id = c("no_such_text", "nor_this"), lang = "en", text = c("{a}", "{b}"))
  )
  for (table in tables) {
    expect_equal(nrow(check("text_slot_unknown", preflight_context(text = table))), 0L)
  }
})

test_that("text_slot_unknown reports a repeated slot once and changes no table (16.2)", {
  text <- data.table::data.table(
    text_id = c("preflight_title", "preflight_title"), lang = c("en", "fr"),
    text = c("{page} and {page} again", "{nom}")
  )
  before_text <- data.table::copy(text)
  before_engine <- data.table::copy(report_texts())
  found <- check("text_slot_unknown", preflight_context(text = text))
  expect_equal(found$detail, c(
    paste(
      "Text preflight_title in language en uses the slot {page}, which the package's row of",
      "that text doesn't have."
    ),
    paste(
      "Text preflight_title in language fr uses the slot {nom}, which the package's row of",
      "that text doesn't have."
    )
  ))
  # Base identical(): any change to the caller's table or the cached engine table, indices
  # included, fails.
  expect_true(identical(text, before_text))
  expect_true(identical(report_texts(), before_engine))
})

test_that("text_slots finds the slots fill_placeholders fills, no more and no fewer", {
  templates <- c(
    "{a} and {b_1}", "{A} {a-b} {} { c } {{d}} {e", "no slots", "{a}{a} {f}", "{9}"
  )
  for (template in templates) {
    found <- text_slots(data.frame(text_id = "t", lang = "en", text = template))
    # With no values at all, fill_placeholders() stops naming each slot it would fill.
    asked <- tryCatch(fill_placeholders(template, list()), error = conditionMessage)
    filled <- if (identical(asked, template)) {
      character()
    } else {
      paste0("{", strsplit(sub("^.* needs a value for (.*)[.]$", "\\1", asked), ", ")[[1L]], "}")
    }
    expect_setequal(found$slot, filled)
  }
  expect_equal(
    text_slots(data.frame(text_id = "t", lang = "en", text = "{a}{a} {f}"))$slot,
    c("{a}", "{a}", "{f}")
  )
})

test_that("allowed_text lists each rule's allowed values in order, blank for a rule with none", {
  allowed <- data.table::data.table(
    rule_id = c("a", "b", "a", "a"), value = c("x", "y", "z", "w")
  )
  expect_equal(
    allowed_text(allowed, c("a", "c", "b", "a", NA)), c("x, z, w", "", "y", "x, z, w", "")
  )
  expect_identical(allowed_text(allowed, character()), character())
  expect_identical(allowed_text(allowed[0L], "a"), "")
  # A pre-flight check has no allowed value; an open rule has its own.
  allowed <- allowed_overrides(test_registry())$severity
  expect_equal(
    allowed_text(allowed, c("dd_pk_missing", "conform", "plaus", "info_rule")),
    c("", "error, warning", "flag", "info")
  )
})

test_that("rule_set_duplicate_row names each row repeating a rule, table and attribute (D16.6)", {
  expect_equal(check("rule_set_duplicate_row", preflight_context()), "no_input")
  rules <- rule_rows(
    rule_id = c("conform", "conform", "conform", "plaus", "conform", NA, NA),
    table_name = c("stations", "stations", "*", "stations", "stations", "hauls", "hauls"),
    attribute_name = c("*", "*", "*", "*", "*", "gear", "gear"),
    severity = c(NA, "warning", NA, NA, NA, NA, NA), class = NA,
    enabled = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  found <- check("rule_set_duplicate_row", test_context(rules))
  # Each repeat names the first row it repeats, a blank rule ID's included (D16.42, D16.43).
  expect_equal(found$detail, c(
    paste(memory_position(2L), "names rule conform for stations.*, as row 1 does."),
    paste(memory_position(5L), "names rule conform for stations.*, as row 1 does."),
    paste(memory_position(7L), "names rule (blank) for hauls.gear, as row 6 does.")
  ))
  expect_true(all(is.na(found$file)))
  expect_equal(nrow(check("rule_set_duplicate_row", test_context(fx_empty_rule_set()))), 0L)
})

test_that("a repeated row read from a file is placed by its line (D14.19, D16.6)", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "rule_id,table_name,attribute_name,severity,class,enabled",
      # A quoted cell over two lines: the rows below start on lines 4, 5 and 6, so the first
      # row is named by its own line, not by its row number.
      "plaus,hauls,\"gear", "note\",,,TRUE",
      "conform,stations,*,,,TRUE", "plaus,stations,*,,,TRUE", "conform,stations,*,warning,,TRUE"
    ),
    file.path(dir, "rules_rules.csv")
  )
  found <- check("rule_set_duplicate_row", test_context(read_rule_set(dir)))
  expect_equal(found$file, "rules_rules.csv")
  expect_equal(found$source_cell, "rules_rules.csv:6")
  expect_equal(
    found$detail,
    "Line 6 of rules_rules.csv names rule conform for stations.*, as line 4 does."
  )
})

test_that("MAGPlot's rule set has no repeated row (D16.6)", {
  context <- preflight_context(magp_rules())
  expect_equal(nrow(preflight_check_functions()$rule_set_duplicate_row(magp_spec(), context)), 0L)
})

# ---- task 14 ----

test_that("setting_unknown_name stops on each name an argument gives that the dictionary lacks", {
  # D18.45 to D18.47: an argument's names travel with the spec.
  settings <- list(
    unknown_codes = c(species = "UNK", colour = "red", colour = "?"),
    design_geometry_table = "catches", design_geometry_by = c("haul_id", "net"),
    design_geometry_columns = "weight", subplot_label_table = "nets",
    subplot_label_columns = "mesh", design_signature_table = "catches",
    design_signature_by = "species", design_signature_columns = "length"
  )
  found <- check("setting_unknown_name", test_context(settings = settings))
  detail <- function(case, ...) {
    report_text(
      paste0("preflight_detail_setting_unknown_name_", case),
      where = report_text("preflight_setting_argument"), ...
    )
  }
  # By case, as rule_set_unknown_column orders its findings; a name given twice once; with an
  # unknown table its columns aren't read (D18.46).
  expect_equal(found$detail, c(
    detail("table", setting = "subplot_label_table", table_name = "nets"),
    detail(
      "attribute",
      setting = "design_geometry_by", table_name = "catches", attribute_name = "net"
    ),
    detail(
      "attribute",
      setting = "design_signature_columns", table_name = "catches", attribute_name = "length"
    ),
    detail("any", setting = "unknown_codes", attribute_name = "colour"),
    detail(
      "key",
      setting = "design_signature_by", table_name = "catches", attribute_name = "species"
    )
  ))
  expect_true(all(is.na(found$file) & is.na(found$source_cell)))
  # No argument or rule-set setting names anything: not run; names it has: a pass.
  expect_equal(check("setting_unknown_name", test_context()), "no_input")
  known <- test_context(settings = list(coordinate_attributes = "depth"))
  expect_equal(nrow(check("setting_unknown_name", known)), 0L)
  # A value of the wrong shape is a caller's error before any check (D18.46).
  expect_error(
    gpq_preflight(fx_fish_spec(), settings = list(coordinate_attributes = NA_character_)),
    "Setting coordinate_attributes (argument)",
    fixed = TRUE
  )
})

test_that("a name from a rule set read from files is placed by its settings row (D18.46)", {
  dir <- withr::local_tempdir()
  writeLines(
    c("rule_id,table_name,attribute_name,severity,class,enabled", "conform,*,*,,,TRUE"),
    file.path(dir, "rules_rules.csv")
  )
  writeLines(
    c(
      "setting,value,type", "lang,en,character",
      "code_set_skip_attributes,station_name; notes,character"
    ),
    file.path(dir, "rules_settings.csv")
  )
  found <- check("setting_unknown_name", test_context(read_rule_set(dir)))
  expect_equal(found$detail, report_text(
    "preflight_detail_setting_unknown_name_any",
    setting = "code_set_skip_attributes", where = report_text("preflight_setting_rule_set"),
    attribute_name = "notes"
  ))
  expect_equal(found$file, "rules_settings.csv")
  expect_equal(found$source_cell, "rules_settings.csv:3")
  # Given in memory, the rule set's row has no line.
  in_memory <- list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(setting = "code_set_skip_attributes", value = "notes", type = "character")
  )
  found <- check("setting_unknown_name", test_context(in_memory))
  expect_equal(c(found$file, found$source_cell), c(NA_character_, NA_character_))
})

test_that("design_signature_by must be a foreign key linked to its parent's key (D18.42)", {
  # b's a1 points at a table with a composite key, which the dictionary links to no key.
  spec <- gpq_read_spec(data.frame(
    table_name = c("a", "a", "b", "b"), attribute_name = c("a1", "a2", "b_id", "a1"),
    key_type = c("PK", "PK", "PK", "FK"), reference_table = c(NA, NA, NA, "a"),
    data_type = "character"
  ))
  context <- test_context(settings = list(
    design_signature_table = "b", design_signature_by = "a1", design_signature_columns = "b_id"
  ))
  found <- check("setting_unknown_name", context, spec)
  expect_equal(found$detail, report_text(
    "preflight_detail_setting_unknown_name_key",
    setting = "design_signature_by", where = report_text("preflight_setting_argument"),
    table_name = "b", attribute_name = "a1"
  ))
})

test_that("option_unknown_name warns of each name an option gives that the dictionary lacks", {
  # D18.47: an option applies to every spec the session checks.
  withr::local_options(
    groundplotqc.coordinate_attributes = c("depth", "latitude"),
    groundplotqc.design_signature_table = "plots"
  )
  context <- test_context()
  found <- check("option_unknown_name", context)
  detail <- function(case, ...) {
    report_text(paste0("preflight_detail_option_unknown_name_", case), ...)
  }
  expect_equal(found$detail, c(
    detail("table", setting = "design_signature_table", table_name = "plots"),
    detail("any", setting = "coordinate_attributes", attribute_name = "latitude")
  ))
  expect_true(all(is.na(found$file) & is.na(found$source_cell)))
  # An option's names are the option check's alone.
  expect_equal(check("setting_unknown_name", context), "no_input")
  withr::local_options(
    groundplotqc.coordinate_attributes = NULL, groundplotqc.design_signature_table = NULL
  )
  expect_equal(check("option_unknown_name", test_context()), "no_input")
})

# task 14's fix round 1 (the datatable review's F1)
test_that("a design column setting given as none names nothing (D18.46)", {
  settings <- list(
    design_geometry_table = "catches", design_geometry_by = "haul_id",
    design_geometry_columns = character()
  )
  expect_equal(nrow(check("setting_unknown_name", test_context(settings = settings))), 0L)
  # MAGPlot's rule set with its signature's columns given as none.
  context <- test_context(magp_rules(), list(design_signature_columns = character()))
  expect_equal(nrow(check("setting_unknown_name", context, magp_spec())), 0L)
  # A blank rule-set row gives none too.
  blank <- list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(
      setting = c("design_geometry_table", "design_geometry_columns"),
      value = c("catches", NA), type = "character"
    )
  )
  expect_equal(nrow(check("setting_unknown_name", test_context(blank))), 0L)
})

test_that("MAGPlot's rule set names only tables and attributes its spec has (D18.45)", {
  results <- suppressWarnings(gpq_preflight(magp_spec(), rules = magp_rules()))
  expect_equal(results$outcome[results$rule_id == "setting_unknown_name"], "pass")
})

# ---- task 14, second commit ----

test_that("a run warns of an option's unknown names and ignores them (D18.47)", {
  withr::local_options(
    groundplotqc.design_geometry_table = "nets",
    groundplotqc.design_signature_columns = c("species", "length")
  )
  # The geometry rule's columns are an argument's, unread without a known table (D18.46).
  settings <- list(
    design_geometry_by = "haul_id", design_geometry_columns = "weight",
    design_signature_table = "catches", design_signature_by = "haul_id"
  )
  run <- function() {
    gpq_check_conformance(fx_fish_clean(), fx_fish_spec(), settings = settings, layers = 4)
  }
  expect_warning(run(), class = "gpq_preflight_warning")
  results <- withCallingHandlers(
    run(),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  rows <- results$results
  # The option's table is ignored, so the rule has none: not run, where reading "nets" would
  # stop on its missing columns.
  geometry <- rows[rows$rule_id == "design_geometry_inconsistent"]
  expect_equal(c(geometry$status, geometry$not_run_reason), c("not_run", "no_input"))
  # length is unknown, so the signature's columns are ignored whole, not signed by species
  # alone, and the rule has none (the final fix wave, D18.90).
  signature <- rows[rows$rule_id == "design_duplicate_signature"]
  expect_equal(c(signature$status, signature$not_run_reason), c("not_run", "no_input"))
})

# ---- M6 task 7 ----

test_that("rule_id_unknown names fallback_class's unregistered rules, by tier (D19.53)", {
  withr::local_options(groundplotqc.severity = NULL, groundplotqc.fallback_class = NULL)
  detail <- function(where, rule_id) {
    report_text(
      "preflight_detail_rule_id_unknown_fallback_class",
      where = where, rule_id = rule_id
    )
  }
  # The setting alone runs the check; registered rules pass.
  known <- test_context(settings = list(fallback_class = c(conform = "source")))
  expect_equal(nrow(check("rule_id_unknown", known)), 0L)
  given <- test_context(settings = list(
    fallback_class = c(conform = "source", nobody = "harmonization")
  ))
  found <- check("rule_id_unknown", given)
  expect_equal(found$detail, detail(report_text("preflight_setting_argument"), "nobody"))
  expect_true(is.na(found$file) && is.na(found$source_cell))
  # The option phrase names its setting (D19.67).
  withr::local_options(groundplotqc.fallback_class = c(by_option = "source"))
  found <- check("rule_id_unknown", test_context())
  expect_equal(found$detail, paste(
    "The fallback_class setting set as the option groundplotqc.fallback_class names rule",
    "by_option, which isn't registered."
  ))
  # A rule set's row gives the whole setting, the option's entry gone, placed by its line.
  dir <- withr::local_tempdir()
  writeLines(
    c("rule_id,table_name,attribute_name,severity,class,enabled", "conform,*,*,,,TRUE"),
    file.path(dir, "rules_rules.csv")
  )
  writeLines(
    c("setting,value,type", "fallback_class,conform=source; ghost=source,character"),
    file.path(dir, "rules_settings.csv")
  )
  found <- check("rule_id_unknown", test_context(read_rule_set(dir)))
  expect_equal(found$detail, detail(report_text("preflight_setting_rule_set"), "ghost"))
  expect_equal(c(found$file, found$source_cell), c("rules_settings.csv", "rules_settings.csv:2"))
  # A rule set given as R objects has no file, so its finding is placed by none.
  in_memory <- list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(setting = "fallback_class", value = "ghost=source", type = "character")
  )
  found <- check("rule_id_unknown", test_context(in_memory))
  expect_equal(found$detail, detail(report_text("preflight_setting_rule_set"), "ghost"))
  expect_true(is.na(found$file) && is.na(found$source_cell))
  # It stops pre-flight.
  withr::local_options(groundplotqc.fallback_class = NULL)
  expect_error(
    gpq_preflight(fx_fish_spec(), settings = list(fallback_class = c(ghost = "source"))),
    class = "gpq_preflight_error"
  )
})

test_that("the option phrase names the severity setting's option as before (D19.67)", {
  # setting_where() takes the setting's name; severity's findings read as they did.
  expect_equal(
    setting_where(c("argument", "option"), "severity"),
    c("given as an argument", "set as the option groundplotqc.severity")
  )
  expect_equal(
    setting_where("option", "fallback_class"), "set as the option groundplotqc.fallback_class"
  )
})
