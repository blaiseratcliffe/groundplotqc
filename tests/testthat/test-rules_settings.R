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

test_that("the declared settings are M3a's two and M4a's five (D14.9, D16.11, D16.12)", {
  expect_named(setting_definitions(), c(
    "lang", "severity", "example_cap", "column_level_min_n", "raw_na_policy", "rogue_codes",
    "unknown_codes"
  ))
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
  # The option tier names the option (RR-8).
  withr::local_options(groundplotqc.lang = "")
  expect_error(resolve_setting("lang"), "Setting lang (option groundplotqc.lang) must be one",
    fixed = TRUE
  )
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
  # The wrong type is told with the setting's type, once (RR-8).
  expect_error(
    check_settings(rules = lang_rule_set(type = "integer")),
    "give the wrong type for lang, which takes character.",
    fixed = TRUE
  )
  expect_error(
    check_settings(rules = lang_rule_set(type = NA_character_)),
    "give the wrong type for lang, which takes character.",
    fixed = TRUE
  )
  blank_setting <- lang_rule_set()
  blank_setting$settings$setting <- NA_character_
  expect_error(check_settings(rules = blank_setting), "can't give there: (blank).", fixed = TRUE)
  twice <- lang_rule_set()
  twice$settings <- rbind(twice$settings, twice$settings)
  expect_error(check_settings(rules = twice), "give lang more than once")
  # A refused row says what a settings row can give, with no "they" to resolve (RR-8).
  severity_row <- lang_rule_set()
  severity_row$settings$setting <- "severity"
  expect_error(
    check_settings(rules = severity_row),
    paste(
      "can't give there: severity. A settings row can give only lang, example_cap,",
      "column_level_min_n, raw_na_policy, rogue_codes, unknown_codes."
    ),
    fixed = TRUE
  )
  unknown_row <- lang_rule_set()
  unknown_row$settings$setting <- "row_cap"
  expect_error(
    check_settings(rules = unknown_row), "can't give there: row_cap. A settings row can give only",
    fixed = TRUE
  )
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
  expect_error(severity_entries(), "Setting severity (option groundplotqc.severity)", fixed = TRUE)
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
  expect_error(severity_entries(), "Setting severity (option groundplotqc.severity)", fixed = TRUE)
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

test_that("M4a's settings resolve through the tiers, a rule-set row read from its text (D16.12)", {
  withr::local_options(
    groundplotqc.example_cap = NULL, groundplotqc.column_level_min_n = NULL,
    groundplotqc.raw_na_policy = NULL, groundplotqc.rogue_codes = NULL,
    groundplotqc.unknown_codes = NULL
  )
  expect_equal(resolve_setting("example_cap"), list(value = 5L, tier = "built-in"))
  expect_equal(resolve_setting("column_level_min_n"), list(value = 10L, tier = "built-in"))
  expect_equal(resolve_setting("raw_na_policy")$value, "when_sentinels")
  expect_equal(resolve_setting("rogue_codes")$value, c("-999", "UNK", "NA", "N/A"))
  expect_equal(resolve_setting("unknown_codes")$value, structure(character(), names = character()))
  rows <- validate_rule_set(list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(
      setting = c("example_cap", "rogue_codes", "unknown_codes", "raw_na_policy"),
      value = c("3", "-1; ZZ", "species=UNKN.SPP; species=XX; genus=G", "never"),
      type = c("integer", "character", "character", "character")
    )
  ))
  expect_equal(resolve_setting("example_cap", rules = rows), list(value = 3, tier = "rule set"))
  expect_equal(resolve_setting("rogue_codes", rules = rows)$value, c("-1", "ZZ"))
  expect_equal(
    resolve_setting("unknown_codes", rules = rows)$value,
    c(species = "UNKN.SPP", species = "XX", genus = "G")
  )
  expect_equal(resolve_setting("raw_na_policy", rules = rows)$value, "never")
  # A blank row gives no codes, as a blank rogue_codes row does.
  blank <- validate_rule_set(list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(
      setting = c("unknown_codes", "rogue_codes"), value = NA_character_, type = "character"
    )
  ))
  expect_equal(
    resolve_setting("unknown_codes", rules = blank)$value,
    structure(character(), names = character())
  )
  expect_identical(resolve_setting("rogue_codes", rules = blank)$value, character())
  withr::local_options(groundplotqc.example_cap = 2)
  expect_equal(resolve_setting("example_cap"), list(value = 2, tier = "option"))
  expect_equal(
    resolve_setting("example_cap", list(example_cap = 7L), rows),
    list(value = 7L, tier = "argument")
  )
})

test_that("a bad value of M4a's settings stops, naming the setting and its tier", {
  withr::local_options(groundplotqc.column_level_min_n = NULL, groundplotqc.unknown_codes = NULL)
  expect_error(
    resolve_setting("column_level_min_n", list(column_level_min_n = 0)),
    "Setting column_level_min_n (argument) must be one whole number, 1 or more",
    fixed = TRUE
  )
  for (bad in list(2.5, -1, NA, c(1, 2), "3", Inf)) {
    expect_error(resolve_setting("example_cap", list(example_cap = bad)), "example_cap (argument)",
      fixed = TRUE
    )
  }
  expect_error(
    resolve_setting("raw_na_policy", list(raw_na_policy = "sometimes")), "raw_na_policy (argument)",
    fixed = TRUE
  )
  expect_error(
    resolve_setting("rogue_codes", list(rogue_codes = c("UNK", NA))), "rogue_codes (argument)",
    fixed = TRUE
  )
  expect_error(
    resolve_setting("unknown_codes", list(unknown_codes = "UNKN.SPP")), "unknown_codes (argument)",
    fixed = TRUE
  )
  rows <- function(setting, value, type) {
    validate_rule_set(list(
      rules = fx_empty_rule_set()$rules,
      settings = data.frame(setting = setting, value = value, type = type)
    ))
  }
  expect_error(
    resolve_setting("column_level_min_n", rules = rows("column_level_min_n", "ten", "integer")),
    "Setting column_level_min_n (rule set)",
    fixed = TRUE
  )
  expect_error(
    resolve_setting("unknown_codes", rules = rows("unknown_codes", "UNKN.SPP", "character")),
    "Setting unknown_codes (rule set)",
    fixed = TRUE
  )
  expect_error(
    check_settings(rules = rows("example_cap", "5", "character")),
    "example_cap, which takes integer",
    fixed = TRUE
  )
})

test_that("a code or name the settings rows' forms can't hold is refused in every tier (D16.21)", {
  withr::local_options(groundplotqc.rogue_codes = NULL, groundplotqc.unknown_codes = NULL)
  rows <- function(setting, value) {
    validate_rule_set(list(
      rules = fx_empty_rule_set()$rules,
      settings = data.frame(setting = setting, value = value, type = "character")
    ))
  }
  # No space after ";", an empty code, a code holding "=", spaces around a name, a trailing
  # "; " (D16.23), a name holding ";" (D16.24), an empty name.
  typos <- c(
    "species=A;species=B", "species=", "species=A=B", "species = UNKN.SPP",
    "species=A;  species=B", "species=A; ", "x;species=A", "=X"
  )
  for (text in typos) {
    expect_error(
      resolve_setting("unknown_codes", rules = rows("unknown_codes", text)),
      "Setting unknown_codes (rule set) must be a character vector of codes named by attribute",
      fixed = TRUE
    )
  }
  # The engine's message gives a generic example, no MAGPlot code (D16.42, m2).
  expect_error(
    resolve_setting("unknown_codes", rules = rows("unknown_codes", "=X")),
    "named by attribute, such as c(colour = \"unknown\"), no code NA",
    fixed = TRUE
  )
  for (text in c("-999;UNK", "-999;  UNK", "; -999", "-999; ")) {
    expect_error(
      resolve_setting("rogue_codes", rules = rows("rogue_codes", text)),
      paste(
        "Setting rogue_codes (rule set) must be a character vector of codes, none of them NA,",
        "empty, padded with spaces or holding \";\"."
      ),
      fixed = TRUE
    )
  }
  expect_error(
    resolve_setting("unknown_codes", list(unknown_codes = c(species = ""))),
    "unknown_codes (argument)",
    fixed = TRUE
  )
  # Names padded, holding "=" or ";" (D16.24), or empty.
  named <- list(
    c(" species" = "X"), c("a=b" = "X"), c("x;species" = "X"), c(species = "A", "B")
  )
  for (codes in named) {
    expect_error(
      resolve_setting("unknown_codes", list(unknown_codes = codes)),
      "unknown_codes (argument)",
      fixed = TRUE
    )
  }
  expect_error(
    resolve_setting("rogue_codes", list(rogue_codes = "N/A ")), "rogue_codes (argument)",
    fixed = TRUE
  )
  withr::local_options(groundplotqc.rogue_codes = c("-999", ""))
  expect_error(
    resolve_setting("rogue_codes"), "rogue_codes (option groundplotqc.rogue_codes)",
    fixed = TRUE
  )
  # Rows in D16.12's form still read.
  expect_equal(
    resolve_setting("unknown_codes", rules = rows("unknown_codes", "species=A; species=B"))$value,
    c(species = "A", species = "B")
  )
})

test_that("setting_text writes a value as settings.csv and the fingerprints read it", {
  expect_equal(setting_text("en"), "en")
  expect_equal(setting_text(10L), "10")
  # A number is written in full, whether it came as an integer or a double (D12.45).
  expect_equal(setting_text(1e5), "100000")
  expect_equal(setting_text(100000L), "100000")
  expect_equal(setting_text(c("-999", "N/A")), "-999; N/A")
  expect_equal(
    setting_text(c(species = "UNKN.SPP", species = "XX")), "species=UNKN.SPP; species=XX"
  )
  expect_identical(setting_text(character()), NA_character_)
})

test_that("a rule fingerprint follows a named setting's names as well as its codes", {
  local_results_registry()
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = "uses_codes", stage = "conformance", layer = 2L, check_type = "test",
    default_severity = "error", default_class = "harmonization",
    inputs = "settings:unknown_codes", strategy = "none", message_id = "preflight_title",
    rule_version = 1L, unit = FALSE
  ))
  spec <- fx_fish_spec()
  one <- rule_fingerprint(
    "uses_codes", spec,
    settings = list(unknown_codes = c(species = "X")), registry = registry
  )
  other <- rule_fingerprint(
    "uses_codes", spec,
    settings = list(unknown_codes = c(genus = "X")), registry = registry
  )
  expect_false(identical(one, other))
})

test_that("a rule fingerprint tells no codes from the one code \"NA\"", {
  local_results_registry()
  registry <- rbind(rule_registry(), data.table::data.table(
    rule_id = "uses_rogue", stage = "conformance", layer = 2L, check_type = "test",
    default_severity = "error", default_class = "harmonization",
    inputs = "settings:rogue_codes", strategy = "none", message_id = "preflight_title",
    rule_version = 1L, unit = FALSE
  ))
  spec <- fx_fish_spec()
  none <- rule_fingerprint(
    "uses_rogue", spec,
    settings = list(rogue_codes = character()), registry = registry
  )
  text_na <- rule_fingerprint(
    "uses_rogue", spec,
    settings = list(rogue_codes = "NA"), registry = registry
  )
  expect_false(identical(none, text_na))
  # The same codes, as an argument or as a rule-set row's text, give one fingerprint.
  rows <- validate_rule_set(list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(setting = "rogue_codes", value = "NA", type = "character")
  ))
  expect_identical(
    rule_fingerprint("uses_rogue", spec, rules = rows, registry = registry), text_na
  )
})
