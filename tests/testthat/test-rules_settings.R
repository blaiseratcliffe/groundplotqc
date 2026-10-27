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

test_that("the declared settings are M3a's two, M4a's five, M5's, M6's and M7a's (D20.39)", {
  expect_named(setting_definitions(), c(
    "lang", "severity", "example_cap", "row_cap", "cell_cap", "column_level_min_n",
    "raw_na_policy", "rogue_codes",
    "unknown_codes", "coordinate_attributes", "code_set_max_values", "code_set_skip_attributes",
    "code_set_skip_pattern", "design_geometry_table", "design_geometry_by",
    "design_geometry_columns", "subplot_label_table", "subplot_label_columns",
    "design_signature_table", "design_signature_by", "design_signature_columns",
    "fallback_class"
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
  expect_error(check_settings(list(colour = "red")), "doesn't have: colour")
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
      "can't give there: severity. A settings row can give only lang, example_cap, row_cap,",
      "cell_cap, column_level_min_n, raw_na_policy, rogue_codes, unknown_codes,",
      "coordinate_attributes,",
      "code_set_max_values, code_set_skip_attributes, code_set_skip_pattern,",
      "design_geometry_table, design_geometry_by, design_geometry_columns, subplot_label_table,",
      "subplot_label_columns, design_signature_table, design_signature_by,",
      "design_signature_columns, fallback_class."
    ),
    fixed = TRUE
  )
  unknown_row <- lang_rule_set()
  unknown_row$settings$setting <- "colour"
  expect_error(
    check_settings(rules = unknown_row), "can't give there: colour. A settings row can give only",
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

test_that("coordinate_attributes resolves through the tiers; a bad name is refused (D18.9)", {
  withr::local_options(groundplotqc.coordinate_attributes = NULL)
  expect_identical(resolve_setting("coordinate_attributes")$value, character())
  rows <- function(value) {
    validate_rule_set(list(
      rules = fx_empty_rule_set()$rules,
      settings = data.frame(setting = "coordinate_attributes", value = value, type = "character")
    ))
  }
  expect_equal(
    resolve_setting("coordinate_attributes", rules = rows("latitude; longitude")),
    list(value = c("latitude", "longitude"), tier = "rule set")
  )
  # A blank row gives none.
  expect_identical(
    resolve_setting("coordinate_attributes", rules = rows(NA_character_))$value, character()
  )
  wants <- paste(
    "must be a character vector of attribute names, none of them NA, empty, padded with",
    "spaces or holding \";\"."
  )
  for (bad in list(c("latitude", NA), "", " latitude", "a;b", c(x = "latitude"), 1)) {
    expect_error(
      resolve_setting("coordinate_attributes", list(coordinate_attributes = bad)),
      paste("Setting coordinate_attributes (argument)", wants),
      fixed = TRUE, info = deparse(bad)
    )
  }
  expect_error(
    resolve_setting("coordinate_attributes", rules = rows("latitude;longitude")),
    paste("Setting coordinate_attributes (rule set)", wants),
    fixed = TRUE
  )
  withr::local_options(groundplotqc.coordinate_attributes = "depth")
  expect_equal(
    resolve_setting("coordinate_attributes"),
    list(value = "depth", tier = "option")
  )
})

# ---- task 7 ----

test_that("the code-set settings resolve, refuse what they can't read (D18.20, D18.25)", {
  withr::local_options(
    groundplotqc.code_set_max_values = NULL, groundplotqc.code_set_skip_attributes = NULL,
    groundplotqc.code_set_skip_pattern = NULL
  )
  expect_equal(resolve_setting("code_set_max_values"), list(value = 25L, tier = "built-in"))
  expect_identical(resolve_setting("code_set_skip_attributes")$value, character())
  expect_identical(resolve_setting("code_set_skip_pattern")$value, character())
  rows <- validate_rule_set(list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(
      setting = c("code_set_max_values", "code_set_skip_attributes", "code_set_skip_pattern"),
      value = c("12", "subplot; meas_cycle", "_id$|^comments$"),
      type = c("integer", "character", "character")
    )
  ))
  expect_equal(resolve_setting("code_set_max_values", rules = rows)$value, 12)
  expect_equal(
    resolve_setting("code_set_skip_attributes", rules = rows)$value, c("subplot", "meas_cycle")
  )
  expect_equal(resolve_setting("code_set_skip_pattern", rules = rows)$value, "_id$|^comments$")
  for (bad in list(0, 2.5, NA, "3", c(1, 2))) {
    expect_error(
      resolve_setting("code_set_max_values", list(code_set_max_values = bad)),
      "Setting code_set_max_values (argument) must be one whole number, 1 or more",
      fixed = TRUE, info = deparse(bad)
    )
  }
  expect_error(
    resolve_setting("code_set_skip_attributes", list(code_set_skip_attributes = "a;b")),
    "Setting code_set_skip_attributes (argument) must be a character vector of attribute names",
    fixed = TRUE
  )
  for (bad in list("(", c("a", "b"), NA_character_, 1)) {
    expect_error(
      resolve_setting("code_set_skip_pattern", list(code_set_skip_pattern = bad)),
      "Setting code_set_skip_pattern (argument) must be one regular expression R can read, or none",
      fixed = TRUE, info = deparse(bad)
    )
  }
  expect_identical(
    resolve_setting("code_set_skip_pattern", list(code_set_skip_pattern = character()))$value,
    character()
  )
})

# ---- task 14 ----

test_that("the design candidates' settings resolve; a table setting takes one name (D18.39)", {
  designs <- c(
    "design_geometry_table", "design_geometry_by", "design_geometry_columns",
    "subplot_label_table", "subplot_label_columns", "design_signature_table",
    "design_signature_by", "design_signature_columns"
  )
  for (name in designs) {
    expect_identical(resolve_setting(name)$value, character(), info = name)
  }
  rows <- validate_rule_set(list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(
      setting = c("design_geometry_table", "design_geometry_by"),
      value = c("magp_design_frames", "magp_design_id; subplot"), type = "character"
    )
  ))
  expect_equal(
    resolve_setting("design_geometry_table", rules = rows),
    list(value = "magp_design_frames", tier = "rule set")
  )
  expect_equal(
    resolve_setting("design_geometry_by", rules = rows)$value, c("magp_design_id", "subplot")
  )
  one_table <- paste(
    "must be one table name, not NA, empty, padded with spaces or holding \";\",",
    "or none."
  )
  for (name in c("design_geometry_table", "subplot_label_table", "design_signature_table")) {
    for (bad in list(c("a", "b"), NA_character_, " a", "a;b", 1)) {
      given <- list(bad)
      names(given) <- name
      expect_error(
        resolve_setting(name, given), paste0("Setting ", name, " (argument) ", one_table),
        fixed = TRUE, info = paste(name, deparse(bad))
      )
    }
  }
  expect_error(
    resolve_setting("design_signature_by", list(design_signature_by = c("a", "b"))),
    paste(
      "Setting design_signature_by (argument) must be one attribute name, not NA, empty,",
      "padded with spaces or holding \";\", or none."
    ),
    fixed = TRUE
  )
  expect_error(
    resolve_setting("design_signature_columns", list(design_signature_columns = "a;b")),
    "Setting design_signature_columns (argument) must be a character vector of attribute names",
    fixed = TRUE
  )
})

# ---- the Copilot fix round ----

test_that("the design candidates' attribute lists refuse a repeated name in every tier (D18.127)", {
  wants <- paste(
    "must be a character vector of attribute names, each given once, none of them NA, empty,",
    "padded with spaces or holding \";\"."
  )
  rows <- function(name, value) {
    validate_rule_set(list(
      rules = fx_empty_rule_set()$rules,
      settings = data.frame(setting = name, value = value, type = "character")
    ))
  }
  lists <- c(
    "design_geometry_by", "design_geometry_columns", "subplot_label_columns",
    "design_signature_columns"
  )
  for (name in lists) {
    given <- function(value) structure(list(value), names = name)
    option <- paste0("groundplotqc.", name)
    for (repeated in list(c("v", "v"), c("u", "v", "u"))) {
      expect_error(
        resolve_setting(name, given(repeated)), paste0("Setting ", name, " (argument) ", wants),
        fixed = TRUE, info = paste(name, deparse(repeated))
      )
    }
    expect_error(
      resolve_setting(name, rules = rows(name, "v; v")),
      paste0("Setting ", name, " (rule set) ", wants),
      fixed = TRUE, info = name
    )
    withr::with_options(structure(list(c("v", "v")), names = option), {
      expect_error(
        resolve_setting(name), paste0("Setting ", name, " (option ", option, ") ", wants),
        fixed = TRUE, info = name
      )
      # Only the tier that wins is checked: a valid argument or rule-set row passes over the
      # option's repeat.
      expect_equal(
        resolve_setting(name, given("v")), list(value = "v", tier = "argument"),
        info = name
      )
      expect_equal(
        resolve_setting(name, rules = rows(name, "u; v")),
        list(value = c("u", "v"), tier = "rule set"),
        info = name
      )
    })
    # No names, and names given once, are still accepted.
    expect_identical(resolve_setting(name, given(character()))$value, character(), info = name)
    expect_equal(resolve_setting(name, given(c("u", "v")))$value, c("u", "v"), info = name)
  }
})

# ---- M6 task 7 ----

test_that("fallback_class resolves whole by tier, each class source or harmonization (D19.53)", {
  withr::local_options(groundplotqc.fallback_class = NULL)
  none <- structure(character(), names = character())
  expect_equal(resolve_setting("fallback_class"), list(value = none, tier = "built-in"))
  withr::local_options(groundplotqc.fallback_class = c(rule_a = "source", rule_b = "harmonization"))
  expect_equal(resolve_setting("fallback_class")$tier, "option")
  # The tier that gives it gives all of it: an argument naming one rule leaves rule_b none.
  expect_equal(
    resolve_setting("fallback_class", list(fallback_class = c(rule_a = "harmonization"))),
    list(value = c(rule_a = "harmonization"), tier = "argument")
  )
  rows <- function(value) {
    validate_rule_set(list(
      rules = fx_empty_rule_set()$rules,
      settings = data.frame(setting = "fallback_class", value = value, type = "character")
    ))
  }
  expect_equal(
    resolve_setting("fallback_class", rules = rows("rule_c=source; rule_d=harmonization")),
    list(value = c(rule_c = "source", rule_d = "harmonization"), tier = "rule set")
  )
  # A blank row gives none, over the option.
  expect_equal(
    resolve_setting("fallback_class", rules = rows(NA_character_)),
    list(value = none, tier = "rule set")
  )
  wants <- "must be a character vector of classes named by rule ID"
  wrong <- list(
    c(rule_a = "depends"), c(rule_a = NA_character_), "source",
    c(rule_a = "source", rule_a = "harmonization"), c(" rule_a" = "source"),
    c("a=b" = "source"), c(rule_a = 1)
  )
  for (bad in wrong) {
    expect_error(
      resolve_setting("fallback_class", list(fallback_class = bad)),
      paste("Setting fallback_class (argument)", wants),
      fixed = TRUE, info = deparse(bad)
    )
  }
  expect_error(
    resolve_setting("fallback_class", rules = rows("rule_a=unclassified")),
    paste("Setting fallback_class (rule set)", wants),
    fixed = TRUE
  )
  # A rule named twice is refused in the rule-set tier and the option tier too (D19.62).
  expect_error(
    resolve_setting("fallback_class", rules = rows("rule_a=source; rule_a=harmonization")),
    paste("Setting fallback_class (rule set)", wants),
    fixed = TRUE
  )
  withr::local_options(groundplotqc.fallback_class = c(rule_a = "Source"))
  expect_error(
    resolve_setting("fallback_class"),
    paste("Setting fallback_class (option groundplotqc.fallback_class)", wants),
    fixed = TRUE
  )
  withr::local_options(
    groundplotqc.fallback_class = c(rule_a = "source", rule_a = "harmonization")
  )
  expect_error(
    resolve_setting("fallback_class"),
    paste("Setting fallback_class (option groundplotqc.fallback_class)", wants),
    fixed = TRUE
  )
  # settings.csv writes it as a rule set's row gives it.
  expect_equal(
    setting_text(c(rule_a = "source", rule_b = "harmonization")),
    "rule_a=source; rule_b=harmonization"
  )
})

# ---- M7a task 6 ----

test_that("row_cap and cell_cap resolve by tier, each a whole number 0 or more (D4.18, D20.39)", {
  withr::local_options(groundplotqc.row_cap = NULL, groundplotqc.cell_cap = NULL)
  expect_equal(resolve_setting("row_cap"), list(value = 500L, tier = "built-in"))
  expect_equal(resolve_setting("cell_cap"), list(value = 100L, tier = "built-in"))
  rows <- validate_rule_set(list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(
      setting = c("row_cap", "cell_cap"), value = c("20", "0"), type = "integer"
    )
  ))
  expect_equal(resolve_setting("row_cap", rules = rows), list(value = 20, tier = "rule set"))
  expect_equal(resolve_setting("cell_cap", rules = rows), list(value = 0, tier = "rule set"))
  withr::local_options(groundplotqc.row_cap = 50, groundplotqc.cell_cap = 200L)
  expect_equal(resolve_setting("row_cap"), list(value = 50, tier = "option"))
  expect_equal(
    resolve_setting("cell_cap", list(cell_cap = 30L), rows), list(value = 30L, tier = "argument")
  )
  for (name in c("row_cap", "cell_cap")) {
    for (bad in list(2.5, -1, NA, c(1, 2), "3", Inf)) {
      given <- list(bad)
      names(given) <- name
      expect_error(
        resolve_setting(name, given),
        paste0("Setting ", name, " (argument) must be one whole number, 0 or more."),
        fixed = TRUE, info = paste(name, deparse(bad))
      )
    }
  }
  wrong_type <- validate_rule_set(list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(setting = "cell_cap", value = "100", type = "character")
  ))
  expect_error(
    check_settings(rules = wrong_type), "give the wrong type for cell_cap, which takes integer.",
    fixed = TRUE
  )
  # settings.csv writes each as text, with its tier.
  written <- settings_metadata(list(row_cap = 10L))
  at <- match(c("row_cap", "cell_cap"), written$setting)
  expect_equal(written$value[at], c("10", "200"))
  expect_equal(written$tier[at], c("argument", "option"))
})
