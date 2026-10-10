# Tests for fingerprints: an issue's readable fingerprint and its pk: form, and a rule's
# definition fingerprint (plan 9.6; D4.2, D4.7, D8.16, D9.22, D15.6 to D15.9).

test_that("an issue's fingerprint is readable text: provider key, else pk: (D4.2, D9.22)", {
  issues <- data.table::data.table(
    rule_id = "value_odd", table_name = "catches", column_name = c("weight", "weight", "*"),
    record_pk = c("c1", "c2", "*"), provider_key = c(NA, "tbl.key=7; tbl.no=2", NA)
  )
  prints <- issue_fingerprint(issues)
  expect_named(prints, c("fingerprint", "fingerprint_pk"))
  expect_equal(prints$fingerprint_pk, c(
    "value_odd|catches|weight|pk:c1", "value_odd|catches|weight|pk:c2",
    "value_odd|catches|*|pk:*"
  ))
  expect_equal(prints$fingerprint, c(
    "value_odd|catches|weight|pk:c1", "value_odd|catches|weight|tbl.key=7; tbl.no=2",
    "value_odd|catches|*|pk:*"
  ))
  # A composite PK's parts, joined with "|", stay as they are (D8.16).
  composite <- issue_fingerprint(data.table::data.table(
    rule_id = "r", table_name = "t", column_name = "c", record_pk = "a|b", provider_key = NA
  ))
  expect_equal(composite$fingerprint, "r|t|c|pk:a|b")
})

test_that("a record whose PK is missing has no pk: fingerprint, its provider key kept (D15.9)", {
  issues <- data.table::data.table(
    rule_id = "pk_rule", table_name = "t", column_name = "id", record_pk = NA_character_,
    provider_key = c(NA, "src.id=9")
  )
  prints <- issue_fingerprint(issues)
  expect_equal(prints$fingerprint_pk, c(NA_character_, NA_character_))
  expect_equal(prints$fingerprint, c(NA, "pk_rule|t|id|src.id=9"))
})

test_that("a rule's fingerprint follows its registry row, rule-set rows, settings and spec", {
  registry <- local_results_registry()
  fish <- fx_fish_spec()
  print_of <- function(rule = "value_odd", spec = fish, ...) rule_fingerprint(rule, spec, ...)
  base <- print_of()
  expect_named(base, "value_odd")
  expect_match(base, "^[0-9a-f]{64}$")
  expect_equal(print_of(), base)
  expect_equal(names(rule_fingerprint(c("value_jump", "value_odd"), fish)), c(
    "value_jump", "value_odd"
  ))
  # Its registry row, rule_version included (D15.6).
  bumped <- data.table::copy(registry)
  data.table::set(bumped, i = which(bumped$rule_id == "value_odd"), j = "rule_version", value = 2L)
  expect_false(print_of(registry = bumped) == base)
  # A rules row naming it, or "*"; not one naming another rule.
  row <- function(rule_id) {
    list(rules = data.frame(
      rule_id = rule_id, table_name = "catches", attribute_name = "*", severity = NA,
      class = NA, enabled = TRUE
    ))
  }
  empty <- print_of(rules = fx_empty_rule_set())
  expect_equal(empty, base)
  expect_false(print_of(rules = row("value_odd")) == empty)
  expect_equal(print_of(rules = row("value_jump")), empty)
  expect_false(print_of(rules = row("*")) == empty)
  # A setting it reads: the language, and its own severity entry, the argument's first.
  expect_false(print_of(settings = list(lang = "fr")) == base)
  own <- print_of(settings = list(severity = c(value_odd = "warning")))
  expect_false(own == base)
  expect_equal(print_of(settings = list(severity = c(value_jump = "flag"))), base)
  withr::local_options(groundplotqc.severity = c(value_odd = "error"))
  expect_equal(print_of(settings = list(severity = c(value_odd = "warning"))), own)
  withr::local_options(groundplotqc.severity = NULL)
  # A spec component it reads (value_jump reads codes), and not one it doesn't.
  jump <- print_of("value_jump")
  more_codes <- fish
  more_codes$codes <- rbind(fish$codes, fish$codes[1L])
  expect_false(print_of("value_jump", spec = more_codes) == jump)
  more_lists <- fish
  more_lists$code_lists <- rbind(fish$code_lists, fish$code_lists[1L])
  expect_equal(print_of("value_jump", spec = more_lists), jump)
  expect_error(print_of("no_such_rule"), "Rules no_such_rule aren't registered")
})

test_that("a rule-set component no row of which names the rule adds nothing (D15.22)", {
  local_results_registry()
  fish <- fx_fish_spec()
  print_of <- function(rules) rule_fingerprint("value_odd", fish, rules = rules)
  row <- function(rule_id) {
    data.frame(
      rule_id = rule_id, table_name = "catches", attribute_name = "*", severity = NA,
      class = NA, enabled = TRUE
    )
  }
  # No rule set, an empty one, and rows that name only other rules give one fingerprint.
  base <- print_of(NULL)
  expect_equal(print_of(fx_empty_rule_set()), base)
  expect_equal(print_of(list(rules = row("value_jump"))), base)
  # A row that names the rule changes it, and taking the row out again restores it.
  both <- rbind(row("value_jump"), row("value_odd"))
  named <- print_of(list(rules = both))
  expect_false(named == base)
  expect_equal(print_of(list(rules = both[both$rule_id != "value_odd", ])), base)
})

test_that("a rule's fingerprint leaves the spec, rules, settings and registry as they were", {
  registry <- local_results_registry()
  spec <- fx_fish_spec()
  rules <- list(rules = data.frame(
    rule_id = "value_odd", table_name = "catches", attribute_name = "*", severity = NA,
    class = NA, enabled = TRUE
  ))
  settings <- list(lang = "fr", severity = c(value_odd = "warning"))
  before <- list(
    spec = lapply(spec, data.table::copy), rules = lapply(rules, data.table::copy),
    settings = settings, registry = data.table::copy(registry)
  )
  rule_fingerprint(c("value_odd", "value_jump"), spec, rules, settings, registry)
  expect_true(identical(unclass(spec), before$spec))
  expect_true(identical(rules, before$rules))
  expect_identical(settings, before$settings)
  expect_true(identical(registry, before$registry))
})

test_that("the language reaches the fingerprint from the argument, a rule-set row or the option", {
  local_results_registry()
  fish <- fx_fish_spec()
  print_of <- function(...) rule_fingerprint("value_odd", fish, ...)
  with_lang <- function(value) {
    list(
      rules = fx_empty_rule_set()$rules,
      settings = data.frame(setting = "lang", value = value, type = "character")
    )
  }
  base <- print_of()
  from_argument <- print_of(settings = list(lang = "fr"))
  expect_false(from_argument == base)
  # A rule-set settings row gives the same fingerprint as the argument with that language.
  expect_equal(print_of(rules = with_lang("fr")), from_argument)
  # So does the option, set for this test only.
  withr::local_options(groundplotqc.lang = "fr")
  expect_equal(print_of(), from_argument)
  # Argument over rule set over option, as 3.5 orders them.
  withr::local_options(groundplotqc.lang = "es")
  expect_equal(print_of(rules = with_lang("fr")), from_argument)
  by_argument <- print_of(rules = with_lang("fr"), settings = list(lang = "de"))
  expect_equal(by_argument, print_of(settings = list(lang = "de")))
  expect_equal(print_of(), print_of(settings = list(lang = "es")))
})

test_that("a spec without a component a rule reads stops, naming it and the rules (D15.7)", {
  local_results_registry()
  fish <- fx_fish_spec()
  fish$codes <- NULL
  expect_error(
    rule_fingerprint(c("value_odd", "value_jump"), fish),
    "The specification lacks codes (read by value_jump).",
    fixed = TRUE
  )
  # A rule that doesn't read it is fingerprinted as before.
  expect_match(rule_fingerprint("value_odd", fish), "^[0-9a-f]{64}$")
  fish$attributes <- NULL
  expect_error(
    rule_fingerprint(c("value_odd", "value_jump"), fish),
    "lacks attributes (read by value_odd); codes (read by value_jump)",
    fixed = TRUE
  )
})

test_that("a spec built in memory and its compiled copy give one fingerprint (D15.7)", {
  local_results_registry()
  spec <- fx_fish_spec()
  dir <- withr::local_tempdir()
  write_compiled_spec(spec, dir)
  compiled <- read_compiled_spec(dir)
  rules <- c("value_odd", "value_jump")
  expect_equal(rule_fingerprint(rules, compiled), rule_fingerprint(rules, spec))
  # Each component's hash is the hash of its compiled file.
  for (name in names(spec_schema())) {
    expect_equal(
      sha256_bytes(compiled_bytes(spec[[name]])),
      unname(tools::sha256sum(file.path(dir, paste0(name, ".csv")))),
      label = name
    )
  }
})
