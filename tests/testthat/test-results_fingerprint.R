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
