# Tests for the rule registry (plan 9.7; D7.25, D9.21, D14.5, D14.10).

test_that("the registry has its columns and one row per rule", {
  registry <- rule_registry()
  expect_equal(vapply(registry, function(x) class(x)[[1L]], ""), c(
    rule_id = "character", stage = "character", layer = "integer", check_type = "character",
    default_severity = "character", default_class = "character", inputs = "character",
    strategy = "character", message_id = "character"
  ))
  expect_equal(anyDuplicated(registry$rule_id), 0L)
  expect_false(anyNA(registry[, !"layer"], recursive = TRUE))
})

test_that("every rule's severity and class fit its stage (D8.12, D9.21)", {
  registry <- rule_registry()
  expect_true(all(registry$stage %in% c("conformance", "plausibility")))
  conformance <- registry$stage == "conformance"
  expect_true(all(registry$default_severity[conformance] %in% c("error", "warning", "info")))
  expect_true(all(registry$default_severity[!conformance] %in% c("flag", "info")))
  quiet <- registry$default_severity == "info" | registry$check_type == "preflight"
  expect_true(all(registry$default_class[quiet] == "none"))
  expect_true(all(registry$default_class[!quiet] %in% c("source", "harmonization", "depends")))
})

test_that("pre-flight checks are registered as 4.2 and D7.23 say, in the checks' order", {
  registry <- rule_registry()
  preflight <- registry[registry$check_type == "preflight", ]
  expect_equal(preflight$rule_id, names(preflight_check_functions()))
  expect_true(all(preflight$stage == "conformance"))
  expect_true(all(is.na(preflight$layer)))
  expect_true(all(preflight$default_severity %in% c("error", "warning")))
  expect_true(all(preflight$strategy == "none"))
  expect_equal(preflight$message_id, preflight$rule_id)
})

test_that("preflight_rules() reads the registry: error stops, warning warns (D14.5)", {
  registry <- rule_registry()
  rules <- preflight_rules()
  expect_equal(rules$rule_id, registry$rule_id[registry$check_type == "preflight"])
  severity <- registry$default_severity[match(rules$rule_id, registry$rule_id)]
  expect_equal(rules$on_failure, ifelse(severity == "error", "stop", "warn"))
})

test_that("allowed_overrides gives each kind of rule its choices, a pre-flight check none", {
  registry <- data.table::data.table(
    rule_id = c("conform", "plaus", "info_rule", "check"),
    stage = c("conformance", "plausibility", "conformance", "conformance"),
    check_type = c("structure", "range", "structure", "preflight"),
    default_severity = c("error", "flag", "info", "error")
  )
  allowed <- allowed_overrides(registry)
  joined <- function(table) {
    vapply(c("conform", "plaus", "info_rule", "check"), function(id) {
      paste(table$value[table$rule_id == id], collapse = " ")
    }, "")
  }
  expect_equal(joined(allowed$severity), c(
    conform = "error warning", plaus = "flag", info_rule = "info", check = ""
  ))
  expect_equal(joined(allowed$class), c(
    conform = "source harmonization depends", plaus = "source harmonization depends",
    info_rule = "none", check = ""
  ))
})
