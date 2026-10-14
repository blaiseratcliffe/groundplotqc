# Tests for the rule registry (plan 9.7; D7.25, D9.21, D14.5, D14.10, D15.6, D15.11, D15.13,
# D15.20, D15.21).

test_that("the registry has its columns and one row per rule", {
  registry <- rule_registry()
  expect_equal(vapply(registry, function(x) class(x)[[1L]], ""), c(
    rule_id = "character", stage = "character", layer = "integer", check_type = "character",
    default_severity = "character", default_class = "character", inputs = "character",
    strategy = "character", message_id = "character", rule_version = "integer"
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

test_that("preflight_rules() follows the registry it is given, not a fixed list (D14.5)", {
  stand_in <- data.table::data.table(
    rule_id = c("check_a", "plain_rule", "check_b"),
    check_type = c("preflight", "structure", "preflight"),
    default_severity = c("warning", "error", "error")
  )
  testthat::local_mocked_bindings(rule_registry = function() stand_in)
  rules <- preflight_rules()
  expect_equal(rules$rule_id, c("check_a", "check_b"))
  expect_equal(rules$on_failure, c("warn", "stop"))
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

test_that("empty override tables for pre-flight checks have the whole registry's columns (D14.5)", {
  registry <- rule_registry()
  allowed <- allowed_overrides(registry[registry$check_type == "preflight", ])
  for (kind in c("severity", "class")) {
    expect_named(allowed[[kind]], c("rule_id", "value"))
    expect_equal(nrow(allowed[[kind]]), 0L)
  }
  full <- allowed_overrides(registry)
  expect_named(full$severity, c("rule_id", "value"))
  expect_named(full$class, c("rule_id", "value"))
})

test_that("the rules that read unknown codes or code lists name them in their inputs", {
  # A rule's fingerprint covers what it reads (D15.6, 9.6; D16.42, m25; R75; D16.45, m51).
  registry <- rule_registry()
  inputs <- strsplit(registry$inputs, "; ", fixed = TRUE)
  names(inputs) <- registry$rule_id
  for (rule in c("coercion_loss", "date_unparseable")) {
    expect_true(all(c("code_list_map", "settings:unknown_codes") %in% inputs[[rule]]), info = rule)
  }
  for (rule in c("sentinel_wrong_type", "sentinel_rogue_code", "species_unknown_code")) {
    expect_true("settings:unknown_codes" %in% inputs[[rule]], info = rule)
  }
})

test_that("the record rules name column_level_min_n in their inputs (D16.47, m60)", {
  # column_level_min_n decides whether a record rule's failures are record issues or one
  # column-level issue (6.3), so the fingerprint of every rule with record outcomes covers it
  # (9.6, D4.7); a unit rule's outcomes are never collapsed. The record rules are found by
  # their outcomes on the fish survey, with an unknown code so species_unknown_code runs.
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(unknown_codes = c(species = "UNK")), 0:2)
  input <- conformance_input(fx_fish_clean(), spec)
  found <- c(
    check_structure(input, context), check_types(input, context), check_sentinels(input, context)
  )
  records <- vapply(found, function(outcomes) {
    any(vapply(outcomes, function(outcome) identical(outcome$kind, "record"), logical(1)))
  }, logical(1))
  expect_identical(sum(records), 10L)
  registry <- rule_registry()
  inputs <- strsplit(registry$inputs, "; ", fixed = TRUE)
  names(inputs) <- registry$rule_id
  for (rule in names(found)) {
    expect_identical(
      "settings:column_level_min_n" %in% inputs[[rule]], records[[rule]],
      info = rule
    )
  }
})

test_that("a severity setting entry changes its conformance rule's fingerprint only (D16.54)", {
  # The severity setting gives a conformance rule its severity, so every one reads it, the
  # settings entries in their names' order (R86); the fingerprint takes in the rule's own
  # entry, the argument's before the option's (9.6, D4.7, R20).
  registry <- rule_registry()
  open <- registry[registry$check_type == "conformance"]
  for (k in seq_len(nrow(open))) {
    inputs <- strsplit(open$inputs[[k]], "; ", fixed = TRUE)[[1L]]
    named <- inputs[startsWith(inputs, "settings:")]
    expect_true("settings:severity" %in% named, info = open$rule_id[[k]])
    expect_identical(named, sort(named, method = "radix"), info = open$rule_id[[k]])
  }
  spec <- fx_fish_spec()
  ids <- c("raw_na", "class_mismatch", "table_missing")
  plain <- rule_fingerprint(ids, spec)
  given <- rule_fingerprint(ids, spec, settings = list(severity = c(raw_na = "warning")))
  expect_false(given[["raw_na"]] == plain[["raw_na"]])
  expect_identical(given[ids[-1L]], plain[ids[-1L]])
  withr::local_options(groundplotqc.severity = c(class_mismatch = "warning"))
  option <- rule_fingerprint(ids, spec)
  expect_false(option[["class_mismatch"]] == plain[["class_mismatch"]])
  expect_identical(option[ids[-2L]], plain[ids[-2L]])
})

test_that("every rule has a rule_version, a whole number from 1 (D15.6)", {
  registry <- rule_registry()
  expect_type(registry$rule_version, "integer")
  expect_false(anyNA(registry$rule_version))
  expect_true(all(registry$rule_version >= 1L))
})

# The top-level objects each file under R/ defines, read from the source tree.
r_objects <- function(root) {
  files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
  data.table::rbindlist(lapply(files, function(path) {
    assigned <- Filter(function(e) {
      is.call(e) && (identical(e[[1L]], as.name("<-")) || identical(e[[1L]], as.name("="))) &&
        is.name(e[[2L]])
    }, as.list(parse(path, keep.source = FALSE)))
    data.table::data.table(
      object = vapply(assigned, function(e) as.character(e[[2L]]), character(1)),
      file = rep(basename(path), length(assigned))
    )
  }))
}

# A check's hash (D15.6, D15.13, D15.20): the SHA-256 of its code and of every object of
# `own` it reaches through the names its code uses, in its body, its argument defaults and
# the functions defined inside it, each written out by deparse(), so comments and layout
# don't count. `name` is the check, or the check with the builders of the input and the
# context it reads, whose code and helpers are taken in the same way (D16.45, m52; D16.47,
# m61). Objects outside `own`
# (R/report_text.R, R/report_html.R and R/rules_registry.R, and other packages') aren't
# followed. The walk's limits: a call by
# string (D15.13) isn't followed; a helper built inside local() and a function held in a
# non-function object aren't scanned; a non-function object is hashed by its deparse(), so a
# top-level data.table constant would carry a pointer that changes with the session (none
# exists now).
check_hash <- function(name, env, own) {
  # The names in a piece of code: its calls and symbols, the defaults in a pairlist of
  # formals and the code of a nested function; the empty symbol of an argument with no
  # default is skipped.
  names_used <- function(x) {
    if (is.symbol(x)) {
      used <- as.character(x)
      if (nzchar(used)) used else character()
    } else if (is.call(x) || is.pairlist(x)) {
      as.character(unlist(lapply(as.list(x), names_used), use.names = FALSE))
    } else {
      character()
    }
  }
  seen <- character()
  queue <- name
  while (length(queue) > 0L) {
    object <- queue[[1L]]
    queue <- queue[-1L]
    if (object %in% seen) {
      next
    }
    seen <- c(seen, object)
    value <- get(object, envir = env, inherits = FALSE)
    if (is.function(value)) {
      used <- c(names_used(formals(value)), names_used(body(value)))
      queue <- c(queue, setdiff(intersect(used, own), seen))
    }
  }
  text <- unlist(lapply(sort(seen, method = "radix"), function(object) {
    value <- get(object, envir = env, inherits = FALSE)
    c(paste("#", object), if (is.environment(value)) "<environment>" else deparse(value))
  }))
  unname(tools::sha256sum(bytes = charToRaw(enc2utf8(paste(text, collapse = "\n")))))
}

# Passes when `current` (hashes named by check function) matches `recorded` (function_name,
# sha256) exactly; fails naming each function whose hash changed, appeared or went, with
# the lines to record.
expect_hashes_recorded <- function(current, recorded) {
  everything <- union(names(current), recorded$function_name)
  was <- recorded$sha256[match(everything, recorded$function_name)]
  now <- unname(current[everything])
  changed <- everything[is.na(was) | is.na(now) | was != now]
  testthat::expect(
    length(changed) == 0L,
    paste0(
      "Check functions whose hash changed, appeared or went: ", paste(changed, collapse = ", "),
      ". Where a rule's behaviour changed, raise its rule_version in R/rules_registry.R ",
      "(add-rule); then record these rows in tests/testthat/fixtures/check_body_hashes.csv ",
      "(under the header `function_name,sha256`, replacing the rows there):\n",
      paste(paste0(names(current), ",", current), collapse = "\n")
    )
  )
  invisible(current)
}

test_that("each check family's hash is recorded; a change needs rule_version (D15.6, D15.13)", {
  testthat::skip_on_covr()
  root <- testthat::test_path("..", "..")
  testthat::skip_if_not(
    length(list.files(file.path(root, "R"), pattern = "[.]R$")) > 0L,
    "the source tree isn't here"
  )
  objects <- r_objects(root)
  left_out <- c("report_text.R", "report_html.R", "rules_registry.R")
  own <- objects$object[!objects$file %in% left_out]
  # The reader worked: the left-out files are read and none of their objects is followed,
  # while a function of another file is.
  expect_true(all(left_out %in% objects$file))
  expect_false(any(objects$object[objects$file %in% left_out] %in% own))
  expect_identical(objects$file[objects$object == "validate_rule_set"], "rules_set.R")
  expect_true("validate_rule_set" %in% own)
  families <- objects$object[
    grepl("^check_", objects$object) & grepl("^check_.*[.]R$", objects$file)
  ]
  ns <- asNamespace("groundplotqc")
  # Every family reads the input conformance_input() builds and the context
  # conformance_context() builds (the code lists and the settings), so its hash takes in both
  # builders and what they call (match_columns(), key_values(), resolve_setting() and the
  # rest), and a change there fails the test too (D16.45, m52; D16.47, m61). The walk doesn't
  # follow rule_fingerprint(), which builds nothing a family reads.
  expect_identical(objects$file[objects$object == "conformance_input"], "check_input.R")
  expect_identical(objects$file[objects$object == "conformance_context"], "check_conformance.R")
  expect_true("rule_fingerprint" %in% own)
  walked <- setdiff(own, "rule_fingerprint")
  current <- vapply(families, function(family) {
    check_hash(c(family, "conformance_input", "conformance_context"), ns, walked)
  }, character(1))
  names(current) <- families
  recorded <- data.table::fread(
    testthat::test_path("fixtures", "check_body_hashes.csv"),
    colClasses = "character"
  )
  expect_hashes_recorded(current, recorded)
})

test_that("a changed check, helper or input builder fails the hash check until recorded (D15.11)", {
  env <- new.env()
  env$check_demo <- function(x) {
    text_demo(helper_demo(x) + 1L)
  }
  env$helper_demo <- function(x) x * 2L
  env$text_demo <- function(x) paste("n =", x)
  # text_demo stands for a function of R/report_text.R, R/report_html.R or R/rules_registry.R:
  # not followed.
  own <- c("check_demo", "helper_demo")
  hash <- function() c(check_demo = check_hash("check_demo", env, own))
  recorded <- data.table::data.table(function_name = "check_demo", sha256 = hash()[[1L]])
  expect_success(expect_hashes_recorded(hash(), recorded))
  # A comment changes nothing.
  env$check_demo <- function(x) {
    # The same code, with a comment.
    text_demo(helper_demo(x) + 1L)
  }
  expect_success(expect_hashes_recorded(hash(), recorded))
  # The check's own code changes: it fails until the new hash is recorded.
  env$check_demo <- function(x) {
    text_demo(helper_demo(x) + 2L)
  }
  expect_failure(expect_hashes_recorded(hash(), recorded))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  # A helper it calls changes (D15.13): the same.
  env$helper_demo <- function(x) x * 3L
  expect_failure(expect_hashes_recorded(hash(), recorded))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  # A function of the left-out files changes: nothing to record.
  env$text_demo <- function(x) paste("count:", x)
  expect_success(expect_hashes_recorded(hash(), recorded))
  # A helper's own helper (a second hop), a constant reached by name, and a helper called
  # only from an argument default, in the check or in a function defined inside it, are
  # followed too (D15.13, D15.20).
  env$check_demo <- function(x, scale = default_demo()) {
    inner <- function(y, step = nested_demo()) y + step
    text_demo(inner(helper_demo(x)) * limit_demo + scale)
  }
  env$helper_demo <- function(x) deep_demo(x) * 3L
  env$deep_demo <- function(x) x + 1L
  env$limit_demo <- 10L
  env$default_demo <- function() 1L
  env$nested_demo <- function() 1L
  own <- c(own, "deep_demo", "limit_demo", "default_demo", "nested_demo")
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  env$deep_demo <- function(x) x + 2L
  expect_failure(expect_hashes_recorded(hash(), recorded))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  env$limit_demo <- 20L
  expect_failure(expect_hashes_recorded(hash(), recorded))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  env$default_demo <- function() 2L
  expect_failure(expect_hashes_recorded(hash(), recorded))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  env$nested_demo <- function() 2L
  expect_failure(expect_hashes_recorded(hash(), recorded))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  # A builder of the input the check reads, named beside it, and the builder's helper are
  # followed too, though the check never calls them (D16.45, m52).
  env$builder_demo <- function(x) match_demo(x)
  env$match_demo <- function(x) x
  own <- c(own, "builder_demo", "match_demo")
  hash <- function() c(check_demo = check_hash(c("check_demo", "builder_demo"), env, own))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  env$match_demo <- function(x) tolower(x)
  expect_failure(expect_hashes_recorded(hash(), recorded))
  recorded$sha256 <- hash()[[1L]]
  expect_success(expect_hashes_recorded(hash(), recorded))
  # A check function appears, or one recorded is gone.
  expect_failure(expect_hashes_recorded(c(hash(), check_new = "0"), recorded))
  expect_failure(expect_hashes_recorded(character(), recorded))
})
