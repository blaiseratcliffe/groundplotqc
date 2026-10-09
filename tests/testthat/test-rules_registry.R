# Tests for the rule registry (plan 9.7; D7.25, D9.21, D14.5, D14.10, D15.6, D15.11, D15.13).

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

# A check's hash (D15.6, D15.13): the SHA-256 of its code and of every object of `own` it
# reaches through the names its code uses, each written out by deparse(), so comments and
# layout don't count. Objects outside `own` (R/report_text.R and R/report_html.R, and other
# packages') aren't followed.
check_hash <- function(name, env, own) {
  seen <- character()
  queue <- name
  while (length(queue) > 0L) {
    object <- queue[[1L]]
    queue <- queue[-1L]
    if (object %in% seen) {
      next
    }
    seen <- c(seen, object)
    value <- get(object, envir = env)
    if (is.function(value)) {
      used <- all.names(parse(text = deparse(value)))
      queue <- c(queue, setdiff(intersect(used, own), seen))
    }
  }
  text <- unlist(lapply(sort(seen), function(object) {
    value <- get(object, envir = env)
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
      "(add-rule); then record these rows in tests/testthat/fixtures/check_body_hashes.csv:\n",
      paste(paste0(names(current), ",", current), collapse = "\n")
    )
  )
  invisible(current)
}

test_that("each check family's hash is recorded; a change needs rule_version (D15.6, D15.13)", {
  root <- testthat::test_path("..", "..")
  testthat::skip_if_not(dir.exists(file.path(root, "R")), "the source tree isn't here")
  objects <- r_objects(root)
  own <- objects$object[!objects$file %in% c("report_text.R", "report_html.R")]
  families <- objects$object[
    grepl("^check_", objects$object) & grepl("^check_.*[.]R$", objects$file)
  ]
  ns <- asNamespace("groundplotqc")
  current <- vapply(families, check_hash, character(1), env = ns, own = own)
  names(current) <- families
  recorded <- data.table::fread(
    testthat::test_path("fixtures", "check_body_hashes.csv"),
    colClasses = "character"
  )
  expect_hashes_recorded(current, recorded)
})

test_that("a changed check or helper fails the hash check until it is recorded (D15.11)", {
  env <- new.env()
  env$check_demo <- function(x) {
    text_demo(helper_demo(x) + 1L)
  }
  env$helper_demo <- function(x) x * 2L
  env$text_demo <- function(x) paste("n =", x)
  # text_demo stands for a function of R/report_text.R or R/report_html.R: not followed.
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
  # A check function appears, or one recorded is gone.
  expect_failure(expect_hashes_recorded(c(hash(), check_new = "0"), recorded))
  expect_failure(expect_hashes_recorded(character(), recorded))
})
