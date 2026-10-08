# Tests for the rule set's schema, validator and reader (plan 3.5; D9.20, D14.2, D14.19).

a_rule_set <- function() {
  list(
    meta = data.frame(
      rule_set_name = "toy", version = 1L, date = "2026-10-07", spec_version = "20261005"
    ),
    rules = data.frame(
      rule_id = c("r1", "r2"), table_name = c("plots", "*"), attribute_name = c("*", "*"),
      severity = c("warning", ""), class = c(NA, " "), enabled = c("true", "FALSE")
    ),
    settings = data.frame(setting = "lang", value = "en", type = "character")
  )
}

bad_bytes <- function() {
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  bad
}

test_that("the rule set has 3.5's components in order", {
  expect_named(rule_set_schema(), c(
    "meta", "rules", "crossfield", "strategies", "tolerances", "buffers", "settings"
  ))
  expect_named(rule_set_schema()$rules, c(
    "rule_id", "table_name", "attribute_name", "severity", "class", "enabled"
  ))
})

test_that("a rule set is copied as text, blanks NA, enabled logical", {
  checked <- validate_rule_set(a_rule_set())
  expect_named(checked, c("meta", "rules", "settings"))
  expect_equal(checked$meta$version, "1")
  expect_equal(checked$rules$severity, c("warning", NA))
  expect_equal(checked$rules$class, c(NA_character_, NA_character_))
  expect_identical(checked$rules$enabled, c(TRUE, FALSE))
  expect_true(all(vapply(checked, data.table::is.data.table, TRUE)))
  expect_null(validate_rule_set(NULL))
})

test_that("components come back in 3.5's order, the caller's tables untouched", {
  given <- a_rule_set()[c("settings", "rules")]
  given$rules <- data.table::as.data.table(given$rules)
  before <- data.table::copy(given$rules)
  checked <- validate_rule_set(given)
  expect_named(checked, c("rules", "settings"))
  data.table::set(checked$rules, j = "rule_id", value = "changed")
  expect_identical(given$rules, before)
  # A sub-assignment by reference into every returned component, none sharing a column with
  # the caller's table. The callers' tables here are data.tables: only a data.table could be
  # reached by that sub-assignment.
  given <- lapply(a_rule_set(), data.table::as.data.table)
  before <- lapply(given, data.table::copy)
  checked <- validate_rule_set(given)
  for (name in names(checked)) {
    data.table::set(checked[[name]], i = 1L, j = 1L, value = "changed")
  }
  expect_identical(given, before)
})

test_that("a component no schema checks yet passes through as a copy", {
  given <- c(a_rule_set(), list(crossfield = data.frame(rule_id = "x", anything = 1)))
  checked <- validate_rule_set(given)
  expect_named(checked, c("meta", "rules", "crossfield", "settings"))
  expect_equal(checked$crossfield$anything, 1)
  given <- list(
    rules = data.table::as.data.table(a_rule_set()$rules),
    crossfield = data.table::data.table(rule_id = c("x", "y"), anything = c(1, 2))
  )
  before <- lapply(given, data.table::copy)
  checked <- validate_rule_set(given)
  data.table::set(checked$crossfield, i = 1L, j = "anything", value = 99)
  data.table::set(checked$crossfield, i = 1L, j = "rule_id", value = "changed")
  expect_identical(given, before)
})

test_that("a rule set that isn't one stops, naming what is wrong", {
  expect_error(validate_rule_set(data.frame(rule_id = "x")), "named list")
  expect_error(validate_rule_set(list(data.frame())), "named list")
  expect_error(validate_rule_set(list(rules = 1)), "rules must be a data.frame")
  set <- a_rule_set()
  expect_error(validate_rule_set(c(set, list(extras = data.frame()))), "extras")
  expect_error(validate_rule_set(set[c("meta", "settings")]), "must have a rules component")
  # The repeated component is named (RR-6).
  expect_error(
    validate_rule_set(c(set, set["meta"])), "`rules` names a component more than once: meta.",
    fixed = TRUE
  )
  expect_error(
    validate_rule_set(c(set, set[c("rules", "meta")])), "more than once: meta, rules.",
    fixed = TRUE
  )
  set$rules$note <- "x"
  set$rules$enabled <- NULL
  expect_error(
    validate_rule_set(set),
    paste(
      "exactly the columns rule_id, table_name, attribute_name, severity, class, enabled",
      "(missing enabled; extra note)"
    ),
    fixed = TRUE
  )
})

test_that("a column named twice, or not one value per row, stops", {
  set <- a_rule_set()
  set$rules <- cbind(set$rules, enabled = c("maybe", "no"))
  expect_error(validate_rule_set(set), "(repeated enabled)", fixed = TRUE)
  set <- a_rule_set()
  set$rules$severity <- matrix(c("error", "warning", "flag", "info"), 2)
  # The column stops are worded as the text table's are (RR-6).
  expect_error(
    validate_rule_set(set),
    "Column `severity` of rule-set component `rules` must hold one value per row.",
    fixed = TRUE
  )
  set <- a_rule_set()
  set$rules$rule_id <- list("a", c("b", "c"))
  expect_error(
    validate_rule_set(set),
    "Column `rule_id` of rule-set component `rules` must hold one value per row.",
    fixed = TRUE
  )
})

test_that("a name with an invalid byte is shown with the byte as <xx> (D12.28)", {
  set <- a_rule_set()
  names(set$rules)[[1L]] <- bad_bytes()
  expect_error(validate_rule_set(set), "extra ok<97>", fixed = TRUE)
  set <- c(a_rule_set(), list(data.frame()))
  names(set)[[4L]] <- bad_bytes()
  expect_error(validate_rule_set(set), "doesn't have: ok<97>.", fixed = TRUE)
})

test_that("meta has one row, enabled is TRUE or FALSE, text is valid UTF-8", {
  set <- a_rule_set()
  set$meta <- rbind(set$meta, set$meta)
  expect_error(validate_rule_set(set), "one row")
  set <- a_rule_set()
  set$rules$enabled <- c("yes", NA)
  expect_error(validate_rule_set(set), "in rows 1, 2")
  set <- a_rule_set()
  set$rules$rule_id[[1L]] <- bad_bytes()
  # The stop names the rows, as the text table's does (RR-6).
  expect_error(
    validate_rule_set(set),
    "Column `rule_id` of rule-set component `rules` holds text that isn't valid UTF-8, in row 1.",
    fixed = TRUE
  )
  set$rules$rule_id <- c(bad_bytes(), bad_bytes())
  expect_error(validate_rule_set(set), "valid UTF-8, in rows 1, 2.", fixed = TRUE)
})

test_that("a long list of rows or lines in a stop shows ten, then how many more (RR-6)", {
  first_ten <- paste(1:10, collapse = ", ")
  set <- a_rule_set()
  set$rules <- data.frame(
    rule_id = letters[1:12], table_name = "*", attribute_name = "*", severity = NA,
    class = NA, enabled = "maybe"
  )
  expect_error(
    validate_rule_set(set), paste0("aren't TRUE or FALSE, in rows ", first_ten, " and 2 more."),
    fixed = TRUE
  )
  set$rules <- data.frame(
    rule_id = rep(bad_bytes(), 11L), table_name = "*", attribute_name = "*", severity = NA,
    class = NA, enabled = TRUE
  )
  expect_error(
    validate_rule_set(set), paste0("valid UTF-8, in rows ", first_ten, " and 1 more."),
    fixed = TRUE
  )
  # A component read from a file gives its file's lines, ten of them.
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "rule_id,table_name,attribute_name,severity,class,enabled",
      paste0(letters[1:12], ",*,*,,,maybe")
    ),
    file.path(dir, "rules_rules.csv")
  )
  expect_error(
    read_rule_set(dir),
    paste0(
      "in ", paste0("line ", 2:11, " of rules_rules.csv", collapse = ", "), " and 2 more."
    ),
    fixed = TRUE
  )
})

test_that("enabled is read in any case of the ASCII letters, a look-alike letter stops", {
  set <- a_rule_set()
  set$rules$enabled <- c("tRuE", "FaLsE")
  expect_identical(validate_rule_set(set)$rules$enabled, c(TRUE, FALSE))
  # U+017F, the long s, has the capital S: base toupper() would have read it as FALSE.
  set$rules$enabled <- c("TRUE", "fal\u017fe")
  expect_error(validate_rule_set(set), "in row 2")
})

test_that("the enabled stop names the file's line when the component has its origin (D14.26)", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "rule_id,table_name,attribute_name,severity,class,enabled",
      "r1,*,*,,,TRUE",
      "r2,*,*,,,",
      "r3,\"two",
      "lines\",*,,,maybe"
    ),
    file.path(dir, "rules_rules.csv")
  )
  expect_error(
    read_rule_set(dir),
    paste0(
      "Rule-set component rules has enabled values that aren't TRUE or FALSE, ",
      "in line 3 of rules_rules.csv, line 4 of rules_rules.csv."
    ),
    fixed = TRUE
  )
  writeLines(
    c("rule_id,table_name,attribute_name,severity,class,enabled", "r1,*,*,,,TRUE", "r2,*,*,,,"),
    file.path(dir, "rules_rules.csv")
  )
  expect_error(read_rule_set(dir), "in line 3 of rules_rules.csv.", fixed = TRUE)
  # The same rule set given in memory has no origin: R's row number, as before.
  set <- a_rule_set()
  set$rules$enabled <- c("TRUE", "")
  expect_error(validate_rule_set(set), "in row 2.", fixed = TRUE)
  # An origin that doesn't match the rows is dropped, so the stop gives R's rows.
  attr(set$rules, "source_file") <- "rules_rules.csv"
  attr(set$rules, "source_lines") <- 3L
  expect_error(validate_rule_set(set), "in row 2.", fixed = TRUE)
})

test_that("an empty rules component is a rule set", {
  set <- list(rules = data.frame(
    rule_id = character(), table_name = character(), attribute_name = character(),
    severity = character(), class = character(), enabled = logical()
  ))
  checked <- validate_rule_set(set)
  expect_equal(nrow(checked$rules), 0L)
  expect_identical(checked$rules$enabled, logical())
})

test_that("a component's origin is carried over when its lines match its rows (D14.19)", {
  set <- a_rule_set()
  attr(set$rules, "source_file") <- "rules_rules.csv"
  attr(set$rules, "source_lines") <- c(2L, 4L)
  checked <- validate_rule_set(set)
  expect_equal(attr(checked$rules, "source_file"), "rules_rules.csv")
  expect_identical(attr(checked$rules, "source_lines"), c(2L, 4L))
  expect_null(attr(checked$meta, "source_file"))
  expect_identical(validate_rule_set(checked), checked)
  attr(set$rules, "source_lines") <- 2L
  checked <- validate_rule_set(set)
  expect_null(attr(checked$rules, "source_file"))
  expect_null(attr(checked$rules, "source_lines"))
  # An origin that names no file, or no whole line from 1 up, is dropped.
  hostile <- list(
    list(bad_bytes(), c(2L, 3L)), list("", c(2L, 3L)), list("f.csv", c(2.7, 3)),
    list("f.csv", c(-1L, 3L)), list("f.csv", c("2", "3")), list(c("a", "b"), c(2L, 3L)),
    list("f.csv", c(Inf, 3)), list("f.csv", c(2, 3e9))
  )
  for (origin in hostile) {
    attr(set$rules, "source_file") <- origin[[1L]]
    attr(set$rules, "source_lines") <- origin[[2L]]
    expect_null(attr(validate_rule_set(set)$rules, "source_file"))
  }
})

test_that("read_rule_set reads a folder of rules_<component>.csv, a space in its path", {
  dir <- file.path(withr::local_tempdir(), "rule set")
  dir.create(dir)
  writeLines(
    c("rule_id,table_name,attribute_name,severity,class,enabled", "r1,*,*,,,TRUE"),
    file.path(dir, "rules_rules.csv")
  )
  writeLines(
    c("setting,value,type", "lang,\"say \"\"hi\"\"\",character"),
    file.path(dir, "rules_settings.csv")
  )
  read <- read_rule_set(dir)
  expect_named(read, c("rules", "settings"))
  expect_true(read$rules$enabled)
  expect_equal(read$settings$value, "say \"hi\"")
  expect_equal(attr(read$rules, "source_file"), "rules_rules.csv")
  expect_identical(attr(read$rules, "source_lines"), 2L)
  expect_equal(attr(read$settings, "source_file"), "rules_settings.csv")
})

test_that("each row keeps the file line it starts on, a quoted cell over two lines counted", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "rule_id,table_name,attribute_name,severity,class,enabled",
      "r1,\"two",
      "lines\",*,,,TRUE",
      "r2,*,*,,,FALSE"
    ),
    file.path(dir, "rules_rules.csv")
  )
  expect_identical(attr(read_rule_set(dir)$rules, "source_lines"), c(2L, 4L))
})

test_that("read_rule_set stops on a file that doesn't read cleanly, or no rules file", {
  dir <- withr::local_tempdir()
  expect_error(read_rule_set(dir), "No rule-set file rules_rules.csv in", fixed = TRUE)
  expect_error(read_rule_set(file.path(dir, "no such")), "No rule-set file rules_rules.csv in")
  expect_error(read_rule_set(NA_character_), "No rule-set file rules_rules.csv in NA.")
  expect_error(read_rule_set(c(dir, dir)), "No rule-set file rules_rules.csv in c(", fixed = TRUE)
  writeLines(
    c("rule_id,table_name,attribute_name,severity,class,enabled", "r1,*,*,,,TRUE,extra"),
    file.path(dir, "rules_rules.csv")
  )
  expect_error(read_rule_set(dir), "rules_rules.csv doesn't read cleanly")
  expect_error(
    read_rule_set(file.path(dir, "rules_rules.csv")), "No rule-set file rules_rules.csv in"
  )
})

test_that("read_rule_set stops on a file holding an invalid byte", {
  dir <- withr::local_tempdir()
  writeBin(
    c(
      charToRaw("rule_id,table_name,attribute_name,severity,class,enabled\nr1,*,*,"),
      as.raw(0x97), charToRaw(",,TRUE\n")
    ),
    file.path(dir, "rules_rules.csv")
  )
  expect_error(read_rule_set(dir), "rules_rules.csv doesn't read cleanly")
})
