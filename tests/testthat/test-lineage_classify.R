# Tests for issue classes (plan 3.4, 9.3, 10.3; D19.28, D19.34, D19.51 to D19.54). The issues
# and overrides are helper-classes.R's.

test_that("each issue takes its rule's class, set by reference and returned invisibly (D19.28)", {
  issues <- fx_class_issues()
  # An issue not evaluated is classed alike (D19.28 (6)).
  data.table::set(issues, i = 3L, j = "status", value = "not_evaluated")
  before <- data.table::copy(issues)
  address <- data.table::address(issues)
  # A rules row gives value_above_max the class source on hauls; a blank class defers; a row
  # makes pk_duplicate depends, so nothing settling it, it is unclassified.
  rules <- list(rules = data.frame(
    rule_id = c("value_above_max", "code_not_in_list", "pk_duplicate"),
    table_name = c("hauls", "catches", "*"), attribute_name = c("*", "species", "*"),
    severity = NA, class = c("source", NA, "depends"), enabled = TRUE
  ))
  expect_invisible(gpq_classify_issues(issues, rules))
  returned <- gpq_classify_issues(issues, rules)
  expect_identical(data.table::address(returned), address)
  expect_equal(issues$issue_class, c(
    "unclassified", "unclassified", "source", "unclassified", "unclassified", "unclassified",
    "unclassified", "unclassified"
  ))
  expect_equal(issues$class_basis, rep("default", 8L))
  # Without a rule set, each rule's registered class: pk_duplicate's harmonization.
  gpq_classify_issues(issues, NULL)
  expect_equal(issues$issue_class[[4L]], "harmonization")
  # Only the two columns change.
  others <- setdiff(names(issues), c("issue_class", "class_basis"))
  expect_identical(issues[, others, with = FALSE], before[, others, with = FALSE])
  # An information rule's issue is given the class none (D19.58): the registry gives
  # parent_without_children that class.
  data.table::set(issues, i = 8L, j = "rule_id", value = "parent_without_children")
  gpq_classify_issues(issues, NULL)
  expect_equal(c(issues$issue_class[[8L]], issues$class_basis[[8L]]), c("none", "default"))
})

test_that("overrides class issues at 10.3's tiers, a table before none within one (D19.54)", {
  issues <- fx_class_issues()
  overrides <- fx_class_overrides(
    fingerprint = c(NA, NA, issues$fingerprint[[1L]], NA, NA, NA),
    rule_id = c("code_not_in_list", "code_not_in_list", NA, NA, NA, NA),
    table_name = c(NA, "catches", NA, NA, NA, "hauls"),
    column_name = c(NA, NA, NA, "minutes", NA, NA),
    issue_class = c(
      "harmonization", "source", "harmonization", "source", "harmonization", "source"
    ),
    water_body = c(NA, NA, NA, "LK", "RV", NA)
  )
  gpq_classify_issues(issues, NULL, overrides = overrides)
  # 1: its fingerprint. 2, 6, 7: the rule on catches before the rule alone. 3: column and
  # group. 4: the group before the table alone, a fixed class reclassed. 5: the table alone.
  # 8: the rule alone before the table alone.
  expect_equal(issues$issue_class, c(
    "harmonization", "source", "source", "harmonization", "source", "source", "source",
    "harmonization"
  ))
  expect_equal(issues$class_basis, rep("override", 8L))
})

test_that("two overrides of the winning tier with other classes leave it unclassified (D9.27)", {
  issues <- fx_class_issues()
  # One group column or two count alike; two rows giving one class are no conflict.
  overrides <- fx_class_overrides(
    rule_id = c(NA, NA, "value_above_max", "value_above_max"),
    issue_class = c("source", "harmonization", "source", "source"),
    water_body = c("LK", "LK", NA, NA), station_id = c(NA, "S1", NA, NA)
  )
  gpq_classify_issues(issues, NULL, overrides = overrides)
  expect_equal(issues$issue_class, c(
    "unclassified", "unclassified", "source", "harmonization", "unclassified", "unclassified",
    "unclassified", "unclassified"
  ))
  expect_equal(issues$class_basis, c(
    "override conflict", "default", "override", "default", "override conflict",
    "override conflict", "override conflict", "override conflict"
  ))
})

test_that("an override's fingerprint meets the issue's fingerprint or its pk: form (D9.22)", {
  issues <- fx_class_issues()
  # Issues 2 and 6 decoded: their fingerprints are the provider keys', their pk: forms
  # unchanged.
  data.table::set(
    issues,
    i = 2L, j = "fingerprint", value = "code_not_in_list|catches|species|catch.id=7"
  )
  data.table::set(
    issues,
    i = 6L, j = "fingerprint", value = "code_not_in_list|catches|species|catch.id=9"
  )
  # Issue 2 is met by its pk: form, issue 6 by its provider key's, issue 7 by its one form.
  overrides <- fx_class_overrides(
    fingerprint = c(
      issues$fingerprint_pk[[2L]], issues$fingerprint[[7L]], issues$fingerprint[[6L]]
    ),
    issue_class = c("source", "harmonization", "source")
  )
  gpq_classify_issues(issues, NULL, overrides = overrides)
  expect_equal(issues$issue_class[c(2L, 6L, 7L)], c("source", "source", "harmonization"))
  expect_equal(issues$class_basis[c(2L, 6L, 7L)], rep("override", 3L))
})

test_that("an override of a renamed or split rule applies to its new rule IDs (D19.51)", {
  testthat::local_mocked_bindings(rule_id_map = function() {
    data.table::data.table(
      old_rule_id = c("code_unknown", "out_of_bounds", "out_of_bounds"),
      new_rule_id = c("code_not_in_list", "value_above_max", "date_unparseable"),
      change = c("rename", "split", "split"), package_version = "0.0.0.9015"
    )
  })
  issues <- fx_class_issues()
  overrides <- fx_class_overrides(
    rule_id = c("code_unknown", "out_of_bounds"), issue_class = c("source", "harmonization")
  )
  gpq_classify_issues(issues, NULL, overrides = overrides)
  expect_equal(issues$issue_class, c(
    "source", "source", "harmonization", "harmonization", "harmonization", "source", "source",
    "source"
  ))
  expect_equal(issues$class_basis[-4L], rep("override", 7L))
  # The rename is made on the reader's own copy: a caller's data.table is as it was.
  as_table <- data.table::as.data.table(overrides)
  kept <- data.table::copy(as_table)
  gpq_classify_issues(issues, NULL, overrides = as_table)
  expect_identical(as_table, kept)
})

test_that("a harmonization_artifact issue no override classed is harmonization (D19.28 (3))", {
  issues <- fx_class_issues()
  data.table::set(issues, i = 5L, j = "reason_code", value = "harmonization_artifact")
  gpq_classify_issues(issues, NULL)
  expect_equal(
    c(issues$issue_class[[5L]], issues$class_basis[[5L]]), c("harmonization", "reason code")
  )
  # An override wins over the reason code.
  one <- fx_class_overrides(table_name = "hauls", column_name = "haul_date")
  gpq_classify_issues(issues, NULL, overrides = one)
  expect_equal(c(issues$issue_class[[5L]], issues$class_basis[[5L]]), c("source", "override"))
  # Two that disagree leave it as the overrides left it.
  two <- fx_class_overrides(
    table_name = "hauls", column_name = "haul_date", issue_class = c("source", "harmonization")
  )
  gpq_classify_issues(issues, NULL, overrides = two)
  expect_equal(
    c(issues$issue_class[[5L]], issues$class_basis[[5L]]),
    c("unclassified", "override conflict")
  )
})

test_that("an issue still depends takes its rule's fallback class, whole by tier (D19.53)", {
  issues <- fx_class_issues()
  withr::local_options(groundplotqc.fallback_class = c(
    code_not_in_list = "harmonization", date_unparseable = "source"
  ))
  gpq_classify_issues(issues, NULL)
  expect_equal(issues$issue_class, c(
    "harmonization", "harmonization", "unclassified", "harmonization", "source",
    "harmonization", "harmonization", "harmonization"
  ))
  expect_equal(issues$class_basis, c(
    "fallback", "fallback", "default", "default", "fallback", "fallback", "fallback", "fallback"
  ))
  # An argument naming one rule replaces the option's entries: the others have none.
  gpq_classify_issues(
    issues, NULL,
    settings = list(fallback_class = c(value_above_max = "source"))
  )
  expect_equal(issues$issue_class, c(
    "unclassified", "unclassified", "source", "harmonization", "unclassified", "unclassified",
    "unclassified", "unclassified"
  ))
  # So does a rule set's settings row.
  rules <- list(
    rules = fx_empty_rule_set()$rules,
    settings = data.frame(
      setting = "fallback_class", value = "date_unparseable=harmonization", type = "character"
    )
  )
  gpq_classify_issues(issues, rules)
  expect_equal(issues$issue_class[c(1L, 5L)], c("unclassified", "harmonization"))
  expect_equal(issues$class_basis[c(1L, 5L)], c("default", "fallback"))
  # An override comes before any fallback.
  one <- fx_class_overrides(table_name = "hauls", column_name = "haul_date")
  gpq_classify_issues(issues, rules, overrides = one)
  expect_equal(c(issues$issue_class[[5L]], issues$class_basis[[5L]]), c("source", "override"))
})

test_that("classification's caller errors (D19.28 (7))", {
  issues <- fx_class_issues()
  run_issues <- paste(
    "`issues` must be the issues table of a run's results, a data.table, as",
    "gpq_check_conformance() returns it."
  )
  expect_error(gpq_classify_issues(as.data.frame(issues), NULL), run_issues, fixed = TRUE)
  expect_error(
    gpq_classify_issues(issues[, !"class_basis"], NULL), run_issues,
    fixed = TRUE
  )
  unregistered <- data.table::copy(issues)
  data.table::set(unregistered, i = 1L, j = "rule_id", value = "no_such_rule")
  expect_error(gpq_classify_issues(unregistered, NULL), run_issues, fixed = TRUE)
  expect_error(
    gpq_classify_issues(issues, NULL, evidence = list(previous = data.frame())),
    paste(
      "`evidence` names tables it doesn't take: previous.",
      "It takes computed, filled, lineage and crosswalk."
    ),
    fixed = TRUE
  )
  expect_error(
    gpq_classify_issues(issues, NULL, evidence = data.frame()),
    "`evidence` must be a list of data.frames or CSV paths, each named once.",
    fixed = TRUE
  )
  expect_error(
    gpq_classify_issues(
      issues, NULL,
      settings = list(fallback_class = c(code_not_in_list = "depends"))
    ),
    "Setting fallback_class (argument)",
    fixed = TRUE
  )
  # The overrides: their columns, a group column of the issues taken.
  colour <- fx_class_overrides(rule_id = "pk_duplicate", colour = "red")
  expect_error(
    gpq_classify_issues(issues, NULL, overrides = colour),
    paste(
      "`overrides` must have the columns fingerprint, rule_id, table_name, column_name,",
      "issue_class, who, when, why, and may have the issues' group columns (water_body,",
      "station_id); not taken: colour."
    ),
    fixed = TRUE
  )
  # Issues with no group columns say so.
  ungrouped <- issues[, !c("water_body", "station_id")]
  expect_error(
    gpq_classify_issues(ungrouped, NULL, overrides = colour),
    paste(
      "`overrides` must have the columns fingerprint, rule_id, table_name, column_name,",
      "issue_class, who, when, why, and may have the issues' group columns (the issues have",
      "none); not taken: colour."
    ),
    fixed = TRUE
  )
  no_why <- fx_class_overrides(rule_id = "pk_duplicate")
  no_why$why <- NULL
  expect_error(gpq_classify_issues(issues, NULL, overrides = no_why), "missing: why.", fixed = TRUE)
  # Both problems are joined with a semicolon.
  colour$why <- NULL
  expect_error(
    gpq_classify_issues(issues, NULL, overrides = colour), "missing: why; not taken: colour.",
    fixed = TRUE
  )
  expect_error(
    gpq_classify_issues(
      issues, NULL,
      overrides = fx_class_overrides(rule_id = "pk_duplicate", issue_class = c("source", "Source"))
    ),
    paste(
      "The override at row 2 of `overrides` gives the class \"Source\"; an override's class is",
      "source or harmonization."
    ),
    fixed = TRUE
  )
  expect_error(
    gpq_classify_issues(issues, NULL, overrides = fx_class_overrides(water_body = NA)),
    paste(
      "The override at row 1 of `overrides` gives no scope: no fingerprint, rule_id,",
      "table_name, column_name or group column."
    ),
    fixed = TRUE
  )
  # Nothing was classed before the stop.
  expect_true(all(is.na(issues$issue_class)))
})

test_that("overrides read from a CSV file are placed by line; one that doesn't read stops", {
  issues <- fx_class_issues()
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(fx_class_overrides(rule_id = "pk_duplicate", issue_class = "Source"), path)
  expect_error(
    gpq_classify_issues(issues, NULL, overrides = path),
    paste0("The override at line 2 of ", basename(path), " gives the class \"Source\""),
    fixed = TRUE
  )
  # A row with no scope is placed by its line too: the second row is the file's line 3.
  data.table::fwrite(fx_class_overrides(rule_id = c("pk_duplicate", NA)), path)
  expect_error(
    gpq_classify_issues(issues, NULL, overrides = path),
    paste0(
      "The override at line 3 of ", basename(path), " gives no scope: no fingerprint, rule_id,",
      " table_name, column_name or group column."
    ),
    fixed = TRUE
  )
  writeLines(
    c(
      "fingerprint,rule_id,table_name,column_name,issue_class,who,when,why",
      ",pk_duplicate,,,source,me,today,checked",
      ",pk_duplicate,,,source,me,today,checked,again"
    ),
    path
  )
  expect_error(
    gpq_classify_issues(issues, NULL, overrides = path), "`overrides` doesn't read cleanly:",
    fixed = TRUE
  )
  # A clean file classes as a data.frame does.
  data.table::fwrite(fx_class_overrides(rule_id = "pk_duplicate"), path)
  gpq_classify_issues(issues, NULL, overrides = path)
  expect_equal(c(issues$issue_class[[4L]], issues$class_basis[[4L]]), c("source", "override"))
})

test_that("an empty issues table or an empty overrides table classes without a stop", {
  issues <- fx_class_issues()
  gpq_classify_issues(issues, NULL)
  classes <- issues$issue_class
  basis <- issues$class_basis
  # Overrides with no rows change nothing.
  none <- fx_class_overrides(rule_id = "pk_duplicate")[0L, ]
  gpq_classify_issues(issues, NULL, overrides = none)
  expect_equal(issues$issue_class, classes)
  expect_equal(issues$class_basis, basis)
  # A run with no issues, as a clean run's: with no overrides, some, and none.
  empty <- fx_class_issues()[0L]
  gpq_classify_issues(empty, NULL)
  gpq_classify_issues(empty, NULL, overrides = fx_class_overrides(rule_id = "pk_duplicate"))
  gpq_classify_issues(empty, NULL, overrides = none)
  expect_identical(nrow(empty), 0L)
  expect_identical(empty$issue_class, character())
  expect_identical(empty$class_basis, character())
})

test_that("an overrides group column stored as integer64 is read as its number (D18.86)", {
  # Found installed without loading it, which skip_if_not_installed() would do: where no
  # earlier test has loaded bit64, classification is what loads it.
  testthat::skip_if(!nzchar(system.file(package = "bit64")), "bit64 isn't installed.")
  # A table kept with saveRDS() and read back where bit64's namespace isn't loaded holds the
  # column as the number's 64 bits in a double. This builds 5000000000 so, loading nothing.
  bits <- writeBin(c(705032704L, 1L), raw(), endian = "little")
  stored <- structure(readBin(bits, "double", endian = "little"), class = "integer64")
  overrides <- fx_class_overrides()
  overrides$water_body <- stored
  issues <- fx_class_issues()
  data.table::set(issues, i = 1L, j = "water_body", value = "5000000000")
  gpq_classify_issues(issues, NULL, overrides = overrides)
  expect_equal(c(issues$issue_class[[1L]], issues$class_basis[[1L]]), c("source", "override"))
  expect_equal(issues$class_basis[-1L], rep("default", 7L))
  # The stored bits are bit64's own for that number.
  expect_identical(unclass(stored), unclass(bit64::as.integer64("5000000000")))
})

test_that("an integer64 overrides or evidence column stops the call without bit64 (D19.80)", {
  # The test mocks base requireNamespace(), which no CRAN check may change (R145).
  testthat::skip_on_cran()
  # The column is built from the number's bits, so the test needs no bit64.
  bits <- writeBin(c(705032704L, 1L), raw(), endian = "little")
  stored <- structure(readBin(bits, "double", endian = "little"), class = "integer64")
  overrides <- fx_class_overrides()
  overrides$water_body <- stored
  lineage <- data.frame(
    table_name = "catches", record_pk = "C3", column_name = "species", raw_value = NA,
    raw_source = "catch.sp"
  )
  lineage$raw_value <- stored
  # base::requireNamespace() stands in for a library without bit64, for this test only.
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  issues <- fx_class_issues()
  stops <- function(table, column) {
    paste(
      sprintf(
        "Table %s: the column %s is stored as integer64, which needs the bit64 package to be",
        table, column
      ),
      "read correctly. Install it with install.packages(\"bit64\") and run again."
    )
  }
  expect_error(
    gpq_classify_issues(issues, NULL, overrides = overrides), stops("overrides", "water_body"),
    fixed = TRUE
  )
  expect_error(
    gpq_classify_issues(issues, NULL, evidence = list(lineage = lineage)),
    stops("lineage", "raw_value"),
    fixed = TRUE
  )
  # With both, the overrides are named first. Nothing was classed before either stop.
  expect_error(
    gpq_classify_issues(issues, NULL, evidence = list(lineage = lineage), overrides = overrides),
    stops("overrides", "water_body"),
    fixed = TRUE
  )
  expect_true(all(is.na(issues$issue_class)))
  # An evidence argument that is malformed, or names a table the call doesn't take, meets its
  # own stop first.
  expect_error(
    gpq_classify_issues(issues, NULL, evidence = lineage, overrides = overrides),
    "`evidence` must be a list of data.frames or CSV paths, each named once.",
    fixed = TRUE
  )
  expect_error(
    gpq_classify_issues(issues, NULL, evidence = list(lineage), overrides = overrides),
    "`evidence` must be a list of data.frames or CSV paths, each named once.",
    fixed = TRUE
  )
  expect_error(
    gpq_classify_issues(issues, NULL, evidence = list(previous = lineage), overrides = overrides),
    "`evidence` names tables it doesn't take: previous.",
    fixed = TRUE
  )
  # A name given twice is not looked through either (R71).
  expect_error(
    gpq_classify_issues(
      issues, NULL,
      evidence = list(lineage = lineage, lineage = lineage), overrides = overrides
    ),
    "`evidence` must be a list of data.frames or CSV paths, each named once.",
    fixed = TRUE
  )
  # With no integer64 column the call doesn't stop, though the library is mocked as absent.
  expect_no_error(
    gpq_classify_issues(issues, NULL, overrides = fx_class_overrides(rule_id = "pk_duplicate"))
  )
})
