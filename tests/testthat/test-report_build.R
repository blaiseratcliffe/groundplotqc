# Routing a run to its report units before any HTML exists (plan 13; D16.16, D18.60, D18.89,
# D20.10, D20.11, D20.14, D20.23, D20.24, D20.29, D20.37, D20.48, D20.54), on the hand-made run
# of two units fx_report_results() (D20.58), each unit's file name (D20.8, D20.70), and
# gpq_build_reports(), which builds each unit's report and issues CSV into a saved run's folder
# (D20.13, D20.25, D20.26, D20.29, D20.38, D20.48, D20.114, D20.116, D20.120). Every test runs
# on CRAN (D20.3) but the worst case's size, which is skipped there (D20.38).

test_that("a unit rule's \"*\" row and issue go to every unit (D16.8, D16.16, D20.24)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  for (unit in units) {
    routed <- route_unit(x, unit, units)
    cells <- paste(routed$results$rule_id, routed$results$table_name, routed$results$unit)
    # table_missing and class_mismatch: their issues, and their whole-delivery rows with
    # undeclared_code_set's.
    expect_true(all(1:2 %in% routed$issues$issue_id))
    expect_true(all(c(
      "table_missing gear_log *", "class_mismatch catches *", "undeclared_code_set stations *"
    ) %in% cells))
  }
})

test_that("a record rule's \"*\" row counting records goes to a run's only unit (D20.24)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  star <- function(routed) {
    results <- routed$results
    record_rules <- results$rule_id %chin% c("code_not_in_list", "value_below_min")
    results$rule_id[results$unit == "*" & record_rules]
  }
  for (unit in units) {
    routed <- route_unit(x, unit, units)
    expect_length(star(routed), 0L)
    # Its issue, a record whose key chain broke, goes to no unit either.
    expect_false(20L %in% routed$issues$issue_id)
  }
  # In a run of one unit, every "*" row and issue is the unit's own.
  routed <- route_unit(x, "100", "100")
  expect_setequal(star(routed), c("code_not_in_list", "value_below_min"))
  expect_true(20L %in% routed$issues$issue_id)
})

test_that("a \"*\" not_run row goes to every unit only when it counts no records (D20.24; R2)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  star_rules <- function(routed) routed$results$rule_id[routed$results$unit == "*"]
  for (unit in units) {
    rules <- star_rules(route_unit(x, unit, units))
    # pk_duplicate's not_run row for gear_log, a table not given (no_table), counts no records.
    expect_true("pk_duplicate" %in% rules)
    # value_below_min's none_checked row counts two records it couldn't reach.
    expect_false("value_below_min" %in% rules)
  }
  # A count that is NA reads as none (R2): with its counts NA, that row goes to every unit too.
  y <- x
  y$results <- data.table::copy(x$results)
  at <- which(y$results$rule_id == "value_below_min")
  for (column in c("n_not_evaluated", "n_unchecked")) {
    data.table::set(y$results, i = at, j = column, value = NA_integer_)
  }
  expect_true("value_below_min" %in% star_rules(route_unit(y, "110", units)))
})

test_that("D18.89's (A) and (B) go to no unit of two, and to a run's only unit (D20.10)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  # (A), issue 13: a key_incoherent side read from 110's site, filed under 100. (B), issues 14
  # and 15: a record of 100's filed under 110 with 100's area, and its not-evaluated issue.
  for (unit in units) {
    expect_false(any(c(13L, 14L, 15L) %in% route_unit(x, unit, units)$issues$issue_id))
  }
  expect_true(13L %in% route_unit(x, "100", "100")$issues$issue_id)
  expect_true(all(c(14L, 15L) %in% route_unit(x, "110", "110")$issues$issue_id))
})

test_that("marked issues of a \"*\"-unit parent and a collapsed row reach no unit (D20.10)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  # Issue 16: a record of 110's whose area came from a parent with no unit. Issue 17: a
  # collapsed row of 110's, one of whose records is marked. Issue 6, a collapsed row none of
  # whose records is, goes to its unit.
  for (unit in units) {
    expect_false(any(c(16L, 17L) %in% route_unit(x, unit, units)$issues$issue_id))
  }
  expect_true(6L %in% route_unit(x, "100", units)$issues$issue_id)
  expect_true(all(c(16L, 17L) %in% route_unit(x, "110", "110")$issues$issue_id))
})

test_that("undeclared_code_set's rows go to their own unit, the marked \"*\" row to none", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  undeclared <- function(routed) {
    routed$issues$value[routed$issues$rule_id == "undeclared_code_set"]
  }
  expect_identical(undeclared(route_unit(x, "100", units)), "AB-secret; Pond")
  expect_identical(undeclared(route_unit(x, "110", units)), "BC-secret")
  # In a run of one unit, the "*" row of values from records with no unit is the unit's too.
  expect_setequal(undeclared(route_unit(x, "110", "110")), c("BC-secret", "ZZ-secret"))
})

test_that("a unit's routed tables hold no other unit's or \"*\" record's values (13; D20.37)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  # Every text cell of the three routed tables.
  text <- function(routed) {
    unlist(lapply(routed, function(table) {
      unlist(table[, vapply(table, is.character, logical(1)), with = FALSE], use.names = FALSE)
    }), use.names = FALSE)
  }
  # The values fx_report_results() plants: each unit's own, and those only "*" rows, marked
  # issues and results rows' examples hold.
  own <- list(
    "100" = c("AB-secret", "A-100-1", "P-100-1", "P-100-2", "ST-100-1"),
    "110" = c("BC-secret", "A-110-1", "P-110-1", "ST-110-2")
  )
  nowhere <- c(
    "ZZ-secret", "S-110-secret", "P-110-9", "C-100-5", "P-100-3", "A-ZZ-1", "P-110-2",
    "ST-110-3", "C-ZZ-13"
  )
  for (unit in units) {
    cells <- text(route_unit(x, unit, units))
    for (value in own[[unit]]) {
      expect_true(any(grepl(value, cells, fixed = TRUE)), info = paste(unit, value))
    }
    # Another unit's areas are kept out too: the routed results rows' breakdowns are NA
    # (D20.77), and the issues filed under 110 with 100's area are marked.
    for (value in c(own[[setdiff(units, unit)]], nowhere)) {
      expect_false(any(grepl(value, cells, fixed = TRUE)), info = paste(unit, value))
    }
  }
})

test_that("route_unit keeps only the lineage rows of its unit's routed cells (D20.61, D20.65)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  # A raw value on the cell of two of 100's issues (7 and 10), one of 110's (18), one of D18.89's
  # (A), marked (13), one on a cell no issue holds, and one on the cell two of 100's issues
  # share (8 and 9); built here, so the fixture is unchanged.
  lineage <- data.table::data.table(
    table_name = c("hauls", "catches", "catches", "catches", "stations", "catches"),
    record_pk = c("H1", "C1", "C11", "C5", "S-none", "C3"),
    column_name = c("gear", "weight", "species", "haul_id", "depth", "weight"),
    raw_value = c("RAW-100-a", "RAW-100-b", "RAW-110", "RAW-MARKED", "RAW-NONE", "RAW-100-c"),
    raw_source = "x"
  )
  before <- data.table::copy(lineage)
  raw <- function(unit, units) route_unit(x, unit, units, lineage)$lineage$raw_value
  # Each row once, the shared cell's too.
  expect_identical(sort(raw("100", units)), c("RAW-100-a", "RAW-100-b", "RAW-100-c"))
  expect_identical(raw("110", units), "RAW-110")
  # A run of one unit takes the marked issue, and its cell's raw value with it.
  expect_setequal(raw("100", "100"), c("RAW-100-a", "RAW-100-b", "RAW-100-c", "RAW-MARKED"))
  # No lineage given: none routed. The input is left as it was.
  expect_null(route_unit(x, "100", units)$lineage)
  expect_identical(lineage, before)
})

test_that("route_unit gives lineage rows only to an issue whose key names one record (D20.117)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  raw <- function(y, unit, units, lineage) route_unit(y, unit, units, lineage)$lineage$raw_value
  # A record with no key names no cell: its issue is positional, and a lineage row whose
  # record_pk is blank goes to no unit, though the issue's record_pk is NA too (D20.61).
  y <- x
  y$issues <- data.table::copy(x$issues)
  data.table::set(
    y$issues,
    i = 10L, j = c("record_pk", "pk_positional"), value = list(NA_character_, TRUE)
  )
  blank <- data.table::data.table(
    table_name = "catches", record_pk = NA_character_, column_name = "weight",
    raw_value = "RAW-NO-PK", raw_source = "x"
  )
  expect_length(raw(y, "100", units, blank), 0L)
  # A key repeated on records of 100 and 110 (pk_duplicate's case) names neither: issues 10
  # (100's) and 19 (110's) both on catches C1's weight, positional, in any run.
  y <- x
  y$issues <- data.table::copy(x$issues)
  data.table::set(
    y$issues,
    i = c(10L, 19L), j = c("record_pk", "pk_positional"), value = list("C1", TRUE)
  )
  repeated <- data.table::data.table(
    table_name = "catches", record_pk = "C1", column_name = "weight",
    raw_value = "RAW-REPEATED", raw_source = "x"
  )
  for (unit in units) {
    expect_length(raw(y, unit, units, repeated), 0L)
    expect_length(raw(y, unit, unit, repeated), 0L)
  }
  # A record whose key is the text "*": 100's issue 11 on it, moved to catches' fish_count. The
  # column-level "*" issue on that column (class_mismatch's, issue 2, every unit's) takes no
  # lineage row; the record's own issue still does.
  y <- x
  y$issues <- data.table::copy(x$issues)
  data.table::set(
    y$issues,
    i = 11L, j = c("record_pk", "column_name"), value = list("*", "fish_count")
  )
  star <- data.table::data.table(
    table_name = "catches", record_pk = "*", column_name = "fish_count",
    raw_value = "RAW-STAR", raw_source = "x"
  )
  expect_true(2L %in% route_unit(y, "110", units)$issues$issue_id)
  expect_length(raw(y, "110", units, star), 0L)
  expect_identical(raw(y, "100", units, star), "RAW-STAR")
})

test_that("a key text two units' records share sends its raw value to neither (D20.118)", {
  # A run: 100's record ("a|b", "c") and 110's ("a", "b|c") both write record_pk "a|b|c", and
  # each fails value_below_min, so each unit has an issue on that text's weight.
  dictionary <- data.frame(
    table_name = "k", attribute_name = c("a", "b", "unit", "weight"),
    key_type = c("PK", "PK", ".", "."), data_type = c(rep("character", 3L), "numeric"),
    min_value = c(NA, NA, NA, "0")
  )
  tables <- list(k = data.frame(
    a = c("a|b", "a", "x"), b = c("c", "b|c", "y"), unit = c("100", "110", "100"),
    weight = c(-5, -5, 1)
  ))
  x <- gpq_check_conformance(tables, gpq_read_spec(dictionary), group_cols = "unit", layers = 3)
  below <- x$issues[x$issues$rule_id == "value_below_min"]
  expect_identical(below$record_pk, c("a|b|c", "a|b|c"))
  expect_identical(below$pk_positional, c(TRUE, TRUE))
  lineage <- data.table::data.table(
    table_name = "k", record_pk = "a|b|c", column_name = "weight", raw_value = "RAW-SHARED",
    raw_source = "x"
  )
  units <- c("100", "110")
  for (unit in units) {
    routed <- route_unit(x, unit, units, lineage)
    expect_true("value_below_min" %in% routed$issues$rule_id)
    expect_length(routed$lineage$raw_value, 0L)
  }
})

test_that("corrections follow their issues", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  expect_identical(route_unit(x, "100", units)$corrections$correction_id, 1L)
  # The second is for D18.89's (B), which reaches no unit of two.
  expect_identical(nrow(route_unit(x, "110", units)$corrections), 0L)
  expect_identical(route_unit(x, "110", "110")$corrections$correction_id, 2L)
})

test_that("the remainder leaves out the not-evaluated issues' records, held back or not (D20.54)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  routed <- route_unit(x, "100", units)
  # The column sits after n_unchecked (D20.62).
  expect_identical(
    names(routed$results),
    append(names(x$results), "n_not_evaluated_own", after = match("n_unchecked", names(x$results)))
  )
  results <- routed$results
  # fk_orphan: 3 records not evaluated (missing-value codes a foreign key may hold), no issue.
  expect_identical(results$n_not_evaluated_own[results$rule_id == "fk_orphan"], 3L)
  # value_above_max of 100's: its one not-evaluated record is issue 9's.
  expect_identical(results$n_not_evaluated_own[results$rule_id == "value_above_max"], 0L)
  routed <- route_unit(x, "110", units)
  above <- routed$results[routed$results$rule_id == "value_above_max"]
  # 110's group in A-100-1 counts issue 15's record, held back from 110 (D18.89's (B)), so
  # none is its own; its group in A-110-1 counts two with no issue.
  expect_identical(above$n_not_evaluated, c(1L, 2L))
  expect_identical(above$n_not_evaluated_own, c(0L, 2L))
  expect_false(15L %in% routed$issues$issue_id)
  # A collapsed not-evaluated issue accounts for its n_records records.
  y <- x
  y$issues <- data.table::copy(x$issues)
  y$results <- data.table::copy(x$results)
  data.table::set(
    y$issues,
    i = 15L, j = c("level", "record_pk", "row_number", "n_records"),
    value = list("column", "*", NA_integer_, 4L)
  )
  above <- y$results$rule_id == "value_above_max"
  at <- which(above & y$results$unit == "110" & y$results$area == "A-100-1")
  data.table::set(y$results, i = at, j = "n_not_evaluated", value = 5L)
  routed <- route_unit(y, "110", units)
  above <- routed$results[routed$results$rule_id == "value_above_max"]
  expect_identical(above$n_not_evaluated_own, c(1L, 2L))
  # M5 m131 (D20.16): parent_without_children writes no issue, so its parents not evaluated (a
  # key with no holder) are all its own, section 10's "can't judge" (D20.54).
  z <- x
  z$results <- data.table::copy(x$results)
  parents <- which(z$results$rule_id == "parent_without_children")
  data.table::set(
    z$results,
    i = parents, j = c("n_checked", "n_not_evaluated"), value = list(1L, 2L)
  )
  routed <- route_unit(z, "110", units)
  own <- routed$results$n_not_evaluated_own[routed$results$rule_id == "parent_without_children"]
  expect_identical(own, 2L)
})

test_that("a remainder below 0 stops as an internal error (D20.54)", {
  local_results_registry()
  x <- fx_report_results()
  x$results <- data.table::copy(x$results)
  at <- which(x$results$rule_id == "value_above_max" & x$results$unit == "100")
  data.table::set(x$results, i = at, j = "n_not_evaluated", value = 0L)
  expect_error(
    route_unit(x, "110", c("100", "110")),
    paste(
      "Internal error: the not-evaluated issues of rule value_above_max on table catches,",
      "column weight hold 1 records, more than the 0 its results row counts not evaluated."
    ),
    fixed = TRUE
  )
})

test_that("a run with no group columns is one unit holding every row and remainder (D20.29)", {
  local_results_registry()
  spec <- fx_fish_spec()
  run_id <- "20261026T120000Z_7a0029"
  run <- run_metadata(
    run_id, as.POSIXct("2026-10-26 12:00:00", tz = "UTC"),
    as.POSIXct("2026-10-26 12:00:01", tz = "UTC")
  )
  metadata <- new_metadata(run, spec, gpq_preflight(spec), rules = fx_empty_rule_set())
  rule_id <- c("raw_na", "value_above_max", "fk_orphan", "class_mismatch")
  results <- new_results(list(
    run_id = run_id, run_scope = "package", rule_id = rule_id, rule_fingerprint = "f1",
    stage = "conformance", layer = c(2L, 3L, 4L, 1L), severity = "error",
    table_name = "catches", column_name = c("weight", "weight", "haul_id", "fish_count"),
    n_checked = c(6L, 5L, 4L, 1L), n_fail = c(1L, 0L, 0L, 1L),
    n_not_evaluated = c(0L, 1L, 2L, 0L), n_unchecked = c(0L, 0L, 0L, NA), reason_code = "*",
    n_distinct_fail_values = c(1L, 0L, 0L, 1L), n_easy_fix = 0L,
    status = c("fail", "pass", "pass", "fail"), message_id = rule_id
  ))
  issues <- new_issues(list(
    issue_id = 1:3, run_id = run_id, run_scope = "package",
    rule_id = c("raw_na", "value_above_max", "class_mismatch"), rule_fingerprint = "f1",
    stage = "conformance", layer = c(2L, 3L, 1L), severity = "error", reason_code = "*",
    table_name = "catches", column_name = c("weight", "weight", "fish_count"),
    record_pk = c("C3", "C3", "*"), row_number = c(3L, 3L, NA),
    pk_positional = c(FALSE, FALSE, NA), level = c("record", "record", "column"),
    n_records = c(NA, NA, 9L), reaches_other_unit = FALSE, root_issue_id = c(NA, 1L, NA),
    status = c("open", "not_evaluated", "open")
  ))
  x <- new_gpq_results(results, issues, new_corrections(), metadata)
  routed <- route_unit(x, NULL, character())
  expect_identical(routed$issues$issue_id, 1:3)
  expect_identical(routed$results$rule_id, rule_id)
  # value_above_max's record is issue 2's; fk_orphan's two have no issue.
  expect_identical(routed$results$n_not_evaluated_own, c(0L, 0L, 2L, 0L))
})

test_that("group columns named with a join's operators route as any others (16.2)", {
  local_results_registry()
  x <- fx_report_results()
  units <- c("100", "110")
  # Names a data.table join's `on` would read as operators (key_matches() keeps them clear), and
  # a third, named cells, holding the areas again, which a grouping `by = cells` would read as
  # its one column.
  y <- x
  y$results <- data.table::copy(x$results)
  y$issues <- data.table::copy(x$issues)
  y$metadata$run <- data.table::copy(x$metadata$run)
  data.table::setnames(y$results, c("unit", "area"), c("unit<1", "area==2"))
  data.table::setnames(y$issues, c("unit", "area"), c("unit<1", "area==2"))
  data.table::set(y$results, j = "cells", value = y$results[["area==2"]])
  data.table::set(y$issues, j = "cells", value = y$issues[["area==2"]])
  data.table::set(y$metadata$run, j = "group_cols", value = "unit<1; area==2; cells")
  for (unit in units) {
    routed <- route_unit(x, unit, units)
    renamed <- route_unit(y, unit, units)
    expect_identical(renamed$issues$issue_id, routed$issues$issue_id)
    expect_identical(renamed$results$n_not_evaluated_own, routed$results$n_not_evaluated_own)
  }
})

test_that("route_unit leaves the run and the lineage as they were and shares no column", {
  local_results_registry()
  x <- fx_report_results()
  # A raw value on the cell of 100's issue 7.
  lineage <- data.table::data.table(
    table_name = "hauls", record_pk = "H1", column_name = "gear", raw_value = "RAW-100-a",
    raw_source = "x"
  )
  before <- lapply(x[c("results", "issues", "corrections")], data.table::copy)
  before_lineage <- data.table::copy(lineage)
  routed <- route_unit(x, "100", c("100", "110"), lineage)
  expect_identical(x[c("results", "issues", "corrections")], before)
  expect_identical(lineage, before_lineage)
  expect_identical(routed$lineage$raw_value, "RAW-100-a")
  # A change by reference to a routed table doesn't reach the run or the lineage.
  data.table::set(routed$issues, j = "value", value = "changed")
  data.table::set(routed$results, j = "n_checked", value = 0L)
  data.table::set(routed$corrections, j = "proposed_value", value = "changed")
  data.table::set(routed$lineage, j = "raw_value", value = "changed")
  expect_identical(x[c("results", "issues", "corrections")], before)
  expect_identical(lineage, before_lineage)
})

test_that("unit_file_name() keeps safe ASCII and writes every other byte as %XX (D20.8)", {
  # A stem decoded: each %XX its byte, any other character itself.
  decode <- function(stem) {
    tokens <- regmatches(stem, gregexpr("%[0-9A-F]{2}|.", stem))[[1L]]
    escaped <- startsWith(tokens, "%")
    bytes <- raw(length(tokens))
    bytes[escaped] <- as.raw(strtoi(substring(tokens[escaped], 2L), 16L))
    bytes[!escaped] <- charToRaw(paste(tokens[!escaped], collapse = ""))
    text <- rawToChar(bytes)
    Encoding(text) <- "UTF-8"
    text
  }
  # MAGPlot's IDs, and any other unit of safe ASCII, are kept as they are.
  safe <- c("100", "220.01", "AB-3_x.v2")
  expect_identical(unit_file_name(safe), safe)
  units <- c(
    "%", "a b", "é", r"(/\:*?"<>|)", "\t", "a\nb", "\U0001F600", ";",
    ".a.", ".", "...", " lead", "trail ",
    "CON", "con.txt", "Com1", "LPT9.x.y", "nul", "aux.", "PRN",
    "CONX", "COM0", "COM10", "xCON", "CON-1", ".CON",
    "preflight", "PreFlight", "national_summary", "preflight.x",
    # The package's own file stems, in any case (D20.70, D20.73); a value only holding one isn't.
    "report", "Results", "ISSUES", "corrections", "issues.csv", "my_report"
  )
  stems <- unit_file_name(units)
  expect_identical(stems, c(
    "%25", "a%20b", "%C3%A9", "%2F%5C%3A%2A%3F%22%3C%3E%7C", "%09", "a%0Ab", "%F0%9F%98%80",
    "%3B", "%2Ea%2E", "%2E", "%2E.%2E", "%20lead", "trail%20",
    "%43ON", "%63on.txt", "%43om1", "%4CPT9.x.y", "%6Eul", "%61ux%2E", "%50RN",
    "CONX", "COM0", "COM10", "xCON", "CON-1", "%2ECON",
    "%70reflight", "%50reFlight", "%6Eational_summary", "preflight.x",
    "%72eport", "%52esults", "%49SSUES", "%63orrections", "issues.csv", "my_report"
  ))
  expect_identical(unit_file_name("Issues"), "%49ssues")
  expect_true(all(grepl("^[A-Za-z0-9._%-]+$", stems)))
  expect_identical(vapply(stems, decode, "", USE.NAMES = FALSE), units)
  # Text marked latin1 is encoded from its UTF-8.
  expect_identical(unit_file_name(iconv("été", "UTF-8", "latin1")), "%C3%A9t%C3%A9")
})

test_that("unit_file_name() stops on stems alike ignoring case or over 200 bytes (D20.8)", {
  expect_error(
    unit_file_name(c("x", "A", "b", "a")),
    paste0(
      "Report units A and a give the same file name, A.html, on a system that ignores ",
      "letter case; rename one of them."
    ),
    fixed = TRUE
  )
  # A reserved name's first character, encoded, keeps "CON" and "con" apart.
  expect_identical(unit_file_name(c("CON", "con")), c("%43ON", "%63on"))
  expect_identical(unit_file_name(strrep("x", 200L)), strrep("x", 200L))
  expect_error(
    unit_file_name(c("y", strrep("x", 201L))),
    paste0(
      "Report unit ", strrep("x", 201L), " gives a file name of 201 bytes once encoded, over ",
      "the limit of 200; use a shorter unit value."
    ),
    fixed = TRUE
  )
  # Bytes, not characters: 33 e-acutes are 198 bytes once encoded, 34 are 204.
  expect_identical(nchar(unit_file_name(strrep("é", 33L))), 198L)
  expect_error(unit_file_name(strrep("é", 34L)), "of 204 bytes once encoded", fixed = TRUE)
})

test_that("unit_file_name() folds letter case in ASCII, whatever the locale's rules (D20.8)", {
  # In a Turkish or Azeri UTF-8 locale on Linux, tolower() gives "I" a dotless i (U+0131).
  # The function runs here with a tolower() that does so, so "I" and "i" must still stop, on
  # any platform and in any locale.
  dotless <- function(x) base::tolower(gsub("I", "ı", x, fixed = TRUE))
  turkish <- unit_file_name
  environment(turkish) <- list2env(
    list(tolower = dotless),
    parent = environment(unit_file_name)
  )
  expect_error(
    turkish(c("I", "i")), "Report units I and i give the same file name, I.html,",
    fixed = TRUE
  )
  expect_error(turkish(c("ID1", "id1")), "Report units ID1 and id1 give", fixed = TRUE)
})

test_that("gpq_build_reports writes each unit's report and CSV, holding only that unit's own", {
  local_results_registry()
  results <- fx_report_results()
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  # A raw value on the cell of each unit's source issue and of a marked issue, built here so the
  # fixture is unchanged (D20.17, D20.61).
  lineage <- data.frame(
    table_name = c("hauls", "catches", "catches"), record_pk = c("H1", "C11", "C5"),
    column_name = c("gear", "species", "haul_id"),
    raw_value = c("RAW-100-own", "RAW-110-own", "RAW-MARKED"), raw_source = "x"
  )
  files <- gpq_build_reports(results, output, lineage = lineage)
  units <- split_units(results$metadata$run$report_units)
  stems <- unit_file_name(units)
  expect_equal(unname(files), as.vector(rbind(
    file.path(output, results$metadata$run$run_id, "reports", paste0(stems, ".html")),
    file.path(output, results$metadata$run$run_id, "tables", paste0(stems, "_issues.csv"))
  )))
  expect_equal(names(files), rep(units, each = 2L))
  expect_true(all(file.exists(files)))
  # Each unit's own planted values appear in its files and never in the other's: record IDs,
  # values, provider keys, group values, the problem unit's key, results-row examples (D20.23).
  # The values fx_report_results() lists: each unit's own, and those only "*" rows, marked
  # issues and a results row's examples hold, which reach neither unit of two.
  own <- list(
    "100" = c("AB-secret", "A-100-1", "P-100-1", "P-100-2", "ST-100-1"),
    "110" = c("BC-secret", "A-110-1", "P-110-1", "ST-110-2")
  )
  nowhere <- c(
    "ZZ-secret", "S-110-secret", "P-110-9", "C-100-5", "P-100-3", "A-ZZ-1", "P-110-2",
    "ST-110-3", "C-ZZ-13"
  )
  # The HTML shows the key column in section 3's "Each issue" table only, so P-100-2, a
  # harmonization issue's plot, is in 100's CSV alone.
  shown <- list("100" = setdiff(own[["100"]], "P-100-2"), "110" = own[["110"]])
  for (unit in units) {
    at <- which(names(files) == unit)
    page <- readChar(files[[at[[1L]]]], file.size(files[[at[[1L]]]]), useBytes = TRUE)
    csv <- readChar(files[[at[[2L]]]], file.size(files[[at[[2L]]]]), useBytes = TRUE)
    # The CSV starts with UTF-8's byte-order mark, and the page loads nothing from elsewhere.
    expect_identical(readBin(files[[at[[2L]]]], "raw", 3L), as.raw(c(0xef, 0xbb, 0xbf)))
    expect_false(grepl("https?://|src=|<link", page))
    for (value in shown[[unit]]) {
      expect_true(grepl(value, page, fixed = TRUE), info = paste(unit, value, "html"))
    }
    for (value in own[[unit]]) {
      expect_true(grepl(value, csv, fixed = TRUE), info = paste(unit, value, "csv"))
    }
    for (value in c(own[[setdiff(units, unit)]], nowhere)) {
      expect_false(grepl(value, page, fixed = TRUE), info = paste(unit, value, "html"))
      expect_false(grepl(value, csv, fixed = TRUE), info = paste(unit, value, "csv"))
    }
    # Its own raw value shows in its report; the other unit's and the marked issue's are in
    # neither of its files (D20.17, D20.61).
    raw <- c("100" = "RAW-100-own", "110" = "RAW-110-own")
    expect_true(grepl(raw[[unit]], page, fixed = TRUE), info = unit)
    for (value in c(raw[[setdiff(units, unit)]], "RAW-MARKED")) {
      expect_false(grepl(value, page, fixed = TRUE), info = paste(unit, value, "html"))
      expect_false(grepl(value, csv, fixed = TRUE), info = paste(unit, value, "csv"))
    }
  }
})

test_that("gpq_build_reports writes only into a saved run's folder and rebuilds cleanly", {
  local_results_registry()
  results <- fx_report_results()
  run_id <- results$metadata$run$run_id
  output <- withr::local_tempdir()
  # Not a results object, and not one folder.
  expect_error(
    gpq_build_reports(list(), output), "`results` must be a gpq_results object.",
    fixed = TRUE
  )
  expect_error(
    gpq_build_reports(results, NA), "`output_dir` must be one folder.",
    fixed = TRUE
  )
  # No saved run (W205).
  expect_error(
    gpq_build_reports(results, output),
    sprintf("`output_dir` holds no saved results of run %s", run_id),
    fixed = TRUE
  )
  run_dir <- gpq_write_results(results, output)
  # A file named reports where the reports' folder goes.
  writeLines("in the way", file.path(run_dir, "reports"))
  expect_error(
    gpq_build_reports(results, output),
    paste0(
      "Can't create the folder ", file.path(run_dir, "reports"), " for the run's reports; ",
      "a file of that name may be in the way."
    ),
    fixed = TRUE
  )
  unlink(file.path(run_dir, "reports"))
  # Pre-flight's page and the run's own tables stay through a rebuild; a stale report and CSV
  # of an earlier build, and a temporary file a stopped build left, go.
  dir.create(file.path(run_dir, "reports"), showWarnings = FALSE)
  writeLines("pre-flight", file.path(run_dir, "reports", "preflight.html"))
  stale <- c(
    file.path(run_dir, "reports", "gone.html"),
    file.path(run_dir, "tables", "gone_issues.csv"),
    file.path(run_dir, "tables", ".gpq_build_1a2b")
  )
  for (path in stale) writeLines("old", path)
  files <- gpq_build_reports(results, output)
  expect_false(any(file.exists(stale)))
  expect_equal(readLines(file.path(run_dir, "reports", "preflight.html")), "pre-flight")
  expect_true(all(file.exists(
    file.path(run_dir, "tables", c("results.csv", "issues.csv", "corrections.csv"))
  )))
  # A rebuild writes the same files again, and nothing else.
  again <- gpq_build_reports(results, output)
  expect_equal(again, files)
  expect_setequal(
    list.files(run_dir, recursive = TRUE, all.files = TRUE),
    c(
      "reports/preflight.html", sub(paste0(run_dir, "/"), "", files, fixed = TRUE),
      "tables/results.csv", "tables/issues.csv", "tables/corrections.csv",
      file.path("metadata", paste0(names(metadata_schema()), ".csv"))
    )
  )
  # A build that stops part way leaves no temporary file (D20.13 (b)): the write stops once the
  # CSV's temporary file is on disk, so only the build's exit can remove it.
  testthat::local_mocked_bindings(fwrite = function(x, file, ...) {
    writeLines("part", file)
    stop("stopped")
  })
  expect_error(gpq_build_reports(results, output), "stopped", fixed = TRUE)
  expect_length(
    list.files(run_dir, pattern = "^[.]gpq_build_", recursive = TRUE, all.files = TRUE), 0L
  )
})

test_that("a run stopped at pre-flight has no reports to build (D20.13, W206)", {
  local_results_registry()
  results <- fx_report_results()
  output <- withr::local_tempdir()
  run <- data.table::copy(results$metadata$run)
  data.table::set(run, j = "outcome", value = "stopped_preflight")
  write_run_metadata(list(run = run), file.path(output, run$run_id))
  expect_error(
    gpq_build_reports(results, output),
    sprintf("Run %s stopped at pre-flight, so it has no results to report.", run$run_id),
    fixed = TRUE
  )
})

test_that("gpq_build_reports needs classed issues and the run's report unit first", {
  local_results_registry()
  results <- fx_report_results()
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  unclassed <- data.table::copy(results)
  unclassed$issues <- data.table::copy(results$issues)
  data.table::set(unclassed$issues, i = 1:2, j = "issue_class", value = NA_character_)
  expect_error(
    gpq_build_reports(unclassed, output),
    "Issues with no class: 2. Classify them first with gpq_classify_issues(issues, rules)",
    fixed = TRUE
  )
  # The fixture's group columns are c("unit", "area"): the report unit, then a breakdown.
  cols <- split_joined(results$metadata$run$group_cols)
  expect_error(
    gpq_build_reports(results, output, group_cols = rev(cols)),
    sprintf("`group_cols` must start with the run's report-unit column, %s", cols[[1L]]),
    fixed = TRUE
  )
  expect_error(
    gpq_build_reports(results, output, group_cols = c(cols[[1L]], "nowhere")),
    "and may add any of its other group columns",
    fixed = TRUE
  )
  # The report unit alone is a valid choice: no breakdowns (D20.26).
  expect_silent(gpq_build_reports(results, output, group_cols = cols[[1L]]))
})

test_that("a run's issues build once classified with no evidence (D20.25)", {
  fish <- fx_fish_spec()
  tables <- fx_fish_clean()
  # A missing column, so the run has an issue to class: column_missing.
  tables$hauls <- data.table::copy(tables$hauls)
  tables$hauls$minutes <- NULL
  results <- suppressWarnings(gpq_check_conformance(
    tables, fish,
    group_cols = "water_body", layers = 0
  ))
  # Classified, then written, then built (D20.116).
  gpq_classify_issues(results$issues, NULL)
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  expect_true(all(file.exists(gpq_build_reports(results, output))))
  # An issue with no class is refused.
  unclassed <- data.table::copy(results)
  unclassed$issues <- data.table::copy(results$issues)
  data.table::set(unclassed$issues, i = 1L, j = "issue_class", value = NA_character_)
  expect_error(gpq_build_reports(unclassed, output), "Issues with no class: 1.", fixed = TRUE)
})

test_that("a Layer 0 run of two units builds both reports, each with the unit-rule rows (D20.48)", {
  fish <- fx_fish_spec()
  tables <- fx_fish_clean()
  # A missing column: Layer 0's unit rule column_missing, a "*" row and a column-level issue.
  tables$hauls <- data.table::copy(tables$hauls)
  tables$hauls$minutes <- NULL
  results <- suppressWarnings(gpq_check_conformance(
    tables, fish,
    group_cols = "water_body", layers = 0
  ))
  gpq_classify_issues(results$issues, NULL)
  units <- split_units(results$metadata$run$report_units)
  expect_gt(length(units), 1L)
  expect_true(all(results$results$water_body == "*"))
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  # Its only group column is the report unit, so `group_cols` may add none (W208b).
  expect_error(
    gpq_build_reports(results, output, group_cols = c("water_body", "x")),
    paste(
      "`group_cols` must be water_body, the run's report-unit column, since the run has no",
      "other group columns."
    ),
    fixed = TRUE
  )
  files <- gpq_build_reports(results, output)
  expect_equal(unique(names(files)), units)
  for (csv in files[grepl("_issues[.]csv$", files)]) {
    expect_true("column_missing" %in% data.table::fread(csv)$rule_id)
  }
})

test_that("a run with no group columns builds one report of every issue (D20.29, R3)", {
  fish <- fx_fish_spec()
  tables <- fx_fish_clean()
  tables$hauls <- data.table::copy(tables$hauls)
  tables$hauls$minutes <- NULL
  results <- suppressWarnings(gpq_check_conformance(tables, fish, layers = 0))
  gpq_classify_issues(results$issues, NULL)
  output <- withr::local_tempdir()
  run_dir <- gpq_write_results(results, output)
  expect_error(
    gpq_build_reports(results, output, group_cols = "water_body"),
    sprintf(
      "Run %s has no group columns, so `group_cols` must be NULL or empty.",
      results$metadata$run$run_id
    ),
    fixed = TRUE
  )
  files <- gpq_build_reports(results, output)
  expect_equal(
    unname(files),
    file.path(run_dir, c("reports/report.html", "tables/report_issues.csv"))
  )
  expect_equal(nrow(data.table::fread(files[[2L]])), nrow(results$issues))
  expect_match(
    paste(readLines(files[[1L]], encoding = "UTF-8"), collapse = "\n"),
    paste("Quality report for run", results$metadata$run$run_id),
    fixed = TRUE
  )
})

test_that("reports/ and tables/ hold only units' files and package_file_stems (D20.70, D20.73)", {
  local_results_registry()
  output <- withr::local_tempdir()
  # A pre-flight stop writes its page: a caller's text naming a slot its text lacks.
  stopped <- file.path(output, "stopped")
  slot <- data.frame(text_id = "raw_na", lang = "en", text = "{nowhere}")
  expect_error(
    gpq_preflight(fx_fish_spec(), text = slot, output_dir = stopped),
    class = "gpq_preflight_error"
  )
  # A completed run of two units written out and built.
  grouped <- fx_report_results()
  gpq_write_results(grouped, output)
  gpq_build_reports(grouped, output)
  # A run with no group columns written out and built: one report.
  tables <- fx_fish_clean()
  tables$hauls <- data.table::copy(tables$hauls)
  tables$hauls$minutes <- NULL
  plain <- suppressWarnings(gpq_check_conformance(tables, fx_fish_spec(), layers = 0))
  gpq_classify_issues(plain$issues, NULL)
  gpq_write_results(plain, output)
  gpq_build_reports(plain, output)
  run_dirs <- list.dirs(output, recursive = FALSE)
  expect_length(run_dirs, 3L)
  expect_true(file.exists(file.path(stopped, "reports", "preflight.html")))
  for (run_dir in run_dirs) {
    run_file <- file.path(run_dir, "metadata", "run.csv")
    units <- if (file.exists(run_file)) {
      split_units(read_run_csv(run_file, metadata_schema()$run)$report_units)
    }
    stems <- unit_file_name(as.character(units))
    unit_files <- c(
      file.path("reports", paste0(stems, ".html")),
      file.path("tables", paste0(stems, "_issues.csv"))
    )
    written <- list.files(run_dir, recursive = TRUE, all.files = TRUE)
    written <- written[dirname(written) %in% c("reports", "tables")]
    expect_gt(length(written), 0L)
    # Every other file is the package's own: <stem>.html, <stem>.csv or report_issues.csv.
    own <- basename(setdiff(written, unit_files))
    stem <- sub("^report_issues[.]csv$", "report", own)
    stem <- sub("[.](html|csv)$", "", stem)
    expect_true(all(stem %in% package_file_stems), info = paste(own, collapse = ", "))
  }
})

test_that(
  "a unit run.csv doesn't list is refused; a run listing no unit builds none (D20.48, D20.114, R1)",
  {
    local_results_registry()
    results <- fx_report_results()
    output <- withr::local_tempdir()
    gpq_write_results(results, output)
    stray <- data.table::copy(results)
    stray$issues <- data.table::copy(results$issues)
    unit_col <- split_joined(results$metadata$run$group_cols)[[1L]]
    data.table::set(stray$issues, i = 1L, j = unit_col, value = "nowhere")
    expect_error(
      gpq_build_reports(stray, output),
      paste(
        "The results hold report units that metadata/run.csv doesn't list: nowhere. They have",
        "changed since they were written; build from the written run, as gpq_read_results()",
        "reads it back."
      ),
      fixed = TRUE
    )
    # R1: run.csv lists no unit, as for a run given no table of its spec; nothing is written.
    empty <- suppressWarnings(gpq_check_conformance(
      list(other = data.frame(a = 1)), fx_fish_spec(),
      group_cols = "water_body", layers = 0
    ))
    expect_true(is.na(empty$metadata$run$report_units))
    gpq_classify_issues(empty$issues, NULL)
    run_dir <- gpq_write_results(empty, output)
    built <- gpq_build_reports(empty, output)
    expect_identical(built, structure(character(), names = character()))
    expect_length(list.files(file.path(run_dir, "reports"), pattern = "[.]html$"), 0L)
    expect_length(list.files(file.path(run_dir, "tables"), pattern = "_issues[.]csv$"), 0L)
  }
)

test_that("gpq_build_reports resolves its caps through the tiers, the rule set being the run's", {
  local_results_registry()
  results <- fx_report_results()
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  # 100's report has tables of more than two rows, so a cap of one or two shows in it.
  files <- gpq_build_reports(results, output, row_cap = 1)
  page <- paste(readLines(files[[1L]], encoding = "UTF-8"), collapse = "\n")
  expect_match(page, "Showing 1 of", fixed = TRUE)
  withr::local_options(groundplotqc.row_cap = 2L)
  files <- gpq_build_reports(results, output)
  page <- paste(readLines(files[[1L]], encoding = "UTF-8"), collapse = "\n")
  expect_match(page, "Showing 2 of", fixed = TRUE)
  # The run's own rule-set row wins over the option, and section 1 shows it with its tier.
  ruled <- data.table::copy(results)
  ruled$metadata$settings <- data.table::copy(results$metadata$settings)
  data.table::set(
    ruled$metadata$settings,
    i = which(ruled$metadata$settings$setting == "row_cap"), j = c("value", "tier"),
    value = list("2", "rule set")
  )
  withr::local_options(groundplotqc.row_cap = 1L)
  files <- gpq_build_reports(ruled, output)
  page <- paste(readLines(files[[1L]], encoding = "UTF-8"), collapse = "\n")
  expect_match(page, "Showing 2 of", fixed = TRUE)
  expect_match(page, "<tr><td>row_cap</td><td>2</td><td>rule set</td></tr>", fixed = TRUE)
  expect_error(
    gpq_build_reports(results, output, cell_cap = -1),
    "Setting cell_cap (argument) must be",
    fixed = TRUE
  )
})

test_that("section 1 shows the settings the build used, the run's own unchanged (D20.120)", {
  local_results_registry()
  results <- fx_report_results()
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  before <- data.table::copy(results)
  files <- gpq_build_reports(results, output, row_cap = 50)
  for (page in files[grepl("[.]html$", files)]) {
    html <- paste(readLines(page, encoding = "UTF-8"), collapse = "\n")
    expect_match(html, "<tr><td>row_cap</td><td>50</td><td>argument</td></tr>", fixed = TRUE)
  }
  # The caller's results, the run's settings and run table among them, are unchanged.
  expect_identical(results, before)
})

test_that("a report at every cap, each cell at its longest, stays under 10 MB (D20.38)", {
  testthat::skip_on_cran()
  # One unit; row_cap + 1 open issues of each class (record level, each its own column, so each
  # "By check" table has row_cap + 1 rows), row_cap + 1 column-level issues and row_cap + 1
  # results rows with unchecked records; every delivered-text cell 200 double quotes, which
  # the cut leaves at cell_cap characters and escaping makes six bytes each (D20.39); two
  # breakdown columns, area and zone, each with row_cap + 1 values for the bars; and a raw value
  # from the lineage input on every source issue's cell. Built on fx_report_results()'s
  # metadata, its run listing the one unit and its three group columns.
  base <- fx_report_results()
  n <- setting_definitions()$row_cap$built_in + 1L
  quotes <- strrep("\"", 200L)
  # Delivered text of 200 characters, distinct by its first five.
  text_of <- function(k) paste0(sprintf("%05d", k), strrep("\"", 195L))
  run_id <- base$metadata$run$run_id
  metadata <- base$metadata
  metadata$run <- data.table::copy(base$metadata$run)
  data.table::set(metadata$run, j = c("report_units", "group_cols"), value = list(
    "100", "unit; area; zone"
  ))
  classes <- c("source", "harmonization", "unclassified")
  k <- seq_len(n)
  record <- data.table::data.table(
    class = rep(classes, each = n), column = sprintf("c%04d", rep(k, 3L)), at = rep(k, 3L)
  )
  m <- nrow(record) + n
  level <- c(rep("record", nrow(record)), rep("column", n))
  issues <- new_issues(list(
    issue_id = seq_len(m), run_id = run_id, run_scope = "package", rule_id = "code_not_in_list",
    rule_fingerprint = "f1", stage = "conformance", layer = 3L, severity = "error",
    reason_code = "*", table_name = "catches",
    column_name = c(record$column, sprintf("w%04d", k)),
    record_pk = c(text_of(record$at), rep("*", n)),
    row_number = c(record$at, rep(NA_integer_, n)),
    pk_positional = ifelse(level == "record", FALSE, NA), level = level,
    n_records = c(rep(NA_integer_, nrow(record)), rep(9L, n)), value = quotes,
    expected = quotes, provider_key = c(rep(quotes, nrow(record)), rep(NA, n)),
    provider_key_status = c(rep("raw", nrow(record)), rep(NA, n)), unit = "100",
    area = text_of(c(record$at, k)), zone = text_of(c(record$at, k)),
    plot = text_of(c(record$at, k)), reaches_other_unit = FALSE, status = "open",
    issue_class = c(record$class, rep(classes, length.out = n))
  ), group_cols = c("unit", "area", "zone"), key_col = "plot")
  results <- new_results(list(
    run_id = run_id, run_scope = "package", rule_id = "code_not_in_list",
    rule_fingerprint = "f1", stage = "conformance", layer = 3L, severity = "error",
    table_name = "catches", column_name = sprintf("u%04d", k), unit = "100",
    area = text_of(k), zone = text_of(k), n_checked = 1L, n_fail = 0L, n_not_evaluated = 0L,
    n_unchecked = 7L, reason_code = "*", n_distinct_fail_values = 0L, n_easy_fix = 0L,
    status = "pass", message_id = "code_not_in_list"
  ), group_cols = c("unit", "area", "zone"))
  worst <- new_gpq_results(results, issues, new_corrections(), metadata)
  # A raw value of 200 characters on each source issue's cell, starting "R" so it can be told
  # from the other cells once cut.
  lineage <- data.frame(
    table_name = "catches", record_pk = text_of(k), column_name = sprintf("c%04d", k),
    raw_value = paste0("R", strrep("\"", 199L)), raw_source = "x"
  )
  output <- withr::local_tempdir()
  gpq_write_results(worst, output)
  files <- gpq_build_reports(worst, output, lineage = lineage)
  page <- files[grepl("[.]html$", files)]
  size <- file.size(page)
  expect_lt(size, 10 * 2^20)
  html <- readChar(page, size, useBytes = TRUE)
  # Each long cell shows cell_cap characters then the ellipsis, escaped six bytes each, the raw
  # values among them.
  cell <- enc2utf8(paste0(strrep("&quot;", 100L), "\U2026"))
  expect_true(grepl(cell, html, fixed = TRUE, useBytes = TRUE))
  raw <- enc2utf8(paste0("R", strrep("&quot;", 99L), "\U2026"))
  expect_true(grepl(raw, html, fixed = TRUE, useBytes = TRUE))
  # The charts by area and by zone, row_cap + 1 values each, show row_cap bars each and say the
  # CSV has them all (D20.90); the chart by table has one bar.
  bars <- gregexpr("<div class=\"bar\">", html, fixed = TRUE, useBytes = TRUE)[[1L]]
  expect_identical(length(bars), 2L * (n - 1L) + 1L)
  capped <- paste(
    "Showing the", n - 1L, "values with the most open issues. The issues CSV,",
    "100_issues.csv, has them all."
  )
  expect_identical(
    length(gregexpr(capped, html, fixed = TRUE, useBytes = TRUE)[[1L]]), 2L
  )
})
