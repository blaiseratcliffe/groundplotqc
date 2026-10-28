# Routing a run to its report units before any HTML exists (plan 13; D16.16, D18.60, D18.89,
# D20.10, D20.11, D20.14, D20.23, D20.24, D20.29, D20.37, D20.48, D20.54), on the hand-made run
# of two units fx_report_results() (D20.58), and each unit's file name (D20.8, D20.70). Every
# test runs on CRAN (D20.3).

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
  # (A), marked (13), and one on a cell no issue holds; built here, so the fixture is unchanged.
  lineage <- data.table::data.table(
    table_name = c("hauls", "catches", "catches", "catches", "stations"),
    record_pk = c("H1", "C1", "C11", "C5", "S-none"),
    column_name = c("gear", "weight", "species", "haul_id", "depth"),
    raw_value = c("RAW-100-a", "RAW-100-b", "RAW-110", "RAW-MARKED", "RAW-NONE"),
    raw_source = "x"
  )
  before <- data.table::copy(lineage)
  raw <- function(unit, units) route_unit(x, unit, units, lineage)$lineage$raw_value
  expect_setequal(raw("100", units), c("RAW-100-a", "RAW-100-b"))
  expect_identical(raw("110", units), "RAW-110")
  # A run of one unit takes the marked issue, and its cell's raw value with it.
  expect_setequal(raw("100", "100"), c("RAW-100-a", "RAW-100-b", "RAW-MARKED"))
  # No lineage given: none routed. The input is left as it was.
  expect_null(route_unit(x, "100", units)$lineage)
  expect_identical(lineage, before)
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
  # Names a data.table join's `on` would read as operators (key_matches() keeps them clear).
  y <- x
  y$results <- data.table::copy(x$results)
  y$issues <- data.table::copy(x$issues)
  y$metadata$run <- data.table::copy(x$metadata$run)
  data.table::setnames(y$results, c("unit", "area"), c("unit<1", "area==2"))
  data.table::setnames(y$issues, c("unit", "area"), c("unit<1", "area==2"))
  data.table::set(y$metadata$run, j = "group_cols", value = "unit<1; area==2")
  for (unit in units) {
    routed <- route_unit(x, unit, units)
    renamed <- route_unit(y, unit, units)
    expect_identical(renamed$issues$issue_id, routed$issues$issue_id)
    expect_identical(renamed$results$n_not_evaluated_own, routed$results$n_not_evaluated_own)
  }
})

test_that("route_unit leaves the run as it was and shares no column with it", {
  local_results_registry()
  x <- fx_report_results()
  before <- lapply(x[c("results", "issues", "corrections")], data.table::copy)
  routed <- route_unit(x, "100", c("100", "110"))
  expect_identical(x[c("results", "issues", "corrections")], before)
  # A change by reference to a routed table doesn't reach the run.
  data.table::set(routed$issues, j = "value", value = "changed")
  data.table::set(routed$results, j = "n_checked", value = 0L)
  data.table::set(routed$corrections, j = "proposed_value", value = "changed")
  expect_identical(x[c("results", "issues", "corrections")], before)
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
