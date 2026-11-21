# Routing a run to its report units before any HTML exists (plan 13; D16.16, D18.60, D18.89,
# D20.10, D20.11, D20.14, D20.23, D20.24, D20.29, D20.37, D20.48, D20.54), on the hand-made run
# of two units fx_report_results() (D20.58), each unit's file name (D20.8, D20.70), and
# gpq_build_reports(), which builds each unit's report and issues CSV into a saved run's folder
# (D20.13, D20.25, D20.26, D20.29, D20.38, D20.48, D20.114, D20.116, D20.120, D20.125,
# D20.126, D22.9, D22.26). Every test runs on CRAN (D20.3) but the worst case's size, which
# is skipped there (D20.38).

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

test_that("a unit's record counts go to it alone; its report ranks its own units (13; D23.38)", {
  # Three more builds of two units and their writes, about 1 s, so skipped on CRAN (D20.3,
  # D23.157): the test below, with its routing and two builds, covers the same on CRAN.
  testthat::skip_on_cran()
  local_results_registry()
  x <- fx_report_results()
  # Counts for 100's plot P-100-1 in two tables, 110's P-110-1, and a plot whose unit is "*",
  # whose key is in no unit's files of the two-unit run.
  x$unit_records <- complete_table(
    list(
      run_id = x$metadata$run$run_id, unit = c("100", "100", "110", "*"),
      area = c("A-100-1", "A-100-1", "A-110-1", "*"),
      plot = c("P-100-1", "P-100-1", "P-110-1", "P-ZZ-77"),
      table_name = c("hauls", "catches", "catches", "catches"), n_records = c(5L, 20L, 30L, 40L)
    ),
    results_schema(c("unit", "area"), "plot")$unit_records, "unit_records"
  )
  units <- c("100", "110")
  expect_identical(route_unit(x, "100", units)$unit_records, x$unit_records[1:2])
  expect_identical(route_unit(x, "110", units)$unit_records, x$unit_records[3L])
  # In a run of one unit, "*" too, as its issues are taken.
  expect_identical(route_unit(x, "110", "110")$unit_records, x$unit_records[3:4])
  output <- withr::local_tempdir()
  gpq_write_results(x, output)
  files <- gpq_build_reports(x, output)
  ranked <- function(unit) {
    file <- files[names(files) == unit][[1L]]
    page <- readChar(file, file.size(file), useBytes = TRUE)
    expect_false(grepl("P-ZZ-77", page, fixed = TRUE), info = unit)
    regmatches(page, regexpr("<section id=\"problem-plots\">.*?</section>", page, perl = TRUE))
  }
  # P-100-1's 25 records, three of them holding an open error (a haul's and two catches'; its
  # warning and its not-evaluated issue don't count); P-110-1's 30, with an open error and an
  # open flag.
  row <- function(area, plot, n, blocked, rate) {
    sprintf(
      paste0(
        "<tr><td>%s</td><td>%s</td><td class=\"num\">%d</td><td class=\"num\">%d</td>",
        "<td class=\"num\">%s</td><td>no</td></tr>"
      ),
      area, plot, n, blocked, rate
    )
  }
  expect_match(ranked("100"), row("A-100-1", "P-100-1", 25L, 3L, "0.12"), fixed = TRUE)
  expect_match(ranked("110"), row("A-110-1", "P-110-1", 30L, 2L, "0.07"), fixed = TRUE)
  expect_false(grepl("P-110-1", ranked("100"), fixed = TRUE))
  # The build reads the three settings as the run recorded them (R60, D20.52): a recorded rate
  # of 0.1 marks P-100-1's 0.12 above it.
  three <- c("blocking_severities", "problem_plot_rate", "problem_plot_min_records")
  recorded <- data.table::copy(x$metadata$settings)
  data.table::set(
    recorded,
    i = which(recorded$setting == "problem_plot_rate"), j = "value", value = "0.1"
  )
  # The build reads the run's units from the saved run.csv alone, so the folder written above
  # serves each build, which replaces the last one's reports.
  x$metadata$settings <- recorded
  files <- gpq_build_reports(x, output)
  expect_match(ranked("100"), "Above 0.1</th>", fixed = TRUE)
  expect_match(ranked("100"), "<td class=\"num\">0.12</td><td>yes</td>", fixed = TRUE)
  # So are the recorded severities and size: with errors alone blocking, P-110-1 holds one
  # blocked record, and with 26 records at least, P-100-1's 25 aren't ranked.
  data.table::set(
    recorded,
    i = which(recorded$setting == "blocking_severities"), j = "value", value = "error"
  )
  data.table::set(
    recorded,
    i = which(recorded$setting == "problem_plot_min_records"), j = "value", value = "26"
  )
  x$metadata$settings <- recorded
  files <- gpq_build_reports(x, output)
  expect_false(grepl("P-100-1", ranked("100"), fixed = TRUE))
  expect_match(ranked("110"), row("A-110-1", "P-110-1", 30L, 1L, "0.03"), fixed = TRUE)
  # A run that recorded none of them, as one an earlier version made, takes the option, or the
  # built-in value where no option is set: blocking errors and flags, 20 records at least.
  x$metadata$settings <- recorded[!recorded$setting %chin% three]
  withr::local_options(
    groundplotqc.problem_plot_rate = 0.5, groundplotqc.blocking_severities = NULL,
    groundplotqc.problem_plot_min_records = NULL
  )
  files <- gpq_build_reports(x, output)
  expect_match(ranked("100"), "Above 0.5</th>", fixed = TRUE)
  expect_match(ranked("100"), row("A-100-1", "P-100-1", 25L, 3L, "0.12"), fixed = TRUE)
  expect_match(ranked("110"), row("A-110-1", "P-110-1", 30L, 2L, "0.07"), fixed = TRUE)
})

test_that("a unit's counts go to it alone, and its report reads the run's settings (13)", {
  local_results_registry()
  x <- fx_report_results()
  # Counts for 100's plot P-100-1 in two tables, 110's P-110-1, and a plot whose unit is "*".
  x$unit_records <- complete_table(
    list(
      run_id = x$metadata$run$run_id, unit = c("100", "100", "110", "*"),
      area = c("A-100-1", "A-100-1", "A-110-1", "*"),
      plot = c("P-100-1", "P-100-1", "P-110-1", "P-ZZ-77"),
      table_name = c("hauls", "catches", "catches", "catches"), n_records = c(5L, 20L, 30L, 40L)
    ),
    results_schema(c("unit", "area"), "plot")$unit_records, "unit_records"
  )
  units <- c("100", "110")
  expect_identical(route_unit(x, "100", units)$unit_records, x$unit_records[1:2])
  expect_identical(route_unit(x, "110", units)$unit_records, x$unit_records[3L])
  # In a run of one unit, "*" too, as its issues are taken.
  expect_identical(route_unit(x, "110", "110")$unit_records, x$unit_records[3:4])
  output <- withr::local_tempdir()
  gpq_write_results(x, output)
  ranked <- function(files, unit) {
    file <- files[names(files) == unit][[1L]]
    page <- readChar(file, file.size(file), useBytes = TRUE)
    expect_false(grepl("P-ZZ-77", page, fixed = TRUE), info = unit)
    regmatches(page, regexpr("<section id=\"problem-plots\">.*?</section>", page, perl = TRUE))
  }
  row <- function(area, plot, n, blocked, rate, above) {
    sprintf(
      paste0(
        "<tr><td>%s</td><td>%s</td><td class=\"num\">%d</td><td class=\"num\">%d</td>",
        "<td class=\"num\">%s</td><td>%s</td></tr>"
      ),
      area, plot, n, blocked, rate, above
    )
  }
  # The run's recorded settings (R60, D20.52): a rate of 0.02 marks P-110-1's 0.03 (30 records,
  # one with an open error, as errors alone block); at least 26 records leave P-100-1's 25 out.
  recorded <- data.table::copy(x$metadata$settings)
  set_recorded <- function(name, value) {
    data.table::set(recorded, i = which(recorded$setting == name), j = "value", value = value)
  }
  set_recorded("problem_plot_rate", "0.02")
  set_recorded("blocking_severities", "error")
  set_recorded("problem_plot_min_records", "26")
  x$metadata$settings <- recorded
  files <- gpq_build_reports(x, output)
  page_100 <- ranked(files, "100")
  expect_false(grepl("P-100-1", page_100, fixed = TRUE))
  page_110 <- ranked(files, "110")
  expect_match(page_110, "Above 0.02</th>", fixed = TRUE)
  expect_match(page_110, row("A-110-1", "P-110-1", 30L, 1L, "0.03", "yes"), fixed = TRUE)
  expect_false(grepl("P-100-1", page_110, fixed = TRUE))
  # A run that recorded none of them, as one an earlier version made, takes the option, or the
  # built-in value where no option is set: blocking errors and flags, 20 records at least.
  three <- c("blocking_severities", "problem_plot_rate", "problem_plot_min_records")
  x$metadata$settings <- recorded[!recorded$setting %chin% three]
  withr::local_options(
    groundplotqc.problem_plot_rate = 0.5, groundplotqc.blocking_severities = NULL,
    groundplotqc.problem_plot_min_records = NULL
  )
  files <- gpq_build_reports(x, output)
  page_100 <- ranked(files, "100")
  expect_match(page_100, "Above 0.5</th>", fixed = TRUE)
  expect_match(page_100, row("A-100-1", "P-100-1", 25L, 3L, "0.12", "no"), fixed = TRUE)
  expect_false(grepl("P-110-1", page_100, fixed = TRUE))
  page_110 <- ranked(files, "110")
  expect_match(page_110, row("A-110-1", "P-110-1", 30L, 2L, "0.07", "no"), fixed = TRUE)
  expect_false(grepl("P-100-1", page_110, fixed = TRUE))
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
  # A run: 100's record ("x", 0.1 + 0.2) and 110's ("x", 0.3) both write record_pk "x|0.3", a
  # number being written to 15 significant digits, and each fails value_below_min, so each
  # unit has an issue on that text's weight. (Parts holding "|" no longer share a text: each
  # is escaped, D23.12.)
  dictionary <- data.frame(
    table_name = "k", attribute_name = c("a", "b", "unit", "weight"),
    key_type = c("PK", "PK", ".", "."),
    data_type = c("character", "numeric", "character", "numeric"),
    min_value = c(NA, NA, NA, "0")
  )
  tables <- list(k = data.frame(
    a = "x", b = c(0.1 + 0.2, 0.3, 1), unit = c("100", "110", "100"), weight = c(-5, -5, 1)
  ))
  x <- gpq_check_conformance(tables, gpq_read_spec(dictionary), group_cols = "unit", layers = 3)
  below <- x$issues[x$issues$rule_id == "value_below_min"]
  expect_identical(below$record_pk, c("x|0.3", "x|0.3"))
  expect_identical(below$pk_positional, c(TRUE, TRUE))
  lineage <- data.table::data.table(
    table_name = "k", record_pk = "x|0.3", column_name = "weight", raw_value = "RAW-SHARED",
    raw_source = "x"
  )
  units <- c("100", "110")
  for (unit in units) {
    routed <- route_unit(x, unit, units, lineage)
    expect_true("value_below_min" %in% routed$issues$rule_id)
    expect_length(routed$lineage$raw_value, 0L)
  }
  # The parts holding "|" are escaped, so 100's key ("a|b", "c") and 110's ("a", "b|c") write
  # different texts, each naming one record, and a lineage row on 100's escaped key reaches
  # 100's issue only (D23.12).
  dictionary$data_type[[2L]] <- "character"
  tables <- list(k = data.frame(
    a = c("a|b", "a"), b = c("c", "b|c"), unit = c("100", "110"), weight = c(-5, -5)
  ))
  x <- gpq_check_conformance(tables, gpq_read_spec(dictionary), group_cols = "unit", layers = 3)
  below <- x$issues[x$issues$rule_id == "value_below_min"]
  expect_identical(below$record_pk, c("a\\|b|c", "a|b\\|c"))
  expect_identical(below$pk_positional, c(FALSE, FALSE))
  lineage <- data.table::data.table(
    table_name = "k", record_pk = "a\\|b|c", column_name = "weight", raw_value = "RAW-ESCAPED",
    raw_source = "x"
  )
  expect_identical(route_unit(x, "100", units, lineage)$lineage$raw_value, "RAW-ESCAPED")
  expect_length(route_unit(x, "110", units, lineage)$lineage$raw_value, 0L)
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
      "column weight hold more records (1) than the 0 its results row counts not evaluated."
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
  x <- new_gpq_results(
    results, issues, new_corrections(), empty_table(results_schema()$unit_records), metadata
  )
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
    # Every group column after the first is blanked in the results rows, both of the run's
    # three (D20.77).
    expect_true(all(is.na(renamed$results[["area==2"]])), info = unit)
    expect_true(all(is.na(renamed$results[["cells"]])), info = unit)
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
    # COM0 and LPT0 too (D20.133); Microsoft's superscript forms by the byte rule (D20.132).
    "COM0", "LPT0", "COM¹", "LPT¹",
    "CONX", "COM10", "xCON", "CON-1", ".CON",
    "preflight", "PreFlight", "national_summary", "preflight.x",
    # The package's own file stems, in any case (D20.70, D20.73, D23.25); a value only holding
    # one isn't.
    "report", "Results", "ISSUES", "corrections", "issues.csv", "my_report",
    "unit_records", "Unit_Records"
  )
  stems <- unit_file_name(units)
  expect_identical(stems, c(
    "%25", "a%20b", "%C3%A9", "%2F%5C%3A%2A%3F%22%3C%3E%7C", "%09", "a%0Ab", "%F0%9F%98%80",
    "%3B", "%2Ea%2E", "%2E", "%2E.%2E", "%20lead", "trail%20",
    "%43ON", "%63on.txt", "%43om1", "%4CPT9.x.y", "%6Eul", "%61ux%2E", "%50RN",
    "%43OM0", "%4CPT0", "COM%C2%B9", "LPT%C2%B9",
    "CONX", "COM10", "xCON", "CON-1", "%2ECON",
    "%70reflight", "%50reFlight", "%6Eational_summary", "preflight.x",
    "%72eport", "%52esults", "%49SSUES", "%63orrections", "issues.csv", "my_report",
    "%75nit_records", "%55nit_Records"
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
  # A coordinate on a source issue of each unit, a latitude of 100's and a longitude of 110's,
  # added here so the fixture's counts are unchanged (D8.19 (2), D20.34).
  coordinates <- new_issues(list(
    issue_id = 21:22, run_id = results$metadata$run$run_id, run_scope = "package",
    rule_id = "coordinate_out_of_bounds", rule_fingerprint = "f1", stage = "conformance",
    layer = 3L, severity = "error", reason_code = "*", table_name = "stations",
    column_name = c("latitude", "longitude"), record_pk = c("ST1", "ST2"), row_number = 1:2,
    pk_positional = FALSE, level = "record", value = c("51.0447331", "-114.0718829"),
    unit = c("100", "110"), area = c("A-100-1", "A-110-1"), plot = c("P-100-1", "P-110-1"),
    reaches_other_unit = FALSE, status = "open", issue_class = "source",
    class_basis = "override"
  ), group_cols = c("unit", "area"), key_col = "plot")
  fingerprints <- issue_fingerprint(coordinates)
  data.table::set(coordinates, j = "fingerprint", value = fingerprints$fingerprint)
  data.table::set(coordinates, j = "fingerprint_pk", value = fingerprints$fingerprint_pk)
  results$issues <- rbind(results$issues, coordinates)
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
  # values, coordinates, provider keys, group values, the problem unit's key, results-row
  # examples (D20.23). The values fx_report_results() lists, with the coordinates planted
  # above: each unit's own, and those only "*" rows, marked issues and a results row's examples
  # hold, which reach neither unit of two.
  own <- list(
    "100" = c("AB-secret", "A-100-1", "P-100-1", "P-100-2", "ST-100-1", "51.0447331"),
    "110" = c("BC-secret", "A-110-1", "P-110-1", "ST-110-2", "114.0718829")
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
    # Every page's guide says proposed corrections come later, naming the unit's own CSV
    # (D22.9).
    proposals <- report_text(
      "report_guide_proposals",
      csv_name = paste0(stems[units == unit], "_issues.csv")
    )
    expect_true(grepl(html_escape(proposals), page, fixed = TRUE), info = unit)
    # The assembled page escapes what it shows: 110's under-review issue holds "<script>&".
    expect_false(grepl("<script>&", page, fixed = TRUE), info = unit)
    if (unit == "110") {
      expect_true(grepl("&lt;script&gt;&amp;", page, fixed = TRUE))
    }
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

test_that("given previous and a register, each unit's sections 11 and 12 hold its own (13)", {
  # About 8 builds, 2 to 3 s, so skipped on CRAN (D20.3, D23.157): the test of a unit's resolved
  # issues, register entries and previous run below, with one build of two units and one of a
  # run of 100 alone, covers the routing of each unit's own on CRAN. This one still holds what
  # that one doesn't: the typed previous run, previous as a CSV path or a run folder, the
  # register without a unit column or with "*", the solo run whose previous names only 100 and
  # "*", the solo run persisting 110's issue on 100's fingerprint, and the build stopping before
  # it removes or writes anything.
  testthat::skip_on_cran()
  local_results_registry()
  results <- fx_report_results()
  issues <- results$issues
  schema <- results_schema(c("unit", "area"), "plot")$issues
  # The previous run: this run's issues but 100's issue 7 and 110's issue 18, each a source
  # issue, so both are new, and four issues this run doesn't have: 100's, 110's, one marked,
  # and one of a record rule whose unit is "*", each holding a value of its own.
  previous <- data.table::copy(issues)[!issues$issue_id %in% c(7L, 18L)]
  data.table::set(previous, j = "run_id", value = "20261001T080000Z_0000aa")
  gone <- complete_table(list(
    issue_id = 101:104, run_id = "20261001T080000Z_0000aa", run_scope = "package",
    rule_id = "code_not_in_list", rule_fingerprint = "f1", stage = "conformance", layer = 3L,
    severity = "error", reason_code = "*", table_name = "hauls", column_name = "gear",
    record_pk = c("H91", "H92", "H93", "H94"), row_number = 1:4, pk_positional = FALSE,
    level = "record", value = c("RES-100-own", "RES-110-own", "RES-MARKED", "RES-STAR"),
    unit = c("100", "110", "100", "*"), area = c("A-100-1", "A-110-1", "A-100-1", "*"),
    plot = c("P-100-1", "P-110-1", "P-100-1", "*"),
    reaches_other_unit = c(FALSE, FALSE, TRUE, FALSE), status = "open",
    issue_class = "source", fingerprint = paste0("gone-", 1:4),
    fingerprint_pk = paste0("gone-pk-", 1:4)
  ), schema, "issues")
  previous <- rbind(previous, gone)
  # The register: first two entries naming 100 for 110's issues 19 and 18, one holding and one
  # lapsed, which no unit shows, and one that lapsed on 100's issue 8; then an entry that holds
  # for issue 8 (a missing value), one for 110's issue 19, one that lapsed on 100's issue 11
  # and one on 110's issue 18; and an entry for issue 14, which reaches no unit. Each issue's
  # list row shows its first entry that holds, its own unit's. The report-unit column is named
  # unit, as a variable of the build is.
  at <- match(c(19L, 18L, 8L, 8L, 19L, 11L, 18L, 14L), issues$issue_id)
  held <- issues$value[at[[1L]]]
  why <- c(
    "WHY-CROSS", "LAPSED-CROSS", "LAPSED-ON-HELD", "WHY-100-own", "WHY-110-own",
    "LAPSED-100-own", "LAPSED-110-own", "WHY-MARKED"
  )
  register <- data.frame(
    fingerprint = issues$fingerprint[at], fingerprint_pk = issues$fingerprint_pk[at],
    rule_id = issues$rule_id[at], table_name = issues$table_name[at],
    column_name = issues$column_name[at], provider_key = issues$provider_key[at],
    acknowledged_value = c(
      held, "OLD-CROSS", "OLD-ON-HELD", NA, held, "OLD-100-own", "OLD-110-own", NA
    ),
    decision = "verified", who = "WHO-secret", when = "2026-11-01", why = why,
    run_scope = "package", unit = c("100", "100", "100", "100", "110", "100", "110", "110"),
    area = issues$area[at], plot = issues$plot[at]
  )
  before <- data.table::copy(results$issues)
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  files <- gpq_build_reports(results, output, previous = previous, acknowledgements = register)
  # The caller's results are as they were (D20.120).
  expect_identical(results$issues, before)
  own <- list(
    "100" = c("RES-100-own", "WHY-100-own", "OLD-100-own", "LAPSED-100-own", "LAPSED-ON-HELD"),
    "110" = c("RES-110-own", "WHY-110-own", "OLD-110-own", "LAPSED-110-own")
  )
  nowhere <- c(
    "RES-MARKED", "RES-STAR", "WHY-MARKED", "WHY-CROSS", "LAPSED-CROSS", "OLD-CROSS", "WHO-secret"
  )
  for (unit in names(own)) {
    at <- which(names(files) == unit)
    page <- readChar(files[[at[[1L]]]], file.size(files[[at[[1L]]]]), useBytes = TRUE)
    csv <- data.table::fread(files[[at[[2L]]]], encoding = "UTF-8", colClasses = "character")
    for (value in own[[unit]]) {
      expect_true(grepl(value, page, fixed = TRUE), info = paste(unit, value))
    }
    for (value in c(own[[setdiff(names(own), unit)]], nowhere)) {
      expect_false(grepl(value, page, fixed = TRUE), info = paste(unit, value))
    }
    # The acknowledged issue's row shows the entry that holds, never the one that lapsed.
    listed <- regmatches(page, regexpr(
      "<caption>Acknowledged issues</caption>.*?</table>", page,
      perl = TRUE
    ))
    expect_true(grepl(own[[unit]][[2L]], listed, fixed = TRUE), info = unit)
    expect_false(grepl("LAPSED-", listed, fixed = TRUE), info = unit)
    # The CSV keeps the acknowledged issue, with its status, and holds no resolved one (D23.34).
    expect_false(any(grepl("RES-", unlist(csv), fixed = TRUE)), info = unit)
    acked <- match(c("100" = 8L, "110" = 19L)[[unit]], issues$issue_id)
    row <- csv$record_pk == issues$record_pk[acked] & csv$rule_id == issues$rule_id[acked]
    expect_equal(csv$status[row], "acknowledged", info = unit)
    # Persisting issues were first seen in the previous run, new ones in this one (D23.35).
    expect_true(grepl("<td>2026-10-01</td>", page, fixed = TRUE), info = unit)
    expect_true(grepl("<td>2026-10-26</td>", page, fixed = TRUE), info = unit)
  }
  # route_unit gives each unit its own resolved issues, the marked and "*" ones to none.
  saved <- gpq_read_results(file.path(output, results$metadata$run$run_id))
  compared <- gpq_compare_runs(saved, previous)
  for (unit in names(own)) {
    routed <- route_unit(results, unit, names(own), resolved = compared$resolved)
    expect_identical(routed$resolved$value, own[[unit]][[1L]], info = unit)
  }
  # A previous run's issues as fread() reads them, a numeric report unit, a record ID of digits
  # and an empty provider key: the build reads them as text (D23.112).
  typed <- as.data.frame(gone)[1:2, ]
  typed$unit <- c(100, 110)
  typed$record_pk <- c(91L, 92L)
  typed$provider_key <- NA
  typed_given <- data.table::copy(typed)
  typed_files <- gpq_build_reports(results, output, previous = typed)
  # The caller's previous run keeps its columns' classes and values.
  expect_identical(typed, typed_given)
  for (unit in names(own)) {
    path <- typed_files[names(typed_files) == unit][[1L]]
    page <- readChar(path, file.size(path), useBytes = TRUE)
    expect_true(grepl(own[[unit]][[1L]], page, fixed = TRUE), info = unit)
    expect_false(grepl(own[[setdiff(names(own), unit)]][[1L]], page, fixed = TRUE), info = unit)
  }
  # A run of unit 100 alone, given entries of 110's on 100's provider keys (D23.110, D23.111):
  # one would hold and one has lapsed, yet neither acknowledges 100's issue nor shows.
  solo <- fx_report_results()
  data.table::set(solo$metadata$run, j = "report_units", value = "100")
  solo$results <- solo$results[solo$results$unit != "110"]
  solo$issues <- solo$issues[solo$issues$unit != "110"]
  solo$corrections <- solo$corrections[solo$corrections$issue_id %in% solo$issues$issue_id]
  shared <- match(c(11L, 10L), issues$issue_id)
  theirs <- data.frame(
    fingerprint = issues$fingerprint[shared], fingerprint_pk = issues$fingerprint_pk[shared],
    rule_id = issues$rule_id[shared], table_name = issues$table_name[shared],
    column_name = issues$column_name[shared], provider_key = issues$provider_key[shared],
    acknowledged_value = c(issues$value[shared[[1L]]], "VALUE-OF-110"), decision = "verified",
    who = "WHO-secret", when = "2026-11-01", why = c("WHY-OF-110", "WHY-LAPSED-110"),
    run_scope = "package", unit = "110", area = "A-110-1", plot = "P-110-1"
  )
  solo_output <- withr::local_tempdir()
  gpq_write_results(solo, solo_output)
  solo_files <- gpq_build_reports(solo, solo_output, acknowledgements = theirs)
  page <- readChar(solo_files[[1L]], file.size(solo_files[[1L]]), useBytes = TRUE)
  for (value in c("WHY-OF-110", "WHY-LAPSED-110", "VALUE-OF-110")) {
    expect_false(grepl(value, page, fixed = TRUE), info = value)
  }
  csv <- data.table::fread(solo_files[[2L]], encoding = "UTF-8", colClasses = "character")
  expect_equal(csv$status[csv$record_pk == issues$record_pk[shared[[1L]]]], "open")
  # A register without the report-unit column matches a record-level entry on the other group
  # columns it holds (D23.128): 110's area and plot on 100's provider keys neither acknowledge
  # 100's issue nor show; with "*" there, the entry that holds acknowledges it and shows.
  theirs$unit <- NULL
  solo_files <- gpq_build_reports(solo, solo_output, acknowledgements = theirs)
  page <- readChar(solo_files[[1L]], file.size(solo_files[[1L]]), useBytes = TRUE)
  for (value in c("WHY-OF-110", "WHY-LAPSED-110", "VALUE-OF-110")) {
    expect_false(grepl(value, page, fixed = TRUE), info = value)
  }
  csv <- data.table::fread(solo_files[[2L]], encoding = "UTF-8", colClasses = "character")
  expect_equal(csv$status[csv$record_pk == issues$record_pk[shared[[1L]]]], "open")
  theirs$area <- "*"
  theirs$plot <- "*"
  solo_files <- gpq_build_reports(solo, solo_output, acknowledgements = theirs)
  page <- readChar(solo_files[[1L]], file.size(solo_files[[1L]]), useBytes = TRUE)
  expect_true(grepl("WHY-OF-110", page, fixed = TRUE))
  csv <- data.table::fread(solo_files[[2L]], encoding = "UTF-8", colClasses = "character")
  expect_equal(csv$status[csv$record_pk == issues$record_pk[shared[[1L]]]], "acknowledged")
  # The run of 100 alone given the previous run of 100 and 110 (D23.127, D23.141, D23.142): as
  # the previous run names another unit, its resolved issues are routed as a run of several
  # units routes them, so 100's page shows its own unmarked one, RES-100-own, but neither
  # 110's, the "*" record-level RES-STAR, the "*" column-level RES-STAR-COLUMN nor its own
  # marked RES-MARKED; with the previous run naming no unit but 100, all its resolved issues
  # are 100's.
  solo_page <- function(previous) {
    solo_files <- gpq_build_reports(solo, solo_output, previous = previous)
    readChar(solo_files[[1L]], file.size(solo_files[[1L]]), useBytes = TRUE)
  }
  star_column <- complete_table(list(
    issue_id = 105L, run_id = "20261001T080000Z_0000aa", run_scope = "package",
    rule_id = "code_not_in_list", rule_fingerprint = "f1", stage = "conformance", layer = 3L,
    severity = "error", reason_code = "*", table_name = "hauls", column_name = "gear",
    record_pk = "*", row_number = NA_integer_, pk_positional = FALSE, level = "column",
    value = "RES-STAR-COLUMN", unit = "*", area = "*", plot = "*", reaches_other_unit = FALSE,
    status = "open", issue_class = "source", fingerprint = "gone-5", fingerprint_pk = "gone-pk-5"
  ), schema, "issues")
  page <- solo_page(rbind(previous, star_column))
  expect_true(grepl("RES-100-own", page, fixed = TRUE))
  for (value in c("RES-STAR", "RES-STAR-COLUMN", "RES-MARKED", "RES-110-own")) {
    expect_false(grepl(value, page, fixed = TRUE), info = value)
  }
  previous_100 <- previous[previous$unit %chin% c("100", "*")]
  page <- solo_page(rbind(previous_100, star_column))
  for (value in c("RES-100-own", "RES-STAR", "RES-STAR-COLUMN", "RES-MARKED")) {
    expect_true(grepl(value, page, fixed = TRUE), info = value)
  }
  # So where every issue of 110's persists, matching one of 100's on its fingerprint: the
  # previous run itself names 110, given as its issues in memory or as a CSV file (D23.142).
  persisting <- previous_100[previous_100$issue_id == 11L]
  data.table::set(
    persisting,
    j = c("issue_id", "unit", "area", "plot"), value = list(120L, "110", "A-110-1", "P-110-1")
  )
  named_110 <- rbind(previous_100, persisting)
  named_csv <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(named_110, named_csv)
  for (given in list(named_110, named_csv)) {
    page <- solo_page(given)
    expect_true(grepl("RES-100-own", page, fixed = TRUE))
    for (value in c("RES-STAR", "RES-MARKED")) {
      expect_false(grepl(value, page, fixed = TRUE), info = value)
    }
  }
  # And given as a run folder, by the units its run.csv lists: a run of 100 and 110 whose
  # resolved issues are 100's marked one and a "*" one.
  earlier <- fx_report_results()
  earlier_gone <- data.table::copy(gone[gone$value %chin% c("RES-MARKED", "RES-STAR")])
  data.table::set(earlier_gone, j = "run_id", value = earlier$metadata$run$run_id)
  earlier$issues <- rbind(earlier$issues[earlier$issues$unit != "110"], earlier_gone)
  earlier$corrections <- earlier$corrections[
    earlier$corrections$issue_id %in% earlier$issues$issue_id
  ]
  earlier_dir <- gpq_write_results(earlier, withr::local_tempdir())
  page <- solo_page(earlier_dir)
  for (value in c("RES-STAR", "RES-MARKED")) {
    expect_false(grepl(value, page, fixed = TRUE), info = value)
  }
  # A bad previous run or register stops the build before it removes or writes anything.
  stamp <- file.mtime(files)
  expect_error(
    gpq_build_reports(results, output, previous = file.path(output, "nowhere")),
    "`previous` must be a run folder, a data.frame or the path of an existing .csv file.",
    fixed = TRUE
  )
  register$decision[[1L]] <- "maybe"
  expect_error(
    gpq_build_reports(results, output, acknowledgements = register),
    "an acknowledgement's decision is verified or rejected_proposal.",
    fixed = TRUE
  )
  expect_error(
    gpq_build_reports(results, output, unmatched_tables = 1),
    "`unmatched_tables` must be NULL or a character vector of table names.",
    fixed = TRUE
  )
  expect_identical(file.mtime(files), stamp)
})

test_that("a unit's resolved issues, register entries and previous run reach it alone (13)", {
  local_results_registry()
  results <- fx_report_results()
  issues <- results$issues
  schema <- results_schema(c("unit", "area"), "plot")$issues
  # The previous run: this run's issues but 100's issue 7 and 110's issue 18, and four issues
  # this run doesn't have: 100's, 110's, one marked and one of a "*" unit, each with its own value.
  previous <- data.table::copy(issues)[!issues$issue_id %in% c(7L, 18L)]
  data.table::set(previous, j = "run_id", value = "20261001T080000Z_0000aa")
  gone <- complete_table(list(
    issue_id = 101:104, run_id = "20261001T080000Z_0000aa", run_scope = "package",
    rule_id = "code_not_in_list", rule_fingerprint = "f1", stage = "conformance", layer = 3L,
    severity = "error", reason_code = "*", table_name = "hauls", column_name = "gear",
    record_pk = c("H91", "H92", "H93", "H94"), row_number = 1:4, pk_positional = FALSE,
    level = "record", value = c("RES-100-own", "RES-110-own", "RES-MARKED", "RES-STAR"),
    unit = c("100", "110", "100", "*"), area = c("A-100-1", "A-110-1", "A-100-1", "*"),
    plot = c("P-100-1", "P-110-1", "P-100-1", "*"),
    reaches_other_unit = c(FALSE, FALSE, TRUE, FALSE), status = "open",
    issue_class = "source", fingerprint = paste0("gone-", 1:4),
    fingerprint_pk = paste0("gone-pk-", 1:4)
  ), schema, "issues")
  previous <- rbind(previous, gone)
  # route_unit gives each unit its own resolved issues, the marked and "*" ones to none.
  compared <- gpq_compare_runs(results, previous)
  own <- list(
    "100" = c("RES-100-own", "WHY-100-own", "OLD-100-own", "LAPSED-100-own", "LAPSED-ON-HELD"),
    "110" = c("RES-110-own", "WHY-110-own", "OLD-110-own", "LAPSED-110-own")
  )
  for (unit in names(own)) {
    routed <- route_unit(results, unit, names(own), resolved = compared$resolved)
    expect_identical(routed$resolved$value, own[[unit]][[1L]], info = unit)
  }
  # The register: two entries naming 100 for 110's issues 19 and 18, one holding and one lapsed,
  # which no unit shows; an entry for 100's issue 8 that lapsed, then one that holds; one for
  # 110's issue 19; and one for issue 14, which reaches no unit.
  at <- match(c(19L, 18L, 8L, 8L, 19L, 14L), issues$issue_id)
  held <- issues$value[at[[1L]]]
  register <- data.frame(
    fingerprint = issues$fingerprint[at], fingerprint_pk = issues$fingerprint_pk[at],
    rule_id = issues$rule_id[at], table_name = issues$table_name[at],
    column_name = issues$column_name[at], provider_key = issues$provider_key[at],
    acknowledged_value = c(held, "OLD-CROSS", "OLD-ON-HELD", NA, held, NA),
    decision = "verified", who = "WHO-secret", when = "2026-11-01",
    why = c(
      "WHY-CROSS", "LAPSED-CROSS", "LAPSED-ON-HELD", "WHY-100-own", "WHY-110-own", "WHY-MARKED"
    ),
    run_scope = "package", unit = c("100", "100", "100", "100", "110", "110"),
    area = issues$area[at], plot = issues$plot[at]
  )
  before <- data.table::copy(results$issues)
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  files <- gpq_build_reports(results, output, previous = previous, acknowledgements = register)
  # The caller's results are as they were (D20.120).
  expect_identical(results$issues, before)
  nowhere <- c(
    "RES-MARKED", "RES-STAR", "WHY-MARKED", "WHY-CROSS", "LAPSED-CROSS", "OLD-CROSS", "WHO-secret"
  )
  for (unit in names(own)) {
    at <- which(names(files) == unit)
    page <- readChar(files[[at[[1L]]]], file.size(files[[at[[1L]]]]), useBytes = TRUE)
    csv <- data.table::fread(files[[at[[2L]]]], encoding = "UTF-8", colClasses = "character")
    for (value in c(own[[unit]][1:2], "2026-10-01", "2026-10-26")) {
      expect_true(grepl(value, page, fixed = TRUE), info = paste(unit, value))
    }
    # The values the previous run or the register holds for the other unit, and the hidden ones.
    other <- setdiff(names(own), unit)
    for (value in c(own[[other]][1:2], nowhere, "LAPSED-ON-HELD"[unit == "110"])) {
      expect_false(grepl(value, page, fixed = TRUE), info = paste(unit, value))
      expect_false(any(grepl(value, unlist(csv), fixed = TRUE)), info = paste(unit, value, "csv"))
    }
    # The acknowledged issue's row shows the entry that holds, never the one that lapsed.
    listed <- regmatches(page, regexpr(
      "<caption>Acknowledged issues</caption>.*?</table>", page,
      perl = TRUE
    ))
    expect_true(grepl(own[[unit]][[2L]], listed, fixed = TRUE), info = unit)
    expect_false(grepl("LAPSED-", listed, fixed = TRUE), info = unit)
    # The CSV keeps the acknowledged issue with its status, and holds no resolved one (D23.34).
    expect_false(any(grepl("RES-", unlist(csv), fixed = TRUE)), info = unit)
    acked <- match(c("100" = 8L, "110" = 19L)[[unit]], issues$issue_id)
    row <- csv$record_pk == issues$record_pk[acked] & csv$rule_id == issues$rule_id[acked]
    expect_equal(csv$status[row], "acknowledged", info = unit)
  }
  # The run of 100 alone, given the previous run of 100 and 110 and entries of 110's on 100's
  # provider keys (D23.110, D23.111, D23.127, D23.141): its page shows its own unmarked resolved
  # issue, but neither 110's, the "*" ones nor its marked one, and no entry of 110's acknowledges
  # or shows.
  solo <- fx_report_results()
  data.table::set(solo$metadata$run, j = "report_units", value = "100")
  solo$results <- solo$results[solo$results$unit != "110"]
  solo$issues <- solo$issues[solo$issues$unit != "110"]
  solo$corrections <- solo$corrections[solo$corrections$issue_id %in% solo$issues$issue_id]
  star_column <- complete_table(list(
    issue_id = 105L, run_id = "20261001T080000Z_0000aa", run_scope = "package",
    rule_id = "code_not_in_list", rule_fingerprint = "f1", stage = "conformance", layer = 3L,
    severity = "error", reason_code = "*", table_name = "hauls", column_name = "gear",
    record_pk = "*", row_number = NA_integer_, pk_positional = FALSE, level = "column",
    value = "RES-STAR-COLUMN", unit = "*", area = "*", plot = "*", reaches_other_unit = FALSE,
    status = "open", issue_class = "source", fingerprint = "gone-5", fingerprint_pk = "gone-pk-5"
  ), schema, "issues")
  shared <- match(c(11L, 10L), issues$issue_id)
  theirs <- data.frame(
    fingerprint = issues$fingerprint[shared], fingerprint_pk = issues$fingerprint_pk[shared],
    rule_id = issues$rule_id[shared], table_name = issues$table_name[shared],
    column_name = issues$column_name[shared], provider_key = issues$provider_key[shared],
    acknowledged_value = c(issues$value[shared[[1L]]], "VALUE-OF-110"), decision = "verified",
    who = "WHO-secret", when = "2026-11-01", why = c("WHY-OF-110", "WHY-LAPSED-110"),
    run_scope = "package", unit = "110", area = "A-110-1", plot = "P-110-1"
  )
  solo_output <- withr::local_tempdir()
  gpq_write_results(solo, solo_output)
  solo_files <- gpq_build_reports(
    solo, solo_output,
    previous = rbind(previous, star_column), acknowledgements = theirs
  )
  page <- readChar(solo_files[[1L]], file.size(solo_files[[1L]]), useBytes = TRUE)
  expect_true(grepl("RES-100-own", page, fixed = TRUE))
  hidden <- c(
    "RES-STAR", "RES-STAR-COLUMN", "RES-MARKED", "RES-110-own", "WHY-OF-110", "WHY-LAPSED-110",
    "VALUE-OF-110", "WHO-secret"
  )
  for (value in hidden) {
    expect_false(grepl(value, page, fixed = TRUE), info = value)
  }
  csv <- data.table::fread(solo_files[[2L]], encoding = "UTF-8", colClasses = "character")
  expect_equal(csv$status[csv$record_pk == issues$record_pk[shared[[1L]]]], "open")
})

test_that("a unit's report counts the run's pk: forms, as the run does (D23.148, D23.152)", {
  local_results_registry()
  results <- fx_report_results()
  issues <- results$issues
  # 110's issue 19 is given 100's issue 11's pk: form and value, as a record sharing its PK
  # would hold; their fingerprints differ. An entry made from issue 11, its unit "*", holds.
  mine <- match(11L, issues$issue_id)
  theirs <- match(19L, issues$issue_id)
  data.table::set(
    issues,
    i = theirs, j = c("fingerprint_pk", "value"),
    value = list(issues$fingerprint_pk[[mine]], issues$value[[mine]])
  )
  register <- data.frame(
    fingerprint = issues$fingerprint[mine], fingerprint_pk = issues$fingerprint_pk[mine],
    rule_id = issues$rule_id[mine], table_name = issues$table_name[mine],
    column_name = issues$column_name[mine], provider_key = issues$provider_key[mine],
    acknowledged_value = issues$value[mine], decision = "verified", who = "A. Reviewer",
    when = "2026-11-01", why = "Checked.", run_scope = "package", unit = "*", area = "*",
    plot = "*"
  )
  # The run's issues: 11 acknowledged, 19 open, its pk: form shared in the run.
  run_issues <- data.table::copy(issues)
  gpq_apply_acknowledgements(run_issues, register)
  expect_equal(run_issues$status[c(mine, theirs)], c("acknowledged", "open"))
  # Each unit's report agrees, though 110's issues alone hold the form once.
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  files <- gpq_build_reports(results, output, acknowledgements = register)
  status_in <- function(unit, at) {
    csv <- data.table::fread(
      files[names(files) == unit][[2L]],
      encoding = "UTF-8", colClasses = "character"
    )
    csv$status[csv$record_pk == issues$record_pk[[at]] & csv$rule_id == issues$rule_id[[at]]]
  }
  expect_equal(status_in("100", mine), "acknowledged")
  expect_equal(status_in("110", theirs), "open")
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
  own_tables <- c("results.csv", "issues.csv", "corrections.csv", "unit_records.csv")
  expect_true(all(file.exists(file.path(run_dir, "tables", own_tables))))
  # A rebuild writes the same files again, and nothing else.
  again <- gpq_build_reports(results, output)
  expect_equal(again, files)
  expect_setequal(
    list.files(run_dir, recursive = TRUE, all.files = TRUE),
    c(
      "reports/preflight.html", sub(paste0(run_dir, "/"), "", files, fixed = TRUE),
      file.path("tables", own_tables),
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
  # Section 1 shows how each table given in memory came, never the run's count of its rows,
  # the whole delivery's (D20.126).
  for (page in files[grepl("[.]html$", files)]) {
    html <- readChar(page, file.size(page), useBytes = TRUE)
    for (table in names(tables)) {
      expect_match(
        html, sprintf("<dt>Table: %s</dt><dd>in memory</dd>", table),
        fixed = TRUE
      )
    }
    expect_false(grepl("in memory; ", html, fixed = TRUE))
  }
})

test_that("a design group spanning two units reaches neither unit's report or CSV (D20.125)", {
  fish <- fx_fish_clean()
  # Hauls 5 (LK) and 6 (RV) copy hauls 2 and 3 with dates of their own. By gear: GN holds LK's
  # haul 1 and RV's haul 4, TN LK's hauls 2 and 5, and EF RV's hauls 3 and 6, each group's
  # dates disagreeing.
  more <- fish$hauls[2:3]
  data.table::set(more, j = c("haul_id", "haul_date"), value = list(
    c("hauls_5", "hauls_6"), c("2020-07-15", "2020-08-21")
  ))
  fish$hauls <- rbind(fish$hauls, more)
  settings <- list(
    design_geometry_table = "hauls", design_geometry_by = "gear",
    design_geometry_columns = "haul_date"
  )
  results <- gpq_check_conformance(
    fish, fx_fish_spec(),
    group_cols = "water_body", settings = settings, layers = 4
  )
  gpq_classify_issues(results$issues, NULL)
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  files <- gpq_build_reports(results, output)
  # Each unit's own dates, compared in a group of its own, are in its files; the other unit's,
  # and GN's two, whose issues are marked, are in neither unit's.
  own <- list(LK = c("2020-06-02", "2020-07-15"), RV = c("2020-06-03", "2020-08-21"))
  spanning <- c("2020-06-01", "2020-06-04")
  for (unit in names(own)) {
    for (path in files[names(files) == unit]) {
      text <- readChar(path, file.size(path), useBytes = TRUE)
      for (value in own[[unit]]) {
        expect_true(grepl(value, text, fixed = TRUE), info = paste(unit, value, path))
      }
      for (value in c(own[[setdiff(names(own), unit)]], spanning)) {
        expect_false(grepl(value, text, fixed = TRUE), info = paste(unit, value, path))
      }
    }
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
  page <- paste(readLines(files[[1L]], encoding = "UTF-8"), collapse = "\n")
  expect_match(page, paste("Quality report for run", results$metadata$run$run_id), fixed = TRUE)
  # Its guide's line on proposed corrections names report_issues.csv (D22.9).
  expect_match(
    page, html_escape(report_text("report_guide_proposals", csv_name = "report_issues.csv")),
    fixed = TRUE
  )
})

test_that("a previous issues table of documented columns only builds section 11 (11; D23.160)", {
  skip_on_cran()
  fish <- fx_fish_spec()
  tables <- fx_fish_clean()
  tables$hauls <- data.table::copy(tables$hauls)
  tables$hauls$minutes <- NULL
  results <- suppressWarnings(gpq_check_conformance(tables, fish, layers = 0))
  gpq_classify_issues(results$issues, NULL)
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  # The earlier run's one issue, which this run no longer holds, in the eight columns
  # gpq_compare_runs() documents and no others.
  documented <- c(
    "run_id", "run_scope", "rule_id", "rule_fingerprint", "table_name", "fingerprint",
    "fingerprint_pk", "first_seen_run"
  )
  previous <- data.table::copy(results$issues[1L, documented, with = FALSE])
  data.table::set(previous, j = "run_id", value = "20261001T080000Z_0000aa")
  data.table::set(previous, j = c("fingerprint", "fingerprint_pk"), value = list("gone", "gone"))
  data.table::set(previous, j = "first_seen_run", value = "20261001T080000Z_0000aa")
  files <- gpq_build_reports(results, output, previous = previous)
  page <- paste(readLines(files[[1L]], encoding = "UTF-8"), collapse = "\n")
  start <- regexpr("<section[^>]*id=\"changes\"", page)
  expect_gt(start, 0L)
  section <- substr(page, start, nchar(page))
  section <- substr(section, 1L, regexpr("</section>", section) - 1L)
  # The resolved issue is listed, by its check ID, and the counts row says one resolved.
  expect_match(section, "column_missing", fixed = TRUE)
  expect_match(
    section,
    sprintf(
      "<td>%s</td>(<td class=\"num\">[0-9]+</td>){2}<td class=\"num\">1</td>",
      html_escape(report_text("report_changes_resolved"))
    )
  )
  # The columns the earlier run lacks are empty in every row, so the table leaves them out.
  for (id in c(
    "report_column_severity", "report_column_column", "report_column_provider_key",
    "report_column_record_pk", "report_column_issue_value"
  )) {
    expect_false(grepl(paste0(">", html_escape(report_text(id)), "<"), section, fixed = TRUE))
  }
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
  worst <- new_gpq_results(
    results, issues, new_corrections(),
    empty_table(results_schema(c("unit", "area", "zone"), "plot")$unit_records), metadata
  )
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
  # values among them, and no cell is left uncut: no run of more than cell_cap quotes.
  cell <- enc2utf8(paste0(strrep("&quot;", 100L), "\U2026"))
  expect_true(grepl(cell, html, fixed = TRUE, useBytes = TRUE))
  expect_false(grepl(strrep("&quot;", 101L), html, fixed = TRUE))
  # The "Each issue" table's row_cap rows each show their raw value, cut.
  raw <- enc2utf8(paste0("R", strrep("&quot;", 99L), "\U2026"))
  expect_equal(
    lengths(regmatches(html, gregexpr(raw, html, fixed = TRUE, useBytes = TRUE))), n - 1L
  )
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

test_that("a text row with a slot its package row lacks stops the build first (D20.138)", {
  local_results_registry()
  results <- fx_report_results()
  output <- withr::local_tempdir()
  run_dir <- gpq_write_results(results, output)
  # An earlier build's report and CSV, which a build that stops on its text must leave.
  dir.create(file.path(run_dir, "reports"), showWarnings = FALSE)
  earlier <- c(
    file.path(run_dir, "reports", "100.html"), file.path(run_dir, "tables", "100_issues.csv")
  )
  for (path in earlier) writeLines("earlier", path)
  # Pre-flight's slot rule (text_slots()) reads every row, in the run's language or another.
  for (lang in c("en", "fr")) {
    text <- data.frame(text_id = "report_title", lang = lang, text = "Report for {unit} {zone}")
    expect_error(
      gpq_build_reports(results, output, text = text),
      paste0(
        "Report text report_title (language ", lang, ") in `text` uses the slot {zone}, which ",
        "the package's row of that text doesn't have."
      ),
      fixed = TRUE
    )
    for (path in earlier) expect_identical(readLines(path), "earlier", info = path)
  }
  # Fewer slots, and a text the package doesn't have, pass, and the build replaces the files.
  text <- data.frame(
    text_id = c("report_title", "provider_note"), lang = "en",
    text = c("Quality report", "A note on {anything}")
  )
  files <- gpq_build_reports(results, output, text = text)
  expect_true(all(earlier %in% files))
  page <- paste(readLines(earlier[[1L]], encoding = "UTF-8"), collapse = "\n")
  expect_match(page, "<h1>Quality report</h1>", fixed = TRUE)
})

test_that("the build stops on exactly the slots pre-flight's text_slot_unknown finds (D22.26)", {
  local_results_registry()
  results <- fx_report_results()
  output <- withr::local_tempdir()
  gpq_write_results(results, output)
  # The same rows through both copies of D14.21's rule: rows that pass (fewer slots, a slot
  # repeated, a text the package lacks); a slot added in English and in another language; a
  # slot another text has and this one lacks; and two rows each adding one, the build naming
  # pre-flight's first.
  cases <- list(
    data.frame(
      text_id = c("report_title", "report_title", "provider_note"), lang = c("en", "de", "en"),
      text = c("Quality report", "{unit} {unit}", "A note on {anything}")
    ),
    data.frame(text_id = "report_title", lang = "en", text = "Report for {unit} {zone}"),
    data.frame(text_id = "report_title", lang = "fr", text = "Rapport {unit} {zone}"),
    data.frame(text_id = "report_title", lang = "en", text = "Report {run_id}"),
    data.frame(
      text_id = c("report_masthead_run", "report_title"), lang = "en",
      text = c("Run {run_id} {when}", "Report for {unit} {zone}")
    )
  )
  expected <- list(
    character(), "report_title en {zone}", "report_title fr {zone}", "report_title en {run_id}",
    c("report_masthead_run en {when}", "report_title en {zone}")
  )
  spec <- fx_fish_spec()
  for (k in seq_along(cases)) {
    found <- preflight_check_functions()[["text_slot_unknown"]](
      spec, preflight_context(text = cases[[k]])
    )
    # Each finding's text, language and slot, as its detail and the build's error name them.
    flagged <- sub(
      "^Text (\\S+) in language (\\S+) uses the slot (\\S+), .*$", "\\1 \\2 \\3", found$detail
    )
    stopped <- tryCatch(
      {
        gpq_build_reports(results, output, text = cases[[k]])
        character()
      },
      error = function(e) {
        sub(
          "^Report text (\\S+) \\(language (\\S+)\\) in `text` uses the slot (\\S+), .*$",
          "\\1 \\2 \\3", conditionMessage(e)
        )
      }
    )
    expect_identical(flagged, expected[[k]], info = paste("case", k))
    # The build stops where pre-flight finds a slot, and only there, naming its first.
    expect_identical(stopped, head(flagged, 1L), info = paste("case", k))
  }
})
