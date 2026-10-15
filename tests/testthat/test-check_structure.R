# Tests for Layer 0, structure (plan 6.1; D2.34, D8.11, D16.8, D16.14).

test_that("check_structure returns units for each Layer 0 rule, one per table or column", {
  fish <- fx_fish_clean()
  fish$extra <- data.frame(a = 1:3)
  fish$catches$colour <- "red"
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 0L)
  found <- check_structure(conformance_input(fish, spec), context)
  expect_named(found, c(
    "table_missing", "table_unexpected", "column_missing", "column_unexpected",
    "column_name_mismatch"
  ))
  kinds <- unique(unlist(lapply(found, function(outcomes) vapply(outcomes, `[[`, "", "kind"))))
  expect_equal(kinds, "unit")
  expect_equal(vapply(found$table_unexpected, `[[`, "", "table_name"), names(fish))
  expect_equal(
    vapply(found$table_unexpected, `[[`, TRUE, "failed"), c(FALSE, FALSE, FALSE, TRUE)
  )
  expect_equal(found$table_unexpected[[4L]]$n_records, 3L)
  unexpected <- Filter(function(outcome) outcome$failed, found$column_unexpected)
  expect_equal(vapply(unexpected, `[[`, "", "column_name"), "colour")
  expect_length(found$column_missing, 15L)
  expect_false(any(vapply(found$column_missing, `[[`, TRUE, "failed")))
})

test_that("an empty table is still checked as a unit, counting no records", {
  fish <- fx_fish_clean()
  fish$catches <- fish$catches[0L]
  fish$catches$weight <- NULL
  results <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0)
  missing <- results$issues[results$issues$rule_id == "column_missing"]
  expect_equal(missing$column_name, "weight")
  expect_equal(missing$n_records, 0L)
})

test_that("with no table of the spec given, each is missing and each given one unexpected", {
  # A full MAGPlot run, about 1 s since M4b cut the run's fixed cost (D16.18, D16.50).
  testthat::skip_on_cran()
  # MAGPlot's group columns are set, and no table of the spec reaches any report unit.
  upper <- fx_magp_clean()
  names(upper) <- toupper(names(upper))
  results <- fx_caught(upper)
  open <- results$issues[results$issues$status == "open"]
  expect_equal(
    as.vector(table(open$rule_id)[c("table_missing", "table_unexpected")]), c(17L, 17L)
  )
  expect_equal(nrow(open), 34L)
})

test_that("an exact name wins over a near one, which is then an extra column", {
  fish <- fx_fish_clean()
  fish$hauls$GEAR <- "x"
  results <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0)
  open <- results$issues
  expect_equal(paste(open$rule_id, open$column_name), "column_unexpected GEAR")
})

test_that("a name with a Unicode space is a near name, and its column is checked (D16.42, m21)", {
  fish <- fx_fish_clean()
  spaced <- paste0("depth", intToUtf8(0xA0))
  # Stored as text with one value that isn't a number, which Layer 1 finds there.
  stations <- data.table::copy(fish$stations)
  stations$depth <- as.character(stations$depth)
  stations$depth[[1L]] <- "deep"
  data.table::setnames(stations, "depth", spaced)
  fish$stations <- stations
  open <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0:1)$issues
  expect_equal(paste(open$rule_id, open$column_name, open$value), c(
    paste("column_name_mismatch depth", spaced), "class_mismatch depth character",
    "coercion_loss depth deep"
  ))
})

# MAGPlot's tables below are each run with MAGPlot's group columns and problem unit (R52),
# and their open issues read as rule, table, column and value.

test_that("MAGPlot's tables or columns reordered, or a data.frame, trip nothing (R52)", {
  # A full MAGPlot run, about 2 s since M4b cut the run's fixed cost (D16.18, D16.50).
  testthat::skip_on_cran()
  tables <- fx_mark_fixture(rev(lapply(fx_magp_clean(), data.table::copy)))
  data.table::setcolorder(tables$magp_trees, rev(names(tables$magp_trees)))
  tables$magp_plots <- as.data.frame(tables$magp_plots)
  issues <- fx_caught(tables)$issues
  expect_equal(sum(issues$status == "open"), 0L)
})

test_that("two near columns: the first is the attribute's; keys renamed by case only mismatch", {
  # A full MAGPlot run, about 2 s since M4b cut the run's fixed cost (D16.18, D16.50).
  testthat::skip_on_cran()
  tables <- fx_mark_fixture(lapply(fx_magp_clean(), data.table::copy))
  # First in the table's order, though "DBH" comes first in C order.
  data.table::setnames(tables$magp_tree_meas, "dbh", "Dbh")
  data.table::set(tables$magp_tree_meas, j = "DBH", value = tables$magp_tree_meas$Dbh)
  # The problem unit's key and a group column.
  data.table::setnames(tables$magp_plots, "magp_plot_id", "MAGP_PLOT_ID")
  data.table::setnames(tables$magp_sites, "jurisdiction", "Jurisdiction")
  issues <- fx_caught(tables)$issues
  open <- issues[issues$status == "open"]
  expect_equal(sort(paste(open$rule_id, open$table_name, open$column_name, open$value)), sort(c(
    "column_name_mismatch magp_tree_meas dbh Dbh", "column_unexpected magp_tree_meas DBH NA",
    "column_name_mismatch magp_plots magp_plot_id MAGP_PLOT_ID",
    "column_name_mismatch magp_sites jurisdiction Jurisdiction"
  )))
})

test_that("a parent table missing trips table_missing only (R52)", {
  # A full MAGPlot run, about 2 s since M4b cut the run's fixed cost (D16.18, D16.50).
  testthat::skip_on_cran()
  # magp_plots, the problem unit's table, which the tables below it reach groups through.
  base <- fx_magp_clean()
  tables <- fx_mark_fixture(lapply(base[names(base) != "magp_plots"], data.table::copy))
  issues <- fx_caught(tables)$issues
  open <- issues[issues$status == "open"]
  expect_equal(paste(open$rule_id, open$table_name, open$column_name), "table_missing magp_plots *")
})

test_that("a key or a group column missing trips column_missing only (R52)", {
  # A full MAGPlot run, about 2 s since M4b cut the run's fixed cost (D16.18, D16.50).
  testthat::skip_on_cran()
  tables <- fx_mark_fixture(lapply(fx_magp_clean(), data.table::copy))
  data.table::set(tables$magp_plots, j = "magp_plot_id", value = NULL)
  data.table::set(tables$magp_sites, j = "jurisdiction", value = NULL)
  issues <- fx_caught(tables)$issues
  open <- issues[issues$status == "open"]
  expect_equal(sort(paste(open$rule_id, open$table_name, open$column_name)), sort(c(
    "column_missing magp_plots magp_plot_id", "column_missing magp_sites jurisdiction"
  )))
})
