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

test_that("an exact name wins over a near one, which is then an extra column", {
  fish <- fx_fish_clean()
  fish$hauls$GEAR <- "x"
  results <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0)
  open <- results$issues
  expect_equal(paste(open$rule_id, open$column_name), "column_unexpected GEAR")
})
