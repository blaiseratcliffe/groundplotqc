# Tests for Layer 1, data types (plan 6.1, 16.2; D2.33, D8.8, D16.8, D16.9).

fish_open <- function(tables, settings = list(), layers = 0:2) {
  results <- gpq_check_conformance(tables, fx_fish_spec(), layers = layers, settings = settings)
  issues <- results$issues
  issues[issues$status == "open"]
}

placed <- function(issues) paste(issues$rule_id, issues$table_name, issues$column_name)

test_that("check_types returns an outcome list for each Layer 1 rule", {
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  found <- check_types(conformance_input(fish, spec), conformance_context(spec, NULL, list(), 1L))
  expect_named(found, c(
    "class_mismatch", "coercion_loss", "date_unparseable", "integer_stored_as_double",
    "value_not_finite"
  ))
  # class_mismatch and coercion_loss check every attribute, the others their types'.
  expect_length(found$class_mismatch, 15L)
  expect_length(found$date_unparseable, 2L)
  expect_length(found$value_not_finite, 4L)
})

test_that("a clean fish run passes Layer 1", {
  expect_equal(nrow(fish_open(fx_fish_clean(), layers = 1)), 0L)
})

test_that("class_mismatch reports a column's class once; integer and double swap as a warning", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$fish_count <- as.double(catches$fish_count)
  fish$catches <- catches
  stations <- data.table::copy(fish$stations)
  stations$first_visit <- data.table::as.IDate(stations$first_visit)
  stations$depth <- as.integer(round(stations$depth))
  fish$stations <- stations
  open <- fish_open(fish, layers = 1)
  expect_setequal(placed(open), c(
    "class_mismatch catches weight", "class_mismatch stations first_visit",
    "integer_stored_as_double catches fish_count", "integer_stored_as_double stations depth"
  ))
  expect_true(all(open$level == "column" & open$record_pk == "*"))
  mismatch <- open[open$rule_id == "class_mismatch"]
  expect_equal(mismatch$value, c("IDate", "character"))
  expect_equal(mismatch$expected, c("character", "double"))
  expect_equal(open$severity[open$rule_id == "integer_stored_as_double"], c("warning", "warning"))
  expect_equal(mismatch$n_records, c(nrow(stations), nrow(catches)))
})

test_that("coercion_loss names the values the coercion loses, Layer 2's left alone (D8.8)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  weight <- as.character(catches$weight)
  # "?" is a character sentinel, "" a blank and "UNK" a rogue code: Layer 2 reports them.
  weight[1:5] <- c("abc", "?", "", "UNK", "-88")
  catches$weight <- weight
  count <- as.character(catches$fish_count)
  count[[1L]] <- "2.5"
  catches$fish_count <- count
  fish$catches <- catches
  open <- fish_open(fish, layers = 1)
  loss <- open[open$rule_id == "coercion_loss"]
  expect_equal(loss$column_name, c("fish_count", "weight"))
  expect_equal(loss$value, c("2.5", "abc"))
  expect_equal(loss$expected, c("integer", "double"))
  expect_equal(loss$row_number, c(1L, 1L))
  expect_equal(loss$record_pk, rep(catches$catch_id[[1L]], 2L))
})

test_that("date_unparseable reads each date back as written; sentinels and Layer 2's values pass", {
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c("2020-02-30", "2020-2-3", "?", "")
  fish$hauls <- hauls
  open <- fish_open(fish, layers = 1)
  bad <- open[open$rule_id == "date_unparseable"]
  expect_equal(bad$value, c("2020-02-30", "2020-2-3"))
  expect_equal(bad$expected, c("%Y-%m-%d", "%Y-%m-%d"))
})

test_that("value_not_finite finds NaN and Inf, stored or written as text", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight[1:2] <- c(Inf, NaN)
  fish$catches <- catches
  stations <- data.table::copy(fish$stations)
  stations$depth <- as.character(stations$depth)
  stations$depth[[1L]] <- "-Inf"
  fish$stations <- stations
  open <- fish_open(fish, layers = 1)
  finite <- open[open$rule_id == "value_not_finite"]
  expect_equal(
    paste(finite$table_name, finite$value), c("stations -Inf", "catches Inf", "catches NaN")
  )
  expect_false("coercion_loss" %in% open$rule_id)
})

test_that("a Layer 1 rule failing every record of a column collapses (6.3)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight <- rep("abc", nrow(catches))
  fish$catches <- catches
  open <- fish_open(fish, settings = list(column_level_min_n = 3L), layers = 1)
  loss <- open[open$rule_id == "coercion_loss"]
  expect_equal(loss$level, "column")
  expect_equal(loss$n_records, nrow(catches))
})

test_that("Layer 1's checks change none of the tables (16.2)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  fish$catches <- catches
  before <- lapply(fish, data.table::copy)
  fish_open(fish, layers = 1)
  for (table in names(fish)) {
    expect_identical(fish[[table]], before[[table]], info = table)
  }
})
