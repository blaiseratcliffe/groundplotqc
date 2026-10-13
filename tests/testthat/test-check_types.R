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
  expect_length(found$coercion_loss, 15L)
  expect_length(found$date_unparseable, 2L)
  expect_length(found$integer_stored_as_double, 4L)
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

test_that("a fraction or a number beyond an integer's range, stored as double, is lost too", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  count <- as.double(catches$fish_count)
  count[1:2] <- c(2.5, 3e9)
  catches$fish_count <- count
  fish$catches <- catches
  open <- fish_open(fish, layers = 1)
  expect_setequal(placed(open), c(
    "integer_stored_as_double catches fish_count", "coercion_loss catches fish_count"
  ))
  loss <- open[open$rule_id == "coercion_loss"]
  expect_equal(loss$value, c("2.5", "3000000000"))
  expect_equal(loss$expected, c("integer", "integer"))
})

test_that("a key's rogue code is coercion_loss's and its \"NA\" is Layer 2's (D8.8, D7.9)", {
  spec <- fx_fish_spec()
  attributes <- data.table::copy(spec$attributes)
  key <- which(attributes$attribute_name == "catch_id")
  data.table::set(attributes, i = key, j = "data_type", value = "whole")
  data.table::set(attributes, i = key, j = "r_class", value = "integer")
  spec$attributes <- attributes
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  ids <- as.character(seq_len(nrow(catches)))
  ids[1:3] <- c("NA", "UNK", "abc")
  catches$catch_id <- ids
  fish$catches <- catches
  issues <- gpq_check_conformance(fish, spec, layers = 1)$issues
  loss <- issues[issues$status == "open" & issues$rule_id == "coercion_loss"]
  expect_equal(loss$value, c("UNK", "abc"))
})

test_that("an ID column's \"NA\" is Layer 2's and a plain column's is coercion_loss's (D8.8)", {
  spec <- fx_fish_spec()
  attributes <- data.table::copy(spec$attributes)
  data.table::set(
    attributes,
    i = which(attributes$attribute_name == "fish_count"), j = "id_marked", value = TRUE
  )
  spec$attributes <- attributes
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  count <- as.character(catches$fish_count)
  count[1:2] <- c("NA", "UNK")
  catches$fish_count <- count
  fish$catches <- catches
  hauls <- data.table::copy(fish$hauls)
  minutes <- as.character(hauls$minutes)
  minutes[[1L]] <- "NA"
  hauls$minutes <- minutes
  fish$hauls <- hauls
  # With "NA" no rogue code, only an ID's "NA" is left to Layer 2; "UNK" is one.
  issues <- gpq_check_conformance(
    fish, spec,
    layers = 1, settings = list(rogue_codes = "UNK")
  )$issues
  loss <- issues[issues$status == "open" & issues$rule_id == "coercion_loss"]
  expect_equal(paste(loss$column_name, loss$value), "minutes NA")
})

test_that("missing_like() leaves a key, an ID and a plain column's values as D8.8 says", {
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 1L)
  columns <- conformance_input(fx_fish_clean(), spec)$columns
  row <- as.list(columns[columns$attribute_name == "fish_count"])
  text <- c("UNK", "NA", "?", "", "-88", "abc", NA)
  # A plain column: a rogue code, "NA" (one), another type's sentinel and a blank are Layer 2's;
  # the column's own sentinel, other text and NA are not.
  expect_equal(
    missing_like(text, row, context), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  # A key column takes no rogue code, and its "NA" is na_text_in_id's.
  key <- modifyList(row, list(key_type = "FK"))
  expect_equal(
    missing_like(text, key, context), c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  # An ID column's "NA" is na_text_in_id's, even where "NA" is no rogue code.
  id <- modifyList(row, list(id_marked = TRUE))
  context <- conformance_context(spec, NULL, list(rogue_codes = c("-999", "UNK")), 1L)
  expect_equal(missing_like(text, row, context)[1:2], c(TRUE, FALSE))
  expect_equal(missing_like(text, id, context)[1:2], c(TRUE, TRUE))
  # A type with no sentinels of its own leaves another type's to Layer 1 (D16.32): "?" is a
  # character sentinel, and the numeric type has none left.
  bare <- spec
  bare$sentinels <- bare$sentinels[bare$sentinels$data_type == "character"]
  context <- conformance_context(bare, NULL, list(), 1L)
  expect_false(missing_like(text, row, context)[[3L]])
})

test_that("a factor, a Date and an all-NA logical column read as their class says (D2.33)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  weight <- as.character(catches$weight)
  weight[[1L]] <- "abc"
  catches$weight <- factor(weight)
  catches$fish_count <- rep(NA, nrow(catches))
  fish$catches <- catches
  stations <- data.table::copy(fish$stations)
  stations$first_visit <- as.Date(stations$first_visit)
  stations$first_visit[[1L]] <- NA
  fish$stations <- stations
  before <- lapply(fish, data.table::copy)
  open <- fish_open(fish, layers = 1)
  expect_setequal(placed(open), c(
    "class_mismatch catches weight", "coercion_loss catches weight",
    "class_mismatch catches fish_count", "class_mismatch stations first_visit"
  ))
  mismatch <- open[open$rule_id == "class_mismatch"]
  expect_setequal(
    paste(mismatch$column_name, mismatch$value),
    c("first_visit Date", "weight factor", "fish_count logical")
  )
  # The factor's label is read, not its code.
  expect_equal(open$value[open$rule_id == "coercion_loss"], "abc")
  for (table in names(fish)) {
    expect_identical(fish[[table]], before[[table]], info = table)
  }
})

test_that("date_unparseable reads each date back as written; sentinels and Layer 2's values pass", {
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c("2020-02-30", "2020-2-3", "?", "")
  fish$hauls <- hauls
  # "-88" is the numeric type's sentinel and "UNK" a rogue code: Layer 2 reports them.
  stations <- data.table::copy(fish$stations)
  stations$first_visit <- c("-88", "UNK")
  fish$stations <- stations
  open <- fish_open(fish, layers = 1)
  bad <- open[open$rule_id == "date_unparseable"]
  expect_equal(bad$value, c("2020-02-30", "2020-2-3"))
  expect_equal(bad$expected, c("%Y-%m-%d", "%Y-%m-%d"))
})

test_that("date_unparseable reads a date format that holds a time back as written", {
  spec <- fx_fish_spec()
  spec$type_map$date_format[spec$type_map$data_type == "day"] <- "%Y-%m-%d %H:%M"
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c("2020-01-01 13:45", "2020-01-02 00:00", "2020-01-03 25:61", "2020-01-04")
  fish$hauls <- hauls
  stations <- data.table::copy(fish$stations)
  stations$first_visit <- c("2020-06-01 08:30", "2020-06-02 23:59")
  fish$stations <- stations
  issues <- gpq_check_conformance(fish, spec, layers = 1)$issues
  bad <- issues[issues$status == "open" & issues$rule_id == "date_unparseable"]
  expect_equal(bad$value, c("2020-01-03 25:61", "2020-01-04"))
  expect_equal(bad$expected, rep("%Y-%m-%d %H:%M", 2L))
})

test_that("another type's sentinel is Layer 2's only where the column's type has some (D16.32)", {
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c("2020-02-30", "-88", "?", "")
  fish$hauls <- hauls
  catches <- data.table::copy(fish$catches)
  weight <- as.character(catches$weight)
  weight[1:2] <- c("?", "abc")
  catches$weight <- weight
  fish$catches <- catches
  reported <- function(spec) {
    issues <- gpq_check_conformance(fish, spec, layers = 1)$issues
    issues[issues$status == "open" & issues$rule_id %in% c("coercion_loss", "date_unparseable")]
  }
  # Both types have sentinels: "-88" and "?" are left to sentinel_wrong_type, "?" is the date
  # type's own and the blank is blank_string's.
  full <- reported(fx_fish_spec())
  expect_setequal(
    paste(full$rule_id, full$value), c("date_unparseable 2020-02-30", "coercion_loss abc")
  )
  # Only the character type keeps its sentinels. A type with none of its own leaves another
  # type's to Layer 1, which reports "?" in the date and the number column (D16.32); "-88" is
  # no one's sentinel now.
  bare <- fx_fish_spec()
  bare$sentinels <- bare$sentinels[bare$sentinels$data_type == "character"]
  found <- reported(bare)
  expect_setequal(
    paste(found$rule_id, found$value),
    c(
      "date_unparseable 2020-02-30", "date_unparseable -88", "date_unparseable ?",
      "coercion_loss ?", "coercion_loss abc"
    )
  )
})

test_that("value_not_finite finds NaN and Inf, stored or written as text", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  # An NA beside the NaN is raw_na's, not a number that isn't finite.
  catches$weight[1:3] <- c(Inf, NaN, NA)
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

test_that("a number column stored as dates is lost as text and holds no NaN or Inf (16.2)", {
  fish <- fx_fish_clean()
  stations <- data.table::copy(fish$stations)
  stations$depth <- data.table::as.IDate(c("2020-06-01", "2020-06-01"))
  fish$stations <- stations
  open <- fish_open(fish, layers = 1)
  expect_setequal(placed(open), c("class_mismatch stations depth", "coercion_loss stations depth"))
  expect_equal(open$value[open$rule_id == "coercion_loss"], rep("2020-06-01", 2L))
})

test_that("a value that isn't valid UTF-8 is reported as given, never a stop or a warning", {
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale, where R stops on it")
  invalid <- rawToChar(as.raw(c(0x32, 0x30, 0x97)))
  expect_false(validUTF8(invalid))
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c("2020-06-01", invalid, "2020-06-03", "2020-06-04")
  fish$hauls <- hauls
  catches <- data.table::copy(fish$catches)
  weight <- as.character(catches$weight)
  weight[[1L]] <- invalid
  catches$weight <- weight
  count <- as.character(catches$fish_count)
  count[[2L]] <- invalid
  catches$fish_count <- count
  fish$catches <- catches
  expect_silent(open <- fish_open(fish, layers = 1))
  expect_setequal(placed(open), c(
    "date_unparseable hauls haul_date", "class_mismatch catches weight",
    "coercion_loss catches weight", "class_mismatch catches fish_count",
    "coercion_loss catches fish_count"
  ))
  for (rule in c("date_unparseable", "coercion_loss")) {
    found <- open[open$rule_id == rule]
    expect_identical(charToRaw(found$value[[1L]]), charToRaw(invalid), info = rule)
  }
  expect_equal(open$row_number[open$rule_id == "date_unparseable"], 2L)
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
