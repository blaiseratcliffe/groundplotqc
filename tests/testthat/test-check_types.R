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

test_that("a key of only Unicode spaces is blank to Layer 4 (D18.15, m53)", {
  # A key of only Unicode spaces is blank, as D16.42 reads it, to Layer 1 too: in a
  # number-typed key stored as text coercion_loss leaves it to pk_missing, and in a text-typed
  # key fk_unassigned reports it (D18.62, D18.63).
  spec <- fx_fish_spec()
  attributes <- data.table::copy(spec$attributes)
  key <- which(attributes$attribute_name == "catch_id")
  data.table::set(attributes, i = key, j = "data_type", value = "whole")
  data.table::set(attributes, i = key, j = "r_class", value = "integer")
  spec$attributes <- attributes
  nbsp <- intToUtf8(0xA0)
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$catch_id <- c(nbsp, as.character(seq_len(nrow(catches))[-1L]))
  catches$haul_id[[2L]] <- nbsp
  fish$catches <- catches
  issues <- gpq_check_conformance(fish, spec, layers = 0:4)$issues
  open <- issues[issues$status == "open" & issues$rule_id != "class_mismatch"]
  expect_equal(
    paste(open$rule_id, open$table_name, open$column_name, open$row_number, open$value),
    paste(c("pk_missing catches catch_id 1", "fk_unassigned catches haul_id 2"), nbsp)
  )
  expect_false(any(issues$rule_id == "coercion_loss"))
})

test_that("a date-typed key of only Unicode spaces is blank to Layer 1 too (D18.62, D18.63)", {
  # date_unparseable leaves it to Layer 4, as coercion_loss does in a number-typed key: a
  # haul's key is pk_missing's and a catch's fk_unassigned's (D7.9, D18.15).
  dictionary <- utils::read.csv(
    example_file("fish_dictionary.csv"),
    colClasses = "character", na.strings = ""
  )
  dictionary$kind[dictionary$field == "haul_id"] <- "day"
  spec <- gpq_read_spec(
    dictionary = dictionary, code_lists = fx_fish_code_lists(),
    column_map = fx_fish_column_map(),
    type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "?", not_applicable = "~"),
      date = c(missing = "?", not_applicable = "~")
    )
  )
  nbsp <- intToUtf8(0xA0)
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  catches <- data.table::copy(fish$catches)
  dates <- format(as.Date("2020-01-01") + seq_len(nrow(hauls)) - 1L)
  catches$haul_id <- dates[match(catches$haul_id, hauls$haul_id)]
  hauls$haul_id <- dates
  hauls$haul_id[[1L]] <- nbsp
  catches$haul_id[[2L]] <- nbsp
  fish$hauls <- hauls
  fish$catches <- catches
  issues <- gpq_check_conformance(fish, spec, layers = 0:4)$issues
  open <- issues[issues$status == "open"]
  # Catch 1 names the haul whose key is now blank, so its key names no haul.
  expect_equal(paste(open$rule_id, open$table_name, open$row_number, open$value), c(
    paste("pk_missing hauls 1", nbsp), paste("fk_orphan catches 1", dates[[1L]]),
    paste("fk_unassigned catches 2", nbsp)
  ))
  expect_false(any(issues$rule_id == "date_unparseable"))
})

test_that("coercion_loss and date_unparseable are at rule_version 2 (D15.6, D18.62, D18.63)", {
  registry <- rule_registry()
  at <- match(c("coercion_loss", "date_unparseable"), registry$rule_id)
  expect_equal(registry$rule_version[at], c(2L, 2L))
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

test_that("missing_like() reads text marked as bytes as no blank, sentinel or code (D16.36)", {
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(rogue_codes = "UNK"), 1L)
  columns <- conformance_input(fx_fish_clean(), spec)$columns
  row <- as.list(columns[columns$attribute_name == "fish_count"])
  marked <- "é"
  Encoding(marked) <- "bytes"
  expect_identical(
    missing_like(c(marked, "?", "UNK", ""), row, context), c(FALSE, TRUE, TRUE, TRUE)
  )
})

# the finish's minors pass (m56)
test_that("missing_like() sends only the values that may be blank to space_padding() (16.2)", {
  # As key_rows() does: blank, of ASCII spaces only, or holding text that isn't plain ASCII.
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 1L)
  columns <- conformance_input(fx_fish_clean(), spec)$columns
  key <- modifyList(as.list(columns[columns$attribute_name == "fish_count"]), list(key_type = "FK"))
  nbsp <- intToUtf8(0xA0)
  text <- c("abc", "", "  ", nbsp, paste0("x", nbsp), "12", NA)
  real <- space_padding
  received <- character()
  testthat::local_mocked_bindings(space_padding = function(text) {
    received <<- c(received, text)
    real(text)
  })
  expect_equal(
    missing_like(text, key, context), c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(received, c("", "  ", nbsp, paste0("x", nbsp)))
})

test_that("an unknown code of the attribute is Layer 2's, in a number or date column (m25)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[[1L]] <- "UNK.W"
  fish$catches <- catches
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date[[1L]] <- "unknown"
  fish$hauls <- hauls
  settings <- list(unknown_codes = c(weight = "UNK.W", haul_date = "unknown"))
  open <- fish_open(fish, settings = settings)
  expect_equal(sort(paste(open$rule_id, open$column_name, open$value)), sort(c(
    "class_mismatch weight character", "species_unknown_code weight UNK.W",
    "species_unknown_code haul_date unknown"
  )))
})

test_that("a padded value whose trimmed form passes is whitespace_padding's alone (m25)", {
  testthat::skip_on_cran()
  # haul_date and fish_count are ID-marked here, so whitespace_padding checks them; weight and
  # first_visit aren't, and keep their padded values Layer 1's.
  spec <- fx_fish_spec()
  attributes <- data.table::copy(spec$attributes)
  marked <- which(attributes$attribute_name %in% c("haul_date", "fish_count"))
  data.table::set(attributes, i = marked, j = "id_marked", value = TRUE)
  spec$attributes <- attributes
  # A no-break space, which as.numeric() doesn't take for a space.
  nbsp <- intToUtf8(0xA0)
  fish <- fx_fish_clean()
  # A padded sentinel, " ?", trimmed is Layer 2's, so it too is whitespace_padding's (R73).
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c(" 2020-06-01", paste0("2020-06-02", nbsp), " 2020-02-30", " ?")
  fish$hauls <- hauls
  catches <- data.table::copy(fish$catches)
  catches$fish_count <- as.character(catches$fish_count)
  catches$fish_count[1:3] <- c(paste0("5", nbsp), " abc", " ?")
  catches$weight <- as.character(catches$weight)
  catches$weight[[1L]] <- paste0("5", nbsp)
  fish$catches <- catches
  stations <- data.table::copy(fish$stations)
  stations$first_visit[[1L]] <- paste0(" ", stations$first_visit[[1L]])
  fish$stations <- stations
  issues <- gpq_check_conformance(fish, spec, layers = 0:2)$issues
  open <- issues[issues$status == "open" & issues$rule_id != "class_mismatch"]
  expect_equal(
    sort(paste(open$rule_id, open$column_name, open$row_number, open$expected)),
    sort(c(
      "whitespace_padding haul_date 1 2020-06-01", "whitespace_padding haul_date 2 2020-06-02",
      "date_unparseable haul_date 3 %Y-%m-%d", "whitespace_padding haul_date 4 ?",
      "whitespace_padding fish_count 1 5", "coercion_loss fish_count 2 integer",
      "whitespace_padding fish_count 3 ?", "coercion_loss weight 1 double",
      "date_unparseable first_visit 1 %Y-%m-%d"
    ))
  )
  # A factor is read by its labels, as whitespace_padding reads it (R72).
  catches <- data.table::copy(fx_fish_clean()$catches)
  counts <- as.character(catches$fish_count)
  counts[[1L]] <- paste0("1", nbsp)
  catches$fish_count <- factor(counts)
  fish <- fx_fish_clean()
  fish$catches <- catches
  issues <- gpq_check_conformance(fish, spec, layers = 0:2)$issues
  open <- issues[issues$status == "open" & issues$rule_id != "class_mismatch"]
  expect_equal(
    paste(open$rule_id, open$column_name, open$row_number, open$expected),
    "whitespace_padding fish_count 1 1"
  )
})

test_that("hexadecimal text is lost, not a number (D16.42, m26)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[1:4] <- c("0x10", "0X1p3", "0x1p9999", "Infinity")
  fish$catches <- catches
  open <- fish_open(fish, layers = 1)
  open <- open[open$rule_id != "class_mismatch"]
  expect_equal(paste(open$rule_id, open$value), c(
    "coercion_loss 0x10", "coercion_loss 0X1p3", "coercion_loss 0x1p9999",
    "value_not_finite Infinity"
  ))
})

test_that("a value of only Unicode spaces outside the keys is blank_string's (m35, D8.8)", {
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  blanks <- c(intToUtf8(0xA0), intToUtf8(c(0x2003, 0x20, 0xA0)))
  hauls$haul_date[[1L]] <- blanks[[1L]]
  fish$hauls <- hauls
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[[2L]] <- blanks[[2L]]
  fish$catches <- catches
  open <- fish_open(fish)
  open <- open[open$rule_id != "class_mismatch"]
  expect_equal(
    paste(open$rule_id, open$column_name, open$row_number, open$value),
    paste(c("blank_string haul_date 1", "blank_string weight 2"), blanks)
  )
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

test_that("a number column stored as dates is lost as text; an infinite date is found at its row", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  # Repeated days, so each distinct day is written once and mapped back to its rows; the
  # infinite one sits at row 5.
  days <- c(18414, 18414, 18415, 18414, Inf, 18415, 18414, 18414)
  catches$weight <- structure(days, class = "Date")
  fish$catches <- catches
  open <- fish_open(fish, layers = 1)
  expect_setequal(placed(open), c(
    "class_mismatch catches weight", "coercion_loss catches weight",
    "value_not_finite catches weight"
  ))
  finite <- open[open$rule_id == "value_not_finite"]
  expect_equal(finite$row_number, 5L)
  expect_equal(finite$value, "Inf")
  # Every other day is lost as the text it is; the infinite one reads as a number.
  loss <- open[open$rule_id == "coercion_loss"]
  expect_equal(loss$row_number, c(1:4, 6:8))
  expect_equal(loss$value[c(1L, 3L)], c("2020-06-01", "2020-06-02"))
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

test_that("a date longer than its format can write is unparseable, never a stop (D16.34)", {
  # In another locale the weekday and month names below may be written in other bytes.
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  # strptime() stops on more than 1000 characters; "2020-06-011" is one over the 10 the
  # format writes.
  long <- strrep("2", 1001L)
  over <- "2020-06-011"
  hauls$haul_date <- c("2020-06-01", long, over, "2020-06-04")
  fish$hauls <- hauls
  expect_silent(open <- fish_open(fish, layers = 1))
  bad <- open[open$rule_id == "date_unparseable"]
  expect_equal(bad$value, c(long, over))
  expect_equal(bad$row_number, c(2L, 3L))
  # The bound is the longest date the format writes in this locale's month and weekday
  # names: the longest written date reads back, one character more does not.
  spec <- fx_fish_spec()
  layout <- "%A, %B %d, %Y"
  spec$type_map$date_format[spec$type_map$data_type == "day"] <- layout
  written <- format(as.Date("2021-01-01") + 0:365, layout)
  longest <- written[which.max(nchar(written, type = "bytes"))]
  hauls$haul_date <- c(longest, paste0(longest, "x"), written[[1L]], strrep(longest, 40L))
  stations <- data.table::copy(fish$stations)
  stations$first_visit <- written[1:2]
  fish$hauls <- hauls
  fish$stations <- stations
  issues <- gpq_check_conformance(fish, spec, layers = 1)$issues
  bad <- issues[issues$status == "open" & issues$rule_id == "date_unparseable"]
  expect_equal(bad$value, c(paste0(longest, "x"), strrep(longest, 40L)))
})

test_that("a format writing AM and PM is bounded by the longer of the two (D16.34)", {
  # Malay writes AM as "PG" and PM as "PTG": a bound from times before noon alone is a byte
  # short of a time after noon.
  old <- Sys.getlocale("LC_TIME")
  withr::defer(Sys.setlocale("LC_TIME", old))
  # The first of these names the system takes is set.
  malay <- Find(
    function(name) nzchar(suppressWarnings(Sys.setlocale("LC_TIME", name))),
    c("ms-MY", "ms_MY.UTF-8", "ms_MY.utf8")
  )
  testthat::skip_if(is.null(malay), "needs a Malay time locale")
  layout <- "%Y-%m-%d %I:%M %p"
  afternoon <- format(as.POSIXct("2021-03-01 13:30", tz = "UTC"), layout, tz = "UTC")
  testthat::skip_if_not(afternoon == "2021-03-01 01:30 PTG", "the locale writes PM otherwise")
  spec <- fx_fish_spec()
  spec$type_map$date_format[spec$type_map$data_type == "day"] <- layout
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c(
    "2021-03-01 01:30 PG", afternoon, "2021-03-01 11:59 PTG", paste0(afternoon, "x")
  )
  fish$hauls <- hauls
  stations <- data.table::copy(fish$stations)
  stations$first_visit <- c("2021-03-02 12:00 PG", "2021-03-02 12:00 PTG")
  fish$stations <- stations
  expect_silent(issues <- gpq_check_conformance(fish, spec, layers = 1)$issues)
  bad <- issues[issues$status == "open" & issues$rule_id == "date_unparseable"]
  expect_equal(bad$value, paste0(afternoon, "x"))
  expect_equal(bad$row_number, 4L)
})

test_that("latin1-marked text is read as UTF-8; text marked as bytes is unreadable (D16.36)", {
  # Each value below is checked by its bytes and how it is marked, in a UTF-8 locale.
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  layout <- "%d/%m/%Y à %Hh%M"
  written <- "01/06/2020 à 13h45"
  latin <- iconv(written, "UTF-8", "latin1")
  marked <- written
  Encoding(marked) <- "bytes"
  expect_identical(Encoding(c(latin, marked)), c("latin1", "bytes"))
  expect_true(validUTF8(marked))
  spec <- fx_fish_spec()
  spec$type_map$date_format[spec$type_map$data_type == "day"] <- layout
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c(latin, marked, written, "01/06/2020 à 25h61")
  fish$hauls <- hauls
  stations <- data.table::copy(fish$stations)
  stations$first_visit <- c(written, latin)
  fish$stations <- stations
  catches <- data.table::copy(fish$catches)
  weight <- as.character(catches$weight)
  weight[1:2] <- c(iconv("7é", "UTF-8", "latin1"), marked)
  catches$weight <- weight
  count <- as.character(catches$fish_count)
  count[[3L]] <- marked
  catches$fish_count <- count
  fish$catches <- catches
  # The run refuses text marked as bytes (D16.38), so the rules and the recording, its second
  # line, are run a step inside it.
  input <- conformance_input(fish, spec)
  context <- conformance_context(spec, NULL, list(), 1L)
  expect_silent(found <- check_types(input, context))
  state <- new_run_state()
  record_layer(found, 1L, input, context, state, "20261215T140312Z_3fa9c1")
  issues <- data.table::rbindlist(state$issues, use.names = TRUE)
  # Each value keeps its bytes, without the mark the results' writer stops on.
  expect_false("bytes" %in% Encoding(issues$value))
  expect_no_error(compiled_bytes(issues))
  open <- issues[issues$status == "open"]
  # The latin1 date reads back as written; the one marked as bytes is never read.
  bad <- open[open$rule_id == "date_unparseable"]
  expect_equal(paste(bad$table_name, bad$row_number), c("hauls 2", "hauls 4"))
  expect_identical(charToRaw(bad$value[[1L]]), charToRaw(marked))
  # Neither is a number: both are lost, as given.
  loss <- open[open$rule_id == "coercion_loss"]
  expect_equal(
    paste(loss$column_name, loss$row_number), c("fish_count 3", "weight 1", "weight 2")
  )
  expect_identical(lapply(loss$value, charToRaw), lapply(c(marked, weight[1:2]), charToRaw))
  # Without it, the run reads the latin1 date back and loses the latin1 number, and its
  # results are written.
  hauls$haul_date[[2L]] <- written
  fish$hauls <- hauls
  catches$weight[[2L]] <- "8"
  catches$fish_count[[3L]] <- "3"
  fish$catches <- catches
  expect_silent(results <- gpq_check_conformance(fish, spec, layers = 1))
  expect_no_error(gpq_write_results(results, withr::local_tempdir()))
  open <- results$issues[results$issues$status == "open"]
  expect_equal(open$row_number[open$rule_id == "date_unparseable"], 4L)
  loss <- open[open$rule_id == "coercion_loss"]
  expect_identical(charToRaw(loss$value), charToRaw(weight[[1L]]))
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

test_that("value_not_finite finds the same rows as the column's NaN and infinite values (D17.22)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches[, weight := as.character(weight)]
  catches[1:3, weight := c("1e999", "-Inf", "nan")]
  fish$catches <- catches
  results <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0:1)
  found <- results$issues[results$issues$rule_id == "value_not_finite", ]
  expect_identical(found$row_number, 1:3)
  expect_identical(found$value, c("1e999", "-Inf", "nan"))
  value <- coerce_column(catches$weight, "double")$value
  expect_identical(which(is.nan(value) | is.infinite(value)), 1:3)
})

test_that("a column of text is parsed once a run, for coercion_loss and value_not_finite (m19)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches[, weight := as.character(weight)]
  catches[1:3, weight := c("1e999", "-Inf", "nan")]
  fish$catches <- catches
  # coerce_column() is called for each text column of a number type by coercion_loss; count the
  # calls that read the weight column. value_not_finite takes its rows from the first call.
  real <- coerce_column
  parsed <- character()
  testthat::local_mocked_bindings(coerce_column = function(x, r_class, ...) {
    if (identical(x, catches$weight)) {
      parsed <<- c(parsed, r_class)
    }
    real(x, r_class, ...)
  })
  results <- gpq_check_conformance(fish, fx_fish_spec(), layers = 0:1)
  expect_identical(parsed, "double")
  found <- results$issues[results$issues$rule_id == "value_not_finite", ]
  expect_identical(found$row_number, 1:3)
})

test_that("cascade_masks() returns NULL where nothing of Layers 1 to 3 failed in the column", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches[, weight := as.character(weight)]
  catches$weight[[2L]] <- "abc"
  fish$catches <- catches
  spec <- fx_fish_spec()
  input <- conformance_input(fish, spec)
  context <- conformance_context(spec, NULL, list(), 1L)
  state <- new_run_state()
  row_group <- input$groups$catches$row_group
  # Nothing recorded yet.
  expect_null(cascade_masks(state, "catches", "weight", 2L, row_group))
  record_layer(
    check_types(input, context), 1L, input, context, state, "20261215T140312Z_3fa9c1"
  )
  # The value Layer 1 lost is masked for Layer 2, under the issue that lost it; a column where
  # nothing failed builds no table.
  masked <- cascade_masks(state, "catches", "weight", 2L, row_group)
  expect_identical(masked$row_number, 2L)
  expect_identical(masked$root_issue_id, state$failed[["catches\rweight"]][[1L]]$issue_id)
  expect_null(cascade_masks(state, "catches", "fish_count", 2L, row_group))
  expect_null(cascade_masks(state, "hauls", "gear", 2L, input$groups$hauls$row_group))
})
