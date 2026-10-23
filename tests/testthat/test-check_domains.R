# Tests for Layer 3, value domains (plan 6.1, 6.2; D8.24, D9.26, D18.2, D18.9, D18.10,
# D18.19 to D18.22): a value against its code list, the data dictionary's range and format
# columns, its contributor's site-ID band, and a text column that holds a few values without a
# code list.
#
# Task cuts (M5): task 4 writes the file with the four helpers and the tests marked "task 4",
# its first test naming code_not_in_list only; task 5 adds the tests marked "task 5" and the
# six DD-column rules to the first test; task 6 those marked "task 6" and
# site_id_out_of_range; task 7 those marked "task 7" and undeclared_code_set. Each cut keeps
# the tests in this order.

# The helpers of test-check_sentinels.R, with the same meanings (R25), the run's layers
# reaching Layer 3.
fish_run <- function(tables, settings = list(), layers = 0:3, rules = NULL, spec = fx_fish_spec()) {
  gpq_check_conformance(tables, spec, layers = layers, settings = settings, rules = rules)
}

opened <- function(results, rule_id = NULL) {
  issues <- results$issues
  issues <- issues[issues$status == "open"]
  if (is.null(rule_id)) {
    return(issues)
  }
  # Worked out before the brackets: rule_id is also a column of the issues.
  wanted <- issues$rule_id == rule_id
  issues[wanted]
}

placed_at <- function(issues) paste(issues$table_name, issues$column_name, issues$row_number)

edited <- function(table, column, rows, value, fish = fx_fish_clean()) {
  copy <- data.table::copy(fish[[table]])
  data.table::set(copy, i = rows, j = column, value = value)
  fish[[table]] <- copy
  fish
}

# ---- task 4 (the names grow with tasks 5 to 7) ----

test_that("check_domains returns an outcome list for each Layer 3 rule", {
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 3L)
  found <- check_domains(conformance_input(fish, spec), context)
  expect_named(found, c(
    "code_not_in_list", "value_below_min", "value_above_max", "string_too_long",
    "pattern_mismatch", "decimals_exceeded", "site_id_out_of_range", "coordinate_out_of_bounds",
    "undeclared_code_set"
  ))
  # The three coded attributes.
  expect_length(found$code_not_in_list, 3L)
  # depth, minutes, fish_count and weight have both bounds; station_name a length and a
  # pattern; depth and weight decimal places.
  expect_length(found$value_below_min, 4L)
  expect_length(found$value_above_max, 4L)
  expect_length(found$string_too_long, 1L)
  expect_length(found$pattern_mismatch, 1L)
  expect_length(found$decimals_exceeded, 2L)
  # The fish spec has no bands and the run no coordinate attributes: one not_run row each.
  expect_equal(found$site_id_out_of_range[[1L]]$reason, "no_input")
  expect_equal(found$coordinate_out_of_bounds[[1L]]$reason, "no_input")
  # No text column holds more values than code_set_max_values, so none is judged (R21).
  expect_length(found$undeclared_code_set, 0L)
})

# ---- task 4 ----

test_that("code_not_in_list finds a code its list lacks, exactly, the list's source expected", {
  spec <- fx_fish_spec()
  fish <- edited("hauls", "gear", 2:3, c("ZZ", "gn"))
  found <- opened(fish_run(fish), "code_not_in_list")
  # A code is matched exactly, so "gn" isn't "GN" (D18.10 (5)).
  expect_equal(placed_at(found), c("hauls gear 2", "hauls gear 3"))
  expect_equal(found$value, c("ZZ", "gn"))
  map <- spec$code_list_map
  expect_equal(found$expected, rep(map$source_name[map$attribute_name == "gear"], 2L))
  expect_equal(nrow(opened(fish_run(fish))), 2L)
  # A padded code is Layer 2's (whitespace_padding), so Layer 3 doesn't evaluate it (D2.10).
  padded <- fish_run(edited("hauls", "gear", 1L, " GN"))
  expect_equal(opened(padded)$rule_id, "whitespace_padding")
  masked <- padded$issues[padded$issues$rule_id == "code_not_in_list"]
  expect_equal(masked$status, "not_evaluated")
  expect_equal(masked$root_issue_id, opened(padded)$issue_id)
})

test_that("code_not_in_list leaves NA, blanks, sentinels and unknown codes to Layer 2 (D18.9)", {
  nbsp <- intToUtf8(160L)
  fish <- edited("hauls", "gear", 1:4, c(NA, "", nbsp, "?"))
  fish <- edited("catches", "species", 1L, "UNK", fish)
  # Layer 3 alone, so no earlier layer masks them: the rule itself leaves them alone (R12),
  # and counts them as checked and passing (R20).
  results <- fish_run(fish, settings = list(unknown_codes = c(species = "UNK")), layers = 3)
  expect_equal(nrow(opened(results)), 0L)
  rows <- results$results[results$results$rule_id == "code_not_in_list"]
  expect_equal(rows$n_checked, c(2L, 4L, 8L))
  expect_true(all(rows$status == "pass"))
})

test_that("a coded number column compares numbers as numbers, text as text (R7, R29)", {
  testthat::skip_on_cran()
  # The fish spec with minutes and the stations' key coded: minutes from a list holding
  # "04", station_id from the stations' own keys.
  dictionary <- utils::read.csv(
    example_file("fish_dictionary.csv"),
    colClasses = "character", na.strings = ""
  )
  coded <- (dictionary$table == "hauls" & dictionary$field == "minutes") |
    (dictionary$table == "stations" & dictionary$field == "station_id")
  dictionary$codes[coded] <- "y"
  spec <- gpq_read_spec(
    dictionary = dictionary,
    code_lists = c(fx_fish_code_lists(), list(
      minutes = data.frame(minutes = c("1", "2", "3", "04")),
      station_id = data.frame(station_id = c("stations_1", "stations_2"))
    )),
    column_map = fx_fish_column_map(),
    type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "?", not_applicable = "~"),
      date = c(missing = "?", not_applicable = "~")
    )
  )
  map <- spec$code_list_map
  expect_setequal(paste(map$table_name, map$attribute_name), c(
    "stations water_body", "hauls gear", "catches species", "hauls minutes",
    "stations station_id"
  ))
  run <- function(fish) opened(fish_run(fish, spec = spec), "code_not_in_list")
  # Stored as numbers: 4 is the code "04"; 5 isn't a code; -99 is a sentinel.
  numbers <- run(edited("hauls", "minutes", 2:3, c(5L, -99L)))
  expect_equal(placed_at(numbers), "hauls minutes 2")
  expect_equal(numbers$value, "5")
  # As text, as a CSV gives it (the whole column replaced): compared as text, so "4" isn't
  # "04" (R7), but "-99.0" is the sentinel -99 (R29).
  text <- run(edited("hauls", "minutes", NULL, c("1", "5", "-99.0", "4")))
  expect_equal(placed_at(text), c("hauls minutes 2", "hauls minutes 4"))
  # A value that failed Layer 1 isn't evaluated: "x" isn't a number (coercion_loss).
  failed <- fish_run(edited("hauls", "minutes", NULL, c("1", "x", "3", "2")), spec = spec)
  coerced <- opened(failed, "coercion_loss")
  masked <- failed$issues[failed$issues$rule_id == "code_not_in_list"]
  expect_equal(placed_at(masked), "hauls minutes 2")
  expect_equal(masked$status, "not_evaluated")
  expect_equal(masked$root_issue_id, coerced$issue_id)
  # A key is checked too (D18.10).
  key <- run(edited("stations", "station_id", 2L, "stations_9"))
  expect_equal(placed_at(key), "stations station_id 2")
  # A factor is compared by its text; so is integer64 (R7).
  fish <- fx_fish_clean()
  fish$hauls <- data.table::copy(fish$hauls)
  data.table::set(fish$hauls, j = "gear", value = factor(c("GN", "ZZ", "EF", "GN")))
  expect_equal(placed_at(run(fish)), "hauls gear 2")
  testthat::skip_if_not_installed("bit64")
  data.table::set(fish$hauls, j = "gear", value = c("GN", "TN", "EF", "GN"))
  data.table::set(fish$hauls, j = "minutes", value = bit64::as.integer64(c(1, 5, 3, 2)))
  wide <- run(fish)
  expect_equal(placed_at(wide), "hauls minutes 2")
  expect_equal(wide$value, "5")
})

test_that("code_not_in_list reads translation tables; UNKN.SPP is Layer 2's (D7.28, D18.9)", {
  # A Layer 3 run on MAGPlot's clean base, its unknown code from MAGPlot's rule set.
  testthat::skip_on_cran()
  tables <- fx_magp_clean()
  meas <- data.table::copy(tables$magp_tree_meas)
  data.table::set(meas, i = 1:2, j = "species", value = c("UNKN.SPP", "QQQ"))
  tables$magp_tree_meas <- meas
  results <- withCallingHandlers(
    gpq_check_conformance(tables, magp_spec(), rules = magp_rules(), layers = 3),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  found <- opened(results)
  expect_equal(paste(found$rule_id, placed_at(found)), "code_not_in_list magp_tree_meas species 2")
  map <- magp_spec()$code_list_map
  expect_equal(
    found$expected,
    map$source_name[map$table_name == "magp_tree_meas" & map$attribute_name == "species"]
  )
})

# task 4's fix round (the datatable review's Minor, 98c095c)
test_that("only the values that may be blank reach space_padding() (16.2)", {
  # domain_skipped() reads the values outside the list, and of those only one that is blank or
  # holds text that isn't plain ASCII goes to space_padding(): here the no-break space, not
  # "ZZ" and never the whole column.
  nbsp <- intToUtf8(160L)
  fish <- edited("hauls", "gear", 1:2, c(nbsp, "ZZ"))
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 3L)
  input <- conformance_input(fish, spec)
  real <- space_padding
  received <- character()
  testthat::local_mocked_bindings(space_padding = function(text) {
    received <<- c(received, text)
    real(text)
  })
  found <- check_domains(input, context)
  expect_identical(received, nbsp)
  # The no-break space is a blank, which the rule leaves to Layer 2; "ZZ" fails.
  gear <- Filter(function(outcome) outcome$column_name == "gear", found$code_not_in_list)
  expect_identical(gear[[1L]]$fail, 2L)
})

# ---- task 5 ----

test_that("value_below_min and value_above_max read the bounds and their flags (D18.9)", {
  fish <- edited("stations", "depth", 1:2, c(-1, 500))
  fish <- edited("hauls", "minutes", 1L, 0L, fish)
  fish <- edited("catches", "weight", 1:2, c(0, 1000), fish)
  fish <- edited("catches", "fish_count", 1L, 10001L, fish)
  results <- fish_run(fish)
  below <- opened(results, "value_below_min")
  # weight's minimum isn't allowed itself (FALSE), depth's maximum and weight's are.
  expect_equal(
    placed_at(below), c("stations depth 1", "hauls minutes 1", "catches weight 1")
  )
  expect_equal(below$value, c("-1", "0", "0"))
  expect_equal(below$expected, c("0", "1", "0"))
  above <- opened(results, "value_above_max")
  expect_equal(placed_at(above), "catches fish_count 1")
  expect_equal(c(above$value, above$expected), c("10001", "10000"))
  expect_equal(nrow(opened(results)), 4L)
})

test_that("the range rules leave sentinels and unknown codes alone, as numbers (D18.9, R29)", {
  fish <- edited("stations", "depth", 1:2, c(-99, -5))
  results <- fish_run(fish, settings = list(unknown_codes = c(depth = "-5")), layers = 3)
  expect_equal(nrow(opened(results)), 0L)
  rows <- results$results
  rows <- rows[rows$rule_id == "value_below_min" & rows$column_name == "depth"]
  expect_equal(c(rows$n_checked, rows$n_fail), c(2L, 0L))
  # Read from text (the whole column replaced), "-99.0" is the sentinel -99.
  text <- edited("stations", "depth", NULL, c("-99.0", "2.5"))
  expect_equal(nrow(opened(fish_run(text, layers = 3))), 0L)
})

test_that("string_too_long counts characters; pattern_mismatch reads the DD's pattern (R3)", {
  long <- edited("stations", "station_name", 1:2, c(strrep("a", 41L), "t-2"))
  results <- fish_run(long)
  too_long <- opened(results, "string_too_long")
  expect_equal(placed_at(too_long), "stations station_name 1")
  expect_equal(too_long$expected, "40")
  mismatch <- opened(results, "pattern_mismatch")
  expect_equal(placed_at(mismatch), "stations station_name 2")
  expect_equal(c(mismatch$value, mismatch$expected), c("t-2", "^[A-Za-z0-9 ]+$"))
  expect_equal(nrow(opened(results)), 2L)
  # Forty two-byte letters are forty characters: not too long, but not the pattern's.
  accented <- strrep(intToUtf8(233L), 40L)
  wide <- fish_run(edited("stations", "station_name", 1L, accented))
  expect_equal(opened(wide)$rule_id, "pattern_mismatch")
  # Sentinels and blanks are Layer 2's (D18.9, R12).
  left <- edited("stations", "station_name", 1:2, c("?", ""))
  expect_equal(nrow(opened(fish_run(left, layers = 3))), 0L)
})

test_that("a date stored as a date is read in its type's format (R28)", {
  dictionary <- utils::read.csv(
    example_file("fish_dictionary.csv"),
    colClasses = "character", na.strings = ""
  )
  dictionary$width[dictionary$field == "first_visit"] <- "8"
  dictionary$format[dictionary$field == "first_visit"] <- "^2020-"
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
  fish <- fx_fish_clean()
  fish$stations <- data.table::copy(fish$stations)
  data.table::set(fish$stations, j = "first_visit", value = as.Date(fish$stations$first_visit))
  data.table::set(fish$stations, i = 2L, j = "first_visit", value = as.Date("2021-01-31"))
  results <- fish_run(fish, layers = 3, spec = spec)
  # "2020-06-01" has ten characters, as the type's format writes it; 2021 isn't the pattern's.
  expect_equal(placed_at(opened(results, "string_too_long")), paste("stations first_visit", 1:2))
  expect_equal(placed_at(opened(results, "pattern_mismatch")), "stations first_visit 2")
  expect_equal(opened(results, "pattern_mismatch")$value, "2021-01-31")
})

test_that("decimals_exceeded counts places as written to 15 digits (R22)", {
  fish <- edited("stations", "depth", 1L, 1.25)
  fish <- edited("catches", "weight", 1:2, c(1.0005, 1.5), fish)
  found <- opened(fish_run(fish), "decimals_exceeded")
  expect_equal(placed_at(found), c("stations depth 1", "catches weight 1"))
  expect_equal(found$value, c("1.25", "1.0005"))
  expect_equal(found$expected, c("1", "3"))
  # "1.50" read from text is 1.5, one place.
  text <- edited("stations", "depth", NULL, c("1.50", "2.5"))
  expect_equal(nrow(opened(fish_run(text, layers = 3), "decimals_exceeded")), 0L)
})

test_that("coordinate_out_of_bounds takes the setting's attributes from the range rules", {
  testthat::skip_on_cran()
  fish <- edited("stations", "depth", 1:2, c(600, -1))
  results <- fish_run(fish, settings = list(coordinate_attributes = "depth"))
  found <- opened(results)
  expect_equal(unique(found$rule_id), "coordinate_out_of_bounds")
  expect_equal(placed_at(found), c("stations depth 1", "stations depth 2"))
  expect_equal(found$expected, c("500", "0"))
  # The range rules leave a coordinate attribute to it (D18.9).
  rows <- results$results
  ranged <- rows$rule_id %chin% c("value_below_min", "value_above_max")
  expect_false("depth" %in% rows$column_name[ranged])
  # Without the setting it doesn't run: one row, no_input (R19).
  plain <- fish_run(fish)$results
  row <- plain[plain$rule_id == "coordinate_out_of_bounds"]
  expect_equal(
    c(row$table_name, row$column_name, row$status, row$not_run_reason),
    c("*", "*", "not_run", "no_input")
  )
  expect_equal(placed_at(opened(fish_run(fish), "value_above_max")), "stations depth 1")
})

test_that("a dictionary column no attribute gives a value is one not_run row (D18.19, R19)", {
  # MAGPlot's DD has max_length and decimals blank, and no bound on a coordinate.
  testthat::skip_on_cran()
  results <- withCallingHandlers(
    gpq_check_conformance(fx_magp_clean(), magp_spec(), rules = magp_rules(), layers = 3),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  expect_equal(nrow(opened(results)), 0L)
  expect_equal(nrow(results$metadata$self_test), 0L)
  rows <- results$results
  for (rule in c("string_too_long", "decimals_exceeded", "coordinate_out_of_bounds")) {
    row <- rows[rows$rule_id == rule]
    expect_equal(
      c(row$table_name, row$column_name, row$status, row$not_run_reason),
      c("*", "*", "not_run", "no_dd_values"),
      info = rule
    )
  }
  # The eight ranges and four patterns are checked.
  checked <- rows[rows$rule_id %chin% c("value_below_min", "value_above_max", "pattern_mismatch")]
  expect_equal(
    as.vector(table(checked$rule_id)[c("value_below_min", "value_above_max", "pattern_mismatch")]),
    c(8L, 8L, 4L)
  )
  expect_true(all(checked$status == "pass"))
})

# task 5's fix round (the datatable review's I1, b30f767)
test_that("a value on two equal excluded coordinate bounds fails once (D18.9)", {
  # depth's minimum and maximum both 5 and both excluded, which pre-flight accepts.
  dictionary <- utils::read.csv(
    example_file("fish_dictionary.csv"),
    colClasses = "character", na.strings = ""
  )
  depth <- dictionary$field == "depth"
  dictionary[depth, c("low", "high", "low_in", "high_in")] <- list("5", "5", "FALSE", "FALSE")
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
  fish <- edited("stations", "depth", 1:2, c(5, 6))
  results <- fish_run(
    fish,
    settings = list(coordinate_attributes = "depth"), layers = 3, spec = spec
  )
  found <- opened(results, "coordinate_out_of_bounds")
  # 5 is beyond both bounds, and is one failure.
  expect_equal(placed_at(found), c("stations depth 1", "stations depth 2"))
  expect_equal(found$value, c("5", "6"))
  expect_equal(found$expected, c("5", "5"))
})

# task 5's fix round (the spec review's finding 2, b30f767)
test_that("coordinate_out_of_bounds reads a one-sided bound (D18.9)", {
  # depth with a maximum of 500 and no minimum.
  dictionary <- utils::read.csv(
    example_file("fish_dictionary.csv"),
    colClasses = "character", na.strings = ""
  )
  dictionary$low[dictionary$field == "depth"] <- NA_character_
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
  fish <- edited("stations", "depth", 1:2, c(-1, 600))
  results <- fish_run(
    fish,
    settings = list(coordinate_attributes = "depth"), layers = 3, spec = spec
  )
  found <- opened(results)
  expect_equal(paste(found$rule_id, placed_at(found)), "coordinate_out_of_bounds stations depth 2")
  expect_equal(c(found$value, found$expected), c("600", "500"))
})

# task 5's fix round (the spec review's finding 2 and the datatable review's M3, b30f767), one
# test in four since the finish's minors pass (m31)
test_that("coordinate_out_of_bounds passes a coordinate within its bounds (R19)", {
  # One row, every station checked.
  fish <- fx_fish_clean()
  rows <- fish_run(fish, settings = list(coordinate_attributes = "depth"))$results
  row <- rows[rows$rule_id == "coordinate_out_of_bounds"]
  expect_equal(c(row$table_name, row$column_name, row$status), c("stations", "depth", "pass"))
  expect_equal(c(row$n_checked, row$n_fail), c(nrow(fish$stations), 0L))
})

test_that("coordinate_out_of_bounds excludes a bound whose flag says so (D18.9)", {
  # weight's minimum 0 is excluded, its maximum 1000 allowed.
  fish <- edited("catches", "weight", 1:2, c(0, 1000))
  found <- opened(fish_run(fish, settings = list(coordinate_attributes = "weight")))
  expect_equal(paste(found$rule_id, placed_at(found)), "coordinate_out_of_bounds catches weight 1")
  expect_equal(c(found$value, found$expected), c("0", "0"))
})

test_that("coordinate_out_of_bounds leaves a sentinel and an unknown code to Layer 2 (R12, R20)", {
  # Both are checked and pass, the stations' depths being the two.
  fish <- edited("stations", "depth", NULL, c(-99, -5))
  results <- fish_run(
    fish,
    settings = list(coordinate_attributes = "depth", unknown_codes = c(depth = "-5")),
    layers = 3
  )
  expect_equal(nrow(opened(results)), 0L)
  row <- results$results[results$results$rule_id == "coordinate_out_of_bounds"]
  expect_equal(c(row$n_checked, row$n_fail), c(2L, 0L))
})

test_that("with every bounded attribute a coordinate, the range rules have none (R19)", {
  # One not_run row each.
  bounded <- c("depth", "minutes", "fish_count", "weight")
  rows <- fish_run(fx_fish_clean(), settings = list(coordinate_attributes = bounded))$results
  for (rule in c("value_below_min", "value_above_max")) {
    row <- rows[rows$rule_id == rule]
    expect_equal(
      c(row$table_name, row$column_name, row$status, row$not_run_reason),
      c("*", "*", "not_run", "no_dd_values"),
      info = rule
    )
  }
  expect_setequal(rows$column_name[rows$rule_id == "coordinate_out_of_bounds"], bounded)
})

# task 5's fix round (the datatable review's M3, b30f767)
test_that("decimals_exceeded writes each distinct number once; a rule coerces a column once", {
  # depth as text, as a CSV gives it; weight's eight values two distinct numbers.
  fish <- edited("stations", "depth", NULL, c("2.5", "2.5"))
  fish <- edited("catches", "weight", NULL, rep(c(1.5, 2), 4L), fish)
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 3L)
  input <- conformance_input(fish, spec)
  real_places <- decimal_places
  real_coerce <- coerce_column
  places <- list()
  coerced <- character()
  testthat::local_mocked_bindings(
    decimal_places = function(number) {
      places[[length(places) + 1L]] <<- number
      real_places(number)
    },
    coerce_column = function(x, r_class, ...) {
      if (identical(x, fish$stations$depth)) {
        coerced <<- c(coerced, r_class)
      }
      real_coerce(x, r_class, ...)
    }
  )
  found <- check_domains(input, context)
  expect_identical(places, list(2.5, c(1.5, 2)))
  # value_below_min, value_above_max and decimals_exceeded each read depth once.
  expect_identical(coerced, rep("double", 3L))
  expect_length(found$decimals_exceeded, 2L)
})

# ---- task 6 ----

test_that("a site's ID is checked against its contributor's band, bounds inclusive (D18.2)", {
  testthat::skip_on_cran()
  # A site's ID moved in every table holding it.
  moved <- function(tables, from, to) {
    tables <- fx_mark_fixture(lapply(tables, data.table::copy))
    for (table in names(tables)) {
      if ("magp_site_id" %in% names(tables[[table]])) {
        at <- which(tables[[table]]$magp_site_id == from)
        data.table::set(tables[[table]], i = at, j = "magp_site_id", value = to)
      }
    }
    tables
  }
  tables <- fx_magp_clean()
  # Sites 1 and 2 are AB's (350001 to 400000), 5 and 6 BC's (450001 to 500000).
  tables <- moved(tables, 350002L, 1L)
  tables <- moved(tables, 350003L, 400000L)
  tables <- moved(tables, 450006L, 450001L)
  tables <- moved(tables, 450007L, 500001L)
  before <- lapply(tables, data.table::copy)
  results <- withCallingHandlers(
    gpq_check_conformance(tables, magp_spec(), rules = magp_rules(), layers = 3),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  # The rule leaves the caller's tables as they were (16.2).
  for (table in names(tables)) {
    expect_identical(tables[[table]], before[[table]], info = table)
  }
  found <- opened(results)
  expect_equal(unique(found$rule_id), "site_id_out_of_range")
  expect_equal(placed_at(found), c("magp_sites magp_site_id 1", "magp_sites magp_site_id 6"))
  expect_equal(found$value, c("1", "500001"))
  expect_equal(found$expected, c("350001 to 400000", "450001 to 500000"))
  # Only the column id_band_columns names is checked.
  rows <- results$results[results$results$rule_id == "site_id_out_of_range"]
  expect_equal(c(rows$table_name, rows$column_name), c("magp_sites", "magp_site_id"))
  expect_equal(c(rows$n_checked, rows$n_fail), c(8L, 2L))
})

test_that("a site whose chain breaks or ends without a band isn't checked (D18.2, D18.21)", {
  testthat::skip_on_cran()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  sites <- tables$magp_sites
  # Site 1's dataset NA, site 2's an orphan, sites 3 and 4's dataset's contributor without a
  # row of the contributor sheet; site 5's own ID NA and site 6's the sentinel -9 (R20).
  data.table::set(sites, i = 1:2, j = "magp_dataset_id", value = c(NA, "999.99"))
  data.table::set(tables$magp_datasets, i = 2L, j = "magp_contributor_id", value = "999")
  data.table::set(sites, i = 1:6, j = "magp_site_id", value = c(1L, 2L, 3L, 4L, NA, -9L))
  run <- function(tables) {
    withCallingHandlers(
      gpq_check_conformance(
        fx_mark_fixture(tables), magp_spec(),
        rules = magp_rules(), layers = 3
      ),
      gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
    )
  }
  results <- run(tables)
  expect_equal(nrow(opened(results, "site_id_out_of_range")), 0L)
  rows <- results$results[results$results$rule_id == "site_id_out_of_range"]
  # Sites 7 and 8 are checked; the six others count in neither n_checked nor n_fail.
  expect_equal(c(rows$n_checked, rows$n_fail, rows$n_not_evaluated), c(2L, 0L, 0L))
  expect_equal(rows$status, "pass")
  # A blank key breaks the chain too (D18.31): with sites 7 and 8 reaching their dataset only
  # through a blank key, nothing is checked, so the row is not_run (D18.22).
  data.table::set(tables$magp_datasets, i = 4L, j = "magp_dataset_id", value = "")
  data.table::set(sites, i = 7:8, j = c("magp_dataset_id", "magp_site_id"), value = list(
    c("", ""), c(7L, 8L)
  ))
  blank <- run(tables)
  expect_equal(nrow(opened(blank, "site_id_out_of_range")), 0L)
  rows <- blank$results[blank$results$rule_id == "site_id_out_of_range"]
  expect_equal(c(rows$status, rows$not_run_reason), c("not_run", "none_checked"))
  expect_equal(c(rows$n_checked, rows$n_fail, rows$n_not_evaluated), c(0L, 0L, 8L))
})

test_that("without a banded column site_id_out_of_range is one not_run row, no_input (R19)", {
  rows <- fish_run(fx_fish_clean())$results
  band <- rows[rows$rule_id == "site_id_out_of_range"]
  expect_equal(
    c(band$table_name, band$column_name, band$status, band$not_run_reason),
    c("*", "*", "not_run", "no_input")
  )
})

# task 6's fix round (the datatable review's #1)
test_that("site_id_out_of_range writes as text no banded ID but a failing one (16.2)", {
  testthat::skip_on_cran()
  spec <- magp_spec()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  # Site 1 outside every band, site 5's ID NA and site 6's the sentinel -9.
  data.table::set(tables$magp_sites, i = c(1L, 5L, 6L), j = "magp_site_id", value = c(1L, NA, -9L))
  input <- conformance_input(tables, spec)
  context <- conformance_context(spec, NULL, list(), 3L)
  ids <- tables$magp_sites$magp_site_id
  real_text <- value_text
  written <- integer()
  testthat::local_mocked_bindings(value_text = function(x) {
    if (identical(x, ids)) {
      written <<- c(written, length(x))
    }
    real_text(x)
  })
  found <- check_domains(input, context)$site_id_out_of_range[[1L]]
  expect_length(written, 0L)
  expect_equal(found$fail, 1L)
  expect_equal(found$expected, "350001 to 400000")
  expect_equal(found$unchecked, c(5L, 6L))
})

# The band comparison in CRAN mode, on a small spec made here (D18.107)
test_that("a site's ID passes inside its own band or on either bound, and fails outside it", {
  # Sites reach their contributor through their dataset: ON's band 1 to 99, AB's 100 to 199.
  dictionary <- data.frame(
    table_name = c("datasets", "datasets", "sites", "sites"),
    attribute_name = c("dataset_id", "contributor_id", "site_id", "dataset_id"),
    key_type = c("PK", NA, "PK", "FK"), reference_table = c(NA, NA, NA, "datasets"),
    data_type = c("character", "character", "integer", "character")
  )
  spec <- gpq_read_spec(
    dictionary,
    code_lists = list(
      ranges = data.frame(
        contributor = c("ON", "AB"), v2_start = c("1", "100"), v2_end = c("99", "199")
      ),
      contributor = data.frame(
        abbreviated_name = c("ON", "AB"), contributor_id = c("130", "100")
      )
    ),
    non_code_sheets = c("ranges", "contributor"),
    id_bands = list(
      sheet = "ranges", label_col = "contributor", start_col = "v2_start", end_col = "v2_end",
      labels_from = c(sheet = "contributor", column = "abbreviated_name"),
      reserved_pattern = NULL, table = "sites", attribute = "site_id",
      key = "dataset_id > contributor_id"
    )
  )
  # ON's sites on its lower bound, inside and on its upper bound; AB's 98 (below its band,
  # though inside ON's), 150 (inside), 200 (above), its two bounds and 1000, above as a number
  # but between "100" and "199" as text (the fixture review's M1).
  tables <- list(
    datasets = data.table::data.table(dataset_id = c("d1", "d2"), contributor_id = c("130", "100")),
    sites = data.table::data.table(
      site_id = c(1L, 50L, 99L, 98L, 150L, 200L, 100L, 199L, 1000L),
      dataset_id = rep(c("d1", "d2"), c(3L, 6L))
    )
  )
  results <- gpq_check_conformance(tables, spec, layers = 3)
  expect_equal(nrow(opened(results)), 3L)
  found <- opened(results, "site_id_out_of_range")
  expect_equal(placed_at(found), c("sites site_id 4", "sites site_id 6", "sites site_id 9"))
  expect_equal(found$value, c("98", "200", "1000"))
  expect_equal(found$expected, rep("100 to 199", 3L))
  rows <- results$results[results$results$rule_id == "site_id_out_of_range"]
  expect_equal(c(rows$table_name, rows$column_name, rows$status), c("sites", "site_id", "fail"))
  expect_equal(c(rows$n_checked, rows$n_fail), c(9L, 3L))
})

# task 6's fix round (the datatable review's #1)
test_that("domain_skipped() reads plain numbers as numbers, as it reads their text (16.2)", {
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 3L)
  # depth's own sentinels are -99 and -88; "NaN" and "7" given here as its unknown codes.
  context$unknown_codes <- c(depth = "NaN", depth = "7")
  input <- conformance_input(fx_fish_clean(), spec)
  row <- input$columns[input$columns$attribute_name == "depth"]
  # -99 - 1e-14 isn't -99, but its text to 15 significant digits is the code "-99".
  x <- c(1.5, NA, NaN, Inf, -99, 7, -88, 7.25, -99 - 1e-14)
  skipped <- domain_skipped(x, row, context, x)
  expect_identical(skipped, domain_skipped(value_text(x), row, context, x))
  expect_identical(skipped, c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  # Without "NaN" among the codes, NaN is left to the rule, as its text is.
  context$unknown_codes <- c(depth = "7")
  expect_false(domain_skipped(NaN, row, context, NaN))
  expect_identical(
    domain_skipped(c(NaN, 1L), row, context, c(NaN, 1)),
    domain_skipped(c("NaN", "1"), row, context, c(NaN, 1))
  )
  # Each distinct value near a code is written once (the finish's minors pass, m34).
  real_text <- value_text
  written <- list()
  testthat::local_mocked_bindings(value_text = function(x) {
    written[[length(written) + 1L]] <<- x
    real_text(x)
  })
  many <- c(-99, -99, 1.5, -99, -88, -88)
  expect_identical(
    domain_skipped(many, row, context, many), c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(written, list(c(-99, -88)))
})

# the Codex round's fix round 1 (D18.117)
test_that("domain_skipped() finds an integer64 column's codes exactly, as bit64 reads them", {
  testthat::skip_if_not_installed("bit64")
  # A number-typed column stored as integer64, at most 100, its missing-value code 2^53 + 1:
  # read as doubles, 2^53 is the code too and was skipped (the spec review's S11 and S11c).
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("t_id", "x"), key_type = c("PK", NA),
    data_type = c("character", "numeric"), max_value = c(NA, "100")
  )
  spec <- gpq_read_spec(
    dictionary,
    sentinels = gpq_sentinels(numeric = c(missing = "9007199254740993"))
  )
  x <- bit64::as.integer64(c("9007199254740992", "50", "9007199254740993", "9007199254740994"))
  tables <- list(t = data.table::data.table(t_id = c("a", "b", "c", "d"), x = x))
  input <- conformance_input(tables, spec)
  row <- input$columns[input$columns$attribute_name == "x"]
  context <- conformance_context(spec, NULL, list(), 3L)
  expect_identical(
    domain_skipped(x, row, context, coerce_column(x, "double")$value),
    c(FALSE, FALSE, TRUE, FALSE)
  )
  found <- opened(gpq_check_conformance(tables, spec, layers = 3))
  expect_equal(paste(found$rule_id, placed_at(found), found$value), c(
    "value_above_max t x 1 9007199254740992", "value_above_max t x 4 9007199254740994"
  ))
  # The code written "+9007199254740993" isn't the value's text, so only bit64's exact reading
  # skips 2^53 + 1, and 2^53 is left to the rule (the scoped re-review's RR-2).
  plus <- gpq_read_spec(
    dictionary,
    sentinels = gpq_sentinels(numeric = c(missing = "+9007199254740993"))
  )
  near <- bit64::as.integer64(c("9007199254740993", "9007199254740992"))
  expect_identical(
    domain_skipped(
      near, row, conformance_context(plus, NULL, list(), 3L), coerce_column(near, "double")$value
    ),
    c(TRUE, FALSE)
  )
})

# ---- task 7 ----

test_that("undeclared_code_set warns on a text column of a few distinct values (D18.20)", {
  # code_set_max_values 1: station_name's two values are judged.
  one <- list(code_set_max_values = 1L)
  same <- edited("stations", "station_name", 1:2, c("North", "North"))
  results <- fish_run(same, settings = one)
  found <- opened(results)
  expect_equal(found$rule_id, "undeclared_code_set")
  expect_equal(found$severity, "warning")
  expect_equal(
    c(found$table_name, found$column_name, found$record_pk, found$level),
    c("stations", "station_name", "*", "column")
  )
  expect_equal(found$n_records, 2L)
  expect_equal(found$value, "North")
  expect_true(is.na(found$expected))
  rows <- results$results[results$results$rule_id == "undeclared_code_set"]
  expect_equal(rows$status, "fail")
  expect_equal(c(rows$n_checked, rows$n_fail), c(1L, 1L))
  # As many distinct values as values: no code set.
  expect_equal(nrow(opened(fish_run(fx_fish_clean(), settings = one))), 0L)
  # A sentinel or a blank isn't counted, so one value is left, too few to judge: no row (R21).
  few <- fish_run(edited("stations", "station_name", 1:2, c("North", "?")), settings = one)
  expect_false("undeclared_code_set" %in% few$results$rule_id)
  expect_false("undeclared_code_set" %in% few$metadata$self_test$rule_id)
  # The two skip settings leave a column alone.
  skipped <- fish_run(
    same,
    settings = c(one, list(code_set_skip_attributes = "station_name"))
  )
  expect_false("undeclared_code_set" %in% skipped$results$rule_id)
  by_pattern <- fish_run(same, settings = c(one, list(code_set_skip_pattern = "_name$")))
  expect_false("undeclared_code_set" %in% by_pattern$results$rule_id)
})

test_that("undeclared_code_set judges more values than the built-in 25, sorted (D9.26)", {
  testthat::skip_on_cran()
  # 26 stations: station_name's 26 values are judged.
  fish <- fx_clean_tables(fx_fish_spec(), n = 26L)
  stations <- data.table::copy(fish$stations)
  data.table::set(stations, j = "station_name", value = rep(c("South", "North"), 13L))
  fish$stations <- stations
  found <- opened(fish_run(fish, layers = 3), "undeclared_code_set")
  expect_equal(found$value, "North; South")
  expect_equal(found$n_records, 26L)
  # A key, a coded column and a date aren't judged.
  rows <- fish_run(fx_clean_tables(fx_fish_spec(), n = 26L), layers = 3)$results
  judged <- rows$column_name[rows$rule_id == "undeclared_code_set"]
  expect_equal(judged, "station_name")
})

# task 7's fix round (the datatable review's F3)
test_that("undeclared_code_set doesn't count a blank, NA or an unknown code (R21)", {
  testthat::skip_on_cran()
  # Each leaves one value, too few to judge with code_set_max_values 1: no row.
  settings <- list(code_set_max_values = 1L, unknown_codes = c(station_name = "Unknown"))
  for (values in list(c("North", "  "), c("North", NA), c("North", "Unknown"))) {
    results <- fish_run(edited("stations", "station_name", 1:2, values), settings = settings)
    expect_false("undeclared_code_set" %in% results$results$rule_id, info = deparse(values))
    expect_false(
      "undeclared_code_set" %in% results$metadata$self_test$rule_id,
      info = deparse(values)
    )
  }
})

# task 7's fix round (the spec review's m2)
test_that("undeclared_code_set doesn't judge an attribute marked as an ID (R11)", {
  testthat::skip_on_cran()
  same <- edited("stations", "station_name", 1:2, c("North", "North"))
  one <- list(code_set_max_values = 1L)
  marked <- fish_run(same, settings = one, spec = fx_fish_spec(id_pattern = "^station_name$"))
  expect_false("undeclared_code_set" %in% marked$results$rule_id)
  # Unmarked, the same column fails.
  expect_equal(opened(fish_run(same, settings = one))$rule_id, "undeclared_code_set")
})

# the final fix wave (D18.103)
test_that("undeclared_code_set's results show example_cap of its values, its issue all of them", {
  testthat::skip_on_cran()
  fish <- fx_clean_tables(fx_fish_spec(), n = 6L)
  stations <- data.table::copy(fish$stations)
  data.table::set(stations, j = "station_name", value = rep(c("South", "North", "East"), 2L))
  fish$stations <- stations
  shown <- c(NA_character_, "East", "East; North; South")
  caps <- c(0L, 1L, 5L)
  for (k in seq_along(caps)) {
    results <- fish_run(fish, settings = list(code_set_max_values = 3L, example_cap = caps[[k]]))
    row <- results$results[results$results$rule_id == "undeclared_code_set"]
    cap <- paste("example_cap", caps[[k]])
    expect_equal(c(row$status, row$examples), c("fail", shown[[k]]), info = cap)
    found <- opened(results, "undeclared_code_set")
    expect_equal(found$value, "East; North; South", info = cap)
  }
})
