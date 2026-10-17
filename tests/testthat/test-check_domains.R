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
  expect_named(found, "code_not_in_list")
  # The three coded attributes.
  expect_length(found$code_not_in_list, 3L)
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
