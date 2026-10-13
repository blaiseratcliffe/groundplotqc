# Tests for Layer 2, missing-value codes (plan 6.1, 6.2, 3.5; D1.8a, D7.3, D7.4, D7.9, D8.8,
# D16.9 to D16.11), and the run end to end on the generic fixtures (18.3).

fish_run <- function(tables, settings = list(), layers = 0:2, rules = NULL, spec = fx_fish_spec()) {
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

test_that("check_sentinels returns an outcome list for each Layer 2 rule", {
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 2L)
  found <- check_sentinels(conformance_input(fish, spec), context)
  expect_named(found, c(
    "raw_na", "blank_string", "sentinel_wrong_type", "sentinel_rogue_code",
    "species_unknown_code", "whitespace_padding", "na_text_in_id"
  ))
  # Outside the keys: 15 attributes less 3 PKs and 2 FKs.
  expect_length(found$raw_na, 10L)
  expect_length(found$sentinel_wrong_type, 15L)
  expect_length(found$na_text_in_id, 5L)
  # The coded attributes and the keys.
  expect_length(found$whitespace_padding, 8L)
})

test_that("the generic fixtures pass every layer, so the engine assumes no MAGPlot name (18.3)", {
  results <- fish_run(fx_fish_clean(), layers = 0:5)
  expect_equal(nrow(opened(results)), 0L)
  expect_equal(nrow(results$metadata$self_test), 0L)
  registry <- rule_registry()
  expect_setequal(
    unique(results$results$rule_id), registry$rule_id[registry$check_type != "preflight"]
  )
  # The fish spec gives no unknown codes, so that rule's one row says why it didn't run.
  expect_equal(
    results$results$not_run_reason[results$results$rule_id == "species_unknown_code"], "no_input"
  )
})

test_that("raw_na finds NA outside the keys, as raw_na_policy says (D1.8a, D7.4, D7.9)", {
  fish <- edited("catches", "weight", 1L, NA_real_)
  fish <- edited("catches", "haul_id", 2L, NA_character_, fish)
  found <- opened(fish_run(fish), "raw_na")
  expect_equal(placed_at(found), "catches weight 1")
  expect_equal(found$expected, "-99; -88")
  expect_equal(nrow(opened(fish_run(fish, list(raw_na_policy = "never")), "raw_na")), 0L)
  never <- fish_run(fish, list(raw_na_policy = "never"))
  expect_false("raw_na" %in% never$results$rule_id)
  expect_false("raw_na" %in% never$metadata$self_test$rule_id)
  # A spec with no numeric sentinels: raw_na can't run on numbers unless always.
  spec <- fx_fish_spec()
  spec$sentinels <- spec$sentinels[spec$sentinels$data_type != "numeric"]
  rows <- fish_run(fish, spec = spec)$results
  numbers <- rows[rows$rule_id == "raw_na" & rows$column_name == "weight"]
  expect_equal(numbers$status, "not_run")
  expect_equal(numbers$not_run_reason, "no_sentinels")
  always <- fish_run(fish, list(raw_na_policy = "always"), spec = spec)
  expect_equal(placed_at(opened(always, "raw_na")), "catches weight 1")
})

test_that("blank_string finds empty and blank text outside the keys", {
  fish <- edited("stations", "station_name", 1:2, c("", "  "))
  fish <- edited("hauls", "haul_id", 1L, "", fish)
  found <- opened(fish_run(fish), "blank_string")
  expect_equal(found$value, c("", "  "))
  expect_equal(unique(found$table_name), "stations")
})

test_that("sentinel_wrong_type finds another type's sentinel, expecting the same role's own", {
  fish <- edited("stations", "station_name", 1:2, c("-99", "-88"))
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[[1L]] <- "?"
  fish$catches <- catches
  open <- opened(fish_run(fish))
  wrong <- open[open$rule_id == "sentinel_wrong_type"]
  expect_equal(paste(wrong$column_name, wrong$value, wrong$expected), c(
    "station_name -99 ?", "station_name -88 ~", "weight ? -99"
  ))
  # Reported once: coercion_loss leaves "?" to Layer 2 (D8.8).
  expect_setequal(unique(open$rule_id), c("sentinel_wrong_type", "class_mismatch"))
})

test_that("sentinel_rogue_code finds rogue codes that aren't codes of the column (D8.8)", {
  gear <- fx_fish_clean()$hauls$gear[[1L]]
  fish <- edited("stations", "station_name", 1L, "N/A")
  fish <- edited("catches", "weight", 1L, -999, fish)
  fish <- edited("hauls", "gear", 1L, gear, fish)
  fish <- edited("hauls", "station_id", 1L, "UNK", fish)
  rogue <- opened(
    fish_run(fish, list(rogue_codes = c("-999", "N/A", gear, "UNK"))), "sentinel_rogue_code"
  )
  expect_equal(paste(rogue$column_name, rogue$value), c("station_name N/A", "weight -999"))
  expect_equal(rogue$expected, c("?; ~", "-99; -88"))
})

test_that("species_unknown_code finds the unknown codes; without any it doesn't run (D16.11)", {
  fish <- edited("catches", "species", 1L, "UNK.SP")
  run <- fish_run(fish, list(unknown_codes = c(species = "UNK.SP")))
  found <- opened(run, "species_unknown_code")
  expect_equal(paste(found$column_name, found$value, found$expected), "species UNK.SP ?")
  rows <- fish_run(fish)$results
  unknown <- rows[rows$rule_id == "species_unknown_code"]
  expect_equal(
    unlist(
      unknown[, c("table_name", "column_name", "status", "not_run_reason")],
      use.names = FALSE
    ),
    c("*", "*", "not_run", "no_input")
  )
  # A name no attribute has: nothing to check, so no row and nothing expected (D16.17).
  elsewhere <- fish_run(fish, list(unknown_codes = c(colour = "red")))
  expect_false("species_unknown_code" %in% elsewhere$results$rule_id)
  expect_false("species_unknown_code" %in% elsewhere$metadata$self_test$rule_id)
})

test_that("whitespace_padding finds padded codes and IDs, keys included", {
  gear <- fx_fish_clean()$hauls$gear[[1L]]
  fish <- edited("hauls", "gear", 1L, paste0(" ", gear))
  fish <- edited("hauls", "haul_id", 2L, paste0(fish$hauls$haul_id[[2L]], " "), fish)
  fish <- edited("stations", "station_name", 1L, " t1", fish)
  found <- opened(fish_run(fish), "whitespace_padding")
  expect_equal(found$column_name, c("haul_id", "gear"))
  expect_equal(found$expected, c(fx_fish_clean()$hauls$haul_id[[2L]], gear))
  expect_equal(unique(found$severity), "warning")
})

test_that("na_text_in_id finds the text NA in an ID; elsewhere it is a rogue code", {
  fish <- edited("catches", "haul_id", 1L, "NA")
  fish <- edited("stations", "station_name", 1L, "NA", fish)
  open <- opened(fish_run(fish))
  expect_equal(placed_at(open[open$rule_id == "na_text_in_id"]), "catches haul_id 1")
  expect_equal(placed_at(open[open$rule_id == "sentinel_rogue_code"]), "stations station_name 1")
})

test_that("without a matrix both sentinels pass (D7.3)", {
  fish <- edited("catches", "weight", 1:2, c(-99, -88))
  fish <- edited("stations", "station_name", 1:2, c("?", "~"), fish)
  expect_equal(nrow(opened(fish_run(fish))), 0L)
})

test_that("a value Layer 1 failed is not evaluated by Layer 2, rooted at its issue (6.2)", {
  catches <- data.table::copy(fx_fish_clean()$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[1:2] <- c("abc", NA)
  fish <- fx_fish_clean()
  fish$catches <- catches
  results <- fish_run(fish)
  issues <- results$issues
  loss <- issues[issues$rule_id == "coercion_loss"]
  masked <- issues[issues$status == "not_evaluated" & issues$column_name == "weight"]
  # Every Layer 2 rule that reads weight masks row 1; raw_na still finds row 2.
  expect_true(all(masked$row_number == 1L & masked$root_issue_id == loss$issue_id))
  expect_setequal(masked$rule_id, c(
    "raw_na", "blank_string", "sentinel_wrong_type", "sentinel_rogue_code"
  ))
  expect_equal(placed_at(opened(results, "raw_na")), "catches weight 2")
  rows <- results$results
  na_row <- rows[rows$rule_id == "raw_na" & rows$column_name == "weight"]
  expect_equal(
    c(na_row$n_checked, na_row$n_fail, na_row$n_not_evaluated), c(nrow(catches) - 1L, 1L, 1L)
  )
})

test_that("a column-level failure's records collapse for each later check (D16.10)", {
  catches <- data.table::copy(fx_fish_clean()$catches)
  catches$weight <- rep("abc", nrow(catches))
  fish <- fx_fish_clean()
  fish$catches <- catches
  issues <- fish_run(fish, list(column_level_min_n = 3L))$issues
  root <- issues$issue_id[issues$rule_id == "coercion_loss"]
  masked <- issues[issues$status == "not_evaluated"]
  expect_true(all(masked$level == "column" & masked$n_records == nrow(catches)))
  expect_true(all(masked$root_issue_id == root))
  expect_equal(anyDuplicated(masked$rule_id), 0L)
})

test_that("a value that isn't valid UTF-8 is reported as given, with no stop or warning (D16.33)", {
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale, where R stops on it")
  padded <- rawToChar(as.raw(c(0x20, 0x41, 0x97)))
  plain <- rawToChar(as.raw(c(0x41, 0x97)))
  expect_false(any(validUTF8(c(padded, plain))))
  # A coded column, an ID that is a foreign key and a plain text column hold the value, each
  # read by Layer 2's text rules.
  fish <- edited("hauls", "gear", 1:2, c(padded, plain))
  fish <- edited("catches", "haul_id", 1L, padded, fish)
  fish <- edited("stations", "station_name", 1:2, c(padded, plain), fish)
  expect_silent(results <- fish_run(fish))
  found <- opened(results)
  expect_equal(placed_at(found), c("hauls gear 1", "catches haul_id 1"))
  expect_setequal(found$rule_id, "whitespace_padding")
  expect_identical(charToRaw(found$value[[1L]]), charToRaw(padded))
  # The expected value is the value trimmed, byte by byte.
  expect_identical(charToRaw(found$expected[[1L]]), charToRaw(plain))
  expect_no_error(gpq_write_results(results, withr::local_tempdir()))
})

test_that("Layer 2 reads latin1 text as UTF-8 and text marked as bytes by no rule (D16.36)", {
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  latin <- iconv(" é", "UTF-8", "latin1")
  marked <- " é"
  Encoding(marked) <- "bytes"
  expect_identical(Encoding(c(latin, marked)), c("latin1", "bytes"))
  expect_false(validUTF8(latin))
  expect_true(validUTF8(marked))
  spec <- fx_fish_spec()
  fish <- edited("hauls", "gear", 1:2, c(latin, marked))
  fish <- edited("stations", "station_name", 1:2, c("-99", marked), fish)
  fish <- edited("catches", "haul_id", 1L, marked, fish)
  input <- conformance_input(fish, spec)
  context <- conformance_context(spec, NULL, list(), 2L)
  # The run refuses text marked as bytes (D16.38), so the rules and the recording, its second
  # line, are run a step inside it.
  expect_silent(found <- check_sentinels(input, context))
  state <- new_run_state()
  record_layer(found, 2L, input, context, state, "20261215T140312Z_3fa9c1")
  issues <- data.table::rbindlist(state$issues, use.names = TRUE)
  expect_false("bytes" %in% Encoding(issues$value))
  expect_no_error(compiled_bytes(issues))
  open <- issues[issues$status == "open"]
  # "-99" is a numeric sentinel in a text column. The latin1 value is padded: its fix is the
  # text trimmed, as UTF-8; the marked ones are padded too, and their fix keeps their bytes
  # without the mark.
  expect_equal(
    paste(open$rule_id, open$table_name, open$column_name, open$row_number),
    c(
      "sentinel_wrong_type stations station_name 1", "whitespace_padding hauls gear 1",
      "whitespace_padding hauls gear 2", "whitespace_padding catches haul_id 1"
    )
  )
  padding <- open[open$rule_id == "whitespace_padding"]
  expect_identical(charToRaw(padding$value[[1L]]), charToRaw(latin))
  expect_identical(charToRaw(padding$value[[2L]]), charToRaw(marked))
  expect_equal(padding$expected[[1L]], "é")
  expect_true(all(validUTF8(padding$expected)))
  expect_false("bytes" %in% Encoding(padding$expected))
  expect_identical(charToRaw(padding$expected[[2L]]), charToRaw("é"))
})

test_that("a run's results pass the results model's checks and round-trip (18.4)", {
  # Hostile text in a group column and in an issue's value (18.4).
  fish <- edited("catches", "weight", 1L, NA_real_)
  fish <- edited("stations", "water_body", 1L, " say \"hi\", twice", fish)
  results <- gpq_check_conformance(
    fish, fx_fish_spec(),
    group_cols = c("water_body", "gear"), problem_unit = list("stations", "station_id")
  )
  expect_no_error(validate_gpq_results(results))
  expect_true(all(results$results$n_fail <= results$results$n_checked))
  issue <- opened(results, "raw_na")
  haul_at <- match(fish$catches$haul_id[[1L]], fish$hauls$haul_id)
  expect_equal(issue$station_id, fish$hauls$station_id[haul_at])
  expect_equal(issue$fingerprint, paste0("raw_na|catches|weight|pk:", fish$catches$catch_id[[1L]]))
  expect_equal(opened(results, "whitespace_padding")$value, " say \"hi\", twice")
  expect_true(" say \"hi\", twice" %in% results$results$water_body)
  run_dir <- gpq_write_results(results, withr::local_tempdir())
  expect_identical(gpq_read_results(run_dir), results)
})
