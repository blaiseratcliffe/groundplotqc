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
  registry <- rule_registry()
  data_rules <- registry$rule_id[registry$check_type != "preflight"]
  # Both toy specs: the fish survey's, and the forest ground plots' with its upper-case names,
  # YYYYMMDD dates and sentinels that read as numbers.
  specs <- list(fish = fx_fish_spec(), forest = fx_forest_spec())
  for (name in names(specs)) {
    spec <- specs[[name]]
    results <- fish_run(fx_clean_tables(spec), layers = 0:5, spec = spec)
    expect_equal(nrow(opened(results)), 0L, info = name)
    expect_equal(nrow(results$metadata$self_test), 0L, info = name)
    # Neither toy has a text column holding more values than code_set_max_values, so
    # undeclared_code_set judges none, nor a foreign key of a number type, which
    # fk_sentinel_unverified reads, nor an ID outside its keys, which src_id_duplicate reads;
    # none writes a row (R21, D16.17).
    expect_setequal(
      unique(results$results$rule_id),
      setdiff(data_rules, c("undeclared_code_set", "fk_sentinel_unverified", "src_id_duplicate"))
    )
    # Neither spec gives unknown codes, so that rule's one row says why it didn't run.
    expect_equal(
      results$results$not_run_reason[results$results$rule_id == "species_unknown_code"],
      "no_input",
      info = name
    )
  }
})

test_that("raw_na finds NA outside the keys, as raw_na_policy says (D1.8a, D7.4, D7.9)", {
  fish <- edited("catches", "weight", 1L, NA_real_)
  fish <- edited("catches", "haul_id", 2L, NA_character_, fish)
  found <- opened(fish_run(fish), "raw_na")
  expect_equal(placed_at(found), "catches weight 1")
  expect_equal(found$expected, "-99; -88")
  never <- fish_run(fish, list(raw_na_policy = "never"))
  expect_equal(nrow(opened(never, "raw_na")), 0L)
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
  # A blank date is Layer 2's, not date_unparseable's. The fish spec's text and date types
  # share their sentinels, "?" and "~".
  fish <- edited("hauls", "haul_date", 1L, "", fish)
  open <- opened(fish_run(fish))
  expect_equal(placed_at(open), c(
    "stations station_name 1", "stations station_name 2", "hauls haul_date 1"
  ))
  expect_setequal(open$rule_id, "blank_string")
  expect_equal(open$expected, rep("?; ~", 3L))
  # The forest spec's differ: a blank date expects the date type's own.
  spec <- fx_forest_spec()
  forest <- edited("PLOTS", "ESTABLISHED", 1L, "", fx_clean_tables(spec))
  blank <- opened(fish_run(forest, spec = spec))
  expect_equal(paste(blank$rule_id, blank$column_name, blank$expected), paste(
    "blank_string ESTABLISHED", paste(spec$sentinels$value[spec$sentinels$data_type == "date"],
      collapse = "; "
    )
  ))
})

test_that("Unicode spaces count in values that aren't plain ASCII (D16.42, m35)", {
  gear <- fx_fish_clean()$hauls$gear[[1L]]
  nbsp <- intToUtf8(0xA0)
  em <- intToUtf8(0x2003)
  summer <- intToUtf8(c(0xE9, 0x74, 0xE9))
  # Only Unicode spaces: blank, in a coded column too, and never also padded.
  fish <- edited("stations", "station_name", 1:2, c(nbsp, paste0(em, " ")))
  fish <- edited("hauls", "gear", 1:2, c(nbsp, paste0(gear, nbsp)), fish)
  # A padded ID and a padded value that isn't plain ASCII, trimmed as characters.
  fish <- edited("catches", "haul_id", 1L, paste0(em, fish$catches$haul_id[[1L]]), fish)
  fish <- edited("catches", "species", 1L, paste0(summer, " "), fish)
  open <- opened(fish_run(fish))
  expect_equal(paste(open$rule_id, placed_at(open)), c(
    "blank_string stations station_name 1", "blank_string stations station_name 2",
    "blank_string hauls gear 1", "whitespace_padding hauls gear 2",
    "whitespace_padding catches haul_id 1", "whitespace_padding catches species 1"
  ))
  padding <- open[open$rule_id == "whitespace_padding"]
  expect_equal(padding$expected, c(gear, fx_fish_clean()$catches$haul_id[[1L]], summer))
})

test_that("in a locale that isn't UTF-8, unmarked UTF-8 values are read as UTF-8 (D16.36, F1)", {
  spec <- fx_fish_spec()
  # A coded value ending in the bytes C3 A0 isn't padded; a no-break space alone is blank.
  voila <- intToUtf8(c(0x76, 0x6F, 0x69, 0x6C, 0xE0))
  nbsp <- intToUtf8(0xA0)
  Encoding(voila) <- "unknown"
  Encoding(nbsp) <- "unknown"
  fish <- edited("hauls", "gear", 1L, voila)
  fish <- edited("stations", "station_name", 1L, nbsp, fish)
  before <- Sys.getlocale("LC_CTYPE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", before)))
  set <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  testthat::skip_if(
    !nzchar(set) || l10n_info()[["UTF-8"]], "LC_CTYPE can't be set to a locale that isn't UTF-8."
  )
  open <- opened(fish_run(fish, spec = spec))
  expect_equal(paste(open$rule_id, placed_at(open)), "blank_string stations station_name 1")
})

test_that("sentinel_wrong_type finds another type's sentinel, expecting the same role's own", {
  fish <- edited("stations", "station_name", 1:2, c("-99", "-88"))
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[[1L]] <- "?"
  fish$catches <- catches
  # A key holds one too (D7.9 leaves only NA, blanks and rogue codes in keys to Layer 4).
  fish <- edited("hauls", "station_id", 1L, "-99", fish)
  open <- opened(fish_run(fish))
  wrong <- open[open$rule_id == "sentinel_wrong_type"]
  expect_equal(paste(wrong$column_name, wrong$value, wrong$expected), c(
    "station_name -99 ?", "station_name -88 ~", "station_id -99 ?", "weight ? -99"
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

test_that("a value another Layer 2 rule owns is not also a rogue code (D7.3, D16.40, D16.41)", {
  # The column's own sentinels pass, though listed as rogue codes (D7.3).
  fish <- edited("stations", "station_name", 1L, "?")
  fish <- edited("catches", "weight", 1L, -99, fish)
  own <- fish_run(fish, list(rogue_codes = c("?", "-99")))
  expect_equal(nrow(opened(own)), 0L)
  # Another type's declared sentinel is sentinel_wrong_type's alone, in a text column and in a
  # date column stored as numbers.
  fish <- edited("stations", "station_name", 1L, "-99")
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_date <- c(-99, 20200602, 20200603, 20200604)
  fish$hauls <- hauls
  other <- fish_run(fish, list(rogue_codes = c("-99", "UNK"), column_level_min_n = 100L))
  found <- opened(other)
  found <- found[found$rule_id != "class_mismatch" & found$rule_id != "date_unparseable"]
  expect_equal(
    paste(found$rule_id, found$table_name, found$column_name, found$row_number, found$value),
    c(
      "sentinel_wrong_type stations station_name 1 -99",
      "sentinel_wrong_type hauls haul_date 1 -99"
    )
  )
  expect_equal(found$expected, c("?", "?"))
  # An unknown code that is also a rogue code (built in) is species_unknown_code's alone.
  fish <- edited("catches", "species", 1L, "UNK")
  unknown <- opened(fish_run(fish, list(unknown_codes = c(species = "UNK"))))
  expect_equal(
    paste(unknown$rule_id, unknown$column_name, unknown$value), "species_unknown_code species UNK"
  )
  # Without it as an unknown code, it is rogue.
  rogue <- opened(fish_run(fish))
  expect_equal(
    paste(rogue$rule_id, rogue$column_name, rogue$value), "sentinel_rogue_code species UNK"
  )
})

test_that("an unknown code that is also a sentinel is reported once (D16.45, m51)", {
  # Another type's sentinel listed as the attribute's unknown code is species_unknown_code's.
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[[1L]] <- "?"
  fish$catches <- catches
  open <- opened(fish_run(fish, list(unknown_codes = c(weight = "?"))))
  open <- open[open$rule_id != "class_mismatch"]
  expect_equal(paste(open$rule_id, placed_at(open)), "species_unknown_code catches weight 1")
  # The column's own sentinel passes, though listed as an unknown code (D7.3), stored as
  # numbers or as text.
  fish <- edited("catches", "weight", 1L, -99)
  fish <- edited("stations", "station_name", 1L, "?", fish)
  own <- fish_run(fish, list(unknown_codes = c(weight = "-99", station_name = "?")))
  expect_equal(nrow(opened(own)), 0L)
})

test_that("a number column stored as numbers holds no text code, so a real 0 isn't a sentinel", {
  # The forest spec's date sentinels, "00000000" and "99999999", read as numbers (R68).
  spec <- fx_forest_spec()
  forest <- fx_clean_tables(spec)
  forest <- edited("TREE_MEAS", "DBH", 1L, 0, forest)
  forest <- edited("TREES", "TREE_NO", 1L, 0L, forest)
  forest <- edited("PLOTS", "PLOT_AREA", 1L, 99999999, forest)
  expect_equal(nrow(opened(fish_run(forest, spec = spec))), 0L)
  # A text code in a date column is still another type's: "-" is the character sentinel.
  forest <- edited("PLOTS", "ESTABLISHED", 1L, "-", forest)
  open <- opened(fish_run(forest, spec = spec))
  expect_equal(
    paste(open$rule_id, open$column_name, open$value, open$expected),
    "sentinel_wrong_type ESTABLISHED - 99999999"
  )
  # A number that isn't a code of another rule is a rogue code where rogue_codes names it:
  # 99999999 in DBH is a real number, no date sentinel (D16.42, m36).
  forest <- edited("TREE_MEAS", "DBH", 1L, 99999999, fx_clean_tables(spec))
  rogue <- opened(fish_run(forest, list(rogue_codes = "99999999"), spec = spec))
  expect_equal(
    paste(rogue$rule_id, rogue$column_name, rogue$value), "sentinel_rogue_code DBH 99999999"
  )
})

test_that("stored as numbers, a column whose type isn't numeric takes text codes as numbers", {
  # The forest spec with "-1" as its character not-applicable code (D16.42, m37).
  spec <- fx_forest_spec()
  sentinels <- data.table::copy(spec$sentinels)
  at <- which(sentinels$data_type == "character" & sentinels$role == "not_applicable")
  data.table::set(sentinels, i = at, j = "value", value = "-1")
  spec$sentinels <- sentinels
  forest <- fx_clean_tables(spec)
  plots <- data.table::copy(forest$PLOTS)
  # A date column held as numbers, as fread() reads YYYYMMDD: -1 is the character code, and
  # 99999999 the column's own, which passes though listed as a rogue code (m39).
  plots$ESTABLISHED <- as.integer(plots$ESTABLISHED)
  plots$ESTABLISHED[1:2] <- c(-1L, 99999999L)
  forest$PLOTS <- plots
  results <- fish_run(forest, list(rogue_codes = c("-1", "99999999")), spec = spec)
  open <- opened(results)
  open <- open[open$rule_id != "class_mismatch"]
  expect_equal(
    paste(open$rule_id, placed_at(open), open$value, open$expected),
    "sentinel_wrong_type PLOTS ESTABLISHED 1 -1 99999999"
  )
  # The column's own codes are compared as text there, so a real 0 isn't the date code
  # "00000000" and is a rogue code where rogue_codes names it (m39).
  forest <- fx_clean_tables(spec)
  plots <- data.table::copy(forest$PLOTS)
  plots$ESTABLISHED <- as.integer(plots$ESTABLISHED)
  plots$ESTABLISHED[[1L]] <- 0L
  forest$PLOTS <- plots
  open <- opened(fish_run(forest, list(rogue_codes = "0"), spec = spec))
  open <- open[open$rule_id != "class_mismatch"]
  expect_equal(
    paste(open$rule_id, placed_at(open), open$value), "sentinel_rogue_code PLOTS ESTABLISHED 1 0"
  )
  # A text column stored as numbers: another type's code is found as if stored as text, so a
  # real 0 still isn't the date code "00000000".
  forest <- fx_clean_tables(fx_forest_spec())
  meas <- data.table::copy(forest$TREE_MEAS)
  meas$MEAS_ID <- seq_len(nrow(meas))
  meas$MEAS_ID[1:2] <- c(99999999L, 0L)
  forest$TREE_MEAS <- meas
  open <- opened(fish_run(forest, spec = fx_forest_spec()))
  open <- open[open$rule_id != "class_mismatch"]
  expect_equal(
    paste(open$rule_id, placed_at(open), open$value, open$expected),
    "sentinel_wrong_type TREE_MEAS MEAS_ID 1 99999999 -"
  )
})

test_that("in a column whose type has no sentinels, another type's is a rogue code (m36)", {
  # sentinel_wrong_type doesn't run there, so a rogue code isn't left to it.
  spec <- fx_fish_spec()
  spec$sentinels <- spec$sentinels[spec$sentinels$data_type != "numeric"]
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$weight <- as.character(catches$weight)
  catches$weight[[1L]] <- "?"
  fish$catches <- catches
  open <- opened(fish_run(fish, list(rogue_codes = "?"), spec = spec))
  open <- open[open$rule_id != "class_mismatch"]
  expect_equal(paste(open$rule_id, placed_at(open)), "sentinel_rogue_code catches weight 1")
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
  # A name no attribute has stops pre-flight given as an argument (D18.45); set as an option
  # it warns and is ignored, so the rule has no code to look for (D18.47).
  expect_error(
    fish_run(fish, list(unknown_codes = c(colour = "red"))),
    class = "gpq_preflight_error"
  )
  withr::local_options(groundplotqc.unknown_codes = c(colour = "red"))
  expect_warning(elsewhere <- fish_run(fish), class = "gpq_preflight_warning")
  unknown <- elsewhere$results[elsewhere$results$rule_id == "species_unknown_code"]
  expect_equal(c(unknown$status, unknown$not_run_reason), c("not_run", "no_input"))
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
  before <- lapply(fish, data.table::copy)
  expect_silent(results <- fish_run(fish))
  # The caller's tables are as they were (16.2).
  for (table in names(fish)) {
    expect_identical(fish[[table]], before[[table]], info = table)
  }
  found <- opened(results)
  expect_equal(placed_at(found), c("hauls gear 1", "catches haul_id 1"))
  expect_setequal(found$rule_id, "whitespace_padding")
  expect_identical(charToRaw(found$value[[1L]]), charToRaw(padded))
  # The expected value is the value trimmed, byte by byte.
  expect_identical(charToRaw(found$expected[[1L]]), charToRaw(plain))
  expect_no_error(gpq_write_results(results, withr::local_tempdir()))
})

test_that("latin1 text is read as UTF-8; text marked as bytes matches no code (D16.36)", {
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
  before <- lapply(fish, data.table::copy)
  input <- conformance_input(fish, spec)
  context <- conformance_context(spec, NULL, list(), 2L)
  # The run refuses text marked as bytes (D16.38), so the rules and the recording, its second
  # line, are run a step inside it.
  expect_silent(found <- check_sentinels(input, context))
  state <- new_run_state()
  record_layer(found, 2L, input, context, state, "20261215T140312Z_3fa9c1")
  # The caller's tables are as they were (16.2).
  for (table in names(fish)) {
    expect_identical(fish[[table]], before[[table]], info = table)
  }
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

test_that("a whole column failing an open Layer 2 rule is one column-level issue per group (6.3)", {
  fish <- edited("hauls", "gear", 1:4, paste0(" ", fx_fish_clean()$hauls$gear))
  # Ungrouped: all 4 records fail, and at least column_level_min_n were checked.
  whole <- opened(fish_run(fish, list(column_level_min_n = 3L)), "whitespace_padding")
  expect_equal(whole$level, "column")
  expect_equal(whole$n_records, 4L)
  expect_true(is.na(whole$expected))
  # Grouped by the station's water body: each of the two groups fails whole.
  grouped <- gpq_check_conformance(
    fish, fx_fish_spec(),
    layers = 2, group_cols = "water_body",
    settings = list(column_level_min_n = 2L)
  )
  issues <- opened(grouped, "whitespace_padding")
  expect_equal(issues$level, c("column", "column"))
  expect_equal(issues$n_records, c(2L, 2L))
  expect_equal(sort(issues$water_body), sort(unique(fish$stations$water_body)))
  rows <- grouped$results[grouped$results$rule_id == "whitespace_padding"]
  rows <- rows[rows$column_name == "gear"]
  expect_equal(rows$n_fail, c(2L, 2L))
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
