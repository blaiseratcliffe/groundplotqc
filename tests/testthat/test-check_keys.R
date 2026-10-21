# Tests for Layer 4, keys and relationships (plan 6.1, 6.2; D7.3, D7.9, D16.2, D18.1, D18.3,
# D18.4, D18.10, D18.14, D18.15, D18.33 to D18.43): primary and foreign keys, the redundant
# keys key_pairs names, repeated src_*_id values, the records no root reaches, childless
# parents and the three design candidates.
#
# Task cuts (M5): task 10 writes the file with the four helpers and the tests marked "task 10",
# its first test naming its six rules only; tasks 11 to 14 add the tests marked with their
# number and their rules to the first test. Each cut keeps the tests in this order.

# The helpers of test-check_sentinels.R, with the same meanings (R25, R37), the run's layers
# reaching Layer 4.
fish_run <- function(tables, settings = list(), layers = 0:4, rules = NULL, spec = fx_fish_spec()) {
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

# ---- task 10 (the names grow with tasks 11 to 14) ----

test_that("check_keys returns an outcome list for each Layer 4 rule", {
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  context <- conformance_context(spec, NULL, list(), 4L)
  found <- check_keys(conformance_input(fish, spec), context)
  expect_named(found, c(
    "pk_duplicate", "pk_missing", "fk_orphan", "fk_unassigned", "fk_sentinel_unverified",
    "fk_not_applicable_mismatch", "src_id_duplicate", "key_incoherent", "record_unreachable",
    "parent_without_children", "design_geometry_inconsistent", "subplot_label_ambiguous",
    "design_duplicate_signature"
  ))
  # Three tables of one key column each; two foreign keys, neither of a number type.
  expect_length(found$pk_duplicate, 3L)
  expect_length(found$pk_missing, 3L)
  expect_length(found$fk_orphan, 2L)
  expect_length(found$fk_unassigned, 2L)
  expect_length(found$fk_sentinel_unverified, 0L)
  expect_equal(
    vapply(found$fk_not_applicable_mismatch, `[[`, "", "reason"), c("no_matrix", "no_matrix")
  )
  # The fish spec marks no ID column outside its keys.
  expect_length(found$src_id_duplicate, 0L)
  # No key_pairs, reach or candidate settings: one not_run row each, no_input.
  for (rule in c(
    "key_incoherent", "record_unreachable", "design_geometry_inconsistent",
    "subplot_label_ambiguous", "design_duplicate_signature"
  )) {
    expect_equal(found[[rule]][[1L]]$reason, "no_input", info = rule)
  }
  expect_length(found$parent_without_children, 2L)
})

# ---- task 10 ----

test_that("pk_duplicate fails every holder of a repeated key, never a missing one (D18.10)", {
  nbsp <- intToUtf8(0xA0)
  fish <- edited("catches", "catch_id", 2:6, c("catches_1", NA, NA, nbsp, nbsp))
  results <- fish_run(fish)
  found <- opened(results, "pk_duplicate")
  # Every holder fails, the first included; two NAs or two blanks repeat nothing: they are
  # pk_missing's.
  expect_equal(placed_at(found), c("catches catch_id 1", "catches catch_id 2"))
  expect_equal(found$value, c("catches_1", "catches_1"))
  expect_equal(placed_at(opened(results, "pk_missing")), paste("catches catch_id", 3:6))
  rows <- results$results[results$results$rule_id == "pk_duplicate"]
  # The missing keys are left alone, checked and passing (R20).
  expect_equal(rows$n_checked[rows$table_name == "catches"], 8L)
})

test_that("a composite key is compared whole, on its first part; each part checked (D18.42)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("a", "b", "v"), key_type = c("PK", "PK", "."),
    data_type = "character"
  )
  spec <- gpq_read_spec(dictionary)
  tables <- list(t = data.table::data.table(
    a = c("1", "1", "1", NA), b = c("x", "y", "x", "x"), v = c("p", "q", "r", "s")
  ))
  results <- gpq_check_conformance(tables, spec, layers = 4)
  found <- opened(results, "pk_duplicate")
  expect_equal(placed_at(found), c("t a 1", "t a 3"))
  expect_equal(found$value, c("1|x", "1|x"))
  expect_equal(placed_at(opened(results, "pk_missing")), "t a 4")
})

test_that("pk_missing finds NA, blanks and a sentinel the key can't hold (D9.24, D18.15)", {
  nbsp <- intToUtf8(0xA0)
  fish <- edited("catches", "catch_id", 1:5, c(NA, "", nbsp, "?", "~"))
  results <- fish_run(fish)
  # Only pk_missing reports them: raw_na and blank_string leave keys alone (D7.9), and no
  # sentinel is allowed in a primary key here (3.5).
  expect_equal(unique(opened(results)$rule_id), "pk_missing")
  found <- opened(results, "pk_missing")
  expect_equal(placed_at(found), paste("catches catch_id", 1:5))
  expect_equal(found$value, c(NA, "", nbsp, "?", "~"))
})

test_that("fk_orphan and fk_unassigned split a key's failures, its open sentinel waits (D18.33)", {
  fish <- edited("catches", "haul_id", 1:5, c("hauls_9", NA, "", "?", "~"))
  results <- fish_run(fish)
  orphan <- opened(results, "fk_orphan")
  expect_equal(placed_at(orphan), "catches haul_id 1")
  expect_equal(c(orphan$value, orphan$expected), c("hauls_9", "hauls"))
  expect_equal(placed_at(opened(results, "fk_unassigned")), paste("catches haul_id", 2:4))
  # Nothing else opens: an NA or blank key isn't raw_na's or blank_string's (D7.9).
  expect_setequal(unique(opened(results)$rule_id), c("fk_orphan", "fk_unassigned"))
  # "~", allowed in a foreign key, is counted as not evaluated, with no issue (D7.3, D7.7).
  rows <- results$results
  row <- rows[rows$rule_id == "fk_orphan" & rows$table_name == "catches"]
  expect_equal(c(row$n_checked, row$n_fail, row$n_not_evaluated), c(7L, 1L, 1L))
  expect_false(any(results$issues$rule_id == "fk_orphan" & results$issues$row_number == 5L))
})

test_that("a padded foreign key is Layer 2's, so fk_orphan doesn't evaluate it (D2.10)", {
  testthat::skip_on_cran()
  fish <- edited("catches", "haul_id", 1L, "hauls_1 ")
  results <- fish_run(fish)
  expect_equal(opened(results)$rule_id, "whitespace_padding")
  masked <- results$issues[results$issues$rule_id == "fk_orphan"]
  expect_equal(masked$status, "not_evaluated")
  expect_equal(masked$root_issue_id, opened(results)$issue_id)
})

test_that("a foreign key's -9 is one fk_sentinel_unverified warning, no key error (D16.2)", {
  testthat::skip_on_cran()
  tables <- fx_magp_clean()
  tables$magp_trees <- data.table::copy(tables$magp_trees)
  data.table::set(tables$magp_trees, i = 1L, j = "magp_site_id", value = -9L)
  results <- fx_caught(tables)
  found <- opened(results)
  expect_equal(found$rule_id, "fk_sentinel_unverified")
  expect_equal(c(found$table_name, found$column_name, found$value), c(
    "magp_trees", "magp_site_id", "-9"
  ))
  expect_equal(found$severity, "warning")
  # Never looked up: fk_orphan counts it as not evaluated (D18.33).
  rows <- results$results
  at <- rows$rule_id == "fk_orphan" & rows$table_name == "magp_trees" &
    rows$column_name == "magp_site_id"
  expect_equal(sum(rows$n_not_evaluated[at]), 1L)
})

test_that("fk_not_applicable_mismatch is not_run, no_matrix, per foreign key and group (D7.3)", {
  fish <- fx_fish_clean()
  results <- gpq_check_conformance(fish, fx_fish_spec(), group_cols = "gear", layers = 4)
  rows <- results$results[results$results$rule_id == "fk_not_applicable_mismatch"]
  expect_setequal(paste(rows$table_name, rows$column_name), c(
    "hauls station_id", "catches haul_id"
  ))
  expect_true(all(rows$status == "not_run" & rows$not_run_reason == "no_matrix"))
  catches <- rows[rows$table_name == "catches"]
  expect_equal(sum(catches$n_not_evaluated), nrow(fish$catches))
})

# task 10's fix round (the spec review's I2)
test_that("a sentinel a primary key may hold repeats nothing: pk_duplicate leaves it alone", {
  testthat::skip_on_cran()
  # With allowed_in_pk TRUE, "~" is neither pk_missing's nor a key to compare (D18.10 (2)).
  spec <- fx_fish_spec()
  sentinels <- data.table::copy(spec$sentinels)
  allowed <- which(sentinels$data_type == "character" & sentinels$value == "~")
  data.table::set(sentinels, i = allowed, j = "allowed_in_pk", value = TRUE)
  spec$sentinels <- sentinels
  results <- fish_run(edited("catches", "catch_id", 1:2, c("~", "~")), spec = spec)
  expect_equal(nrow(opened(results)), 0L)
  rows <- results$results[results$results$rule_id == "pk_duplicate"]
  row <- rows[rows$table_name == "catches"]
  expect_equal(c(row$n_checked, row$n_fail), c(8L, 0L))
})

# task 10's fix round (the spec review's m1)
test_that("a composite key missing a part's column is not_run, no_column, on its first part", {
  # pk_duplicate compares nothing without every part (D9.24, D18.42).
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("a", "b", "v"), key_type = c("PK", "PK", "."),
    data_type = "character"
  )
  spec <- gpq_read_spec(dictionary)
  tables <- list(t = data.table::data.table(a = c("1", "1", "2"), v = c("p", "q", "r")))
  results <- gpq_check_conformance(tables, spec, layers = 4)
  row <- results$results[results$results$rule_id == "pk_duplicate"]
  expect_equal(c(row$column_name, row$status, row$not_run_reason), c("a", "not_run", "no_column"))
  expect_equal(nrow(opened(results)), 0L)
})

# task 10's fix round (the fixture review's Minor 4)
test_that("a repeated primary key fails each holder in its own group (D18.10 (2))", {
  fish <- fx_fish_clean()
  gear <- fish$hauls$gear[match(fish$catches$haul_id, fish$hauls$haul_id)]
  other <- which(gear != gear[[1L]])[[1L]]
  fish <- edited("catches", "catch_id", other, "catches_1", fish)
  results <- gpq_check_conformance(fish, fx_fish_spec(), group_cols = "gear", layers = 4)
  expect_equal(opened(results, "pk_duplicate")$row_number, c(1L, other))
  rows <- results$results[results$results$rule_id == "pk_duplicate"]
  rows <- rows[rows$table_name == "catches"]
  expect_setequal(rows$gear[rows$n_fail == 1L], c(gear[[1L]], gear[[other]]))
  expect_equal(sum(rows$n_fail), 2L)
})

# task 10's fix round (the spec review's m3 and the fixture review's Minor 4)
test_that("a number foreign key warns once per record holding its allowed code, as text too", {
  # haul_id a whole number, read from a CSV file as text: -88 (not applicable, allowed in a
  # foreign key) written "-88.0" and "-88", and -99 (missing, refused) written "-99.0", each
  # read as its code (D18.33, R29).
  spec <- fx_fish_spec()
  attributes <- data.table::copy(spec$attributes)
  is_haul <- which(attributes$attribute_name == "haul_id")
  data.table::set(attributes, i = is_haul, j = "data_type", value = "whole")
  data.table::set(attributes, i = is_haul, j = "r_class", value = "integer")
  spec$attributes <- attributes
  fish <- fx_clean_tables(spec)
  # Named apart from the package's as_text(), which it would hide (m56).
  with_ids <- function(fish, ids) {
    catches <- data.table::copy(fish$catches)
    data.table::set(catches, j = "haul_id", value = ids)
    fish$catches <- catches
    fish
  }
  ids <- as.character(fish$catches$haul_id)
  ids[1:3] <- c("-88.0", "-88", "-99.0")
  results <- fish_run(with_ids(fish, ids), layers = 4, spec = spec)
  found <- opened(results, "fk_sentinel_unverified")
  expect_equal(placed_at(found), paste("catches haul_id", 1:2))
  expect_equal(found$value, c("-88.0", "-88"))
  expect_equal(unique(found$severity), "warning")
  expect_equal(placed_at(opened(results, "fk_unassigned")), "catches haul_id 3")
  expect_setequal(unique(opened(results)$rule_id), c("fk_sentinel_unverified", "fk_unassigned"))
  rows <- results$results
  row <- rows[rows$rule_id == "fk_orphan" & rows$table_name == "catches"]
  expect_equal(c(row$n_fail, row$n_not_evaluated), c(0L, 2L))
  # A column full of them collapses by column_level_min_n, as usual (D18.33).
  every <- rep("-88", nrow(fish$catches))
  full <- fish_run(
    with_ids(fish, every),
    settings = list(column_level_min_n = 5L), layers = 4, spec = spec
  )
  collapsed <- opened(full, "fk_sentinel_unverified")
  expect_equal(collapsed$level, "column")
  expect_equal(collapsed$n_records, nrow(fish$catches))
})

# task 10's fix round (the spec review's m3)
test_that("a foreign key to a composite primary key isn't followed; fk_unassigned checks it", {
  # keys gives it no reference attribute, so fk_orphan has nothing to check (D18.42).
  dictionary <- data.frame(
    table_name = c("parent", "parent", "child", "child"),
    attribute_name = c("a", "b", "child_id", "a"), key_type = c("PK", "PK", "PK", "FK"),
    reference_table = c(NA, NA, NA, "parent"), data_type = "character"
  )
  spec <- gpq_read_spec(dictionary)
  expect_true(is.na(spec$keys$reference_attribute[spec$keys$key_type == "FK"]))
  tables <- list(
    parent = data.table::data.table(a = c("1", "1"), b = c("x", "y")),
    child = data.table::data.table(child_id = c("c1", "c2", "c3"), a = c("1", "9", NA))
  )
  results <- gpq_check_conformance(tables, spec, layers = 4)
  # "9" names no parent's a, but isn't looked up.
  expect_false(any(results$results$rule_id == "fk_orphan"))
  found <- opened(results)
  expect_equal(paste(found$rule_id, placed_at(found)), "fk_unassigned child a 3")
})

# task 10's fix round (the datatable review's F1 and the fixture review's Minor 4)
test_that("key_rows() writes stored numbers as text only near a code, its rows unchanged (16.2)", {
  # A date-typed key stored as plain numbers: a code holds where the number's text is the
  # code, so a real 0 isn't "00000000" (m39) and 99999999.00001, near 99999999, isn't
  # "99999999"; only the values near a code that reads as a number are written as text.
  row <- data.table::data.table(family = "date")
  sentinels <- data.table::data.table(
    data_type = "date", value = c("00000000", "99999999", "X"),
    allowed_in_pk = FALSE, allowed_in_fk = c(FALSE, TRUE, TRUE)
  )
  x <- c(0, 99999999, 99999999.00001, 20200101, NA, 99999999)
  real_text <- value_text
  written <- list()
  testthat::local_mocked_bindings(value_text = function(x) {
    written[[length(written) + 1L]] <<- x
    real_text(x)
  })
  found <- key_rows(x, row, sentinels, "allowed_in_fk")
  expect_equal(found, list(missing = 5L, open = c(2L, 6L)))
  # The rows the whole column written as text gives.
  text <- real_text(x)
  expect_equal(found$missing, sort(c(which(is.na(x)), which(text %in% "00000000"))))
  expect_equal(found$open, which(text %in% c("99999999", "X")))
  expect_equal(written, list(0, c(99999999, 99999999.00001, 99999999)))
  # Stored as integers, the same (the finish's minors pass, m56).
  integers <- c(0L, 99999999L, 20200101L, NA, 99999999L)
  written <- list()
  expect_equal(
    key_rows(integers, row, sentinels, "allowed_in_fk"), list(missing = 4L, open = c(2L, 5L))
  )
  expect_equal(written, list(0L, c(99999999L, 99999999L)))
})

# the finish's minors pass (m55, D18.64)
test_that("key_rows() reads integer64 as plain stored numbers, as text only near a code", {
  testthat::skip_if_not_installed("bit64")
  # A date-typed key stored as integer64, as a DBI query of a BIGINT column returns it: a code
  # holds where the value's text is the code, so a real 0 isn't "00000000" (m39), and only the
  # values near a code that reads as a number are written as text (16.2).
  row <- data.table::data.table(family = "date")
  sentinels <- data.table::data.table(
    data_type = "date", value = c("00000000", "99999999", "X"),
    allowed_in_pk = FALSE, allowed_in_fk = c(FALSE, TRUE, TRUE)
  )
  x <- bit64::as.integer64(c(0, 99999999, 20200101, NA, 99999999))
  real_text <- value_text
  written <- list()
  testthat::local_mocked_bindings(value_text = function(x) {
    written[[length(written) + 1L]] <<- as.character(x)
    real_text(x)
  })
  found <- key_rows(x, row, sentinels, "allowed_in_fk")
  expect_equal(found, list(missing = 4L, open = c(2L, 5L)))
  expect_equal(written, list("0", c("99999999", "99999999")))
  # A number-typed key compares its codes as numbers, as stored numbers do.
  numbers <- data.table::data.table(
    data_type = "numeric", value = c("-9", "-1.0"), allowed_in_pk = FALSE,
    allowed_in_fk = c(FALSE, TRUE)
  )
  wide <- bit64::as.integer64(c(-9, 4, -1, NA))
  expect_equal(
    key_rows(wide, data.table::data.table(family = "numeric"), numbers, "allowed_in_fk"),
    key_rows(c(-9, 4, -1, NA), data.table::data.table(family = "numeric"), numbers, "allowed_in_fk")
  )
  expect_equal(
    key_rows(wide, data.table::data.table(family = "numeric"), numbers, "allowed_in_fk"),
    list(missing = c(1L, 4L), open = 3L)
  )
})

test_that("an integer64 key's codes are pk_missing's, fk_unassigned's or waiting (m55, D18.64)", {
  testthat::skip_if_not_installed("bit64")
  # A text-typed key stored as integer64 whose codes read as numbers: "-2" is refused, and
  # "99999999" allowed in a foreign key (3.5). Each is read as stored plain numbers read it.
  dictionary <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "character"
  )
  spec <- gpq_read_spec(
    dictionary,
    sentinels = gpq_sentinels(character = c(missing = "-2", not_applicable = "99999999"))
  )
  run <- function(stored) {
    tables <- list(
      p = data.table::data.table(p_id = stored(c(1, 2))),
      k = data.table::data.table(
        k_id = stored(c(10, 11, 12, 13, -2, -2)), p_id = stored(c(1, -2, 99999999, 7, 1, 2))
      )
    )
    gpq_check_conformance(tables, spec, layers = 4)
  }
  results <- run(bit64::as.integer64)
  found <- opened(results)
  # Two keys of -2 repeat nothing: they are pk_missing's; 99999999 is never looked up.
  expect_equal(paste(found$rule_id, placed_at(found), found$value), c(
    "pk_missing k k_id 5 -2", "pk_missing k k_id 6 -2", "fk_orphan k p_id 4 7",
    "fk_unassigned k p_id 2 -2"
  ))
  rows <- results$results
  row <- rows[rows$rule_id == "fk_orphan"]
  expect_equal(c(row$n_checked, row$n_fail, row$n_not_evaluated), c(5L, 1L, 1L))
  plain <- opened(run(identity))
  expect_equal(
    paste(plain$rule_id, placed_at(plain), plain$value),
    paste(found$rule_id, placed_at(found), found$value)
  )
})

test_that("pk_duplicate fails every holder of a repeated integer64 key, the first too", {
  # bit64's duplicated() ignores fromLast, so the first holder was missed (D18.10 (2)).
  testthat::skip_if_not_installed("bit64")
  dictionary <- data.frame(
    table_name = "k", attribute_name = c("k_id", "v"), key_type = c("PK", "."),
    data_type = c("integer", "character")
  )
  tables <- list(k = data.table::data.table(
    k_id = bit64::as.integer64(c(10, 11, 11, 12, 11)), v = c("a", "b", "c", "d", "e")
  ))
  results <- gpq_check_conformance(tables, gpq_read_spec(dictionary), layers = 4)
  found <- opened(results, "pk_duplicate")
  expect_equal(placed_at(found), paste("k k_id", c(2L, 3L, 5L)))
  expect_equal(found$value, rep("11", 3L))
  expect_true(all(found$pk_positional))
})

# the Codex fix round (D18.111): number-typed keys stored as integer64, as a DBI query of a
# BIGINT column returns them. A double holds whole numbers exactly only to 2^53, so 2^53 + 1
# read as one is 2^53.
test_that("an integer64 child key a double can't hold joins nothing, failing coercion_loss", {
  testthat::skip_if_not_installed("bit64")
  dictionary <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "numeric"
  )
  spec <- gpq_read_spec(dictionary)
  # Codex's example: the first child names 2^53 + 1, the parent holds 2^53.
  tables <- list(
    p = data.table::data.table(p_id = bit64::as.integer64("9007199254740992")),
    k = data.table::data.table(
      k_id = bit64::as.integer64(1:2),
      p_id = bit64::as.integer64(c("9007199254740993", "9007199254740992"))
    )
  )
  # Layer 4 alone: the first child names no parent.
  results <- gpq_check_conformance(tables, spec, layers = 4)
  found <- opened(results)
  expect_equal(
    paste(found$rule_id, placed_at(found), found$value), "fk_orphan k p_id 1 9007199254740993"
  )
  # Layers 0 to 4: coercion_loss reports it, and Layer 4 leaves it not evaluated (6.2).
  results <- gpq_check_conformance(tables, spec, layers = 0:4)
  found <- opened(results)
  found <- found[found$rule_id != "class_mismatch"]
  expect_equal(
    paste(found$rule_id, placed_at(found), found$value, found$expected),
    "coercion_loss k p_id 1 9007199254740993 double"
  )
  issues <- results$issues
  masked <- issues[issues$status == "not_evaluated" & issues$layer == 4L]
  expect_equal(
    paste(masked$rule_id, placed_at(masked)),
    paste(c("fk_orphan", "fk_unassigned", "fk_sentinel_unverified"), "k p_id 1")
  )
  expect_equal(unique(masked$root_issue_id), found$issue_id)
  row <- results$results[results$results$rule_id == "fk_orphan"]
  expect_equal(c(row$n_checked, row$n_fail, row$n_not_evaluated), c(1L, 0L, 1L))
})

test_that("an integer64 parent key a double can't hold takes no child (D18.111)", {
  testthat::skip_if_not_installed("bit64")
  dictionary <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "numeric"
  )
  spec <- gpq_read_spec(dictionary)
  # The first parent holds 2^53 + 1, the first child names 2^53, which no parent holds.
  tables <- list(
    p = data.table::data.table(p_id = bit64::as.integer64(c("9007199254740993", "5"))),
    k = data.table::data.table(
      k_id = bit64::as.integer64(1:2), p_id = bit64::as.integer64(c("9007199254740992", "5"))
    )
  )
  # Layer 4 alone: the child is an orphan, and the first parent has no child.
  results <- gpq_check_conformance(tables, spec, layers = 4)
  found <- opened(results)
  expect_equal(
    paste(found$rule_id, placed_at(found), found$value), "fk_orphan k p_id 1 9007199254740992"
  )
  row <- results$results[results$results$rule_id == "parent_without_children"]
  expect_equal(c(row$n_checked, row$n_fail), c(2L, 1L))
  # Layers 0 to 4: coercion_loss reports the parent's key, which Layer 4 leaves not evaluated;
  # the child's own key is still an orphan (D18.91).
  results <- gpq_check_conformance(tables, spec, layers = 0:4)
  found <- opened(results)
  found <- found[found$rule_id != "class_mismatch"]
  expect_equal(paste(found$rule_id, placed_at(found), found$value, found$expected), c(
    "coercion_loss p p_id 1 9007199254740993 double", "fk_orphan k p_id 1 9007199254740992 p"
  ))
  issues <- results$issues
  masked <- issues[issues$status == "not_evaluated" & issues$layer == 4L]
  expect_equal(
    paste(masked$rule_id, placed_at(masked)), c("pk_duplicate p p_id 1", "pk_missing p p_id 1")
  )
  expect_equal(unique(masked$root_issue_id), found$issue_id[[1L]])
})

test_that("integer64 parent keys 2^53 and 2^53 + 1 don't collide in the joins (D18.111)", {
  testthat::skip_if_not_installed("bit64")
  dictionary <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "numeric"
  )
  spec <- gpq_read_spec(dictionary)
  # Both children name 2^53, the first parent's key; the second parent's is 2^53 + 1.
  tables <- list(
    p = data.table::data.table(
      p_id = bit64::as.integer64(c("9007199254740992", "9007199254740993"))
    ),
    k = data.table::data.table(
      k_id = bit64::as.integer64(1:2),
      p_id = bit64::as.integer64(c("9007199254740992", "9007199254740992"))
    )
  )
  # The second parent holds no key the first holds: it is no holder of the first's key, and
  # no child names it. pk_duplicate compares the keys as stored (R147).
  expect_identical(pk_holders(conformance_input(tables, spec), "p"), c(1L, NA))
  results <- gpq_check_conformance(tables, spec, layers = 4)
  expect_equal(nrow(opened(results)), 0L)
  rows <- results$results
  row <- rows[rows$rule_id == "pk_duplicate" & rows$table_name == "p"]
  expect_equal(c(row$n_checked, row$n_fail), c(2L, 0L))
  row <- rows[rows$rule_id == "parent_without_children"]
  expect_equal(c(row$n_checked, row$n_fail), c(2L, 1L))
  row <- rows[rows$rule_id == "fk_orphan"]
  expect_equal(c(row$n_checked, row$n_fail), c(2L, 0L))
  # Layers 0 to 4: coercion_loss reports the second parent's key, which Layer 4 leaves not
  # evaluated; both children join the first parent.
  results <- gpq_check_conformance(tables, spec, layers = 0:4)
  found <- opened(results)
  found <- found[found$rule_id != "class_mismatch"]
  expect_equal(
    paste(found$rule_id, placed_at(found), found$value, found$expected),
    "coercion_loss p p_id 2 9007199254740993 double"
  )
  issues <- results$issues
  masked <- issues[issues$status == "not_evaluated" & issues$layer == 4L]
  expect_equal(
    paste(masked$rule_id, placed_at(masked)), c("pk_duplicate p p_id 2", "pk_missing p p_id 2")
  )
  row <- results$results[results$results$rule_id == "fk_orphan"]
  expect_equal(c(row$n_checked, row$n_fail), c(2L, 0L))
})

# ---- task 11 ----

test_that("key_incoherent compares each pair's sides, the failing pairs labelled (D18.1, D18.40)", {
  dictionary <- utils::read.csv(
    example_file("fish_dictionary.csv"),
    colClasses = "character", na.strings = ""
  )
  # The catches carry three copies: their station, their haul's gear and its date.
  extra <- data.frame(
    table = "catches", field = c("station_id", "gear", "haul_date"),
    kind = c("text", "text", "day"), key = c("fk", NA, NA), parent = c("stations", NA, NA),
    codes = c(NA, "y", NA), notes = c("Station.", "Gear.", "Date.")
  )
  # The dictionary's range and format columns, blank for the three (task 3).
  extra[setdiff(names(dictionary), names(extra))] <- NA_character_
  spec <- gpq_read_spec(
    dictionary = rbind(dictionary, extra), code_lists = fx_fish_code_lists(),
    key_pairs = data.frame(
      table_name = "catches", left = c("station_id", "haul_id > gear", "haul_id > haul_date"),
      right = c("haul_id > station_id", "gear", "haul_date"), note = NA
    ),
    column_map = fx_fish_column_map(),
    type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "?", not_applicable = "~"),
      date = c(missing = "?", not_applicable = "~")
    )
  )
  # fx_clean_tables() copies the one-attribute left side (station_id); the other two copies
  # are set here from each catch's haul.
  fish <- fx_clean_tables(spec)
  catches <- data.table::copy(fish$catches)
  haul <- match(catches$haul_id, fish$hauls$haul_id)
  data.table::set(catches, j = "gear", value = fish$hauls$gear[haul])
  data.table::set(catches, j = "haul_date", value = fish$hauls$haul_date[haul])
  fish$catches <- catches
  expect_equal(nrow(opened(fish_run(fish, spec = spec))), 0L)
  # A catch at the wrong station; one whose gear and date both differ from its haul's; one
  # whose gear alone does.
  # A gear neither catch 2's haul nor catch 3's has.
  other_gear <- setdiff(c("GN", "TN", "EF"), catches$gear[2:3])[[1L]]
  broken <- edited("catches", "station_id", 1L, "stations_2", fish)
  broken <- edited("catches", "gear", 2:3, c(other_gear, other_gear), broken)
  broken <- edited("catches", "haul_date", 2L, "2001-01-01", broken)
  found <- opened(fish_run(broken, spec = spec), "key_incoherent")
  expect_equal(placed_at(found), c(
    "catches station_id 1", "catches haul_id 2", "catches haul_id 3"
  ))
  expect_equal(found$value[[1L]], "station_id=stations_2")
  expect_equal(found$expected[[1L]], "station_id=stations_1")
  # Pairs sharing a first attribute are one outcome: only the failing pairs, in key_pairs'
  # order, each labelled by its left side's last attribute (D18.40).
  expect_equal(found$value[[2L]], paste0(
    "gear=", catches$gear[[2L]], "; haul_date=", catches$haul_date[[2L]]
  ))
  expect_equal(found$expected[[2L]], paste0("gear=", other_gear, "; haul_date=2001-01-01"))
  expect_equal(found$value[[3L]], paste0("gear=", catches$gear[[3L]]))
  # A broken chain or a sentinel side isn't compared; a record none of whose pairs was
  # compared isn't checked (D18.1, D18.21).
  unlinked <- edited("catches", "haul_id", 4L, NA_character_, fish)
  unlinked <- edited("catches", "station_id", 4L, "~", unlinked)
  results <- fish_run(unlinked, spec = spec)
  expect_equal(nrow(opened(results, "key_incoherent")), 0L)
  rows <- results$results[results$results$rule_id == "key_incoherent"]
  expect_equal(rows$n_checked, c(7L, 7L))
})

test_that("key_incoherent without key_pairs is one not_run row, no_input", {
  testthat::skip_on_cran()
  results <- fish_run(fx_fish_clean())
  row <- results$results[results$results$rule_id == "key_incoherent"]
  expect_equal(c(row$table_name, row$status, row$not_run_reason), c("*", "not_run", "no_input"))
})

test_that("the clean bases' key_pairs copies agree with their parents (D18.30, D18.41)", {
  testthat::skip_on_cran()
  results <- fx_caught(fx_magp_clean())
  rows <- results$results[results$results$rule_id == "key_incoherent"]
  # One row per table and first attribute of MAGPlot's 21 pairs, all passing.
  pairs <- magp_spec()$key_pairs
  heads <- unique(paste(pairs$table_name, sub(" > .*$", "", pairs$left)))
  expect_setequal(unique(paste(rows$table_name, rows$column_name)), heads)
  expect_true(all(rows$status == "pass"))
})

# task 11's fix round (the datatable review's F1)
test_that("key_incoherent compares mixed storage as text, each distinct value written once", {
  # Copies read from a CSV file as text against their parent's integer and Date columns.
  dictionary <- data.frame(
    table_name = c("parent", "parent", "parent", "child", "child", "child", "child"),
    attribute_name = c("p_id", "n", "day", "c_id", "p_id", "n", "day"),
    key_type = c("PK", NA, NA, "PK", "FK", NA, NA),
    reference_table = c(NA, NA, NA, NA, "parent", NA, NA),
    data_type = c("character", "integer", "date", "character", "character", "integer", "date")
  )
  spec <- gpq_read_spec(dictionary, key_pairs = data.frame(
    table_name = "child", left = c("n", "day"), right = c("p_id > n", "p_id > day"), note = NA
  ))
  tables <- list(
    parent = data.table::data.table(
      p_id = c("a", "b", "c", "d"), n = c(1L, 2L, 7L, 4L),
      day = as.Date(c("2020-06-01", "2020-06-02", "2020-06-03", "2020-06-04"))
    ),
    # Row 4, both copies missing, is the only row to reach parent d.
    child = data.table::data.table(
      c_id = paste0("c", 1:6), p_id = c("a", "b", "a", "d", "b", "c"),
      n = c("1", "2", "9", NA, "5", "07"),
      day = c("2020-06-01", "2020-06-09", "2020-06-01", NA, "2020-06-02", "2020-06-03")
    )
  )
  real_text <- value_text
  written <- list()
  testthat::local_mocked_bindings(value_text = function(x) {
    written[[length(written) + 1L]] <<- x
    real_text(x)
  })
  results <- gpq_check_conformance(tables, spec, layers = 4)
  found <- opened(results, "key_incoherent")
  # As text, "07" isn't 7; row 4 is checked by neither pair (D18.1).
  expect_equal(placed_at(found), c("child n 3", "child n 5", "child n 6", "child day 2"))
  expect_equal(found$value, c("n=9", "n=5", "n=07", "day=2020-06-09"))
  expect_equal(found$expected, c("n=1", "n=2", "n=7", "day=2020-06-02"))
  rows <- results$results[results$results$rule_id == "key_incoherent"]
  expect_equal(rows$n_checked, c(5L, 5L))
  # Written as text (16.2): each distinct value the compared rows reach, once, then the
  # failing rows' sides; never the skipped row's parent d.
  expect_equal(Filter(is.integer, written), list(c(1L, 2L, 7L), c(1L, 2L, 7L)))
  expect_equal(
    Filter(function(x) inherits(x, "Date"), written),
    list(as.Date(c("2020-06-01", "2020-06-02", "2020-06-03")), as.Date("2020-06-02"))
  )
})

# the finish's minors pass (m67)
test_that("doubles differing in their last digits stay apart, whatever the session's rounding", {
  # data.table's setNumericRounding() above 0 makes its ranks take such doubles as one; Layer 4
  # reads them exactly, and leaves the session's setting as it was.
  rounding <- data.table::setNumericRounding(2L)
  withr::defer(data.table::setNumericRounding(rounding))
  near <- c(123456789012.345, 123456789012.346)
  dictionary <- data.frame(
    table_name = c("parent", "parent", "child", "child", "child"),
    attribute_name = c("p_id", "x", "c_id", "p_id", "x"),
    key_type = c("PK", NA, "PK", "FK", NA), reference_table = c(NA, NA, NA, "parent", NA),
    data_type = c("character", "numeric", "character", "character", "numeric")
  )
  spec <- gpq_read_spec(dictionary, key_pairs = data.frame(
    table_name = "child", left = "x", right = "p_id > x", note = NA
  ))
  # key_incoherent: each child's copy, read as text, is its parent's.
  tables <- list(
    parent = data.table::data.table(p_id = c("a", "b"), x = near),
    child = data.table::data.table(
      c_id = c("c1", "c2"), p_id = c("a", "b"), x = c("123456789012.345", "123456789012.346")
    )
  )
  expect_equal(nrow(opened(gpq_check_conformance(tables, spec, layers = 4))), 0L)
  expect_identical(data.table::getNumericRounding(), 2L)
  # design_geometry_inconsistent: two groups whose values differ in their last digits.
  tables$parent$x <- near
  tables$child <- data.table::data.table(c_id = c("c1", "c2"), p_id = c("a", "a"), x = near)
  settings <- list(
    design_geometry_table = "child", design_geometry_by = "x", design_geometry_columns = "c_id"
  )
  results <- gpq_check_conformance(tables, spec, layers = 4, settings = settings)
  expect_equal(nrow(opened(results, "design_geometry_inconsistent")), 0L)
  # The session's setting is put back when a check stops too (the minors pass's fix round,
  # dt M3).
  testthat::local_mocked_bindings(key_rows = function(...) stop("a check stopped"))
  expect_error(gpq_check_conformance(tables, spec, layers = 4), "a check stopped", fixed = TRUE)
  expect_identical(data.table::getNumericRounding(), 2L)
})

# the finish's minors pass (m68)
test_that("key_incoherent compares an integer64 or POSIXct side, or one attribute, as text", {
  testthat::skip_if_not_installed("bit64")
  # Copies read as text against a parent's integer64 and POSIXct columns, and an integer64
  # column against a text one of its own table (a right side of one attribute).
  dictionary <- data.frame(
    table_name = c(rep("parent", 3L), rep("child", 5L)),
    attribute_name = c("p_id", "n", "t", "c_id", "p_id", "n", "t", "m"),
    key_type = c("PK", NA, NA, "PK", "FK", NA, NA, NA),
    reference_table = c(NA, NA, NA, NA, "parent", NA, NA, NA),
    data_type = c(
      "character", "integer", "date", "character", "character", "integer", "date", "integer"
    )
  )
  spec <- gpq_read_spec(dictionary, key_pairs = data.frame(
    table_name = "child", left = c("n", "t", "m"), right = c("p_id > n", "p_id > t", "n"),
    note = NA
  ))
  tables <- list(
    parent = data.table::data.table(
      p_id = c("a", "b"), n = bit64::as.integer64(c(5, 6)),
      t = as.POSIXct(c("2020-06-01", "2020-06-02"), tz = "UTC")
    ),
    child = data.table::data.table(
      c_id = c("c1", "c2", "c3"), p_id = c("a", "b", "a"), n = c("5", "7", "5"),
      t = c("2020-06-01", "2020-06-02", "2020-06-09"), m = bit64::as.integer64(c(5, 7, 6))
    )
  )
  found <- opened(gpq_check_conformance(tables, spec, layers = 4), "key_incoherent")
  expect_equal(placed_at(found), c("child n 2", "child t 3", "child m 3"))
  expect_equal(found$value, c("n=7", "t=2020-06-09", "m=6"))
  expect_equal(found$expected, c("n=6", "t=2020-06-01", "m=5"))
})

# ---- task 12 ----

test_that("src_id_duplicate fails every holder of a repeated ID within its report unit (D18.3)", {
  spec <- fx_fish_spec(id_pattern = "^station_name$")
  fish <- edited("stations", "station_name", 1:2, c("same", "same"))
  # Without group columns the stations are one pool. Layer 4 alone, as the toy's pattern for
  # station_name refuses "same" in Layer 3, which would mask it.
  found <- opened(fish_run(fish, spec = spec, layers = 4), "src_id_duplicate")
  expect_equal(placed_at(found), c("stations station_name 1", "stations station_name 2"))
  expect_equal(found$value, c("same", "same"))
  # Each station in its own report unit, its water body: nothing repeats within one.
  by_water <- gpq_check_conformance(fish, spec, group_cols = "water_body", layers = 4)
  expect_equal(nrow(opened(by_water, "src_id_duplicate")), 0L)
  # The sentinels, NA and blanks are no IDs: never compared, checked and passing (D18.35).
  marks <- edited("stations", "station_name", 1:2, c("?", "?"))
  rows <- fish_run(marks, spec = spec, layers = 4)$results
  row <- rows[rows$rule_id == "src_id_duplicate"]
  expect_equal(c(row$n_checked, row$n_fail), c(2L, 0L))
})

test_that("a record whose report unit is \"*\" isn't checked: its group none_checked (D18.35)", {
  spec <- fx_fish_spec(id_pattern = "^station_name$")
  fish <- edited("stations", "water_body", 1:2, c(NA, NA))
  fish <- edited("stations", "station_name", 1:2, c("same", "same"), fish)
  results <- gpq_check_conformance(fish, spec, group_cols = "water_body", layers = 4)
  expect_equal(nrow(opened(results, "src_id_duplicate")), 0L)
  row <- results$results[results$results$rule_id == "src_id_duplicate"]
  expect_equal(c(row$water_body, row$status, row$not_run_reason), c("*", "not_run", "none_checked"))
  expect_equal(c(row$n_checked, row$n_not_evaluated), c(0L, 2L))
})

test_that("a contributor's documented one-to-many ID isn't warned; another's is (D18.3, D18.6)", {
  testthat::skip_on_cran()
  spec <- magp_spec()
  tables <- fx_magp_clean()
  labels <- record_labels(conformance_input(tables, spec), "magp_trees")
  # Each tree's A2 label, through its dataset's contributor and the contributor sheet.
  expect_true(all(labels %chin% c("AB", "BC")))
  ab <- which(labels == "AB")[1:2]
  bc <- which(labels == "BC")[1:2]
  trees <- data.table::copy(tables$magp_trees)
  data.table::set(trees, i = c(ab, bc), j = "src_tree_id", value = c("a1", "a1", "b1", "b1"))
  tables$magp_trees <- trees
  # BC's A2 gives src_tree_id an ID (A2!D13), so its row applies (D18.36).
  spec$one_to_many <- data.table::data.table(
    contributor_label = "BC", table_name = "magp_trees", attribute_name = "src_tree_id",
    note = NA_character_, source_cell = NA_character_
  )
  results <- withCallingHandlers(
    gpq_check_conformance(
      fx_mark_fixture(tables), spec,
      group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
      rules = magp_rules(), layers = 4
    ),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  found <- opened(results, "src_id_duplicate")
  expect_equal(found$row_number, ab)
  expect_equal(found$value, c("a1", "a1"))
  # A row pre-flight finds can't apply is left unapplied: documented for AB, which isn't a
  # label A2 uses, so the row isn't applied, AB's repeats still warn and BC's now do too
  # (D18.36).
  spec$one_to_many$contributor_label <- "AB"
  results <- withCallingHandlers(
    gpq_check_conformance(
      fx_mark_fixture(tables), spec,
      group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
      rules = magp_rules(), layers = 4
    ),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  expect_equal(sort(opened(results, "src_id_duplicate")$row_number), sort(c(ab, bc)))
})

# task 12's fix round (the datatable review's Minors 1 and 3)
test_that("each table's labels are found once, through the key's own table (D18.6, 16.2)", {
  # Sites reach their contributor through their dataset; both of a site's IDs are documented
  # for ON, so ON's repeats are left alone and AB's warn. Notes reach no contributor.
  dictionary <- data.frame(
    table_name = c("datasets", "datasets", "sites", "sites", "sites", "sites", "notes"),
    attribute_name = c(
      "dataset_id", "contributor_id", "site_id", "dataset_id", "src_site_id", "src_plot_id",
      "note_id"
    ),
    key_type = c("PK", NA, "PK", "FK", NA, NA, "PK"),
    reference_table = c(NA, NA, NA, "datasets", NA, NA, NA),
    appendix = c(NA, NA, NA, NA, "A2", "A2", NA),
    data_type = c(
      "character", "character", "integer", "character", "character", "character", "character"
    )
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
    lineage_spec = data.frame(
      contributor_label = c("ON", "ON", "AB", "AB"), table_name = "sites",
      attribute_name = c("src_site_id", "src_plot_id", "src_site_id", "src_plot_id"),
      spec_type = "id", source_text = c("site.id", "plot.id", "site.id", "plot.id"), note = NA,
      source_cell = c("A2!D2", "A2!D3", "A2!F2", "A2!F3")
    ),
    id_pattern = "^src_.*_id$",
    id_bands = list(
      sheet = "ranges", label_col = "contributor", start_col = "v2_start", end_col = "v2_end",
      labels_from = c(sheet = "contributor", column = "abbreviated_name"),
      reserved_pattern = NULL, table = "sites", attribute = "site_id",
      key = "dataset_id > contributor_id"
    ),
    one_to_many = data.frame(
      contributor_label = "ON", table_name = "sites",
      attribute_name = c("src_site_id", "src_plot_id"), note = NA
    ),
    column_map = gpq_column_map(lineage_flag = c(column = "appendix", value = "A2"))
  )
  tables <- list(
    datasets = data.table::data.table(
      dataset_id = c("d1", "d2"), contributor_id = c("130", "100")
    ),
    sites = data.table::data.table(
      site_id = c(1L, 2L, 101L, 102L), dataset_id = c("d1", "d1", "d2", "d2"),
      src_site_id = c("a", "a", "b", "b"), src_plot_id = c("p", "p", "q", "q")
    ),
    notes = data.table::data.table(note_id = "n1")
  )
  input <- conformance_input(tables, spec)
  # Through a foreign key, from the key's own table, and from a table that reaches no holder.
  expect_equal(record_labels(input, "sites"), c("ON", "ON", "AB", "AB"))
  expect_equal(record_labels(input, "datasets"), c("ON", "AB"))
  expect_equal(record_labels(input, "notes"), NA_character_)
  # The home key stored as numbers, a factor or integer64 reads as its text does; a key NA or
  # blank, and a site whose dataset names no row, reach no label (the finish's minors pass,
  # m72).
  labels_of <- function(contributor_id) {
    stored <- list(
      datasets = data.table::data.table(
        dataset_id = c("d1", "d2", "d3", "d4"), contributor_id = contributor_id
      ),
      sites = data.table::data.table(
        site_id = 1:5, dataset_id = c("d1", "d2", "d3", "d4", "d9"), src_site_id = "s",
        src_plot_id = "p"
      )
    )
    record_labels(conformance_input(stored, spec), "sites")
  }
  reached <- c("ON", "AB", NA, NA, NA)
  expect_equal(labels_of(c("130", "100", NA, " ")), reached)
  expect_equal(labels_of(c(130, 100, NA, NA)), reached)
  expect_equal(labels_of(factor(c("130", "100", NA, " "))), reached)
  if (requireNamespace("bit64", quietly = TRUE)) {
    expect_equal(labels_of(bit64::as.integer64(c(130, 100, NA, NA))), reached)
  }
  real_labels <- record_labels
  calls <- character()
  testthat::local_mocked_bindings(record_labels = function(input, table) {
    calls <<- c(calls, table)
    real_labels(input, table)
  })
  results <- withCallingHandlers(
    gpq_check_conformance(tables, spec, layers = 4),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  expect_equal(placed_at(opened(results, "src_id_duplicate")), c(
    "sites src_site_id 3", "sites src_site_id 4", "sites src_plot_id 3", "sites src_plot_id 4"
  ))
  # The sites' labels are found once for both their documented IDs.
  expect_equal(calls, "sites")
})

# ---- task 13 ----

test_that("record_unreachable reports unmeasured records from below, once (D18.4, D18.43)", {
  testthat::skip_on_cran()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  tables <- fx_mark_fixture(tables)
  # Tree 2 never measured: its measurements now name tree 1, of the same subplot.
  meas <- tables$magp_tree_meas
  data.table::set(
    meas,
    i = which(meas$magp_tree_id == tables$magp_trees$magp_tree_id[[2L]]),
    j = "magp_tree_id", value = tables$magp_trees$magp_tree_id[[1L]]
  )
  # Site 8 never visited: its visits removed with everything measured on them, so its plots,
  # subplots and trees are unmeasured too.
  site <- tables$magp_sites$magp_site_id[[8L]]
  gone <- tables$magp_site_visits$magp_visit_id[tables$magp_site_visits$magp_site_id == site]
  plot_meas <- tables$magp_plot_meas$magp_plotmeas_id[tables$magp_plot_meas$magp_visit_id %in% gone]
  subp <- tables$magp_subplot_meas$magp_subpmeas_id[
    tables$magp_subplot_meas$magp_plotmeas_id %in% plot_meas
  ]
  tree_meas <- meas$magp_treemeas_id[meas$magp_subpmeas_id %in% subp]
  keep <- function(table, column, out) {
    tables[[table]] <<- tables[[table]][!tables[[table]][[column]] %in% out]
  }
  keep("magp_site_visits", "magp_visit_id", gone)
  keep("magp_plot_meas", "magp_plotmeas_id", plot_meas)
  keep("magp_subplot_meas", "magp_subpmeas_id", subp)
  keep("magp_tree_meas", "magp_treemeas_id", tree_meas)
  keep("magp_tree_cond", "magp_treemeas_id", tree_meas)
  keep("magp_tree_age", "magp_treemeas_id", tree_meas)
  keep("magp_tally_trees", "magp_subpmeas_id", subp)
  tables <- fx_mark_fixture(tables)
  results <- fx_caught(tables)
  found <- opened(results, "record_unreachable")
  # The site, its plots, subplots and trees each once, and tree 2; its disturbance and
  # treatment are under the unreached site, so left alone (unchecked).
  expect_setequal(unique(found$table_name), c(
    "magp_sites", "magp_plots", "magp_subplots", "magp_trees"
  ))
  expect_equal(sum(found$table_name == "magp_sites"), 1L)
  expect_true(tables$magp_trees$magp_tree_id[[2L]] %in% found$record_pk)
  # Trees are warnings in MAGPlot's rule set (D18.13); sites, plots and subplots errors.
  expect_equal(unique(found$severity[found$table_name == "magp_trees"]), "warning")
  expect_equal(unique(found$severity[found$table_name == "magp_sites"]), "error")
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  dist <- rows[rows$table_name == "magp_site_dist"]
  # Each of site 8's disturbances is under it, so unchecked; the others are checked.
  expect_equal(sum(dist$n_checked), sum(tables$magp_site_dist$magp_site_id != site))
  expect_equal(sum(dist$n_fail), 0L)
})

test_that("a record with no key isn't evaluated, rooted at pk_missing; a repeat reaches both", {
  testthat::skip_on_cran()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  # Tree 3's key missing: no measurement can name it (D18.14).
  data.table::set(tables$magp_trees, i = 3L, j = "magp_tree_id", value = NA_character_)
  # Tree 4 takes tree 5's key: a measurement of either reaches both holders (D18.14).
  data.table::set(
    tables$magp_trees,
    i = 4L, j = "magp_tree_id", value = tables$magp_trees$magp_tree_id[[5L]]
  )
  results <- fx_caught(fx_mark_fixture(tables))
  issues <- results$issues
  unreachable <- issues[issues$rule_id == "record_unreachable" & issues$table_name == "magp_trees"]
  missing <- issues[issues$rule_id == "pk_missing" & issues$table_name == "magp_trees"]
  expect_equal(unreachable$row_number[unreachable$status == "not_evaluated"], 3L)
  expect_equal(
    unreachable$root_issue_id[unreachable$status == "not_evaluated"], missing$issue_id
  )
  expect_false(any(unreachable$row_number %in% 4:5 & unreachable$status == "open"))
})

test_that("record_unreachable without its root table is not_run, no_table (D18.43)", {
  testthat::skip_on_cran()
  tables <- fx_magp_clean()
  tables$magp_site_visits <- NULL
  results <- fx_caught(fx_mark_fixture(tables))
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  # The rows exist, as all() of none is TRUE (task 13's fix round: the spec review's F2, the
  # datatable review's F4).
  expect_gt(nrow(rows), 0L)
  expect_true(all(rows$status == "not_run" & rows$not_run_reason == "no_table"))
  expect_equal(nrow(opened(results, "record_unreachable")), 0L)
})

test_that("parent_without_children counts childless parents per relation, no issue (D18.34)", {
  fish <- fx_fish_clean()
  # Haul 1 loses its catches.
  catches <- fish$catches[fish$catches$haul_id != "hauls_1"]
  fish$catches <- catches
  results <- gpq_check_conformance(fish, fx_fish_spec(), group_cols = "gear", layers = 4)
  rows <- results$results[results$results$rule_id == "parent_without_children"]
  of_catches <- rows[rows$table_name == "catches"]
  expect_equal(unique(of_catches$column_name), "haul_id")
  # The hauls' own groups: one row per gear, the hauls counted.
  hauls <- fx_fish_clean()$hauls
  expect_equal(sum(of_catches$n_checked), nrow(hauls))
  expect_equal(sum(of_catches$n_fail), 1L)
  expect_equal(of_catches$gear[of_catches$n_fail == 1L], hauls$gear[[1L]])
  expect_false(any(results$issues$rule_id == "parent_without_children"))
  # An empty child table: every parent childless; an empty parent table: no_records.
  empty <- fx_fish_clean()
  empty$catches <- empty$catches[0L]
  rows <- fish_run(empty, layers = 4)$results
  row <- rows[rows$rule_id == "parent_without_children" & rows$table_name == "catches"]
  expect_equal(c(row$n_checked, row$n_fail), c(4L, 4L))
  no_stations <- fx_fish_clean()
  no_stations$stations <- no_stations$stations[0L]
  rows <- fish_run(no_stations, layers = 4)$results
  row <- rows[rows$rule_id == "parent_without_children" & rows$table_name == "hauls"]
  expect_equal(c(row$status, row$not_run_reason), c("not_run", "no_records"))
  # A parent table not given: not_run, no_table.
  rows <- fish_run(fx_fish_clean()[c("hauls", "catches")], layers = 4)$results
  row <- rows[rows$rule_id == "parent_without_children" & rows$table_name == "hauls"]
  expect_equal(c(row$status, row$not_run_reason), c("not_run", "no_table"))
})

# task 13's fix round (the datatable review's F3): the walk run in CRAN mode too (D18.48)
test_that("record_unreachable walks the fish toy from its root: a station no haul names fails", {
  # The hauls are the root; a catch is reached through its haul, and a station through a haul
  # that names it (D18.4).
  spec <- fx_fish_spec(reach = data.frame(
    table_name = c("hauls", "catches", "hauls"), attribute_name = c(NA, "haul_id", "station_id"),
    how = c("root", "from_parent", "from_child")
  ))
  # Every haul at station 1, so no haul names station 2; catch 1 names no haul.
  fish <- edited("hauls", "station_id", 3:4, c("stations_1", "stations_1"))
  fish <- edited("catches", "haul_id", 1L, "hauls_9", fish)
  real_rows <- reach_rows
  walks <- 0L
  testthat::local_mocked_bindings(reach_rows = function(input, reach) {
    walks <<- walks + 1L
    real_rows(input, reach)
  })
  results <- fish_run(fish, layers = 4, spec = spec)
  expect_equal(walks, 1L)
  found <- opened(results, "record_unreachable")
  expect_equal(placed_at(found), "stations station_id 2")
  expect_equal(c(found$value, found$severity), c("stations_2", "error"))
  # Station 1 reached and passing, every haul a root's, and catch 1, under no reached haul,
  # unchecked: it is fk_orphan's.
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  expect_equal(rows$table_name, c("stations", "hauls", "catches"))
  expect_equal(rows$n_checked, c(2L, 4L, 7L))
  expect_equal(rows$n_fail, c(1L, 0L, 0L))
  expect_equal(placed_at(opened(results, "fk_orphan")), "catches haul_id 1")
})

# the finish's minors pass (m80)
test_that("pre-flight's walk and record_unreachable check the same tables (D18.65)", {
  testthat::skip_on_cran()
  # read_reach() and check_keys() each say which tables the walk checks. Without its root
  # rows, a walk reaches none of them, so reach_invalid names each table it still checks: each
  # of those record_unreachable checks with the roots, and all of them but the roots.
  walks <- list(
    from_above = data.frame(
      table_name = c("stations", "hauls", "catches"),
      attribute_name = c(NA, "station_id", "haul_id"), how = c("root", "from_parent", "from_parent")
    ),
    # The hauls are checked as the from_child target, with no row of their own.
    from_below = data.frame(
      table_name = c("catches", "catches"), attribute_name = c(NA, "haul_id"),
      how = c("root", "from_child")
    ),
    with_exempt = data.frame(
      table_name = c("stations", "hauls", "catches"), attribute_name = c(NA, "station_id", NA),
      how = c("root", "from_parent", "exempt")
    )
  )
  for (name in names(walks)) {
    reach <- walks[[name]]
    roots <- reach$table_name[reach$how == "root"]
    rows <- fish_run(fx_fish_clean(), layers = 4, spec = fx_fish_spec(reach = reach))$results
    checked <- unique(rows$table_name[rows$rule_id == "record_unreachable"])
    findings <- fx_fish_spec(reach = reach[reach$how != "root", ])$read_findings
    details <- findings$detail[findings$rule_id == "reach_invalid"]
    expected <- report_text("preflight_detail_reach_invalid_unreachable", table_name = checked)
    expect_true(all(details %in% expected), info = name)
    expect_true(all(expected[!checked %in% roots] %in% details), info = name)
    expect_true(all(roots %in% checked), info = name)
  }
})

# task 13's fix round (the fixture review's Minor 4)
test_that("a design no plot measurement names is one record_unreachable warning (D18.13)", {
  testthat::skip_on_cran()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  added <- tables$magp_designs[1L]
  data.table::set(added, j = "magp_design_id", value = "magp_designs_9")
  tables$magp_designs <- rbind(tables$magp_designs, added)
  found <- opened(fx_caught(fx_mark_fixture(tables)))
  expect_equal(
    paste(found$rule_id, found$table_name, found$record_pk, found$severity),
    "record_unreachable magp_designs magp_designs_9 warning"
  )
})

# task 13's fix round 2 (D18.66): what only a table not given could reach is left unchecked
test_that("a record only a table not given could reach is unchecked, by a second walk", {
  testthat::skip_on_cran()
  # The catches are the root and reach the stations through the hauls, which aren't given.
  spec <- fx_fish_spec(reach = data.frame(
    table_name = c("catches", "catches", "hauls"),
    attribute_name = c(NA, "haul_id", "station_id"), how = c("root", "from_child", "from_child")
  ))
  real_rows <- reach_rows
  walks <- 0L
  testthat::local_mocked_bindings(reach_rows = function(input, reach) {
    walks <<- walks + 1L
    real_rows(input, reach)
  })
  results <- fish_run(fx_fish_clean()[c("stations", "catches")], spec = spec)
  # The second walk takes the hauls as a root. Layer 0 reports them, once; no station fails,
  # each unchecked, so their group checked nothing (D18.21, D18.22).
  expect_equal(walks, 2L)
  found <- opened(results)
  expect_equal(paste(found$rule_id, found$table_name), "table_missing hauls")
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  stations <- rows[rows$table_name == "stations"]
  expect_equal(c(stations$status, stations$not_run_reason), c("not_run", "none_checked"))
  expect_equal(c(stations$n_checked, stations$n_not_evaluated), c(0L, 2L))
})

# task 13's fix round 2 (D18.66)
test_that("a record no walk could reach fails though a table isn't given; joins made once", {
  testthat::skip_on_cran()
  # The hauls are the root; the catches, linked both ways, aren't given, so a second walk
  # takes them as a root; no haul names station 2, so even that walk doesn't reach it.
  spec <- fx_fish_spec(reach = data.frame(
    table_name = c("hauls", "catches", "catches", "hauls"),
    attribute_name = c(NA, "haul_id", "haul_id", "station_id"),
    how = c("root", "from_parent", "from_child", "from_child")
  ))
  fish <- edited("hauls", "station_id", 3:4, c("stations_1", "stations_1"))
  real_rows <- reach_rows
  real_chain <- chain_rows
  walks <- 0L
  inside <- FALSE
  joins <- 0L
  testthat::local_mocked_bindings(
    reach_rows = function(input, reach) {
      walks <<- walks + 1L
      inside <<- TRUE
      on.exit(inside <<- FALSE)
      real_rows(input, reach)
    },
    chain_rows = function(input, table, steps) {
      joins <<- joins + inside
      real_chain(input, table, steps)
    }
  )
  results <- fish_run(fish[c("stations", "hauls")], spec = spec)
  # Two walks, the hauls' one join made in the first and kept for the second.
  expect_equal(c(walks, joins), c(2L, 1L))
  found <- opened(results)
  expect_equal(
    paste(found$rule_id, placed_at(found)),
    c("table_missing catches * NA", "record_unreachable stations station_id 2")
  )
})

# task 13's fix round 2 (D18.66): the re-review's case
test_that("MAGPlot without its plot measurements: table_missing once, and tree 2 alone fails", {
  testthat::skip_on_cran()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  # Tree 2 never measured: its measurements now name tree 1, of the same subplot.
  meas <- tables$magp_tree_meas
  data.table::set(
    meas,
    i = which(meas$magp_tree_id == tables$magp_trees$magp_tree_id[[2L]]),
    j = "magp_tree_id", value = tables$magp_trees$magp_tree_id[[1L]]
  )
  tables$magp_plot_meas <- NULL
  results <- fx_caught(fx_mark_fixture(tables))
  found <- opened(results)
  # The plots, designs, subplots and other trees, reached only through the plot
  # measurements, are unchecked; tree 2, which no walk reaches even through them, fails.
  expect_setequal(paste(found$rule_id, found$table_name), c(
    "table_missing magp_plot_meas", "record_unreachable magp_trees"
  ))
  expect_equal(
    found$record_pk[found$rule_id == "record_unreachable"], tables$magp_trees$magp_tree_id[[2L]]
  )
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  for (table in c("magp_plots", "magp_subplots", "magp_designs")) {
    expect_equal(sum(rows$n_checked[rows$table_name == table]), 0L, info = table)
  }
})

# task 13's fix round 3 (D18.68): what only a key column not given could reach is unchecked
test_that("a record only a key column not given could reach is unchecked; another still fails", {
  testthat::skip_on_cran()
  # The catches are the root and reach the hauls, then the stations, through their keys; no
  # haul names station 2.
  spec <- fx_fish_spec(reach = data.frame(
    table_name = c("catches", "catches", "hauls"),
    attribute_name = c(NA, "haul_id", "station_id"), how = c("root", "from_child", "from_child")
  ))
  fish <- edited("hauls", "station_id", 3:4, c("stations_1", "stations_1"))
  real_rows <- reach_rows
  walks <- 0L
  testthat::local_mocked_bindings(reach_rows = function(input, reach) {
    walks <<- walks + 1L
    real_rows(input, reach)
  })
  # Every column given: one walk, and station 2 fails.
  found <- opened(fish_run(fish, spec = spec))
  expect_equal(walks, 1L)
  expect_equal(paste(found$rule_id, placed_at(found)), "record_unreachable stations station_id 2")
  # The catches' haul_id not given: a second walk counts that link as reaching every haul with
  # a key. Layer 0 reports the column, once; the hauls are unchecked, and station 2 still fails.
  walks <- 0L
  catches <- data.table::copy(fish$catches)
  data.table::set(catches, j = "haul_id", value = NULL)
  fish$catches <- catches
  results <- fish_run(fish, spec = spec)
  expect_equal(walks, 2L)
  found <- opened(results)
  expect_equal(paste(found$rule_id, found$table_name, found$column_name, found$row_number), c(
    "column_missing catches haul_id NA", "record_unreachable stations station_id 2"
  ))
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  hauls <- rows[rows$table_name == "hauls"]
  expect_equal(c(hauls$status, hauls$not_run_reason), c("not_run", "none_checked"))
  stations <- rows[rows$table_name == "stations"]
  expect_equal(c(stations$n_checked, stations$n_fail), c(1L, 1L))
})

# task 13's fix round 3 (D18.68): the second re-review's cases
test_that("MAGPlot without a link's key column: column_missing once, and tree 2 alone fails", {
  testthat::skip_on_cran()
  # The subplot measurements' key to their plot measurement not given, and tree 2 never
  # measured: its measurements now name tree 1, of the same subplot.
  tables <- lapply(fx_magp_clean(), data.table::copy)
  meas <- tables$magp_tree_meas
  data.table::set(
    meas,
    i = which(meas$magp_tree_id == tables$magp_trees$magp_tree_id[[2L]]),
    j = "magp_tree_id", value = tables$magp_trees$magp_tree_id[[1L]]
  )
  data.table::set(tables$magp_subplot_meas, j = "magp_plotmeas_id", value = NULL)
  found <- opened(fx_caught(fx_mark_fixture(tables)))
  expect_setequal(paste(found$rule_id, found$table_name, found$column_name), c(
    "column_missing magp_subplot_meas magp_plotmeas_id",
    "record_unreachable magp_trees magp_tree_id"
  ))
  expect_equal(
    found$record_pk[found$rule_id == "record_unreachable"], tables$magp_trees$magp_tree_id[[2L]]
  )
  # The tree measurements' key to their tree not given: every tree unchecked, none failing.
  tables <- lapply(fx_magp_clean(), data.table::copy)
  data.table::set(tables$magp_tree_meas, j = "magp_tree_id", value = NULL)
  results <- fx_caught(fx_mark_fixture(tables))
  found <- opened(results)
  expect_equal(
    paste(found$rule_id, found$table_name, found$column_name),
    "column_missing magp_tree_meas magp_tree_id"
  )
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  expect_equal(sum(rows$n_checked[rows$table_name == "magp_trees"]), 0L)
})

# task 13's fix round 3 (R114, the re-reviews' O1 and Minor 2)
test_that("a root not given reaches no child row whose key is NA, blank or a sentinel", {
  # The hauls, not given, taken as a root as the second walk takes them: a catch is reached
  # through them only when its haul_id holds a key, as a join to a given haul reads it.
  fish <- edited("catches", "haul_id", 1:4, c(NA, intToUtf8(0xA0), "?", "~"))
  input <- conformance_input(fish[c("stations", "catches")], fx_fish_spec())
  reach <- data.table::data.table(
    table_name = c("hauls", "catches"), attribute_name = c(NA, "haul_id"),
    how = c("root", "from_parent")
  )
  expect_equal(reach_rows(input, reach)$catches, rep(c(FALSE, TRUE), each = 4L))
})

# the finish's minors pass (m85)
test_that("a NaN key reaches nothing and holds no key, as key_rows() reads it as missing", {
  # match() keeps NaN apart from NA, so a NaN child key would join a NaN parent key.
  dictionary <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "numeric"
  )
  spec <- gpq_read_spec(dictionary)
  tables <- list(
    p = data.table::data.table(p_id = c(1, NaN)),
    k = data.table::data.table(k_id = c(1, 2, 3), p_id = c(1, NaN, 5))
  )
  input <- conformance_input(tables, spec)
  chain <- parse_chain("p_id > p_id", "k", spec$keys, spec$attributes)
  expect_equal(chain_rows(input, "k", chain$steps), c(1L, NA, NA))
  expect_equal(pk_holders(input, "p"), c(1L, NA))
  # The parents not given, taken as a root: a child holding NaN holds no key.
  reach <- data.table::data.table(
    table_name = c("p", "k"), attribute_name = c(NA, "p_id"), how = c("root", "from_parent")
  )
  expect_equal(reach_rows(conformance_input(tables["k"], spec), reach)$k, c(TRUE, FALSE, TRUE))
  # A NaN child key stored as doubles is written "NaN" against a parent's key stored as text,
  # and still reaches nothing, though the parent holds the text "NaN" (the minors pass's fix
  # round, spec F2).
  typed_text <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "character"
  )
  text_spec <- gpq_read_spec(typed_text)
  mixed <- list(
    p = data.table::data.table(p_id = c("1", "NaN")),
    k = data.table::data.table(k_id = c("a", "b"), p_id = c(1, NaN))
  )
  chain <- parse_chain("p_id > p_id", "k", text_spec$keys, text_spec$attributes)
  expect_equal(chain_rows(conformance_input(mixed, text_spec), "k", chain$steps), c(1L, NA))
  # The other way round, a child's text "NaN" against a parent's NaN stored as doubles,
  # reaches nothing too, so fk_orphan reports it as given (the minors pass's fix round 2,
  # D18.87).
  back <- list(
    p = data.table::data.table(p_id = c(1, NaN)),
    k = data.table::data.table(k_id = c("a", "b"), p_id = c("1", "NaN"))
  )
  expect_equal(chain_rows(conformance_input(back, text_spec), "k", chain$steps), c(1L, NA))
  orphan <- opened(gpq_check_conformance(back, text_spec, layers = 4), "fk_orphan")
  expect_equal(paste(placed_at(orphan), orphan$value), "k p_id 2 NaN")
})

# the finish's minors pass (m86)
test_that("an integer key is masked by its whole codes only: -99.5 isn't -99", {
  dictionary <- data.frame(
    table_name = c("p", "k", "k"), attribute_name = c("p_id", "k_id", "p_id"),
    key_type = c("PK", "PK", "FK"), reference_table = c(NA, NA, "p"), data_type = "integer"
  )
  spec <- gpq_read_spec(
    dictionary,
    sentinels = gpq_sentinels(numeric = c(missing = -99.5, not_applicable = -88))
  )
  tables <- list(
    p = data.table::data.table(p_id = c(-99L, -88L, 3L)),
    k = data.table::data.table(k_id = 1:3, p_id = c(-99L, -88L, 3L))
  )
  input <- conformance_input(tables, spec)
  chain <- parse_chain("p_id > p_id", "k", spec$keys, spec$attributes)
  expect_equal(chain_rows(input, "k", chain$steps), c(1L, NA, 3L))
  expect_equal(pk_holders(input, "p"), c(1L, NA, 3L))
})

# task 13's fix round 4 (the datatable re-review's F2): a link from a table not given whose own
# key column isn't given either
test_that("MAGPlot without its plot measurements or their key's column: tree 2 alone fails", {
  testthat::skip_on_cran()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  # Tree 2 never measured: its measurements now name tree 1, of the same subplot.
  meas <- tables$magp_tree_meas
  data.table::set(
    meas,
    i = which(meas$magp_tree_id == tables$magp_trees$magp_tree_id[[2L]]),
    j = "magp_tree_id", value = tables$magp_trees$magp_tree_id[[1L]]
  )
  tables$magp_plot_meas <- NULL
  data.table::set(tables$magp_subplot_meas, j = "magp_plotmeas_id", value = NULL)
  results <- fx_caught(fx_mark_fixture(tables))
  found <- opened(results)
  # Every subplot measurement counts as reached through the plot measurements, so the
  # subplots and the other trees are unchecked; tree 2, which no walk reaches, fails.
  expect_setequal(paste(found$rule_id, found$table_name, found$column_name), c(
    "table_missing magp_plot_meas *", "column_missing magp_subplot_meas magp_plotmeas_id",
    "record_unreachable magp_trees magp_tree_id"
  ))
  expect_equal(
    found$record_pk[found$rule_id == "record_unreachable"], tables$magp_trees$magp_tree_id[[2L]]
  )
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  expect_equal(sum(rows$n_checked[rows$table_name == "magp_subplots"]), 0L)
})

# task 13's fix round 4 (D18.73)
test_that("pk_holders() gives no holder to a missing-value code, whatever comes first", {
  # The fish spec with "X" and "Z" as the missing-value codes of text, as MAGPlot's; the
  # stations keyed "X", "Z" and a key marked UTF-8, which makes R hash text by its content, so
  # match()'s slip is the same in every session.
  spec <- gpq_read_spec(
    dictionary = example_file("fish_dictionary.csv"), code_lists = fx_fish_code_lists(),
    column_map = fx_fish_column_map(), type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "X", not_applicable = "Z"),
      date = c(missing = "X", not_applicable = "Z")
    )
  )
  stations <- fx_fish_clean()$stations[c(1L, 2L, 1L, 1L, 1L, 2L)]
  marked <- paste0("stations_", intToUtf8(0xE9))
  data.table::set(
    stations,
    j = "station_id", value = c("stations_1", "stations_2", "X", "Z", marked, "Z")
  )
  expect_equal(Encoding(marked), "UTF-8")
  input <- conformance_input(list(stations = stations), spec)
  expect_equal(pk_holders(input, "stations"), c(1L, 2L, NA, NA, 5L, NA))
})

# task 13's fix round 4 (D18.75): the second walk masks both sides' types' missing-value codes
test_that("a key holding the other side's type's missing-value code reaches nothing", {
  testthat::skip_on_cran()
  # trips (the root) > hauls (a whole-number key) > catches (a text haul_id); the catches name
  # the stations. Catch 3's haul_id is "-88", the numbers' not-applicable code.
  dictionary <- data.frame(
    table = c("trips", "hauls", "hauls", "catches", "catches", "catches", "stations"),
    field = c("trip_id", "haul_id", "trip_id", "catch_id", "haul_id", "station_id", "station_id"),
    kind = c("text", "whole", "text", "text", "text", "text", "text"),
    key = c("pk", "pk", "fk", "pk", "fk", "fk", "pk"),
    parent = c(NA, NA, "trips", NA, "hauls", "stations", NA), codes = NA, notes = "x", low = NA,
    high = NA, low_in = NA, high_in = NA, format = NA, width = NA, places = NA, unit = NA
  )
  reach <- data.frame(
    table_name = c("trips", "hauls", "catches", "catches"),
    attribute_name = c(NA, "trip_id", "haul_id", "station_id"),
    how = c("root", "from_parent", "from_parent", "from_child")
  )
  sentinels <- gpq_sentinels(
    numeric = c(missing = -99, not_applicable = -88),
    character = c(missing = "?", not_applicable = "~"),
    date = c(missing = "?", not_applicable = "~")
  )
  spec <- gpq_read_spec(
    dictionary = dictionary, column_map = fx_fish_column_map(),
    type_map = gpq_type_map(example_file("fish_types.csv")), sentinels = sentinels, reach = reach
  )
  tables <- list(
    trips = data.table::data.table(trip_id = "t1"),
    hauls = data.table::data.table(haul_id = 1:2, trip_id = "t1"),
    catches = data.table::data.table(
      catch_id = c("c1", "c2", "c3"), haul_id = c("1", "2", "-88"), station_id = c("s1", "s2", "s3")
    ),
    stations = data.table::data.table(station_id = c("s1", "s2", "s3"))
  )
  # With the hauls given, catch 3 joins no haul, so station 3 fails.
  results <- gpq_check_conformance(tables, spec, layers = 4)
  expect_equal(placed_at(opened(results, "record_unreachable")), "stations station_id 3")
  # Without them, the second walk reaches catches 1 and 2 only, as a join would: stations 1
  # and 2 are unchecked, and station 3 still fails.
  results <- gpq_check_conformance(tables[c("trips", "catches", "stations")], spec, layers = 4)
  expect_equal(placed_at(opened(results, "record_unreachable")), "stations station_id 3")
  rows <- results$results[results$results$rule_id == "record_unreachable"]
  expect_equal(
    c(rows$n_checked[rows$table_name == "stations"], rows$n_fail[rows$table_name == "stations"]),
    c(1L, 1L)
  )
  # The parent's side: the catches' station_id a whole number, not given, taken as a root as the
  # second walk takes it; a station keyed "-88" is one no catch could name.
  dictionary$kind[[6L]] <- "whole"
  spec <- gpq_read_spec(
    dictionary = dictionary, column_map = fx_fish_column_map(),
    type_map = gpq_type_map(example_file("fish_types.csv")), sentinels = sentinels
  )
  input <- conformance_input(
    list(stations = data.table::data.table(station_id = c("1", "2", "-88"))), spec
  )
  walk <- data.table::data.table(
    table_name = c("catches", "catches"), attribute_name = c(NA, "station_id"),
    how = c("root", "from_child")
  )
  expect_equal(reach_rows(input, walk)$stations, c(TRUE, TRUE, FALSE))
})

# task 13's fix round 4 (D18.76): a Layer 3 failure in a key column doesn't mask Layer 4 there
test_that("a key that fails Layer 3 is still a key to Layer 4; Layers 1 and 2 still mask it", {
  testthat::skip_on_cran()
  # hauls.station_id is a foreign key with a code list; hauls.water_body, a coded copy of its
  # station's, is no key.
  dictionary <- data.frame(
    table = c("stations", "stations", "hauls", "hauls", "hauls"),
    field = c("station_id", "water_body", "haul_id", "station_id", "water_body"),
    kind = "text", key = c("pk", NA, "pk", "fk", NA), parent = c(NA, NA, NA, "stations", NA),
    codes = c(NA, "y", NA, "y", "y"), notes = "x", low = NA, high = NA, low_in = NA,
    high_in = NA, format = NA, width = NA, places = NA, unit = NA
  )
  spec <- gpq_read_spec(
    dictionary = dictionary,
    code_lists = list(
      water_body = example_file("fish_water_body.csv"),
      station_id = data.frame(station_id = c("s1", "s2"))
    ),
    column_map = fx_fish_column_map(), type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "?", not_applicable = "~"),
      date = c(missing = "?", not_applicable = "~")
    ),
    key_pairs = data.frame(
      table_name = "hauls", left = "water_body", right = "station_id > water_body", note = NA
    )
  )
  stations <- data.table::data.table(station_id = c("s1", "s2"), water_body = c("LK", "RV"))
  # Haul 2 names a station outside the code list and the stations; haul 3's key is padded
  # (Layer 2); haul 4's water_body is outside its list.
  hauls <- data.table::data.table(
    haul_id = c("h1", "h2", "h3", "h4"), station_id = c("s1", "s9", "s2 ", "s2"),
    water_body = c("LK", "LK", "RV", "XX")
  )
  results <- gpq_check_conformance(list(stations = stations, hauls = hauls), spec, layers = 0:4)
  issues <- results$issues
  padding <- issues[issues$rule_id == "whitespace_padding"]
  coded <- issues[issues$rule_id == "code_not_in_list" & issues$column_name == "water_body"]
  orphan <- issues[issues$rule_id == "fk_orphan"]
  orphan <- orphan[order(orphan$row_number)]
  # Haul 2's key fails code_not_in_list and is still followed: fk_orphan fails it. Haul 3's
  # padding still masks it.
  expect_equal(orphan$row_number, 2:3)
  expect_equal(orphan$status, c("open", "not_evaluated"))
  expect_equal(orphan$root_issue_id, c(NA, padding$issue_id))
  row <- results$results[results$results$rule_id == "fk_orphan"]
  expect_equal(c(row$n_checked, row$n_fail, row$n_not_evaluated), c(3L, 1L, 1L))
  # Haul 4's water_body, no key, fails code_not_in_list, which still masks key_incoherent.
  incoherent <- issues[issues$rule_id == "key_incoherent"]
  expect_equal(incoherent$row_number, 4L)
  expect_equal(incoherent$status, "not_evaluated")
  expect_equal(incoherent$root_issue_id, coded$issue_id)
  # A column-level failure in a key: every haul names a station outside the code list, and
  # haul 3 one the stations lack. fk_orphan still checks each haul, and fails haul 3's.
  stations <- data.table::data.table(station_id = c("s1", "s2", "s3"), water_body = "RV")
  hauls <- data.table::data.table(
    haul_id = c("h1", "h2", "h3"), station_id = c("s3", "s3", "s9"), water_body = "RV"
  )
  results <- gpq_check_conformance(
    list(stations = stations, hauls = hauls), spec,
    layers = 0:4, settings = list(column_level_min_n = 2L)
  )
  coded <- opened(results, "code_not_in_list")
  expect_equal(c(coded$level, coded$column_name), c("column", "station_id"))
  expect_equal(placed_at(opened(results, "fk_orphan")), "hauls station_id 3")
  row <- results$results[results$results$rule_id == "fk_orphan"]
  expect_equal(c(row$n_checked, row$n_fail, row$n_not_evaluated), c(3L, 1L, 0L))
  # A column-level failure of Layer 2 in a key still masks Layer 4: every haul's key padded
  # (the finish's minors pass, m89).
  hauls <- data.table::data.table(
    haul_id = c("h1", "h2", "h3"), station_id = c("s1 ", "s2 ", "s1 "), water_body = "RV"
  )
  results <- gpq_check_conformance(
    list(stations = stations, hauls = hauls), spec,
    layers = 0:4, settings = list(column_level_min_n = 2L)
  )
  padding <- opened(results, "whitespace_padding")
  expect_equal(c(padding$level, padding$column_name), c("column", "station_id"))
  masked <- results$issues[results$issues$rule_id == "fk_orphan"]
  expect_equal(c(masked$level, masked$status), c("column", "not_evaluated"))
  expect_equal(masked$root_issue_id, padding$issue_id)
  row <- results$results[results$results$rule_id == "fk_orphan"]
  expect_equal(c(row$n_checked, row$n_fail, row$n_not_evaluated), c(0L, 0L, 3L))
})

# task 13's fix round 4 (D18.76): the band rule on MAGPlot's site IDs
test_that("a site ID outside its band is still checked as a key (D18.76)", {
  testthat::skip_on_cran()
  tables <- lapply(fx_magp_clean(), data.table::copy)
  # Two sites more, of site 1's contributor, sharing one ID in a reserved band; no visit
  # names them.
  sites <- tables$magp_sites
  added <- sites[c(1L, 1L)]
  data.table::set(added, j = "magp_site_id", value = c(900500L, 900500L))
  tables$magp_sites <- rbind(sites, added)
  results <- fx_caught(fx_mark_fixture(tables))
  rows <- nrow(sites) + 1:2
  for (rule in c("site_id_out_of_range", "pk_duplicate", "record_unreachable")) {
    found <- opened(results, rule)
    expect_equal(found$row_number[found$table_name == "magp_sites"], rows, info = rule)
  }
  # pk_missing checks them too, and nothing on the sites is left not evaluated.
  keyed <- c("pk_duplicate", "pk_missing", "record_unreachable")
  on_sites <- results$results$table_name == "magp_sites" & results$results$rule_id %in% keyed
  counts <- results$results[on_sites]
  for (rule in keyed) {
    mine <- counts$rule_id == rule
    expect_equal(sum(counts$n_checked[mine]), nrow(sites) + 2L, info = rule)
    expect_equal(sum(counts$n_not_evaluated[mine]), 0L, info = rule)
  }
})

# ---- task 14 ----

test_that("design_geometry_inconsistent fails a group's records whose values differ (K1)", {
  settings <- list(
    design_geometry_table = "catches", design_geometry_by = c("haul_id", "species"),
    design_geometry_columns = c("weight", "fish_count")
  )
  clean <- fish_run(fx_fish_clean(), settings)
  expect_equal(nrow(opened(clean, "design_geometry_inconsistent")), 0L)
  # Catch 2 takes catch 1's species: two records of different keys, one group (D18.39).
  fish <- edited("catches", "species", 2L, fx_fish_clean()$catches$species[[1L]])
  found <- opened(fish_run(fish, settings), "design_geometry_inconsistent")
  expect_equal(placed_at(found), c(
    "catches weight 1", "catches weight 2", "catches fish_count 1", "catches fish_count 2"
  ))
  expect_equal(found$value, c("1.5", "2.5", "1", "2"))
  expect_equal(found$expected, c("1.5; 2.5", "1.5; 2.5", "1; 2", "1; 2"))
  # Without its settings: one not_run row, no_input.
  row <- fish_run(fx_fish_clean())$results
  row <- row[row$rule_id == "design_geometry_inconsistent"]
  expect_equal(c(row$status, row$not_run_reason), c("not_run", "no_input"))
  # A record whose grouping value is NA or blank isn't grouped: catches 1 and 2 differ in
  # weight and count, but neither has a species (D18.39).
  for (none in c(NA_character_, "")) {
    ungrouped <- fish_run(edited("catches", "species", 1:2, none), settings)
    expect_equal(nrow(opened(ungrouped, "design_geometry_inconsistent")), 0L, info = none)
  }
})

test_that("subplot_label_ambiguous fails records of other keys sharing the labels (K8)", {
  settings <- list(
    subplot_label_table = "hauls", subplot_label_columns = c("station_id", "gear")
  )
  expect_equal(nrow(opened(fish_run(fx_fish_clean(), settings), "subplot_label_ambiguous")), 0L)
  fish <- edited("hauls", "gear", 2L, fx_fish_clean()$hauls$gear[[1L]])
  found <- opened(fish_run(fish, settings), "subplot_label_ambiguous")
  expect_equal(placed_at(found), c("hauls gear 1", "hauls gear 2"))
  expect_equal(found$value, rep(fx_fish_clean()$hauls$gear[[1L]], 2L))
  # A record whose label is NA or blank isn't grouped: hauls 1 and 2 share a gear but have no
  # station. (The last label is the reported cell, which Layer 2 would mask.)
  for (none in c(NA_character_, "")) {
    unlabelled <- edited("hauls", "station_id", 1:2, none, fish = fish)
    found <- opened(fish_run(unlabelled, settings), "subplot_label_ambiguous")
    expect_equal(nrow(found), 0L, info = none)
  }
  # Records of one key aren't ambiguous: haul 2 given haul 1's key and gear is pk_duplicate's.
  same_key <- fish_run(edited("hauls", "haul_id", 2L, "hauls_1", fish = fish), settings)
  expect_equal(nrow(opened(same_key, "subplot_label_ambiguous")), 0L)
  expect_equal(nrow(opened(same_key, "pk_duplicate")), 2L)
})

test_that("design_duplicate_signature fails keys of one report unit sharing rows (K10)", {
  settings <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = "species"
  )
  fish <- fx_fish_clean()
  # Hauls 1 and 3 catch the same species, as do hauls 2 and 4: in one pool every haul fails,
  # on the hauls' key, each expecting its twin.
  found <- opened(fish_run(fish, settings), "design_duplicate_signature")
  expect_equal(placed_at(found), paste("hauls haul_id", 1:4))
  expect_equal(found$expected, c("hauls_3", "hauls_4", "hauls_1", "hauls_2"))
  # Each haul's rows are sorted before they are compared: haul 3's two catches swapped change
  # nothing.
  swapped <- edited("catches", "species", 5:6, fx_fish_clean()$catches$species[6:5])
  again <- opened(fish_run(swapped, settings), "design_duplicate_signature")
  expect_equal(placed_at(again), placed_at(found))
  expect_equal(again$expected, found$expected)
  # Each station a report unit: its two hauls differ, so nothing fails.
  by_station <- gpq_check_conformance(
    fish, fx_fish_spec(),
    group_cols = "station_id", settings = settings, layers = 4
  )
  expect_equal(nrow(opened(by_station, "design_duplicate_signature")), 0L)
})

# the finish's minors pass (m100)
test_that("a design candidate without its table or a column is not_run; a \"*\" unit unchecked", {
  testthat::skip_on_cran()
  geometry <- list(
    design_geometry_table = "catches", design_geometry_by = c("haul_id", "species"),
    design_geometry_columns = c("weight", "fish_count")
  )
  signature <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = "species"
  )
  reasons <- function(tables, settings, rule) {
    rows <- fish_run(tables, settings)$results
    rows <- rows[rows$rule_id == rule]
    paste(rows$table_name, rows$column_name, rows$not_run_reason)
  }
  fish <- fx_fish_clean()
  no_catches <- fish[c("stations", "hauls")]
  expect_equal(
    reasons(no_catches, geometry, "design_geometry_inconsistent"),
    c("catches weight no_table", "catches fish_count no_table")
  )
  expect_equal(
    reasons(no_catches, signature, "design_duplicate_signature"), "hauls haul_id no_table"
  )
  # A grouping column, or a column signed, not given.
  without <- function(column) {
    tables <- fish
    tables$catches <- data.table::copy(fish$catches)
    data.table::set(tables$catches, j = column, value = NULL)
    tables
  }
  expect_equal(
    reasons(without("species"), geometry, "design_geometry_inconsistent"),
    c("catches weight no_column", "catches fish_count no_column")
  )
  expect_equal(
    reasons(without("species"), signature, "design_duplicate_signature"), "hauls haul_id no_column"
  )
  expect_equal(
    reasons(without("haul_id"), signature, "design_duplicate_signature"), "hauls haul_id no_column"
  )
  # A haul whose report unit is "*" (its station not named) isn't checked: haul 3, its twin,
  # then has none, and hauls 2 and 4 still fail (K10).
  water <- fish$stations$water_body[[1L]]
  units <- edited("stations", "water_body", 1:2, c(water, water), fish)
  units <- edited("hauls", "station_id", 1L, NA_character_, units)
  results <- gpq_check_conformance(
    units, fx_fish_spec(),
    group_cols = "water_body", settings = signature, layers = 4
  )
  found <- opened(results, "design_duplicate_signature")
  expect_equal(placed_at(found), paste("hauls haul_id", c(2L, 4L)))
  rows <- results$results[results$results$rule_id == "design_duplicate_signature"]
  star <- rows[rows$water_body == "*"]
  expect_equal(c(star$status, star$not_run_reason), c("not_run", "none_checked"))
  expect_equal(star$n_not_evaluated, 1L)
})

# ---- task 14's fix round 1 ----

test_that("a number read as text is compared by its number, written as given (D18.80)", {
  testthat::skip_on_cran()
  dir <- withr::local_tempdir()
  # The fish tables as CSV files, every column read as text, catches' weight as given here.
  as_csv <- function(tables, weight, name) {
    tables$catches <- data.table::copy(tables$catches)
    data.table::set(tables$catches, j = "weight", value = weight)
    paths <- file.path(dir, paste0(name, "_", names(tables), ".csv"))
    names(paths) <- names(tables)
    for (table in names(tables)) {
      data.table::fwrite(tables[[table]], paths[[table]])
    }
    paths
  }
  weights <- as.character(fx_fish_clean()$catches$weight)
  geometry <- list(
    design_geometry_table = "catches", design_geometry_by = c("haul_id", "species"),
    design_geometry_columns = "weight"
  )
  # Catch 2 joins catch 1's group with its weight: 1.5 and "1.50" agree, as 1.5 and 1.5 do.
  grouped <- edited("catches", "species", 2L, "WAE")
  as_double <- edited("catches", "weight", 2L, 1.5, fish = grouped)
  expect_equal(nrow(opened(fish_run(as_double, geometry), "design_geometry_inconsistent")), 0L)
  same <- as_csv(grouped, replace(weights, 1:2, c("1.5", "1.50")), "same")
  expect_equal(nrow(opened(fish_run(same, geometry), "design_geometry_inconsistent")), 0L)
  # Another number fails, its value and expected as written.
  other <- as_csv(grouped, replace(weights, 1:2, c("1.5", "2.50")), "other")
  found <- opened(fish_run(other, geometry), "design_geometry_inconsistent")
  expect_equal(found$value, c("1.5", "2.50"))
  expect_equal(found$expected, c("1.5; 2.50", "1.5; 2.50"))
  # A signature differing only so is a duplicate: haul 3's catches weigh what haul 1's do.
  signature <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = c("species", "weight")
  )
  twins <- as_csv(fx_fish_clean(), replace(weights, 5:6, c("1.50", "2.5")), "twins")
  found <- opened(fish_run(twins, signature), "design_duplicate_signature")
  expect_equal(placed_at(found), c("hauls haul_id 1", "hauls haul_id 3"))
  expect_equal(found$expected, c("hauls_3", "hauls_1"))
  # Text that doesn't coerce is compared as written: "n/a" twice in hauls 1 and 3 makes twins,
  # "n/a" against "tbd" doesn't.
  written <- as_csv(
    fx_fish_clean(), replace(weights, 1:6, c("n/a", "n/a", "x", "x", "n/a", "n/a")), "written"
  )
  found <- opened(fish_run(written, signature), "design_duplicate_signature")
  expect_equal(placed_at(found), c("hauls haul_id 1", "hauls haul_id 3"))
  apart <- as_csv(
    fx_fish_clean(), replace(weights, 1:6, c("n/a", "n/a", "x", "x", "n/a", "tbd")), "apart"
  )
  expect_equal(nrow(opened(fish_run(apart, signature), "design_duplicate_signature")), 0L)
  # A factor is read as its labels: 1.5 and "1.50" agree there too.
  as_factor <- data.table::copy(grouped$catches)
  data.table::set(
    as_factor,
    j = "weight", value = factor(replace(weights, 1:2, c("1.5", "1.50")))
  )
  factored <- grouped
  factored$catches <- as_factor
  found <- opened(fish_run(factored, geometry, layers = 4), "design_geometry_inconsistent")
  expect_equal(nrow(found), 0L)
})

test_that("expected lists example_cap values, then the count of the rest (D18.81)", {
  testthat::skip_on_cran()
  geometry <- list(
    design_geometry_table = "catches", design_geometry_by = c("haul_id", "species"),
    design_geometry_columns = "weight"
  )
  grouped <- edited("catches", "species", 2L, "WAE")
  capped <- opened(
    fish_run(grouped, c(geometry, list(example_cap = 1L))), "design_geometry_inconsistent"
  )
  expect_equal(capped$expected, rep("1.5 (and 1 more)", 2L))
  full <- opened(
    fish_run(grouped, c(geometry, list(example_cap = 2L))), "design_geometry_inconsistent"
  )
  expect_equal(full$expected, rep("1.5; 2.5", 2L))
  # Hauls 1, 2 and 3 catch the same species: each expects the other two, or one and a count.
  signature <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = "species"
  )
  three <- edited("catches", "species", 3:4, c("WAE", "NOP"))
  found <- opened(fish_run(three, signature), "design_duplicate_signature")
  expect_equal(placed_at(found), paste("hauls haul_id", 1:3))
  expect_equal(found$expected, c("hauls_2; hauls_3", "hauls_1; hauls_3", "hauls_1; hauls_2"))
  capped <- opened(
    fish_run(three, c(signature, list(example_cap = 1L))), "design_duplicate_signature"
  )
  expect_equal(
    capped$expected, c("hauls_2 (and 1 more)", "hauls_1 (and 1 more)", "hauls_1 (and 1 more)")
  )
  expect_equal(capped$expected[[1L]], report_text("expected_capped", values = "hauls_2", n = 1L))
  full <- opened(
    fish_run(three, c(signature, list(example_cap = 2L))), "design_duplicate_signature"
  )
  expect_equal(full$expected, found$expected)
})

test_that("at example_cap 0 expected is NA, as examples is (D18.82)", {
  testthat::skip_on_cran()
  geometry <- list(
    design_geometry_table = "catches", design_geometry_by = c("haul_id", "species"),
    design_geometry_columns = "weight", example_cap = 0L
  )
  found <- opened(
    fish_run(edited("catches", "species", 2L, "WAE"), geometry), "design_geometry_inconsistent"
  )
  expect_equal(placed_at(found), c("catches weight 1", "catches weight 2"))
  expect_equal(found$expected, c(NA_character_, NA_character_))
  signature <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = "species", example_cap = 0L
  )
  found <- opened(
    fish_run(edited("catches", "species", 3:4, c("WAE", "NOP")), signature),
    "design_duplicate_signature"
  )
  expect_equal(placed_at(found), paste("hauls haul_id", 1:3))
  expect_equal(found$expected, rep(NA_character_, 3L))
})

test_that("design_geometry_inconsistent sorts expected in C order, whatever the locale", {
  settings <- list(
    design_geometry_table = "catches", design_geometry_by = "haul_id",
    design_geometry_columns = "species"
  )
  # testthat sorts in C order: the run is made in a locale whose own order puts "aaa" before
  # "NOP", where one is installed, and upper case still sorts first.
  sorts_lower_first <- function(locale) {
    withr::local_collate(locale)
    identical(sort(c("NOP", "aaa")), c("aaa", "NOP"))
  }
  locales <- c("en_US.UTF-8", "English_United States.utf8", "en_GB.UTF-8")
  usable <- Filter(function(locale) isTRUE(suppressWarnings(sorts_lower_first(locale))), locales)
  testthat::skip_if(length(usable) == 0L, "no locale sorting lower case first is installed")
  withr::local_collate(usable[[1L]])
  fish <- edited("catches", "species", 1L, "aaa")
  found <- opened(fish_run(fish, settings, layers = 4), "design_geometry_inconsistent")
  expect_equal(found$expected[found$row_number %in% 1:2], rep("NOP; aaa", 2L))
  # design_duplicate_signature lists its twins so too: hauls 1 to 3 share their species, and
  # hauls 2 and 3 are renamed "NOP" and "aaa".
  three <- edited("catches", "species", 3:4, c("WAE", "NOP"))
  three <- edited("hauls", "haul_id", 2:3, c("NOP", "aaa"), fish = three)
  three <- edited("catches", "haul_id", 3:6, c("NOP", "NOP", "aaa", "aaa"), fish = three)
  signature <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = "species"
  )
  twins <- opened(fish_run(three, signature, layers = 4), "design_duplicate_signature")
  expect_equal(twins$expected[twins$row_number == 1L], "NOP; aaa")
})

test_that("design_geometry_inconsistent gives each failing group its own expected", {
  settings <- list(
    design_geometry_table = "catches", design_geometry_by = c("haul_id", "species"),
    design_geometry_columns = "weight"
  )
  # Catches 4 and 8 join catches 3 and 7: the failing groups are the third and the sixth.
  fish <- edited("catches", "species", c(4L, 8L), c("YEP", "YEP"))
  found <- opened(fish_run(fish, settings), "design_geometry_inconsistent")
  expect_equal(placed_at(found), paste("catches weight", c(3L, 4L, 7L, 8L)))
  expect_equal(found$expected, c("3.5; 4.5", "3.5; 4.5", "7.5; 8.5", "7.5; 8.5"))
})

test_that("a blank value reads as the blank text in expected, capped or not (D18.83)", {
  testthat::skip_on_cran()
  settings <- list(
    design_geometry_table = "hauls", design_geometry_by = "station_id",
    design_geometry_columns = "gear"
  )
  # Haul 1's gear blank, as written, of only spaces or of a no-break space: station 1's hauls
  # disagree on it, station 2's on EF and GN. Values sort as given (C order), so an empty or
  # ASCII-space value comes first and a no-break space after the letters.
  nbsp <- intToUtf8(0xA0)
  for (none in c("", "  ", "\t", nbsp)) {
    first <- if (none == nbsp) {
      c("TN; (blank)", "TN (and 1 more)")
    } else {
      c("(blank); TN", "(blank) (and 1 more)")
    }
    fish <- edited("hauls", "gear", 1L, none)
    full <- opened(fish_run(fish, settings, layers = 4), "design_geometry_inconsistent")
    expect_equal(full$expected, rep(c(first[[1L]], "EF; GN"), each = 2L), info = none)
    expect_equal(full$value[[1L]], none, info = none)
    capped <- opened(
      fish_run(fish, c(settings, list(example_cap = 1L)), layers = 4),
      "design_geometry_inconsistent"
    )
    expect_equal(
      capped$expected, rep(c(first[[2L]], "EF (and 1 more)"), each = 2L),
      info = none
    )
    # At example_cap 0 no value is shown, a blank one neither (D18.82; the finish's minors
    # pass, m101).
    none_shown <- opened(
      fish_run(fish, c(settings, list(example_cap = 0L)), layers = 4),
      "design_geometry_inconsistent"
    )
    expect_equal(none_shown$expected, rep(NA_character_, 4L), info = none)
  }
  expect_equal(report_text("preflight_blank_value"), "(blank)")
  # Two distinct blanks in one group each show as the blank text, every value listed and
  # counted (R133; m101): all four hauls at station 1.
  fish <- edited("hauls", "station_id", 3:4, "stations_1")
  fish <- edited("hauls", "gear", 1:2, c("", "  "), fish = fish)
  full <- opened(fish_run(fish, settings, layers = 4), "design_geometry_inconsistent")
  expect_equal(full$expected, rep("(blank); (blank); EF; GN", 4L))
  capped <- opened(
    fish_run(fish, c(settings, list(example_cap = 1L)), layers = 4),
    "design_geometry_inconsistent"
  )
  expect_equal(capped$expected, rep("(blank) (and 3 more)", 4L))
})

test_that("design_duplicate_signature writes a column of another type as text (D18.39)", {
  settings <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = "species"
  )
  # A text-typed column stored as numbers: 0.1 + 0.2 and 0.3 write alike, so hauls 1 and 3
  # are twins.
  catches <- data.table::copy(fx_fish_clean()$catches)
  data.table::set(catches, j = "species", value = c(0.1 + 0.2, 1, 2, 3, 0.3, 1, 4, 5))
  fish <- fx_fish_clean()
  fish$catches <- catches
  found <- opened(fish_run(fish, settings, layers = 4), "design_duplicate_signature")
  expect_equal(placed_at(found), c("hauls haul_id 1", "hauls haul_id 3"))
})

test_that("subplot_label_ambiguous doesn't run on a table given without its key (D9.24)", {
  testthat::skip_on_cran()
  settings <- list(
    subplot_label_table = "hauls", subplot_label_columns = c("station_id", "gear")
  )
  fish <- edited("hauls", "gear", 2L, fx_fish_clean()$hauls$gear[[1L]])
  hauls <- data.table::copy(fish$hauls)
  data.table::set(hauls, j = "haul_id", value = NULL)
  fish$hauls <- hauls
  rows <- fish_run(fish, settings)$results
  row <- rows[rows$rule_id == "subplot_label_ambiguous"]
  expect_equal(c(row$status, row$not_run_reason), c("not_run", "no_column"))
})

test_that("subplot_label_ambiguous leaves a record without a key to pk_missing (R45)", {
  testthat::skip_on_cran()
  settings <- list(
    subplot_label_table = "hauls", subplot_label_columns = c("station_id", "gear")
  )
  fish <- edited("hauls", "gear", 2L, fx_fish_clean()$hauls$gear[[1L]])
  # Haul 2's key NA, or the missing-value code "?", which a key can't hold: it has none.
  for (none in c(NA_character_, "?")) {
    run <- fish_run(edited("hauls", "haul_id", 2L, none, fish = fish), settings)
    expect_equal(nrow(opened(run, "subplot_label_ambiguous")), 0L, info = none)
    expect_equal(placed_at(opened(run, "pk_missing")), "hauls haul_id 2", info = none)
  }
})

test_that("subplot_label_ambiguous compares a composite key by its parts (D18.42)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("a", "b", "label"), key_type = c("PK", "PK", "."),
    data_type = "character"
  )
  spec <- gpq_read_spec(dictionary)
  # "x|y" and "z", and "x" and "y|z": two keys, which their parts joined with "|" would make one.
  tables <- list(t = data.table::data.table(
    a = c("x|y", "x"), b = c("z", "y|z"), label = c("L", "L")
  ))
  settings <- list(subplot_label_table = "t", subplot_label_columns = "label")
  results <- gpq_check_conformance(tables, spec, settings = settings, layers = 4)
  expect_equal(placed_at(opened(results, "subplot_label_ambiguous")), c("t label 1", "t label 2"))
})

test_that("design_duplicate_signature keeps rows apart whatever their text holds", {
  settings <- list(
    design_signature_table = "catches", design_signature_by = "haul_id",
    design_signature_columns = "species"
  )
  # Haul 1's rows "P\nQ" and "X", haul 2's "P" and "Q\nX": joined by line breaks they read alike.
  fish <- edited("catches", "species", 1:4, c("P\nQ", "X", "P", "Q\nX"))
  found <- opened(fish_run(fish, settings, layers = 4), "design_duplicate_signature")
  expect_equal(nrow(found), 0L)
})

# ---- the final fix wave ----

test_that("design_geometry_by naming an unknown attribute in an option isn't read (D18.90)", {
  testthat::skip_on_cran()
  withr::local_options(
    groundplotqc.design_geometry_table = "catches",
    groundplotqc.design_geometry_by = c("haul_id", "nothing_here"),
    groundplotqc.design_geometry_columns = "weight"
  )
  run <- function() {
    withCallingHandlers(
      fish_run(fx_fish_clean(), layers = 4),
      gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
    )
  }
  expect_warning(fish_run(fx_fish_clean(), layers = 4), class = "gpq_preflight_warning")
  # Grouped by haul_id alone, every catch would fail: the rule doesn't run instead.
  rows <- run()$results
  row <- rows[rows$rule_id == "design_geometry_inconsistent"]
  expect_equal(c(row$table_name, row$status, row$not_run_reason), c("*", "not_run", "no_input"))
  # design_geometry_columns keeps the per-name drop, which only narrows what is compared: each
  # haul's two catches are of different species.
  withr::local_options(
    groundplotqc.design_geometry_by = "haul_id",
    groundplotqc.design_geometry_columns = c("nothing_here", "species")
  )
  found <- opened(run(), "design_geometry_inconsistent")
  expect_equal(placed_at(found), paste("catches species", 1:8))
})

test_that("subplot_label_columns naming an unknown attribute in an option isn't read (D18.90)", {
  testthat::skip_on_cran()
  withr::local_options(
    groundplotqc.subplot_label_table = "hauls",
    groundplotqc.subplot_label_columns = c("nothing_here", "gear")
  )
  results <- withCallingHandlers(
    fish_run(fx_fish_clean(), layers = 4),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  # Labelled by gear alone, hauls 1 and 4 would fail: the rule doesn't run instead.
  rows <- results$results
  row <- rows[rows$rule_id == "subplot_label_ambiguous"]
  expect_equal(c(row$table_name, row$status, row$not_run_reason), c("*", "not_run", "no_input"))
  expect_equal(nrow(opened(results, "subplot_label_ambiguous")), 0L)
})

test_that("design_signature_columns naming an unknown attribute in an option isn't read (D18.90)", {
  withr::local_options(
    groundplotqc.design_signature_table = "catches",
    groundplotqc.design_signature_by = "haul_id",
    groundplotqc.design_signature_columns = c("species", "nothing_here")
  )
  results <- withCallingHandlers(
    fish_run(fx_fish_clean(), layers = 4),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  # Signed by species alone, hauls 1 and 3, and 2 and 4, would be twins: the rule doesn't run
  # instead.
  rows <- results$results
  row <- rows[rows$rule_id == "design_duplicate_signature"]
  expect_equal(c(row$table_name, row$status, row$not_run_reason), c("*", "not_run", "no_input"))
  expect_equal(nrow(opened(results, "design_duplicate_signature")), 0L)
})

test_that("the other settings an option names keep their known names (D18.90)", {
  withr::local_options(
    groundplotqc.coordinate_attributes = c("depth", "nothing_here"),
    groundplotqc.code_set_skip_attributes = c("nothing_here", "station_name"),
    groundplotqc.unknown_codes = c(nothing_here = "X", species = "UNK")
  )
  context <- conformance_context(fx_fish_spec(), NULL, list(), 0:4)
  expect_equal(context$coordinate_attributes, "depth")
  expect_equal(context$code_set_skip_attributes, "station_name")
  expect_equal(context$unknown_codes, c(species = "UNK"))
})

test_that("pk_duplicate compares a number-typed key by its number, as the joins do (D18.100)", {
  testthat::skip_on_cran()
  dir <- withr::local_tempdir()
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("k", "v"), key_type = c("PK", "."),
    data_type = c("integer", "character")
  )
  as_csv <- function(name, lines) {
    path <- file.path(dir, paste0(name, ".csv"))
    writeLines(c("k,v", lines), path)
    c(t = path)
  }
  # "1" and "01" read from a CSV file are one key to the joins, so both holders fail, each
  # reported as given.
  read <- as_csv("number", c("1,a", "01,b", "2,c"))
  found <- opened(gpq_check_conformance(read, gpq_read_spec(dictionary), layers = 0:4))
  expect_equal(paste(found$rule_id, placed_at(found)), paste("pk_duplicate", c("t k 1", "t k 2")))
  expect_equal(found$value, c("1", "01"))
  # Text that isn't a number is compared as written: "n/a" twice repeats, "tbd" doesn't. Layer 4
  # alone, as coercion_loss would mask them.
  written <- as_csv("written", c("n/a,a", "n/a,b", "tbd,c"))
  found <- opened(gpq_check_conformance(written, gpq_read_spec(dictionary), layers = 4))
  expect_equal(placed_at(found), c("t k 1", "t k 2"))
  # A text-typed key is compared as given: "1" and "01" are two keys.
  dictionary$data_type <- "character"
  results <- gpq_check_conformance(read, gpq_read_spec(dictionary), layers = 0:4)
  expect_equal(nrow(opened(results)), 0L)
})

# the final fix wave's follow-up
test_that("subplot_label_ambiguous compares a number-typed key as pk_duplicate does (D18.104)", {
  testthat::skip_on_cran()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "t.csv")
  writeLines(c("k,label", "1,L", "01,L", "2,M"), path)
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("k", "label"), key_type = c("PK", "."),
    data_type = c("integer", "character")
  )
  settings <- list(subplot_label_table = "t", subplot_label_columns = "label")
  # "1" and "01" are one key: only pk_duplicate reports the two records.
  results <- gpq_check_conformance(
    c(t = path), gpq_read_spec(dictionary),
    layers = 0:4, settings = settings
  )
  found <- opened(results)
  expect_equal(paste(found$rule_id, placed_at(found)), paste("pk_duplicate", c("t k 1", "t k 2")))
  # A text-typed key is compared as given: two keys sharing a label.
  dictionary$data_type <- "character"
  results <- gpq_check_conformance(
    c(t = path), gpq_read_spec(dictionary),
    layers = 0:4, settings = settings
  )
  found <- opened(results)
  expect_equal(
    paste(found$rule_id, placed_at(found)),
    paste("subplot_label_ambiguous", c("t label 1", "t label 2"))
  )
})

test_that("key_incoherent compares differing plain numbers again as text (D18.101)", {
  dictionary <- data.frame(
    table_name = c("parent", "parent", "child", "child", "child"),
    attribute_name = c("p_id", "x", "c_id", "p_id", "x"),
    key_type = c("PK", NA, "PK", "FK", NA), reference_table = c(NA, NA, NA, "parent", NA),
    data_type = c("character", "numeric", "character", "character", "numeric")
  )
  spec <- gpq_read_spec(dictionary, key_pairs = data.frame(
    table_name = "child", left = "x", right = "p_id > x", note = NA
  ))
  # Child 1's copy differs from its parent's past the 15th significant digit, which
  # value_text() doesn't write; child 2's within it; child 3's is its parent's as stored.
  tables <- list(
    parent = data.table::data.table(p_id = c("a", "b", "c"), x = c(0.1 + 0.2, 1, 5)),
    child = data.table::data.table(
      c_id = c("c1", "c2", "c3"), p_id = c("a", "b", "c"), x = c(0.3, 2, 5)
    )
  )
  real_text <- value_text
  written <- list()
  testthat::local_mocked_bindings(value_text = function(x) {
    written[[length(written) + 1L]] <<- x
    real_text(x)
  })
  found <- opened(gpq_check_conformance(tables, spec, layers = 4), "key_incoherent")
  expect_equal(placed_at(found), "child x 2")
  expect_equal(c(found$value, found$expected), c("x=2", "x=1"))
  # Only the rows that differ as stored are written as text, never child 3's.
  doubles <- Filter(is.double, written)
  expect_true(any(vapply(doubles, identical, logical(1), c(0.3, 2))))
  expect_false(any(vapply(doubles, function(x) 5 %in% x, logical(1))))
})

test_that("a number-typed attribute's expected lists blanks, numbers, then the rest (D18.102)", {
  testthat::skip_on_cran()
  dictionary <- data.frame(
    table_name = "t", attribute_name = c("id", "g", "v"), key_type = c("PK", ".", "."),
    data_type = c("character", "character", "numeric")
  )
  spec <- gpq_read_spec(dictionary)
  settings <- list(
    design_geometry_table = "t", design_geometry_by = "g", design_geometry_columns = "v"
  )
  expected_at <- function(v, cap) {
    tables <- list(t = data.table::data.table(id = paste0("r", seq_along(v)), g = "a", v = v))
    results <- gpq_check_conformance(
      tables, spec,
      layers = 4, settings = c(settings, list(example_cap = cap))
    )
    unique(opened(results, "design_geometry_inconsistent")$expected)
  }
  # Read as text: numbers that sort otherwise as text, a blank, and two values that aren't
  # numbers, in C order after the numbers.
  read <- c("10", "2", "tbd", "  ", "25", "3", "n/a", "4")
  expect_equal(expected_at(read, 10L), "(blank); 2; 3; 4; 10; 25; n/a; tbd")
  expect_equal(
    expected_at(read, 5L),
    report_text("expected_capped", values = "(blank); 2; 3; 4; 10", n = 3L)
  )
  # Stored as numbers, the same order.
  stored <- c(10, 2, 2, 4, 25, 3, 3, 4)
  expect_equal(expected_at(stored, 10L), "2; 3; 4; 10; 25")
  expect_equal(expected_at(stored, 3L), "2; 3; 4 (and 2 more)")
})
