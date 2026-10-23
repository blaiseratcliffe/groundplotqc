# Tests for lineage decoding (plan 9.3, 9.6, 10.1, 10.2; D19.10 to D19.17, D19.41 to D19.48;
# R1 to R9). The fixtures are helper-lineage.R's.

# The trees' issues in their rows' order, one per tree.
decoded_by_tree <- function(issues) {
  trees <- issues[issues$table_name == "trees"]
  trees[order(trees$row_number), c(
    "record_pk", "provider_key", "provider_key_status", "fingerprint", "fingerprint_pk"
  )]
}

test_that("each status, its key and the fingerprint (D19.13, D19.15, D19.16, D19.47)", {
  spec <- fx_lineage_spec()
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  returned <- gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_identical(returned, issues)
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key_status, c(
    "decoded", "partial", "raw", "decoded", "decoded", "no_lineage_spec", "fallback_pk",
    "decoded"
  ))
  expect_equal(got$provider_key, c(
    "survey.site=41; survey.tree=7", "survey.site=41; survey.tree=X", "41_7_b",
    "field.tree_key=k100", "field.tree_key=a200", "e5", NA, "survey.site=41; survey.tree=7"
  ))
  # t1 and t8 hold one key, so both keep the pk: form; a unique key gives the fingerprint.
  expect_equal(got$fingerprint[c(1L, 8L)], got$fingerprint_pk[c(1L, 8L)])
  expect_equal(got$fingerprint[[4L]], "value_below_min|trees|height|field.tree_key=k100")
  expect_equal(got$fingerprint[[3L]], "value_below_min|trees|height|41_7_b")
  expect_equal(got$fingerprint[[7L]], got$fingerprint_pk[[7L]])
})

test_that("a derived part is labelled with its bare attribute name (D19.47)", {
  lineage <- data.frame(
    contributor_label = "north", table_name = c("sites", "trees"),
    attribute_name = c("src_site_id", "src_tree_id"), spec_type = "id",
    source_text = c("survey.site", "src_site_id + kind"), note = NA, source_cell = NA
  )
  spec <- fx_lineage_spec(lineage_spec = lineage)
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(decoded_by_tree(issues)$provider_key[[1L]], "survey.site=41; kind=7")
})

test_that("a part's conditions are tried in order before the entry's own part (D19.12)", {
  when <- rbind(
    fx_lineage_when("kind == \"A\"", "field.age_key"),
    fx_lineage_when("kind %in% c(\"A\", \"L\")", "field.any_key")
  )
  spec <- fx_lineage_spec(lineage_when = when)
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key[4:5], c("field.any_key=k100", "field.age_key=a200"))
})

test_that("a condition reads a parent's column through a chain (D19.27)", {
  spec <- fx_lineage_spec(lineage_when = fx_lineage_when("site_id > src_site_id == \"S9\""))
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(
    decoded_by_tree(issues)$provider_key[4:5], c("field.age_key=k100", "field.age_key=a200")
  )
})

test_that("a flag is joined by key, a record without one holding no condition (D19.42, D19.43)", {
  spec <- fx_lineage_spec(lineage_when = fx_lineage_when("flag.tagged == \"Y\""))
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  flags <- list(trees = data.frame(tree_id = c("t5", "t4"), tagged = c("N", "Y")))
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor", flags = flags)
  expect_equal(
    decoded_by_tree(issues)$provider_key[4:5], c("field.age_key=k100", "field.tree_key=a200")
  )
  # A repeated PK: each holder gets the key's flag.
  twice <- tables
  twice$trees$tree_id[[5L]] <- "t4"
  issues <- fx_lineage_issues(spec, twice)
  gpq_decode_lineage(
    issues, twice, spec,
    key_col = "contributor",
    flags = list(trees = data.frame(tree_id = "t4", tagged = "Y"))
  )
  expect_equal(
    decoded_by_tree(issues)$provider_key[4:5], c("field.age_key=k100", "field.age_key=a200")
  )
})

test_that("flags of the wrong shape are a caller's error (D19.42; R2)", {
  spec <- fx_lineage_spec(lineage_when = fx_lineage_when("flag.tagged == \"Y\""))
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  decode <- function(flags) {
    gpq_decode_lineage(issues, tables, spec, key_col = "contributor", flags = flags)
  }
  expect_error(decode(list(data.frame(tree_id = "t4", tagged = "Y"))), "named once")
  expect_error(decode(list(stands = data.frame(tree_id = "t4", tagged = "Y"))), "stands")
  expect_error(decode(list(trees = data.frame(tagged = "Y"))), "tree_id")
  expect_error(
    decode(list(trees = data.frame(tree_id = c("t4", "t4"), tagged = "Y"))), "more than once"
  )
  expect_error(decode(list(trees = data.frame(tree_id = "t99", tagged = "Y"))), "t99")
  expect_error(decode(list(trees = data.frame(tree_id = "t4", other = "Y"))), "flag.tagged")
  expect_error(decode(NULL), "flag.tagged")
})

test_that("tables that aren't the issues' own stop decoding (D19.14)", {
  spec <- fx_lineage_spec()
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  shorter <- tables
  shorter$trees <- shorter$trees[1:3, ]
  expect_error(
    gpq_decode_lineage(issues, shorter, spec, key_col = "contributor"), "rows"
  )
  sorted <- tables
  sorted$trees <- sorted$trees[8:1, ]
  expect_error(
    gpq_decode_lineage(issues, sorted, spec, key_col = "contributor"), "holds"
  )
  # The PK is compared as the run writes it, so a number-typed key matches its own row.
  numbered <- tables
  numbered$trees$tree_id <- 1:8
  numbered_issues <- fx_lineage_issues(spec, numbered)
  expect_no_error(gpq_decode_lineage(numbered_issues, numbered, spec, key_col = "contributor"))
})

test_that("without a separator a composite ID is raw and a single one decodes (R7)", {
  spec <- fx_lineage_spec(separator = NULL)
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key_status[c(1L, 4L)], c("raw", "decoded"))
})

test_that("an ID with labelled alternatives is raw (D19.48)", {
  lineage <- data.frame(
    contributor_label = "south", table_name = "trees", attribute_name = "src_tree_id",
    spec_type = "id", source_text = "PSP: a.x + b.y | other: c.z", note = NA, source_cell = NA
  )
  spec <- fx_lineage_spec(lineage_spec = lineage)
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key_status[4:5], c("raw", "raw"))
  expect_equal(got$provider_key[4:5], c("k100", "a200"))
})

test_that("a value holding the separator is raw, its parts never guessed (D8.17)", {
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[[1L]] <- "41_7_"
  spec <- fx_lineage_spec()
  issues <- fx_lineage_issues(spec, tables)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(decoded_by_tree(issues)$provider_key_status[[1L]], "raw")
})

test_that("a missing, blank or sentinel ID gives fallback_pk (D19.13)", {
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[1:3] <- c(NA, " ", "X")
  spec <- fx_lineage_spec()
  issues <- fx_lineage_issues(spec, tables)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key_status[1:3], rep("fallback_pk", 3L))
  expect_true(all(is.na(got$provider_key[1:3])))
})

test_that("decoding changes no table and keeps the issues' other columns (16.2)", {
  spec <- fx_lineage_spec()
  tables <- fx_lineage_tables()
  before <- data.table::copy(tables)
  issues <- fx_lineage_issues(spec, tables)
  kept <- setdiff(names(issues), c("provider_key", "provider_key_status", "fingerprint"))
  other <- data.table::copy(issues[, kept, with = FALSE])
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_identical(tables, before)
  expect_identical(issues[, kept, with = FALSE], other)
})

test_that("the issues and key_col are checked (W166, W167)", {
  spec <- fx_lineage_spec()
  tables <- fx_lineage_tables()
  issues <- fx_lineage_issues(spec, tables)
  expect_error(
    gpq_decode_lineage(as.data.frame(issues), tables, spec, key_col = "contributor"),
    "data.table"
  )
  expect_error(gpq_decode_lineage(issues, tables, spec, key_col = "nowhere"), "key_col")
  expect_error(gpq_decode_lineage(issues, tables, list(), key_col = "contributor"), "spec")
})

test_that("a label comes from the bands' labels sheet when key_col ends their chain (D19.11)", {
  spec <- fx_lineage_spec()
  spec$id_band_columns <- data.table::data.table(
    table_name = "sites", attribute_name = "site_id", key_attribute = "contributor",
    labels_sheet = "who", labels_column = "label"
  )
  spec$code_lists <- data.table::data.table(
    sheet = "who", source_row = c(1L, 1L, 2L, 2L),
    sheet_column = c("contributor", "label", "contributor", "label"),
    value = c("contributor", "label", "south", "north"), source_cell = NA_character_
  )
  labels <- contributor_labels(spec, "contributor", c("south", "north", "*", NA))
  expect_equal(labels, c("north", NA, NA, NA))
  # A sheet keying a row "*" or spaces only labels that value, as record_labels() has always
  # read it; the decoder makes such a value NA before it calls this (D19.69).
  nbsp <- strrep(intToUtf8(160L), 2L)
  spec$code_lists <- rbind(spec$code_lists, data.table::data.table(
    sheet = "who", source_row = c(3L, 3L, 4L, 4L),
    sheet_column = c("contributor", "label", "contributor", "label"),
    value = c("*", "any", nbsp, "spaces"), source_cell = NA_character_
  ))
  expect_equal(contributor_labels(spec, "contributor", c("*", nbsp, NA)), c("any", "spaces", NA))
})
