# Tests for lineage decoding (plan 9.3, 9.6, 10.1, 10.2; D19.10 to D19.17, D19.41 to D19.48;
# R1 to R9). The fixtures are helper-lineage.R's: the issues are made by hand, except in the
# two tests that need a run's own (D19.77).

# The trees' issues in their rows' order, one per tree.
decoded_by_tree <- function(issues) {
  trees <- issues[issues$table_name == "trees"]
  trees[order(trees$row_number), c(
    "record_pk", "provider_key", "provider_key_status", "fingerprint", "fingerprint_pk"
  )]
}

# The default spec, read once for the tests that don't change it (D19.79).
lineage_spec_default <- fx_lineage_spec()

test_that("each status, its key and the fingerprint (D19.13, D19.15, D19.16, D19.47)", {
  spec <- lineage_spec_default
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
  # Classification meets the decoded fingerprints (D9.22, D19.54): an override on t4's
  # provider-key fingerprint and one on t1's, which shares its key with t8 and so keeps the pk:
  # form, class those two issues and no other.
  gpq_classify_issues(
    issues, NULL,
    overrides = fx_class_overrides(
      fingerprint = c(got$fingerprint[[4L]], got$fingerprint[[1L]]),
      issue_class = c("harmonization", "source")
    )
  )
  classed <- issues[issues$table_name == "trees"]
  classed <- classed[order(classed$row_number)]
  expect_equal(classed$class_basis == "override", c(TRUE, FALSE, FALSE, TRUE, rep(FALSE, 4L)))
  expect_equal(classed$issue_class[c(1L, 4L)], c("source", "harmonization"))
  expect_equal(sum(issues$class_basis == "override"), 2L)
})

test_that("a derived part is labelled with its bare attribute name (D19.47)", {
  lineage <- data.frame(
    contributor_label = "north", table_name = c("sites", "trees"),
    attribute_name = c("src_site_id", "src_tree_id"), spec_type = "id",
    source_text = c("survey.site", "src_site_id + kind"), note = NA, source_cell = NA
  )
  spec <- fx_lineage_spec(lineage_spec = lineage)
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, 1:8)
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
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key[4:5], c("field.any_key=k100", "field.age_key=a200"))
})

test_that("a condition reads a parent's column through a chain (D19.27)", {
  spec <- fx_lineage_spec(lineage_when = fx_lineage_when("site_id > src_site_id == \"S9\""))
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(
    decoded_by_tree(issues)$provider_key[4:5], c("field.age_key=k100", "field.age_key=a200")
  )
})

test_that("a flag is joined by key, a record without one holding no condition (D19.42, D19.43)", {
  spec <- fx_lineage_spec(lineage_when = fx_lineage_when("flag.tagged == \"Y\""))
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, 1:8)
  flags <- list(trees = data.frame(tree_id = c("t5", "t4"), tagged = c("N", "Y")))
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor", flags = flags)
  expect_equal(
    decoded_by_tree(issues)$provider_key[4:5], c("field.age_key=k100", "field.tree_key=a200")
  )
  # A record whose key the flags don't give (t5 here) holds no condition.
  gpq_decode_lineage(
    issues, tables, spec,
    key_col = "contributor",
    flags = list(trees = data.frame(tree_id = "t4", tagged = "Y"))
  )
  expect_equal(
    decoded_by_tree(issues)$provider_key[4:5], c("field.age_key=k100", "field.tree_key=a200")
  )
  # A repeated PK: each holder gets the key's flag.
  twice <- tables
  twice$trees$tree_id[[5L]] <- "t4"
  issues <- fx_lineage_made_issues(twice, 1:8)
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
  issues <- fx_lineage_made_issues(tables, 1:8)
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
  spec <- lineage_spec_default
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, 1:8)
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
  numbered_issues <- fx_lineage_made_issues(numbered, 1:8)
  expect_no_error(gpq_decode_lineage(numbered_issues, numbered, spec, key_col = "contributor"))
  # A record whose PK is missing has a positional record_pk, which isn't compared (D19.14).
  nameless <- tables
  nameless$trees$tree_id[[2L]] <- NA
  nameless_issues <- fx_lineage_made_issues(nameless, 1:8)
  expect_true(nameless_issues$pk_positional[[2L]])
  expect_no_error(gpq_decode_lineage(nameless_issues, nameless, spec, key_col = "contributor"))
})

test_that("without a separator a composite ID is raw and a single one decodes (R7)", {
  spec <- fx_lineage_spec(separator = NULL)
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, 1:8)
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
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key_status[4:5], c("raw", "raw"))
  expect_equal(got$provider_key[4:5], c("k100", "a200"))
})

test_that("a value holding the separator is raw, its parts never guessed (D8.17)", {
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[[1L]] <- "41_7_"
  spec <- lineage_spec_default
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(decoded_by_tree(issues)$provider_key_status[[1L]], "raw")
})

test_that("a missing, blank or sentinel ID gives fallback_pk (D19.13)", {
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[1:3] <- c(NA, " ", "X")
  spec <- lineage_spec_default
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  got <- decoded_by_tree(issues)
  expect_equal(got$provider_key_status[1:3], rep("fallback_pk", 3L))
  expect_true(all(is.na(got$provider_key[1:3])))
})

test_that("decoding changes no table and keeps the issues' other columns (16.2)", {
  spec <- lineage_spec_default
  tables <- fx_lineage_tables()
  before <- data.table::copy(tables)
  issues <- fx_lineage_issues(spec, tables)
  kept <- setdiff(names(issues), c("provider_key", "provider_key_status", "fingerprint"))
  other <- data.table::copy(issues[, kept, with = FALSE])
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_identical(tables, before)
  expect_identical(issues[, kept, with = FALSE], other)
  # Tables given as data.tables: none changed, and none left with a key or an index.
  tables <- lapply(tables, data.table::as.data.table)
  before <- lapply(tables, data.table::copy)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_identical(tables, before)
  expect_null(data.table::key(tables$trees))
  expect_null(data.table::indices(tables$trees))
  expect_null(data.table::indices(tables$sites))
})

test_that("the issues and key_col are checked (W176, W177)", {
  spec <- lineage_spec_default
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, 1:8)
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
  # read it; the decoder makes such a value NA before it calls this (D19.69). The labels are
  # contributors the lineage spec has entries for.
  nbsp <- strrep(intToUtf8(160L), 2L)
  spec$code_lists <- rbind(spec$code_lists, data.table::data.table(
    sheet = "who", source_row = c(3L, 3L, 4L, 4L),
    sheet_column = c("contributor", "label", "contributor", "label"),
    value = c("*", "north", nbsp, "south"), source_cell = NA_character_
  ))
  expect_equal(contributor_labels(spec, "contributor", c("*", nbsp, NA)), c("north", "south", NA))
  # So a record whose key_col value is "*" or blank has no label, and so no lineage spec, while
  # the sheet's own key gives its label's decoding (M6's minors pass, m32). t1, t2 and t8 hold
  # one ID, each labelled by its own issue's value: t1 and t2 as delivered, which is one key,
  # and t8 as north's, which is another.
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[[2L]] <- "41_7"
  issues <- fx_lineage_made_issues(tables, c(1L, 2L, 8L))
  data.table::set(issues, j = "contributor", value = c("*", nbsp, "south"))
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(issues$provider_key_status, c("no_lineage_spec", "no_lineage_spec", "decoded"))
  expect_equal(issues$provider_key, c("41_7", "41_7", "survey.site=41; survey.tree=7"))
  expect_equal(issues$fingerprint, c(
    issues$fingerprint_pk[1:2], "value_below_min|trees|height|survey.site=41; survey.tree=7"
  ))
})

# ---- the task 5 fix round: the statuses' corners and the shared keys, issues made by hand ----

test_that("a -1 part, a nested ID that can't expand, an unparseable entry (R5, R6, R8)", {
  tables <- fx_lineage_tables()
  # R5: a part that is a missing-value code of another type is a missing part.
  spec <- lineage_spec_default
  tables$trees$src_tree_id[[1L]] <- "41_-1"
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(issues$provider_key_status[[1L]], "partial")
  expect_equal(issues$provider_key[[1L]], "survey.site=41; survey.tree=-1")
  # R6: a nested ID whose inner entry has alternatives, and one nested in itself, are raw; R8:
  # an unparseable entry, and a contributor with none, are no_lineage_spec; both as delivered.
  lineage <- data.frame(
    contributor_label = c("north", "north", "south", "east"),
    table_name = c("sites", "trees", "trees", "trees"),
    attribute_name = c("src_site_id", "src_tree_id", "src_tree_id", "src_tree_id"),
    spec_type = "id",
    source_text = c(
      "PSP: a.x | other: b.y", "src_site_id + survey.tree", "src_tree_id + survey.k",
      "not a notation"
    ),
    note = NA, source_cell = NA
  )
  # More nested IDs that can't expand, each for a contributor of its own with a tree of its own
  # (t9 to t13): the inner entry is blank, no_id (X), not_applicable (Z) or unparseable, or the
  # inner ID is an entry of two tables (site_id, of sites and of trees).
  inner <- data.frame(
    contributor_label = c("blank", "noid", "notapp", "badtext", "twotables", "twotables"),
    table_name = c(rep("sites", 5L), "trees"),
    attribute_name = c(rep("src_site_id", 4L), "site_id", "site_id"),
    spec_type = "id",
    source_text = c(NA, "X", "Z", "not a notation", "survey.site", "survey.site"),
    note = NA, source_cell = NA
  )
  outer <- data.frame(
    contributor_label = c("blank", "noid", "notapp", "badtext", "twotables"),
    table_name = "trees", attribute_name = "src_tree_id", spec_type = "id",
    source_text = c(rep("src_site_id + survey.tree", 4L), "site_id + survey.tree"),
    note = NA, source_cell = NA
  )
  spec <- fx_lineage_spec(lineage_spec = rbind(lineage, inner, outer))
  tables <- fx_lineage_tables()
  tables$sites <- rbind(tables$sites, data.frame(
    site_id = paste0("s", 5:9), contributor = outer$contributor_label,
    src_site_id = paste0("N", 1:5)
  ))
  tables$trees <- rbind(tables$trees, data.frame(
    tree_id = paste0("t", 9:13), site_id = paste0("s", 5:9), kind = "L",
    src_tree_id = paste0("41_", 1:5), height = -5
  ))
  issues <- fx_lineage_made_issues(tables, 1:13)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(issues$provider_key_status, c(
    "raw", "raw", "raw", "raw", "raw", "no_lineage_spec", "no_lineage_spec", "raw", rep("raw", 5L)
  ))
  expect_equal(issues$provider_key, tables$trees$src_tree_id)
  # A missing ID column: no key, the pk: fingerprint.
  spec <- lineage_spec_default
  tables$trees$src_tree_id <- NULL
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(unique(issues$provider_key_status), "fallback_pk")
  expect_true(all(is.na(issues$provider_key)))
  expect_equal(issues$fingerprint, issues$fingerprint_pk)
})

test_that("a key is shared with a holder that has no issue, not with another contributor's (R9)", {
  spec <- lineage_spec_default
  tables <- fx_lineage_tables()
  # t6 (east, no entry) holds t4's ID: the same text, but t6's key is the ID as delivered and
  # t4's is south's labelled one, so neither is shared. t1 and t8 hold one key.
  tables$trees$src_tree_id[[6L]] <- "k100"
  # Issues on t1, t4 and t6: their contributors are north, south and east.
  issues <- fx_lineage_made_issues(tables, c(1L, 4L, 6L))
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(
    issues$provider_key, c("survey.site=41; survey.tree=7", "field.tree_key=k100", "k100")
  )
  # t8, the second holder of t1's key, has no issue.
  expect_equal(issues$fingerprint[[1L]], issues$fingerprint_pk[[1L]])
  expect_equal(issues$fingerprint[[2L]], "value_below_min|trees|height|field.tree_key=k100")
  expect_equal(issues$fingerprint[[3L]], "value_below_min|trees|height|k100")
})

test_that("an integer64 ID's shared key takes the pk: form on each holder (D19.15, D18.10)", {
  # bit64's duplicated() ignores fromLast, so the first holder of a repeated ID was missed.
  testthat::skip_if_not_installed("bit64")
  # The session's numeric rounding is raised for the whole test: data.table reads an integer64
  # exactly at any rounding, so the decoder resets none (R59, M6's minors pass, m25).
  rounding <- data.table::setNumericRounding(2L)
  withr::defer(data.table::setNumericRounding(rounding))
  spec <- lineage_spec_default
  tables <- fx_lineage_tables()
  # South's two trees hold one ID, which one contributor's single-part entry decodes; t2 and t3
  # hold IDs that differ in their lowest bit only.
  ids <- bit64::as.integer64(c(41, 42, 43, 100, 100, 55, NA, 44))
  ids[2:3] <- bit64::as.integer64(2^62) + 0:1
  tables$trees$src_tree_id <- ids
  decode <- function(rows) {
    issues <- fx_lineage_made_issues(tables, rows)
    gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
    issues
  }
  both <- decode(4:5)
  expect_equal(both$provider_key, rep("field.tree_key=100", 2L))
  expect_equal(both$fingerprint, both$fingerprint_pk)
  # An issue on one holder only, the first or the second: the other holder still shares it.
  first <- decode(4L)
  expect_equal(first$fingerprint, first$fingerprint_pk)
  second <- decode(5L)
  expect_equal(second$fingerprint, second$fingerprint_pk)
  # An ID held once keeps the provider key's fingerprint.
  alone <- decode(c(4L, 6L))
  expect_equal(alone$fingerprint[[2L]], "value_below_min|trees|height|55")
  # Two IDs that differ in the lowest bit are two IDs: each keeps the provider key's
  # fingerprint, and decoding leaves the session's rounding as it found it.
  pair <- decode(2:3)
  expect_equal(pair$fingerprint, paste0(
    "value_below_min|trees|height|", c("4611686018427387904", "4611686018427387905")
  ))
  expect_equal(data.table::getNumericRounding(), 2L)
})

# ---- the checkpoint's fixes ----

test_that("decoding the trees alone labels each holder of a shared ID as the run did (D19.76)", {
  tables <- fx_lineage_tables()
  # The trees alone, as R57's test gives MAGPlot's: no tree reaches its site's contributor.
  trees <- list(trees = tables$trees)
  # t1 and t8 hold one key, each labelled by its own issue's contributor: both keep the pk:
  # form.
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, trees, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$provider_key[c(1L, 8L)], rep("survey.site=41; survey.tree=7", 2L))
  expect_equal(issues$fingerprint[c(1L, 8L)], issues$fingerprint_pk[c(1L, 8L)])
  # An issue on t1 only: t8, which the trees alone can't label, counts as holding t1's key. A
  # key held once keeps the provider key's fingerprint.
  issues <- fx_lineage_made_issues(tables, c(1L, 4L))
  gpq_decode_lineage(issues, trees, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$fingerprint[[1L]], issues$fingerprint_pk[[1L]])
  expect_equal(issues$fingerprint[[2L]], "value_below_min|trees|height|field.tree_key=k100")
  # With the run's tables nothing changes (D19.76): t6 (east, no issue) holds t4's ID, but its
  # own contributor labels it, so t4 keeps its key (M6's minors pass, m34).
  whole <- tables
  whole$trees$src_tree_id[[6L]] <- "k100"
  issues <- fx_lineage_made_issues(whole, 4L)
  gpq_decode_lineage(issues, whole, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$fingerprint, "value_below_min|trees|height|field.tree_key=k100")
  # A holder is unlabelled where the column key_col is in isn't given, or no row of its home
  # table is reached (R61): t8, no issue, then counts as holding t1's key.
  issues <- fx_lineage_made_issues(tables, 1L)
  no_column <- tables
  no_column$sites$contributor <- NULL
  gpq_decode_lineage(issues, no_column, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$fingerprint, issues$fingerprint_pk)
  issues <- fx_lineage_made_issues(tables, 1L)
  no_link <- tables
  no_link$trees$site_id <- NULL
  gpq_decode_lineage(issues, no_link, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$fingerprint, issues$fingerprint_pk)
})

test_that("a table's records are decoded by its first ID attribute that has an entry (R4)", {
  # kind is marked an ID too, ahead of src_tree_id, but the lineage spec gives it no entry.
  spec <- lineage_spec_default
  spec$attributes <- data.table::copy(spec$attributes)
  at <- which(spec$attributes$attribute_name == "kind")
  data.table::set(spec$attributes, at, "id_marked", TRUE)
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, 1:8)
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(
    issues$provider_key[c(1L, 4L)], c("survey.site=41; survey.tree=7", "field.tree_key=k100")
  )
  expect_equal(issues$provider_key_status[c(1L, 4L)], c("decoded", "decoded"))
})

# ---- M7a: a key that can't split back exactly (D20.35) ----

test_that("a part or a label holding \"; \" or \"=\" gives raw, the ID as delivered (D20.35)", {
  tables <- fx_lineage_tables()
  # North's IDs split on "_" into two parts: t1's second part holds "; ", and t2's first holds
  # "=" beside a missing-value code, which alone would be partial. South's ID is one part: t4's
  # holds "=". t8 keeps "41_7", now held by no other tree, and decodes.
  tables$trees$src_tree_id[c(1L, 2L, 4L)] <- c("41_7; b", "41=_X", "k=100")
  issues <- fx_lineage_made_issues(tables, c(1L, 2L, 4L, 8L))
  gpq_decode_lineage(issues, tables, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$provider_key_status, c("raw", "raw", "raw", "decoded"))
  expect_equal(
    issues$provider_key, c("41_7; b", "41=_X", "k=100", "survey.site=41; survey.tree=7")
  )
  # The fingerprint follows the key: the ID as delivered for a raw record (9.6), as no other
  # tree holds any of the four keys.
  expect_equal(issues$fingerprint, paste0(
    "value_below_min|trees|height|",
    c("41_7; b", "41=_X", "k=100", "survey.site=41; survey.tree=7")
  ))
  # A label holding "=": a spec read by gpq_read_spec() gives none (a part's source is a
  # table.column or a bare name), so south's is changed after reading, and t4's clean ID is
  # raw too.
  spec <- lineage_spec_default
  spec$lineage_spec <- data.table::copy(spec$lineage_spec)
  at <- which(spec$lineage_spec$part_source == "field.tree_key")
  data.table::set(spec$lineage_spec, at, "part_source", "field.tree=key")
  tables <- fx_lineage_tables()
  issues <- fx_lineage_made_issues(tables, c(4L, 8L))
  gpq_decode_lineage(issues, tables, spec, key_col = "contributor")
  expect_equal(issues$provider_key_status, c("raw", "decoded"))
  expect_equal(issues$provider_key, c("k100", "survey.site=41; survey.tree=7"))
})

test_that("\"=\" is found in a part's bytes, in UTF-8 and latin1 text alike (D20.35)", {
  # t4's one part holds "=" beside a non-ASCII letter, in UTF-8, and t5's is the same text
  # marked latin1: both raw. t2's second part is the letter alone, with no joiner: decoded.
  utf8 <- "é=1"
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  expect_identical(Encoding(c(utf8, latin1)), c("UTF-8", "latin1"))
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[c(2L, 4L, 5L)] <- c("41_é", utf8, latin1)
  issues <- fx_lineage_made_issues(tables, c(2L, 4L, 5L))
  gpq_decode_lineage(issues, tables, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$provider_key_status, c("decoded", "raw", "raw"))
  expect_equal(issues$provider_key, c("survey.site=41; survey.tree=é", utf8, utf8))
})

# ---- M7a: a key held by a record with another ID (D20.107) ----

test_that("an ID spelled as another record's decoded key shares the pk: form (D20.107)", {
  spelled <- "survey.site=41; survey.tree=7"
  # South's ID is one part, so t4's, spelled as north's t8's decoded key, is raw (D20.35) and
  # its key is its ID: t8's key. t1 moves off "41_7", so no two trees share an ID.
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[c(1L, 4L)] <- c("42_7", spelled)
  issues <- fx_lineage_made_issues(tables, c(1L, 4L, 8L))
  gpq_decode_lineage(issues, tables, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$provider_key_status, c("decoded", "raw", "decoded"))
  expect_equal(issues$provider_key, c("survey.site=42; survey.tree=7", spelled, spelled))
  expect_equal(issues$fingerprint, c(
    "value_below_min|trees|height|survey.site=42; survey.tree=7", issues$fingerprint_pk[2:3]
  ))
  # Each finds the other where it alone has an issue: t8 as its key is t4's ID, t4 as its
  # key's values joined by the separator are t8's ID.
  issues <- fx_lineage_made_issues(tables, 8L)
  gpq_decode_lineage(issues, tables, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$fingerprint, issues$fingerprint_pk)
  issues <- fx_lineage_made_issues(tables, 4L)
  gpq_decode_lineage(issues, tables, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$fingerprint, issues$fingerprint_pk)
  # A holder so found that the tables can't label counts as holding the key (D19.76): t8,
  # where sites lack their contributor column.
  issues <- fx_lineage_made_issues(tables, 4L)
  no_column <- tables
  no_column$sites$contributor <- NULL
  gpq_decode_lineage(issues, no_column, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$fingerprint, issues$fingerprint_pk)
  # M6's case: north's entry has two parts, and t1's ID, spelled as t8's key, doesn't split
  # on "_", so it is raw and its key is its ID: t8's key.
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[1L] <- spelled
  issues <- fx_lineage_made_issues(tables, c(1L, 8L))
  gpq_decode_lineage(issues, tables, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$provider_key_status, c("raw", "decoded"))
  expect_equal(issues$provider_key, c(spelled, spelled))
  expect_equal(issues$fingerprint, issues$fingerprint_pk)
  # A record found by a key but holding another doesn't count: east's t6 has no entry, so its
  # key is its ID, whose value "41_7" is t1's and t8's ID, but they decode to north's key.
  tables <- fx_lineage_tables()
  tables$trees$src_tree_id[6L] <- "field.tree_key=41_7"
  issues <- fx_lineage_made_issues(tables, 6L)
  gpq_decode_lineage(issues, tables, lineage_spec_default, key_col = "contributor")
  expect_equal(issues$provider_key_status, "no_lineage_spec")
  expect_equal(issues$fingerprint, "value_below_min|trees|height|field.tree_key=41_7")
})
