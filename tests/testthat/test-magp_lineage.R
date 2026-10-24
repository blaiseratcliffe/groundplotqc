# Tests for MAGPlot's derived flags and its lineage spec (D19.17, D19.27, D19.41, D19.83; R1).

test_that("age_tree is Y only for a tree every measurement of which has tree_type A", {
  tables <- list(
    magp_trees = data.frame(magp_tree_id = c("a", "b", "c", "d")),
    magp_tree_meas = data.frame(
      magp_tree_id = c("a", "a", "b", "b", "c"),
      tree_type = c("A", "A", "A", "L", NA)
    )
  )
  flags <- age_tree_flags(tables)
  expect_named(flags, "magp_trees")
  expect_equal(flags$magp_trees$magp_tree_id, c("a", "b", "c", "d"))
  expect_equal(flags$magp_trees$age_tree, c("Y", "N", "N", "N"))
})

test_that("a measurement whose tree key matches no tree adds no row (R1)", {
  tables <- list(
    magp_trees = data.frame(magp_tree_id = c("a", "a", NA, " ")),
    magp_tree_meas = data.frame(magp_tree_id = c("a", "orphan"), tree_type = c("A", "A"))
  )
  flags <- age_tree_flags(tables)$magp_trees
  expect_equal(flags$magp_tree_id, "a")
  expect_equal(flags$age_tree, "Y")
})

test_that("a table or column not given flags no tree Y", {
  expect_equal(nrow(age_tree_flags(list())$magp_trees), 0L)
  only_trees <- list(magp_trees = data.frame(magp_tree_id = "a"))
  expect_equal(age_tree_flags(only_trees)$magp_trees$age_tree, "N")
})

test_that("a number-typed tree key's flags are keyed by its numbers, each key once", {
  tables <- list(
    magp_trees = data.frame(magp_tree_id = c(1, NA, 2, 1)),
    magp_tree_meas = data.frame(magp_tree_id = c(1, 2, 2), tree_type = c("A", "A", "L"))
  )
  flags <- age_tree_flags(tables)$magp_trees
  expect_equal(flags$magp_tree_id, c(1, 2))
  expect_equal(flags$age_tree, c("Y", "N"))
})

test_that("MAGPlot's spec decodes A2-shaped IDs of a BC and an ON contributor (D19.30)", {
  spec <- magp_spec()
  trees <- data.table::copy(magp_example$magp_trees)
  meas <- data.table::copy(magp_example$magp_tree_meas)
  # Three ON trees (the first an age tree: every measurement of it has tree_type "A"; the second
  # and third hold one ID) and three BC trees (a plot and a tree number, one with a missing
  # plot, one with too many parts).
  rows <- 1:6
  data.table::set(trees, rows, "src_tree_id", c(
    "9001", "9002", "9002", "4021_1_17", "4021_X_18", "4021_1_1_9"
  ))
  data.table::set(meas, which(meas$magp_tree_id == trees$magp_tree_id[[1L]]), "tree_type", "A")
  # A measurement whose tree key matches no tree adds no flag, and decoding with the flags
  # stops on nothing.
  meas <- rbind(meas, meas[1L])
  meas$magp_tree_id[[nrow(meas)]] <- "no such tree"
  flags <- age_tree_flags(list(magp_trees = trees, magp_tree_meas = meas))
  expect_true(all(flags$magp_trees$magp_tree_id %in% trees$magp_tree_id))
  age_trees <- flags$magp_trees$magp_tree_id[flags$magp_trees$age_tree == "Y"]
  expect_equal(age_trees, trees$magp_tree_id[[1L]])
  # One planted issue per tree, made by hand (helper-lineage.R), the run's contributor column
  # giving each tree's contributor. The builder plants value_below_min on a column "height",
  # which magp_trees lacks, so the issues are moved to tree_dist, a number column it has
  # (M6's minors pass, m35).
  issues <- fx_lineage_made_issues(fx_mark_fixture(list(magp_trees = trees)), rows)
  data.table::set(issues, j = "magp_contributor_id", value = rep(c("170", "110"), each = 3L))
  data.table::set(issues, j = "column_name", value = "tree_dist")
  moved <- sub("|height|", "|tree_dist|", issues$fingerprint_pk, fixed = TRUE)
  data.table::set(issues, j = "fingerprint_pk", value = moved)
  planted <- spec$attributes$attribute_name[spec$attributes$table_name == "magp_trees"]
  expect_true(all(issues$column_name %in% planted))
  decode <- function(flags) {
    gpq_decode_lineage(
      issues, list(magp_trees = trees), spec,
      key_col = "magp_contributor_id", flags = flags
    )
  }
  decode(flags)
  expect_equal(issues$provider_key, c(
    "tblAgeTree.agetreekey=9001", "tblTree.treekey=9002", "tblTree.treekey=9002",
    "faib_header.site_identifier=4021; faib_plot_header.plot=1; faib_tree_detail.tree_no=17",
    "faib_header.site_identifier=4021; faib_plot_header.plot=X; faib_tree_detail.tree_no=18",
    "4021_1_1_9"
  ))
  expect_equal(issues$provider_key_status, c(
    "decoded", "decoded", "decoded", "decoded", "partial", "raw"
  ))
  expect_equal(
    issues$fingerprint[[1L]], "value_below_min|magp_trees|tree_dist|tblAgeTree.agetreekey=9001"
  )
  # The two trees holding one ID share a key, so theirs is the pk: form (D19.15); no other is.
  expect_equal(issues$fingerprint[2:3], issues$fingerprint_pk[2:3])
  expect_false(any(issues$fingerprint[-(2:3)] == issues$fingerprint_pk[-(2:3)]))
  # ON's condition reads the flag, which a caller who gives none is told of.
  expect_error(decode(NULL), "flag.age_tree")
  # ON's other two conditions, on the same spec (D19.27): a tree measurement's own tree_type, and
  # a subplot measurement's subplot, reached by its foreign key, being of frame_type "R".
  tree_meas <- data.table::copy(magp_example$magp_tree_meas)
  data.table::set(tree_meas, 1:2, "src_treemeas_id", c("9101", "9102"))
  on_tree_meas <- fx_lineage_made_issues(fx_mark_fixture(list(magp_tree_meas = tree_meas)), 1:2)
  data.table::set(on_tree_meas, j = "magp_contributor_id", value = "170")
  gpq_decode_lineage(
    on_tree_meas, list(magp_tree_meas = tree_meas), spec,
    key_col = "magp_contributor_id"
  )
  expect_equal(tree_meas$tree_type[1:2], c("A", "H"))
  expect_equal(
    on_tree_meas$provider_key, c("tblAgeTree.agetreekey=9101", "tblTreeMsr.treemsrkey=9102")
  )
  subplots <- data.table::copy(magp_example$magp_subplots)
  data.table::set(subplots, 1L, "frame_type", "R")
  subplot_meas <- data.table::copy(magp_example$magp_subplot_meas)
  data.table::set(subplot_meas, 1:2, "src_subpmeas_id", c("8101", "8102"))
  on_subplot_meas <- fx_lineage_made_issues(
    fx_mark_fixture(list(magp_subplots = subplots, magp_subplot_meas = subplot_meas)), 1:2
  )
  data.table::set(on_subplot_meas, j = "magp_contributor_id", value = "170")
  gpq_decode_lineage(
    on_subplot_meas, list(magp_subplots = subplots, magp_subplot_meas = subplot_meas), spec,
    key_col = "magp_contributor_id"
  )
  expect_equal(subplot_meas$magp_subplot_id[1:2], subplots$magp_subplot_id[1:2])
  expect_equal(
    on_subplot_meas$provider_key,
    c("tblVegPlot.vegplotkey=8101", "tblTreeGrowthPlot.treegrowthplotkey=8102")
  )
})

test_that("MAGPlot's lineage spec has ON's three conditions and the separator (D19.27, D19.41)", {
  lineage <- magp_spec()$lineage_spec
  expect_equal(unique(lineage$separator), "_")
  conditioned <- lineage[!is.na(lineage$when)]
  expect_equal(conditioned$contributor_label, rep("ON", 3L))
  # Each row's table, ID, provider column (D19.17, D19.27) and condition.
  expect_setequal(
    paste(
      conditioned$table_name, conditioned$attribute_name, conditioned$part_source,
      conditioned$when
    ),
    c(
      "magp_trees src_tree_id tblAgeTree.agetreekey flag.age_tree == \"Y\"",
      "magp_tree_meas src_treemeas_id tblAgeTree.agetreekey tree_type == \"A\"",
      paste(
        "magp_subplot_meas src_subpmeas_id tblVegPlot.vegplotkey",
        "magp_subplot_id > frame_type == \"R\""
      )
    )
  )
  # All three condition the first part of a one-part ID.
  expect_equal(conditioned$part_order, rep(1L, 3L))
  expect_equal(conditioned$id_status, rep("single", 3L))
  # Each condition sits just before its part's own A2 row.
  at <- which(!is.na(lineage$when))
  own <- lineage[at + 1L]
  expect_true(all(is.na(own$when)))
  expect_equal(own$table_name, conditioned$table_name)
  expect_equal(own$attribute_name, conditioned$attribute_name)
  expect_equal(own$part_order, conditioned$part_order)
  manifest <- magp_spec()$manifest
  expect_equal(
    manifest$file[manifest$input == "lineage_when"], "data-raw/magp/lineage_when.csv"
  )
})
