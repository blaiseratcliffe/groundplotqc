# Tests for MAGPlot's derived flags (D19.17, D19.41; R1).

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
  # Three ON trees (the first an age tree: every measurement of it has tree_type "A") and three
  # BC trees (a plot and a tree number, one with a missing plot, one with too many parts).
  rows <- 1:6
  data.table::set(trees, rows, "src_tree_id", c(
    "9001", "9002", "9003", "4021_1_17", "4021_X_18", "4021_1_1_9"
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
  # giving each tree's contributor.
  issues <- fx_lineage_made_issues(fx_mark_fixture(list(magp_trees = trees)), rows)
  data.table::set(issues, j = "magp_contributor_id", value = rep(c("170", "110"), each = 3L))
  decode <- function(flags) {
    gpq_decode_lineage(
      issues, list(magp_trees = trees), spec,
      key_col = "magp_contributor_id", flags = flags
    )
  }
  decode(flags)
  expect_equal(issues$provider_key, c(
    "tblAgeTree.agetreekey=9001", "tblTree.treekey=9002", "tblTree.treekey=9003",
    "faib_header.site_identifier=4021; faib_plot_header.plot=1; faib_tree_detail.tree_no=17",
    "faib_header.site_identifier=4021; faib_plot_header.plot=X; faib_tree_detail.tree_no=18",
    "4021_1_1_9"
  ))
  expect_equal(issues$provider_key_status, c(
    "decoded", "decoded", "decoded", "decoded", "partial", "raw"
  ))
  expect_equal(
    issues$fingerprint[[1L]], "value_below_min|magp_trees|height|tblAgeTree.agetreekey=9001"
  )
  # ON's condition reads the flag, which a caller who gives none is told of.
  expect_error(decode(NULL), "flag.age_tree")
})
