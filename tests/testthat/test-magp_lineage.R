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

test_that("the flags decode MAGPlot's ON age trees, an orphan measurement stopping nothing", {
  spec <- magp_spec()
  trees <- data.table::copy(magp_example$magp_trees)
  meas <- data.table::copy(magp_example$magp_tree_meas)
  meas <- rbind(meas, meas[1L])
  meas$magp_tree_id[[nrow(meas)]] <- "no such tree"
  flags <- age_tree_flags(list(magp_trees = trees, magp_tree_meas = meas))
  expect_true(all(flags$magp_trees$magp_tree_id %in% trees$magp_tree_id))
  input <- conformance_input(list(magp_trees = trees, magp_tree_meas = meas), spec)
  expect_no_error(flag_values(flags, input))
})

test_that("a table or column not given flags no tree Y", {
  expect_equal(nrow(age_tree_flags(list())$magp_trees), 0L)
  only_trees <- list(magp_trees = data.frame(magp_tree_id = "a"))
  expect_equal(age_tree_flags(only_trees)$magp_trees$age_tree, "N")
})
