# Tests for the column map and the type map (plan 3.4, 4.1; D12.19, D12.22, D12.54).

test_that("gpq_column_map has MAGPlot's names by default and checks its arguments", {
  map <- gpq_column_map()
  expect_s3_class(map, "gpq_column_map")
  expect_named(map, c(
    "table", "attribute", "type", "key_type", "reference", "lookup", "description", "lineage_flag"
  ))
  expect_equal(map$type, c("data_type", "datatype"))
  expect_null(map$lineage_flag)
  flagged <- gpq_column_map(lineage_flag = c(column = "appendix", value = "A2"))
  expect_equal(flagged$lineage_flag, c(column = "appendix", value = "A2"))
  expect_error(gpq_column_map(table = c("a", "b")), "table")
  expect_error(gpq_column_map(lineage_flag = "appendix"), "lineage_flag")
  expect_false("..." %in% names(formals(gpq_column_map)))
})

test_that("gpq_column_map refuses blank and invalid names (D12.54)", {
  expect_error(gpq_column_map(lineage_flag = c(column = "", value = "A2")), "lineage_flag")
  expect_error(gpq_column_map(lineage_flag = c(column = "a", value = NA)), "lineage_flag")
  bad <- rawToChar(as.raw(c(0x61, 0x97)))
  Encoding(bad) <- "UTF-8"
  expect_error(gpq_column_map(table = bad), "table")
})

test_that("gpq_type_map gives D2.15's four rows and replaces them with a map", {
  map <- gpq_type_map()
  expect_equal(map$data_type, c("character", "integer", "numeric", "date"))
  expect_equal(map$r_class, c("character", "integer", "double", "character"))
  expect_equal(map$date_format, c(NA, NA, NA, "%Y-%m-%d"))
  fish <- gpq_type_map(example_file("fish_types.csv"))
  expect_equal(fish$data_type, c("text", "whole", "real", "day"))
  expect_equal(fish$date_format[[4L]], "%Y-%m-%d")
})

test_that("gpq_type_map refuses a bad map", {
  bad_class <- data.frame(data_type = "x", r_class = "factor", date_format = NA)
  expect_error(gpq_type_map(bad_class), "r_class")
  repeated <- data.frame(data_type = c("x", "x"), r_class = "character", date_format = NA)
  expect_error(gpq_type_map(repeated), "data_type")
  dated_number <- data.frame(data_type = "x", r_class = "double", date_format = "%Y")
  expect_error(gpq_type_map(dated_number), "date_format")
  expect_error(gpq_type_map(data.frame(data_type = "x")), "columns")
  expect_error(gpq_type_map(tempdir()), "doesn't exist")
  bad <- rawToChar(as.raw(c(0x61, 0x97)))
  Encoding(bad) <- "UTF-8"
  invalid <- data.frame(data_type = bad, r_class = "character", date_format = NA)
  expect_error(gpq_type_map(invalid), "UTF-8")
})

test_that("the forest example's defect list names records that exist", {
  defects <- read_csv_text(example_file("forest_defects.csv"))$data
  trees <- read_csv_text(example_file("forest_trees.csv"))$data
  meas <- read_csv_text(example_file("forest_tree_meas.csv"))$data
  plots <- read_csv_text(example_file("forest_plots.csv"))$data
  expect_true(all(defects$record_pk[defects$table_name == "TREES"] %in% trees$TREE_ID))
  expect_true(all(defects$record_pk[defects$table_name == "TREE_MEAS"] %in% meas$MEAS_ID))
  expect_false("P03" %in% plots$PLOT_ID)
  expect_equal(
    defects$rule_id,
    c("fk_orphan", "categorical_change", "monotonic_decrease", "status_transition")
  )
})
