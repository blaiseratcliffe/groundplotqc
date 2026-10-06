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

test_that("gpq_column_map refuses a name or flag value of only spaces or tabs (D12.27)", {
  # Blank means empty after trimming, so such a name can never match a column.
  for (blank in c(" ", "\t", "  \t ")) {
    info <- deparse(blank)
    expect_error(gpq_column_map(table = blank), "`table` must be one column name", info = info)
    expect_error(
      gpq_column_map(type = c("data_type", blank)), "`type` must be one or more column names",
      info = info
    )
    for (flag in list(c(column = blank, value = "A2"), c(column = "appendix", value = blank))) {
      expect_error(
        gpq_column_map(lineage_flag = flag), "`lineage_flag` must be NULL or c(column = ",
        fixed = TRUE, info = info
      )
    }
  }
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
  # Each pattern is its own check's wording, which the columns check's message lacks, since
  # that one also names data_type, r_class and date_format.
  bad_class <- data.frame(data_type = "x", r_class = "factor", date_format = NA)
  expect_error(gpq_type_map(bad_class), "r_class is character, integer or double")
  repeated <- data.frame(data_type = c("x", "x"), r_class = "character", date_format = NA)
  expect_error(gpq_type_map(repeated), "data_type must be filled and unique")
  dated_number <- data.frame(data_type = "x", r_class = "double", date_format = "%Y")
  expect_error(gpq_type_map(dated_number), "date_format is only for types of r_class character")
  expect_error(
    gpq_type_map(data.frame(data_type = "x")), "has the columns data_type, r_class and date_format"
  )
  expect_error(gpq_type_map(tempdir()), "doesn't exist")
  bad <- rawToChar(as.raw(c(0x61, 0x97)))
  Encoding(bad) <- "UTF-8"
  invalid <- data.frame(data_type = bad, r_class = "character", date_format = NA)
  expect_error(gpq_type_map(invalid), "UTF-8")
})

test_that("gpq_type_map refuses a data.frame with a repeated column, naming it (D12.54)", {
  # The first copy would be used silently otherwise.
  repeated <- data.frame(
    data_type = "x", r_class = "character", date_format = NA, r_class = "double",
    check.names = FALSE
  )
  expect_error(
    gpq_type_map(repeated), "A type map's column names must not repeat: r_class.",
    fixed = TRUE
  )
  twice <- cbind(repeated, data_type = "y")
  expect_error(
    gpq_type_map(twice), "A type map's column names must not repeat: r_class, data_type.",
    fixed = TRUE
  )
})

test_that("gpq_type_map refuses what isn't NULL, a data.frame or one CSV path (D12.54)", {
  expect_error(
    gpq_type_map(NA_character_), "`map` names a file that doesn't exist.",
    fixed = TRUE
  )
  expect_error(
    gpq_type_map(1), "`map` must be NULL, a data.frame or the path of a CSV file.",
    fixed = TRUE
  )
})

test_that("a blank data_type is refused, and a map with no rows is a map with none", {
  for (blank in c("", "   ", NA_character_)) {
    map <- data.frame(data_type = c("x", blank), r_class = "character", date_format = NA)
    expect_error(
      gpq_type_map(map), "data_type must be filled and unique",
      info = deparse(blank)
    )
  }
  none <- gpq_type_map(data.frame(
    data_type = character(), r_class = character(), date_format = character()
  ))
  expect_equal(nrow(none), 0L)
  expect_named(none, c("data_type", "r_class", "date_format"))
})

test_that("gpq_type_map reads a path ending in .csv, in any case, and names `map` otherwise", {
  lines <- c("data_type,r_class,date_format", "x,character,")
  txt <- withr::local_tempfile(fileext = ".txt")
  writeLines(lines, txt)
  expect_error(
    gpq_type_map(txt), "`map` must be the path of a file ending in .csv.",
    fixed = TRUE
  )
  upper <- withr::local_tempfile(fileext = ".CSV")
  writeLines(lines, upper)
  expect_equal(gpq_type_map(upper)$data_type, "x")
})

test_that("the forest example's defect list names records that exist", {
  defects <- read_csv_text(example_file("forest_defects.csv"))$data
  trees <- read_csv_text(example_file("forest_trees.csv"))$data
  meas <- read_csv_text(example_file("forest_tree_meas.csv"))$data
  plots <- read_csv_text(example_file("forest_plots.csv"))$data
  # A subset that comes out empty would pass every `all()` below, so each is counted first.
  in_trees <- defects$record_pk[defects$table_name == "TREES"]
  in_meas <- defects$record_pk[defects$table_name == "TREE_MEAS"]
  expect_equal(length(in_trees), 1L)
  expect_equal(length(in_meas), 3L)
  expect_true(all(in_trees %in% trees$TREE_ID))
  expect_true(all(in_meas %in% meas$MEAS_ID))
  expect_equal(
    defects$rule_id,
    c("fk_orphan", "categorical_change", "monotonic_decrease", "status_transition")
  )
  # The orphan: P03-T001 is in plot P03, which PLOTS lacks.
  expect_equal(trees$PLOT_ID[trees$TREE_ID == in_trees], "P03")
  expect_false("P03" %in% plots$PLOT_ID)
  # Each temporal defect is a change in 2020's record against 2015's, for the same tree.
  change <- function(earlier, later, column) {
    at <- match(c(earlier, later), meas$MEAS_ID)
    expect_equal(length(unique(meas$TREE_ID[at])), 1L, info = column)
    expect_equal(substr(meas$MEAS_DATE[at], 1L, 4L), c("2015", "2020"), info = column)
    meas[[column]][at]
  }
  expect_equal(change("M003", "M004", "SPECIES"), c("SW", "BW"))
  expect_equal(change("M005", "M006", "DBH"), c("21.4", "19.0"))
  expect_equal(change("M007", "M008", "STATUS"), c("D", "L"))
})
