# Tests for attribution to scripts (plan 10.4; D4.4 to D4.6, D23.27, D23.39, D23.40): each
# issue's script, block and attribution status from the attribution map, set by reference by
# one keyed join. Hand-made issues and maps; the synthetic example map read as a run reads it.

# A map with 10.4's own columns, one row per element of the arguments, keyed by `region`.
map_rows <- function(region, table_name, attribute_name, script_file, script_block = NA,
                     script_commit = NA) {
  data.frame(
    region = region, table_name = table_name, attribute_name = attribute_name,
    script_file = script_file, script_block = script_block, script_commit = script_commit,
    note = NA
  )
}

# Issues with only the columns attribution reads and sets, and one it doesn't, all as a run
# leaves them before attribution: the three set columns NA.
issue_rows <- function(region, table_name, column_name) {
  data.table::data.table(
    rule_id = "value_above_max", table_name = table_name, column_name = column_name,
    region = region, script_file = NA_character_, script_block = NA_character_,
    attribution_status = NA_character_
  )
}

test_that("an issue takes its row's script and block, its status from the commits (D23.40)", {
  map <- rbind(
    map_rows("north", "plots", "slope", "north.R", "block 3", "a1b2c3d"),
    map_rows("north", "plots", "aspect", "north.R", "block 3"),
    map_rows("north", "trees", "species", "shared.R", "block 7", "e4f5a6b"),
    map_rows("north", "trees", "*", "north.R", "block 8", "a1b2c3d"),
    map_rows("north", "*", "*", "north.R"),
    map_rows("south", "*", "*", "south.R", "all")
  )
  # In order: north's three mapped columns and a table-level issue on trees, which its column
  # "*" meets; a north column with no row; south's issue on a column with no row; an east
  # issue, whose region has no row at all; and an issue with no region ("*"), which the map's
  # rows of "*" alone would meet.
  issues <- issue_rows(
    region = c("north", "north", "north", "north", "north", "south", "east", "*"),
    table_name = c("plots", "plots", "trees", "trees", "trees", "plots", "plots", "plots"),
    column_name = c("slope", "aspect", "species", "*", "height", "slope", "slope", "slope")
  )
  before <- data.table::copy(issues)
  address <- data.table::address(issues)
  out <- withVisible(attribute_issues(issues, map, "region"))
  # Set by reference and returned invisibly, the other columns untouched.
  expect_false(out$visible)
  expect_identical(data.table::address(out$value), address)
  expect_identical(issues[, 1:4], before[, 1:4])
  expect_equal(
    issues$script_file,
    c("north.R", "north.R", "shared.R", "north.R", "north.R", "south.R", NA, NA)
  )
  expect_equal(
    issues$script_block,
    c("block 3", "block 3", "block 7", "block 8", NA, "all", NA, NA)
  )
  # No commit for the delivery's scripts, as magp_meta records none today (request 5): every
  # mapped issue's script version is unknown; the others are unmapped, with their region's
  # whole script or none.
  expect_equal(
    issues$attribution_status,
    c(rep("script_version_unknown", 4L), rep("unmapped", 4L))
  )
  # The delivery's commit given: current where the row's is the same, may_be_stale where it
  # differs or the row gives none; unmapped stays unmapped.
  attribute_issues(issues, map, "region", script_commit = "a1b2c3d")
  expect_equal(
    issues$attribution_status,
    c("current", "may_be_stale", "may_be_stale", "current", rep("unmapped", 4L))
  )
})

test_that("the map is read as pre-flight reads it, keyed by the caller's column (D23.27)", {
  map <- rbind(
    map_rows("north", "plots", "slope", "north.R", "block 3", "a1b2c3d"),
    map_rows("north", "*", "*", "north.R")
  )
  names(map)[[1L]] <- "jurisdiction"
  issues <- issue_rows(c("north", "north"), "plots", c("slope", "aspect"))
  data.table::setnames(issues, "region", "jurisdiction")
  # From a CSV file, a number in a cell read as its text, as from a data.frame.
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(map, path)
  from_frame <- data.table::copy(attribute_issues(data.table::copy(issues), map, "jurisdiction"))
  attribute_issues(issues, path, "jurisdiction")
  expect_identical(issues, from_frame)
  expect_equal(issues$script_file, c("north.R", "north.R"))
  expect_equal(issues$attribution_status, c("script_version_unknown", "unmapped"))
  # The map's key column is the one the caller names: a map keyed by another column, or with
  # one beyond its own, is a caller's error in the reader's words.
  expect_error(
    attribute_issues(issues, map, "region"),
    paste(
      "`attribution_map` must have exactly the columns region, table_name, attribute_name,",
      "script_file, script_block, script_commit, note; missing: region; not taken:",
      "jurisdiction."
    ),
    fixed = TRUE
  )
  map$colour <- "red"
  expect_error(attribute_issues(issues, map, "jurisdiction"), "not taken: colour.", fixed = TRUE)
  # So is a file that doesn't read cleanly, naming its first problem.
  writeLines(
    c(
      "jurisdiction,table_name,attribute_name,script_file,script_block,script_commit,note",
      "north,plots,slope,north.R,,,",
      "north,plots,aspect,north.R,,,,again"
    ),
    path
  )
  expect_error(
    attribute_issues(issues, path, "jurisdiction"), "`attribution_map` doesn't read cleanly"
  )
})

test_that("a map row with a blank key matches nothing; a repeated key's first row wins", {
  # Pre-flight stops on both (input_unparseable, D23.39): attribution never meets them in a
  # run, and reads them this way if called on its own.
  map <- rbind(
    map_rows("north", "plots", NA, "blank.R"),
    map_rows("north", "plots", "slope", "first.R"),
    map_rows("north", "plots", "slope", "second.R"),
    map_rows(NA, "*", "*", "nobody.R")
  )
  issues <- issue_rows(c("north", "north", NA), "plots", c("slope", NA, "slope"))
  attribute_issues(issues, map, "region")
  expect_equal(issues$script_file, c("first.R", NA, NA))
  expect_equal(issues$attribution_status, c("script_version_unknown", "unmapped", "unmapped"))
  # An empty map, or no issues, sets the three columns all the same.
  attribute_issues(issues, map[0, ], "region")
  expect_equal(issues$script_file, rep(NA_character_, 3L))
  expect_equal(issues$attribution_status, rep("unmapped", 3L))
  none <- issues[0]
  attribute_issues(none, map, "region")
  expect_identical(none$attribution_status, character())
})

test_that("the example map passes pre-flight and attributes MAGPlot's issues by jurisdiction", {
  path <- system.file("extdata", "examples", "attribution_map.csv", package = "groundplotqc")
  found <- preflight_check_functions()[["input_unparseable"]](
    magp_spec(), preflight_context(inputs = list(attribution_map = path))
  )
  expect_equal(nrow(found), 0L)
  # Two of magp_example's jurisdictions, each with a mapped column and one the map lacks.
  issues <- issue_rows(
    c("AB", "AB", "BC", "BC"), c("magp_sites", "magp_sites", "magp_plots", "magp_plots"),
    c("estab_date", "comments", "plot_slope", "comments")
  )
  data.table::setnames(issues, "region", "jurisdiction")
  attribute_issues(issues, path, "jurisdiction")
  expect_false(anyNA(issues$script_file))
  expect_equal(
    issues$attribution_status,
    rep(c("script_version_unknown", "unmapped"), 2L)
  )
})
