# Tests for chains of foreign keys in D18.1's " > " notation (D18.1, D18.2, D18.27, D18.31):
# parse_chain(), chain_rows() and chain_values().
#
# Task cuts (M5): task 6's refactor commit (6a) writes the file without the last test, which
# D18.31's commit (6b) adds.

test_that("parse_chain() follows each foreign key to the attribute where the chain ends", {
  spec <- magp_spec()
  keys <- spec$keys
  attributes <- spec$attributes
  one <- parse_chain("magp_dataset_id > magp_contributor_id", "magp_sites", keys, attributes)
  expect_equal(nrow(one$steps), 1L)
  expect_equal(
    c(one$steps$table_name, one$steps$attribute_name, one$steps$reference_table),
    c("magp_sites", "magp_dataset_id", "magp_datasets")
  )
  expect_equal(c(one$table, one$attribute), c("magp_datasets", "magp_contributor_id"))
  two <- parse_chain(
    "magp_site_id > magp_dataset_id > magp_contributor_id", "magp_plots", keys, attributes
  )
  expect_equal(two$steps$table_name, c("magp_plots", "magp_sites"))
  expect_equal(c(two$table, two$attribute), c("magp_datasets", "magp_contributor_id"))
  # A chain of one part is an attribute of the table itself.
  own <- parse_chain("elevation", "magp_sites", keys, attributes)
  expect_equal(nrow(own$steps), 0L)
  expect_equal(c(own$table, own$attribute), c("magp_sites", "elevation"))
})

test_that("parse_chain() gives NULL for a chain that doesn't parse", {
  spec <- magp_spec()
  parse <- function(chain, table = "magp_sites") {
    parse_chain(chain, table, spec$keys, spec$attributes)
  }
  # Not one text.
  expect_null(parse(NA_character_))
  expect_null(parse(c("elevation", "slope")))
  expect_null(parse(1))
  # A blank, padded or missing part, or a separator without its spaces.
  for (chain in c(
    "", " > elevation", "magp_dataset_id > ", "magp_dataset_id >  magp_contributor_id",
    " magp_dataset_id > magp_contributor_id", "magp_dataset_id>magp_contributor_id",
    "magp_dataset_id > > magp_contributor_id"
  )) {
    expect_null(parse(chain), info = chain)
  }
  # A part that isn't a foreign key of the table reached, or a last part it lacks.
  expect_null(parse("elevation > magp_contributor_id"))
  expect_null(parse("magp_dataset_id > elevation"))
  expect_null(parse("magp_contributor_id", "magp_sites"))
  expect_null(parse("magp_dataset_id > magp_contributor_id", "magp_plots"))
})

test_that("chain_rows() gives each row its row where the keys end, NA where they break", {
  spec <- fx_fish_spec()
  fish <- fx_fish_clean()
  steps <- function(chain, table) parse_chain(chain, table, spec$keys, spec$attributes)$steps
  input <- conformance_input(fish, spec)
  # Hauls 1 and 2 are station 1's, 3 and 4 station 2's; each catch is a haul's in turn.
  up <- steps("station_id > water_body", "hauls")
  expect_equal(chain_rows(input, "hauls", up), c(1L, 1L, 2L, 2L))
  through <- steps("haul_id > station_id > water_body", "catches")
  expect_equal(chain_rows(input, "catches", through), rep(1:2, each = 4L))
  # With no steps, every row is its own.
  expect_equal(chain_rows(input, "stations", steps("water_body", "stations")), 1:2)
  # NA, a sentinel and an orphan key break the chain, for the rows after them too.
  hauls <- data.table::copy(fish$hauls)
  data.table::set(hauls, i = 1:3, j = "station_id", value = c(NA, "?", "stations_9"))
  broken <- fish
  broken$hauls <- hauls
  input <- conformance_input(broken, spec)
  expect_equal(chain_rows(input, "hauls", up), c(NA, NA, NA, 2L))
  expect_equal(chain_rows(input, "catches", through), c(rep(NA, 6L), 2L, 2L))
  # A key stored as numbers joins the parent's text as text (D16.42, m9).
  numbered <- fish
  numbered$stations <- data.table::copy(fish$stations)
  numbered$hauls <- data.table::copy(fish$hauls)
  data.table::set(numbered$stations, j = "station_id", value = c("1", "2"))
  data.table::set(numbered$hauls, j = "station_id", value = c(1L, 1L, 2L, 2L))
  input <- conformance_input(numbered, spec)
  expect_equal(chain_rows(input, "hauls", up), c(1L, 1L, 2L, 2L))
})

test_that("chain_values() reads the attribute where the chain ends, NA where it breaks", {
  spec <- fx_fish_spec()
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  data.table::set(catches, i = 1L, j = "haul_id", value = "hauls_9")
  fish$catches <- catches
  input <- conformance_input(fish, spec)
  chain <- parse_chain("haul_id > station_id > water_body", "catches", spec$keys, spec$attributes)
  expect_equal(chain_values(input, "catches", chain), c(NA, rep("LK", 3L), rep("RV", 4L)))
  # A table reached whose column wasn't given gives NA throughout.
  stations <- data.table::copy(fish$stations)
  data.table::set(stations, j = "water_body", value = NULL)
  fish$stations <- stations
  input <- conformance_input(fish, spec)
  expect_true(all(is.na(chain_values(input, "catches", chain))))
})
