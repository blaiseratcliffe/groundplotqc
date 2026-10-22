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

test_that("a blank key matches nothing, so no record reaches a parent through it (D18.31)", {
  spec <- fx_fish_spec()
  steps <- parse_chain("station_id > water_body", "hauls", spec$keys, spec$attributes)$steps
  # Empty, ASCII spaces and a no-break space, on both sides of the join (D16.42, D18.15).
  for (blank in c("", "  ", intToUtf8(160L))) {
    fish <- fx_fish_clean()
    fish$stations <- data.table::copy(fish$stations)
    fish$hauls <- data.table::copy(fish$hauls)
    data.table::set(fish$stations, i = 1L, j = "station_id", value = blank)
    data.table::set(fish$hauls, i = 1:2, j = "station_id", value = blank)
    input <- conformance_input(fish, spec, "water_body")
    expect_equal(chain_rows(input, "hauls", steps), c(NA, NA, 2L, 2L), info = blank)
    # So hauls 1 and 2 have no group value, where they had station 1's.
    groups <- input$groups$hauls
    expect_equal(
      groups$values$water_body[groups$row_group], c("*", "*", "RV", "RV"),
      info = blank
    )
    # The record reaches nothing through reach_column() either.
    expect_equal(reach_column(input, "hauls", "water_body")$rows, c(NA, NA, 2L, 2L), info = blank)
  }
})

# task 13's fix round 4 (D18.73): match()'s `incomparables` of more than one value isn't always
# honoured, so the keys that match nothing are masked after the match
test_that("a missing-value code matches nothing, whatever codes the parent's key holds", {
  # The fish spec with "X" and "Z" as the missing-value codes of text and dates, as MAGPlot's.
  spec <- gpq_read_spec(
    dictionary = example_file("fish_dictionary.csv"), code_lists = fx_fish_code_lists(),
    column_map = fx_fish_column_map(), type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "X", not_applicable = "Z"),
      date = c(missing = "X", not_applicable = "Z")
    )
  )
  # The stations keyed "X", "Z" and a key marked UTF-8, which makes R hash text by its content,
  # so the slip is the same in every session; haul 1 names "Z".
  fish <- fx_fish_clean()
  stations <- rbind(fish$stations, fish$stations[c(1L, 1L, 1L)])
  marked <- paste0("stations_", intToUtf8(0xE9))
  data.table::set(stations, i = 3:5, j = "station_id", value = c("X", "Z", marked))
  data.table::set(stations, i = 3:5, j = "water_body", value = c("LK", "RV", "LK"))
  fish$stations <- stations
  fish$hauls <- data.table::copy(fish$hauls)
  data.table::set(fish$hauls, i = 1L, j = "station_id", value = "Z")
  expect_equal(Encoding(stations$station_id[[5L]]), "UTF-8")
  input <- conformance_input(fish, spec, "water_body")
  steps <- parse_chain("station_id > water_body", "hauls", spec$keys, spec$attributes)$steps
  # Haul 1's "Z" doesn't reach the station keyed "Z", although "X" comes before it, so neither
  # do its catches.
  expect_equal(chain_rows(input, "hauls", steps), c(NA, 1L, 2L, 2L))
  through <- parse_chain("haul_id > station_id > water_body", "catches", spec$keys, spec$attributes)
  expect_equal(chain_rows(input, "catches", through$steps), c(NA, NA, 1L, 1L, 2L, 2L, 2L, 2L))
  # So haul 1 has no group value, where it took the station keyed "Z"'s (reach_column()).
  groups <- input$groups$hauls
  expect_equal(groups$values$water_body[groups$row_group], c("*", "LK", "RV", "RV"))
  expect_equal(reach_column(input, "hauls", "water_body")$rows, c(NA, 1L, 2L, 2L))
})

# the Copilot fix round (R170)
test_that("parse_chain() passes over a dictionary row with no table or attribute name", {
  # Grandchildren g reach their children c and through them the parents p. p and g each have a
  # row with no attribute name, and one row has no table name but the name "missing".
  spec <- gpq_read_spec(data.frame(
    table_name = c("p", "p", "p", "c", "c", "g", "g", "g", NA),
    attribute_name = c("pid", "name", NA, "cid", "pid", "gid", "cid", NA, "missing"),
    key_type = c("PK", ".", ".", "PK", "FK", "PK", "FK", ".", "."),
    reference_table = c(NA, NA, NA, NA, "p", NA, "c", NA, NA),
    data_type = "character"
  ))
  parse <- function(chain, table) parse_chain(chain, table, spec$keys, spec$attributes)
  # An endpoint the table lacks doesn't parse, beside the row with no attribute name, through
  # no foreign key or two.
  expect_no_condition(own <- parse("missing", "g"))
  expect_null(own)
  expect_no_condition(far <- parse("cid > pid > missing", "g"))
  expect_null(far)
  # Nor beside the row with no table name.
  expect_no_condition(nameless <- parse("missing", "c"))
  expect_null(nameless)
  # A real endpoint still resolves.
  expect_equal(parse("cid", "g")[c("table", "attribute")], list(table = "g", attribute = "cid"))
  through <- parse("cid > pid > name", "g")
  expect_equal(through$steps$table_name, c("g", "c"))
  expect_equal(through[c("table", "attribute")], list(table = "p", attribute = "name"))
})
