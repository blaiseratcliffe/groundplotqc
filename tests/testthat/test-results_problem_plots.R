# Tests for the problem plots' record counts: unit_record_counts(), the run's unit_records
# table (plan 8.8, 9.1; D9.31, D23.24).

test_that("a run counts each problem unit's records per group and table (8.8; D23.24)", {
  # The fish survey's stations as the problem unit, grouped by water body. Haul 1 names a blank
  # station, so it and its catches have no unit; haul 4 names a station no record holds, which
  # is its unit still, as its issues would carry it, in no water body.
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  data.table::set(hauls, i = c(1L, 4L), j = "station_id", value = c("", "S-none"))
  fish$hauls <- hauls
  results <- suppressWarnings(gpq_check_conformance(
    fish, fx_fish_spec(),
    group_cols = "water_body", problem_unit = list("stations", "station_id"), layers = 0
  ))
  # By table in the dictionary's order, then by group and key.
  expect_identical(results$unit_records, data.table::data.table(
    run_id = results$metadata$run$run_id,
    water_body = c("LK", "RV", "*", "LK", "RV", "*", "LK", "RV"),
    station_id = c(
      "stations_1", "stations_2", "S-none", "stations_1", "stations_2", "S-none", "stations_1",
      "stations_2"
    ),
    table_name = rep(c("stations", "hauls", "catches"), c(2L, 3L, 3L)),
    n_records = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L)
  ))
  # A key column that is a group column comes once, and the counts are the same.
  spec <- fx_fish_spec()
  run_id <- results$metadata$run$run_id
  input <- conformance_input(fish, spec, c("water_body", "station_id"), "station_id")
  expect_identical(unit_record_counts(input, run_id), results$unit_records)
  # Without a problem unit the table is empty, with the group columns only.
  empty <- unit_record_counts(conformance_input(fish, spec, "water_body"), run_id)
  expect_identical(
    empty, empty_table(c(
      run_id = "character", water_body = "character", table_name = "character",
      n_records = "integer"
    ))
  )
})

test_that("records reading their unit in two tables, or as numbers alike, count as one (16.2)", {
  # The unit's key is zone, a number trees hold as their plots do. Trees 1 and 2 keep their own
  # 4.75, trees 3 and 7 their own 0.1 + 0.2 and 0.3, which read alike, and tree 4 takes its
  # plot's 0.3, so the three count as one unit of trees; tree 5's missing-value code takes its
  # plot's 1.5, and tree 6, whose plot no record holds, has no unit.
  dictionary <- data.frame(
    table_name = c(rep("plots", 2L), rep("trees", 3L)),
    attribute_name = c("plot_id", "zone", "tree_id", "plot_id", "zone"),
    key_type = c("PK", ".", "PK", "FK", "."),
    reference_table = c(NA, NA, NA, "plots", NA),
    data_type = c("character", "numeric", "character", "character", "numeric")
  )
  spec <- gpq_read_spec(dictionary, sentinels = gpq_sentinels(numeric = c(missing = -1)))
  tables <- list(
    plots = data.table::data.table(plot_id = c("p1", "p2"), zone = c(1.5, 0.3)),
    trees = data.table::data.table(
      tree_id = paste0("t", 1:7), plot_id = c("p1", "p2", "p1", "p2", "p1", "p9", "p1"),
      zone = c(4.75, 4.75, 0.1 + 0.2, NA, -1, NA, 0.3)
    )
  )
  input <- conformance_input(tables, spec, key_col = "zone")
  expect_equal(input$key_rows$trees$home, c(rep("trees", 3L), "plots", "plots", NA, "trees"))
  counted <- unit_record_counts(input, "20261117T120000Z_abc123")
  expect_identical(counted, data.table::data.table(
    run_id = "20261117T120000Z_abc123", zone = c("0.3", "1.5", "0.3", "1.5", "4.75"),
    table_name = rep(c("plots", "trees"), c(2L, 3L)), n_records = c(1L, 1L, 3L, 1L, 2L)
  ))
  # The key as the issues carry it.
  expect_identical(
    sort(unique(key_text(input, "trees", 1:7))), c("*", sort(unique(counted$zone)))
  )
})
