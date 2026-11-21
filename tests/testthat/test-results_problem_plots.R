# Tests for the problem plots' record counts: unit_record_counts(), the run's unit_records
# table (plan 8.8, 9.1; D9.31, D23.24); and rates, problem_plot_rates() (D23.37).

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

test_that("a unit's rate counts its records with an open blocking issue, each once (8.8; D23.37)", {
  # Group columns n and area, named like the counts' own, so no caller's column meets them
  # (16.2); the key column plot. P1 has 6 records in two tables, P2 4, P3 2.
  unit_records <- complete_table(list(
    run_id = "20261117T120000Z_abc123", n = c("u1", "u1", "u1", "u1", "u2"),
    area = c("A1", "A1", "A1", "A2", "B1"), plot = c("P1", "P1", "P2", "P3", "P4"),
    table_name = c("plots", "trees", "trees", "trees", "trees"),
    n_records = c(1L, 5L, 4L, 2L, 3L)
  ), results_schema(c("n", "area"), "plot")$unit_records, "unit_records")
  # Issue by issue: P1's tree 2 twice (two rules), counted once; its plot record by a flag; a
  # warning on P2, not blocking; an error on P2 acknowledged, one not evaluated (its root P1's
  # tree 2) and a column-level one, none counted; an error whose unit is "*" and one of a unit
  # with no counts (P9), neither counted; P4's error, its own.
  rule_id <- c(
    "raw_na", "value_above_max", "code_not_in_list", "decimals_exceeded", "raw_na",
    "value_above_max", "raw_na", "raw_na", "raw_na", "raw_na"
  )
  level <- c(rep("record", 6L), "column", rep("record", 3L))
  issues <- new_issues(list(
    issue_id = 1:10, run_id = "20261117T120000Z_abc123", run_scope = "package",
    rule_id = rule_id, rule_fingerprint = "f", stage = "conformance",
    layer = c(2L, 3L, 3L, 3L, 2L, 3L, 2L, 2L, 2L, 2L),
    severity = c("error", "error", "flag", "warning", rep("error", 6L)), reason_code = "*",
    table_name = c("trees", "trees", "plots", rep("trees", 7L)),
    column_name = "x",
    record_pk = c("T2", "T2", "P1", "T7", "T7", "T2", "*", "T20", "T30", "T40"),
    row_number = c(2L, 2L, 1L, 7L, 7L, 2L, NA, 20L, 30L, 40L),
    pk_positional = ifelse(level == "record", FALSE, NA), level = level,
    n_records = ifelse(level == "column", 4L, NA_integer_),
    n = c("u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1", "u1", "u2"),
    area = c("A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1", "B1"),
    plot = c("P1", "P1", "P1", "P2", "P2", "P2", "P2", "*", "P9", "P4"),
    reaches_other_unit = FALSE, root_issue_id = c(rep(NA, 5L), 1L, rep(NA, 4L)),
    status = c(rep("open", 4L), "acknowledged", "not_evaluated", rep("open", 4L))
  ), group_cols = c("n", "area"), key_col = "plot")
  rates <- problem_plot_rates(unit_records, issues, c("n", "area"), "plot", c("error", "flag"))
  expect_identical(rates, data.table::data.table(
    n = c("u1", "u1", "u1", "u2"), area = c("A1", "A1", "A2", "B1"),
    plot = c("P1", "P2", "P3", "P4"), n_records = c(6L, 4L, 2L, 3L),
    n_blocked = c(2L, 0L, 0L, 1L), rate = c(2 / 6, 0, 0, 1 / 3)
  ))
  # Counted within the report unit alone, and with warnings blocking too.
  rates <- problem_plot_rates(unit_records, issues, "n", "plot", c("error", "warning"))
  expect_identical(rates$plot, c("P1", "P2", "P3", "P4"))
  expect_identical(rates$n_blocked, c(1L, 1L, 0L, 1L))
  # A caller's column named like one of the rates is refused when the run starts (D20.76).
  for (name in c("n_blocked", "rate")) {
    expect_error(
      results_schema(c("unit", name), "plot"),
      sprintf("The caller's columns %s have the names of columns the results already have.", name),
      fixed = TRUE
    )
  }
})
