# Tests for the clean base, the generic fixtures, the fixture marker and the answer sheet
# (plan 18.3; D5.11, D8.21, D15.10), and for magp_example, the example data built from the
# clean base (18.1, 21 M3b).

# Every path of foreign keys from each row of `table` to each ancestor table, as that
# ancestor's PK value: a list by ancestor of a matrix, one column per path.
key_paths <- function(tables, spec, table) {
  links <- spec$keys[spec$keys$key_type == "FK"]
  primary <- spec$keys[spec$keys$key_type == "PK"]
  pk_of <- function(name) primary$attribute_name[primary$table_name == name]
  # Each path's values from `rows` of `name`, named by the ancestor the path reaches.
  walk <- function(name, rows) {
    reached <- list()
    for (k in which(links$table_name == name)) {
      parent <- links$reference_table[[k]]
      values <- tables[[name]][[links$attribute_name[[k]]]][rows]
      parent_rows <- match(values, tables[[parent]][[pk_of(parent)]])
      reached <- c(
        reached, structure(list(as.character(values)), names = parent), walk(parent, parent_rows)
      )
    }
    reached
  }
  paths <- walk(table, seq_len(nrow(tables[[table]])))
  ancestors <- factor(names(paths), unique(names(paths)))
  lapply(split(paths, ancestors), function(x) do.call(cbind, unname(x)))
}

# The properties every clean base has, whatever its spec.
expect_clean_base <- function(tables, spec) {
  fx_require_fixture(tables)
  a <- spec$attributes
  expect_equal(names(tables), unique(a$table_name))
  date_formats <- spec$type_map$date_format[match(a$data_type, spec$type_map$data_type)]
  for (name in names(tables)) {
    table <- tables[[name]]
    columns <- a[a$table_name == name]
    expect_equal(names(table), columns$attribute_name, label = name)
    expect_equal(
      unname(vapply(table, function(x) class(x)[[1L]], "")),
      fifelse(columns$r_class == "double", "numeric", columns$r_class),
      label = name
    )
    expect_false(anyNA(table), label = name)
    expect_true(isTRUE(attr(table, "gpq_fixture")), label = name)
    for (k in seq_len(nrow(columns))) {
      column <- columns$attribute_name[[k]]
      value <- as.character(table[[column]])
      where <- paste0(name, ".", column)
      expect_false(any(value != trimws(value) | !nzchar(value)), label = where)
      coded <- spec$codes$table_name == name & spec$codes$attribute_name == column
      codes <- spec$codes$code[coded]
      if (length(codes) > 0L) {
        expect_true(all(value %in% codes), label = where)
      }
      format <- date_formats[a$table_name == name][[k]]
      if (!is.na(format)) {
        expect_false(anyNA(as.Date(value, format)), label = where)
      }
      expect_false(any(value %in% spec$sentinels$value), label = where)
    }
  }
  # Primary keys are unique; each foreign key points to its parent's row; and every two key
  # paths from a record to one ancestor reach the same row.
  keys <- spec$keys
  for (k in which(keys$key_type == "PK")) {
    expect_false(anyDuplicated(tables[[keys$table_name[[k]]]][[keys$attribute_name[[k]]]]) > 0L)
  }
  for (k in which(keys$key_type == "FK" & keys$reference_table %in% names(tables))) {
    values <- tables[[keys$table_name[[k]]]][[keys$attribute_name[[k]]]]
    parent <- tables[[keys$reference_table[[k]]]][[keys$reference_attribute[[k]]]]
    where <- paste0(keys$table_name[[k]], ".", keys$attribute_name[[k]])
    expect_true(all(values %in% parent), label = where)
  }
  for (name in names(tables)) {
    for (paths in key_paths(tables, spec, name)) {
      expect_true(all(paths == paths[, 1L]), label = name)
    }
  }
  expect_true(isTRUE(attr(tables, "gpq_fixture")))
}

test_that("MAGPlot's clean base has every DD table and column, of its R class (21 M3b gate)", {
  spec <- magp_spec()
  tables <- fx_magp_clean()
  expect_equal(length(tables), 17L)
  expect_clean_base(tables, spec)
  # Two contributors, each with two datasets.
  expect_equal(nrow(tables$magp_contributors), 2L)
  expect_equal(as.vector(table(tables$magp_datasets$magp_contributor_id)), c(2L, 2L))
})

test_that("each dataset is in one jurisdiction and contributor, as its sheet row has it (D15.32)", {
  spec <- magp_spec()
  sheet <- spec$datasets
  tables <- fx_magp_clean()
  sites <- tables$magp_sites
  datasets <- tables$magp_datasets
  of_site <- match(sites$magp_dataset_id, sheet$magp_dataset_id)
  of_dataset <- match(datasets$magp_dataset_id, sheet$magp_dataset_id)
  expect_equal(sites$jurisdiction, sheet$jurisdiction[of_site])
  expect_equal(datasets$magp_contributor_id, sheet$magp_contributor_id[of_dataset])
  # So each dataset's sites hold one jurisdiction, and the four datasets span two.
  per_dataset <- tapply(sites$jurisdiction, sites$magp_dataset_id, data.table::uniqueN)
  expect_equal(as.vector(per_dataset), rep(1L, 4L))
  expect_equal(sort(unique(sites$jurisdiction)), c("AB", "BC"))
  # Still a clean base: its codes, keys and key paths hold.
  expect_clean_base(tables, spec)
})

test_that("the clean base's spec passes pre-flight with MAGPlot's rule set (21 M3b gate)", {
  expect_no_error(suppressWarnings(gpq_preflight(magp_spec(), rules = magp_rules())))
})

test_that("the toy specs build clean the same way: the generic fixtures (18.3)", {
  expect_clean_base(fx_fish_clean(), fx_fish_spec())
  expect_equal(vapply(fx_fish_clean(), nrow, 1L), c(stations = 2L, hauls = 4L, catches = 8L))
  expect_clean_base(fx_clean_tables(fx_forest_spec()), fx_forest_spec())
})

test_that("the clean base refuses a spec it can't build", {
  spec <- fx_fish_spec()
  looped <- spec
  looped$keys <- rbind(spec$keys, data.table::data.table(
    table_name = "stations", attribute_name = "station_name", key_type = "FK", key_part = 1L,
    reference_table = "catches", reference_attribute = "catch_id"
  ))
  expect_error(fx_clean_tables(looped), "go round in a circle")
  twice <- spec
  twice$keys <- rbind(spec$keys, spec$keys[spec$keys$key_type == "PK"][1L])
  expect_error(fx_clean_tables(twice), "single-column PK")
  pointing <- spec
  pointing$keys <- rbind(spec$keys, data.table::data.table(
    table_name = "hauls", attribute_name = "haul_id", key_type = "FK", key_part = 1L,
    reference_table = "hauls", reference_attribute = "haul_id"
  ))
  expect_error(fx_clean_tables(pointing), "points at itself")
  elsewhere <- spec
  elsewhere$keys <- data.table::copy(spec$keys)
  data.table::set(elsewhere$keys,
    i = which(spec$keys$table_name == "catches" & spec$keys$key_type == "FK"),
    j = "reference_attribute", value = "gear"
  )
  expect_error(fx_clean_tables(elsewhere), "point at their parent's PK")
  # A value that can't take its column's class: a text code in a column of whole numbers.
  untyped <- spec
  untyped$attributes <- data.table::copy(spec$attributes)
  data.table::set(untyped$attributes,
    i = which(spec$attributes$attribute_name == "water_body"), j = "r_class", value = "integer"
  )
  expect_error(fx_clean_tables(untyped), "can't make stations.water_body of class integer")
  # A coded key with fewer codes than the table has rows.
  short <- spec
  short$codes <- rbind(spec$codes, data.table::data.table(
    table_name = "stations", attribute_name = "station_id", code = "S1",
    source_type = "sheet", source_name = "stations", source_row = 2L
  ))
  expect_error(fx_clean_tables(short), "needs 2 codes for the key stations.station_id")
})

test_that("a foreign key that can't agree with its record's ancestors stops the base (D15.31)", {
  # Three roots (r2, r3, x), p1 pointing at r2 and r3, p2 at x, r2 and r3, and d at p1 and
  # p2. p1 pairs every r2 row with every r3 row but p2 pairs them one to one, so a d record
  # that p1 places on the pair (r2 = 1, r3 = 2) has no p2 row with that pair.
  # The ancestor p1 and p2 share (r2) is also built under the names of the build's own
  # variables at the join (line, shared, wanted, join_on), which a table of that name must not
  # turn into a column read.
  for (ancestor in c("r2", "line", "shared", "wanted", "join_on")) {
    made <- c(ancestor, "r3", "x", "p1", "p2", "d")
    links <- data.table::data.table(
      table_name = c("p1", "p1", "p2", "p2", "p2", "d", "d"),
      parent = c(ancestor, "r3", "x", ancestor, "r3", "p1", "p2")
    )
    owners <- c(made, links$table_name)
    column_names <- c(paste0(made, "_id"), paste0(links$parent, "_id"))
    spec <- list(
      attributes = data.table::data.table(
        table_name = owners, attribute_name = column_names, data_type = "text",
        r_class = "character", id_marked = FALSE
      ),
      keys = data.table::data.table(
        table_name = owners, attribute_name = column_names,
        key_type = rep(c("PK", "FK"), c(6L, 7L)), key_part = 1L,
        reference_table = c(rep(NA_character_, 6L), links$parent),
        reference_attribute = c(rep(NA_character_, 6L), paste0(links$parent, "_id"))
      ),
      codes = data.table::data.table(
        table_name = character(), attribute_name = character(), code = character()
      ),
      sentinels = data.table::data.table(data_type = character(), value = character()),
      type_map = data.table::data.table(data_type = "text", date_format = NA_character_)
    )
    expect_error(
      fx_clean_tables(spec), "can't make d.p2_id agree with the record's ancestors",
      info = ancestor
    )
    # Without p2's pairing of the ancestor and r3 the same tables build.
    spec$keys <- spec$keys[!(spec$keys$table_name == "p2" & spec$keys$reference_table %in% "r3")]
    spec$attributes <- spec$attributes[
      !(spec$attributes$table_name == "p2" & spec$attributes$attribute_name == "r3_id")
    ]
    expect_clean_base(fx_clean_tables(spec), spec)
  }
})

test_that("the fixture guard refuses tables without the marker (18.3, D8.21)", {
  tables <- fx_fish_clean()
  expect_invisible(fx_require_fixture(tables))
  expect_error(fx_require_fixture(unclass(list(a = 1))), "no gpq_fixture attribute")
  plain <- tables
  attr(plain, "gpq_fixture") <- NULL
  expect_error(fx_require_fixture(plain), "Test helpers take fixtures only")
  # A table taken out of the list is still marked; one that was never marked is refused.
  expect_invisible(fx_require_fixture(tables$stations))
  expect_error(fx_require_fixture(data.table::data.table(a = 1L)), "no gpq_fixture attribute")
})

test_that("the fixture marker goes on a list of data.tables only (D8.21)", {
  one <- data.table::data.table(a = 1L)
  marked <- fx_mark_fixture(list(one = one))
  expect_true(isTRUE(attr(marked, "gpq_fixture")))
  expect_true(isTRUE(attr(marked$one, "gpq_fixture")))
  # A single data.table would mark each of its columns, so it is refused, as is a list
  # holding anything else.
  expect_error(fx_mark_fixture(data.table::data.table(a = 1L)), "takes a list of data.tables")
  expect_error(fx_mark_fixture(list(a = data.frame(a = 1L))), "takes a list of data.tables")
  expect_error(fx_mark_fixture(1:3), "takes a list of data.tables")
})

test_that("the answer sheet and the defect blocks match, every rule covered (18.3)", {
  sheet <- fx_answer_sheet()
  blocks <- fx_defect_blocks()
  expect_equal(names(sheet), c(
    "defect_id", "table_name", "column_name", "record_pk", "edit", "rule_id",
    "cascade_rule_ids", "witness"
  ))
  expect_setequal(sheet$defect_id, names(blocks))
  expect_false(anyDuplicated(sheet$defect_id) > 0L)
  expect_false(anyDuplicated(names(blocks)) > 0L)
  expect_true(all(vapply(sheet, is.character, logical(1))))
  expect_true(all(vapply(blocks, is.function, logical(1))))
  registry <- rule_registry()
  data_rules <- registry$rule_id[registry$check_type != "preflight"]
  expect_true(all(sheet$rule_id %in% data_rules))
  # Every data rule has a defect block; pre-flight checks have their planted spec.
  expect_equal(setdiff(data_rules, sheet$rule_id), character())
  # No row lists its own rule as a cascade, which would let the rule's other issues pass.
  own_cascade <- mapply(function(rule, cascade) rule %in% split_joined(cascade),
    sheet$rule_id, sheet$cascade_rule_ids,
    USE.NAMES = FALSE
  )
  expect_false(any(own_cascade))
})

test_that("magp_example is the clean base, marked as synthetic (D15.10)", {
  # Compared column by column: a saved data.table comes back without its spare slots.
  expect_equal(lapply(magp_example, as.list), lapply(fx_magp_clean(), as.list))
  expect_true(isTRUE(attr(magp_example, "gpq_fixture")))
  expect_true(all(vapply(magp_example, function(table) {
    data.table::is.data.table(table) && isTRUE(attr(table, "gpq_fixture"))
  }, logical(1))))
})

test_that("the clean base raises nothing in Layers 0 to 2 (18.3)", {
  # A full MAGPlot run, about 2 s since M4b cut the run's fixed cost (D16.18, D16.50).
  testthat::skip_on_cran()
  results <- fx_caught(fx_magp_clean())
  expect_equal(sum(results$issues$status == "open"), 0L)
  # Nothing fails, and every rule reports: a row that couldn't run (not_run) is allowed.
  expect_equal(sum(results$results$status == "fail"), 0L)
  expect_equal(nrow(results$metadata$self_test), 0L)
})

test_that("every defect block stops on tables that aren't fixtures (18.3)", {
  # lapply() returns a plain list, which lacks the marker the guard reads.
  unmarked <- lapply(fx_magp_clean(), data.table::copy)
  blocks <- fx_defect_blocks()
  for (id in names(blocks)) {
    expect_error(blocks[[id]](unmarked), "fixtures only", info = id)
  }
})

test_that("planted vs caught: each block caught by its rule with its witness, its cascade too", {
  # About 2 s a run since M4b cut the run's fixed cost (D16.18).
  testthat::skip_on_cran()
  base <- fx_magp_clean()
  sheet <- fx_answer_sheet()
  blocks <- fx_defect_blocks()
  for (i in seq_len(nrow(sheet))) {
    row <- sheet[i]
    results <- fx_caught(blocks[[row$defect_id]](base))
    caught <- results$issues[results$issues$status == "open"]
    is_hit <- caught$rule_id == row$rule_id & caught$table_name == row$table_name &
      caught$column_name == row$column_name & caught$record_pk %in% row$record_pk
    hit <- caught[is_hit]
    expect_equal(nrow(hit), 1L, info = row$defect_id)
    expect_identical(hit$value, row$witness, info = row$defect_id)
    cascade <- split_joined(row$cascade_rule_ids)
    allowed <- c(row$rule_id, cascade)
    expect_equal(setdiff(caught$rule_id, allowed), character(), info = row$defect_id)
    # Each cascade rule the sheet lists raises an open issue too (D16.42, m27).
    expect_equal(setdiff(cascade, caught$rule_id), character(), info = row$defect_id)
    # Nor does another issue of the planted rule, elsewhere, pass: all but the hit are cascade.
    expect_equal(sum(!is_hit & !caught$rule_id %in% cascade), 0L, info = row$defect_id)
  }
})

test_that("fx_magp_scaled() builds the AB file's shape, its keys agreeing, marked (16.3, D17.7)", {
  # About 3 s; skipped on CRAN. Of the helper's tests only the defect-rates test below runs
  # there (about 0.4 s).
  testthat::skip_on_cran()
  spec <- magp_spec()
  tables <- fx_magp_scaled(0.01)
  expect_true(isTRUE(attr(tables, "gpq_fixture")))
  expect_true(all(vapply(tables, function(t) isTRUE(attr(t, "gpq_fixture")), logical(1))))
  expect_equal(names(tables), unique(spec$attributes$table_name))
  # D1.9a's three counts at 0.01x, one contributor with its two datasets.
  expect_equal(nrow(tables$magp_tree_cond), 20123L)
  expect_equal(nrow(tables$magp_tree_meas), 16153L)
  expect_equal(nrow(tables$magp_trees), 5175L)
  # R6's other 14 tables at 0.01x: 4,000 sites, 12,485 visits (three or four a site, rounded),
  # one plot of four subplots per site, one plot measurement per visit with four subplot
  # measurements, a site disturbance and a site treatment per site, 10,000 tree ages, 8,000
  # tally trees, and two designs per contributor with two frames per design. A change to the
  # ratios fails here.
  other <- c(
    magp_contributors = 1L, magp_datasets = 2L, magp_sites = 40L, magp_site_dist = 40L,
    magp_site_treat = 40L, magp_site_visits = 125L, magp_plots = 40L, magp_subplots = 160L,
    magp_designs = 2L, magp_design_frames = 4L, magp_plot_meas = 125L,
    magp_subplot_meas = 500L, magp_tree_age = 100L, magp_tally_trees = 80L
  )
  expect_equal(vapply(tables[names(other)], nrow, integer(1)), other)
  expect_clean_base(tables, spec)
})

test_that("fx_magp_scaled()'s many groups: each sheet dataset in its contributor, jurisdiction", {
  # About 3 s; skipped on CRAN as the test above.
  testthat::skip_on_cran()
  spec <- magp_spec()
  sheet <- spec$datasets
  tables <- fx_magp_scaled(0.01, many_groups = TRUE)
  datasets <- tables$magp_datasets
  expect_setequal(datasets$magp_dataset_id, sheet$magp_dataset_id)
  at <- match(datasets$magp_dataset_id, sheet$magp_dataset_id)
  expect_equal(datasets$magp_contributor_id, sheet$magp_contributor_id[at])
  sites <- tables$magp_sites
  of_site <- match(sites$magp_dataset_id, sheet$magp_dataset_id)
  expect_equal(sites$jurisdiction, sheet$jurisdiction[of_site])
  expect_setequal(sites$magp_dataset_id, sheet$magp_dataset_id)
  expect_equal(nrow(tables$magp_trees), 5175L)
  expect_clean_base(tables, spec)
})

test_that("fx_magp_scaled()'s defect run plants each defect at its rate (D17.8)", {
  tables <- fx_magp_scaled(0.01, defects = TRUE)
  meas <- tables$magp_tree_meas
  trees <- tables$magp_trees
  # D17.8's rate: 0.1% of a table's rows, rounded up (at this size every defect's first row,
  # 1, 2 or 3, gives the same count).
  rate <- function(n) ceiling(0.001 * n)
  expect_equal(sum(meas$height == "tall"), rate(nrow(meas)))
  expect_equal(sum(is.infinite(meas$DBH)), rate(nrow(meas)))
  expect_equal(sum(meas$species == "UNKN.SPP"), rate(nrow(meas)))
  visits <- tables$magp_site_visits
  expect_equal(sum(visits$meas_date == "2020-02-30"), rate(nrow(visits)))
  expect_equal(sum(is.na(trees$tree_dist)), rate(nrow(trees)))
  expect_equal(sum(trees$comments == ""), rate(nrow(trees)))
  expect_equal(sum(trees$comments == "N/A"), rate(nrow(trees)))
  expect_equal(sum(trees$tree_origin == "-9"), rate(nrow(trees)))
  expect_equal(sum(trees$tree_origin == "AP "), rate(nrow(trees)))
  expect_equal(sum(trees$src_tree_id == "NA"), rate(nrow(trees)))
  expect_true(is.double(meas$visit_num))
  expect_false("magp_tree_age" %in% names(tables))
  expect_true("magp_notes" %in% names(tables))
  expect_false("comments" %in% names(tables$magp_tree_cond))
  expect_equal(unique(tables$magp_plots$colour), "red")
  # m9, m34 and m64.
  expect_true(is.integer(trees$magp_tree_id) && is.integer(meas$magp_tree_id))
  expect_true(all(meas$magp_tree_id %in% trees$magp_tree_id))
  expect_true(all(endsWith(meas$magp_frame_id, "  ")))
  expect_equal(sum(meas$tree_wt == "x"), length(seq.int(1L, nrow(meas), by = 2L)))
})

test_that("the defect run raises each answer-sheet rule (D17.8)", {
  # A full MAGPlot run (D16.50).
  testthat::skip_on_cran()
  results <- fx_caught(fx_magp_scaled(0.01, defects = TRUE))
  open <- results$issues[results$issues$status == "open"]
  expect_equal(setdiff(fx_answer_sheet()$rule_id, open$rule_id), character())
})
