# Tests for the scripts in data-raw/ (D12.23). They run from the source tree and skip in a
# built package, which leaves data-raw out (plan 19.3).

load_data_raw <- function(name) {
  path <- testthat::test_path("..", "..", "data-raw", name)
  if (!file.exists(path)) {
    testthat::skip(paste(name, "is not in this tree"))
  }
  env <- new.env(parent = asNamespace("groundplotqc"))
  sys.source(path, envir = env)
  env
}

hand_kept <- function(name) {
  path <- testthat::test_path("..", "..", "data-raw", "magp", name)
  if (!file.exists(path)) {
    testthat::skip(paste(name, "is not in this tree"))
  }
  read <- read_csv_text(path)
  # A committed hand-kept file reads clean, or the build would stop on it (D12.58).
  testthat::expect_equal(nrow(read$malformed), 0L)
  testthat::expect_equal(nrow(read$invalid), 0L)
  read$data
}

# The tree's spec/ and data-raw/magp/, or a skip where either is missing (a built package).
tree_dirs <- function() {
  spec_dir <- testthat::test_path("..", "..", "spec")
  config_dir <- testthat::test_path("..", "..", "data-raw", "magp")
  if (!dir.exists(spec_dir) || !dir.exists(config_dir)) {
    testthat::skip("spec/ or data-raw/magp/ is not in this tree")
  }
  list(spec = spec_dir, config = config_dir)
}

# What the build makes of the tree's spec/, built once for the tests that read its result and
# handed out as a copy, so one test's subsetting can't leave an index on the next one's
# (D12.27).
tree_spec <- local({
  built <- NULL
  function() {
    testthat::skip_if_not_installed("readxl")
    dirs <- tree_dirs()
    if (is.null(built)) {
      built <<- load_data_raw("build_magp_config.R")$build_magp_spec(dirs$spec, dirs$config)
    }
    data.table::copy(built)
  }
})

# One translation table of the tree's spec/, read apart from the build; it reads clean, or an
# expected list built from it would lose rows as the built one would (D12.58).
tree_table <- function(kind) {
  path <- list.files(
    tree_dirs()$spec,
    pattern = paste0("^[0-9]{8}_magpv2_", kind, "[.]csv$"), full.names = TRUE
  )
  testthat::expect_length(path, 1L)
  read <- read_csv_text(path)
  testthat::expect_equal(nrow(read$malformed), 0L)
  testthat::expect_equal(nrow(read$invalid), 0L)
  read$data
}

test_that("spec_files finds one file per kind and stops on two of one kind", {
  build <- load_data_raw("build_magp_config.R")
  dir <- withr::local_tempdir()
  kinds <- c("DD.xlsx", "Lookup_Tables.xlsx", "datasets.csv", "A2.xlsx", "species.csv")
  file.create(file.path(dir, paste0("20260925_magpv2_", kinds)))
  files <- build$spec_files(dir)
  expect_equal(sort(names(files)), sort(c("DD", "Lookup_Tables", "datasets", "A2", "species")))
  file.create(file.path(dir, "20261003_magpv2_DD.xlsx"))
  expect_error(build$spec_files(dir), "more than one dated file of: DD")
})

test_that("spec_files stops on a kind it needs that spec/ lacks", {
  build <- load_data_raw("build_magp_config.R")
  dir <- withr::local_tempdir()
  kinds <- c("DD.xlsx", "Lookup_Tables.xlsx", "datasets.csv", "species.csv")
  file.create(file.path(dir, paste0("20260925_magpv2_", kinds)))
  expect_error(build$spec_files(dir), "spec/ has no A2 file.", fixed = TRUE)
})

test_that("apply_spec_exceptions applies a row and stops on a stale one", {
  build <- load_data_raw("build_magp_config.R")
  dictionary <- data.table::data.table(
    table_name = "s", attribute_name = "ec_zone", data_type = "numeric"
  )
  rows <- data.table::data.table(
    exception_id = "x", table_name = "s", attribute_name = "ec_zone", dd_column = "data_type",
    dd_value = "numeric", applied_value = "character", decision = "D2.17", note = NA
  )
  applied <- build$apply_spec_exceptions(data.table::copy(dictionary), rows)
  expect_equal(applied$data_type, "character")
  dictionary$data_type <- "character"
  expect_error(build$apply_spec_exceptions(dictionary, rows), "row x is stale")
})

test_that("apply_spec_exceptions leaves the caller's dictionary as it was, on a stop too", {
  build <- load_data_raw("build_magp_config.R")
  dictionary <- data.table::data.table(
    table_name = "s", attribute_name = c("ec_zone", "ec_region"), data_type = "numeric"
  )
  given <- data.table::copy(dictionary)
  rows <- data.table::data.table(
    exception_id = c("x", "y"), table_name = "s", attribute_name = c("ec_zone", "ec_region"),
    dd_column = "data_type", dd_value = c("numeric", "integer"), applied_value = "character",
    decision = "D2.17", note = NA
  )
  applied <- build$apply_spec_exceptions(dictionary, rows[1L])
  expect_equal(applied$data_type, c("character", "numeric"))
  expect_identical(dictionary, given)
  # Row x applies before row y is found stale.
  expect_error(build$apply_spec_exceptions(dictionary, rows), "row y is stale")
  expect_identical(dictionary, given)
})

test_that("apply_spec_exceptions stops on a row that names no single DD cell", {
  build <- load_data_raw("build_magp_config.R")
  dictionary <- data.table::data.table(
    table_name = "s", attribute_name = c("a", "a", "b"), data_type = "numeric"
  )
  exception <- function(attribute_name, dd_column) {
    data.table::data.table(
      exception_id = "x", table_name = "s", attribute_name = attribute_name,
      dd_column = dd_column, dd_value = "numeric", applied_value = "character",
      decision = "D2.17", note = NA
    )
  }
  no_cell <- "spec_exceptions.csv row x names no single DD cell."
  # An attribute the DD lacks, one it holds twice, and a DD column it lacks.
  expect_error(
    build$apply_spec_exceptions(dictionary, exception("c", "data_type")), no_cell,
    fixed = TRUE
  )
  expect_error(
    build$apply_spec_exceptions(dictionary, exception("a", "data_type")), no_cell,
    fixed = TRUE
  )
  expect_error(
    build$apply_spec_exceptions(dictionary, exception("b", "key_type")), no_cell,
    fixed = TRUE
  )
})

test_that("a DD row an exception fixes gives no finding; the raw row is located (D12.33)", {
  build <- load_data_raw("build_magp_config.R")
  workbook <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("stand-in", workbook)
  dictionary <- data.table::data.table(
    table_name = "s", attribute_name = c("s_id", "ec_zone"), key_type = c("PK", "."),
    data_type = c("character", "text")
  )
  rows <- data.table::data.table(
    exception_id = "x", table_name = "s", attribute_name = "ec_zone", dd_column = "data_type",
    dd_value = "text", applied_value = "character", decision = "D2.17", note = NA
  )
  origins <- list(dictionary = list(path = workbook, sheet = "DD", rows = 2L))
  unknown_type <- function(dd) {
    results <- preflight_checks(gpq_read_spec(dd, origins = origins))
    results[results$rule_id == "dd_type_unknown", ]
  }
  raw <- unknown_type(data.table::copy(dictionary))
  expect_equal(raw$source_cell, paste0(basename(workbook), ":3"))
  fixed <- unknown_type(build$apply_spec_exceptions(data.table::copy(dictionary), rows))
  expect_equal(fixed$outcome, "pass")
})

test_that("the manifest names hand-kept files by repo path, the exceptions included (D12.33)", {
  build <- load_data_raw("build_magp_config.R")
  config <- withr::local_tempdir()
  writeLines("exception_id", file.path(config, "spec_exceptions.csv"))
  writeLines("sheet", file.path(config, "precedence.csv"))
  spec <- gpq_read_spec(
    data.frame(table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"),
    precedence = data.frame(
      sheet = "s", key_col = "k", attribute_name = "c", winner = "datasets", blank_rule = "wins"
    ),
    origins = list(precedence = list(path = file.path(config, "precedence.csv"), rows = 2L))
  )
  given <- data.table::copy(spec$manifest)
  manifest <- build$record_hand_kept(spec, config)$manifest
  # The caller's spec keeps its manifest as it was.
  expect_identical(spec$manifest, given)
  expect_equal(manifest$file[manifest$input == "precedence"], "data-raw/magp/precedence.csv")
  exceptions <- manifest[manifest$input == "dictionary:exceptions", ]
  expect_equal(exceptions$file, "data-raw/magp/spec_exceptions.csv")
  expect_equal(
    exceptions$sha256, unname(tools::sha256sum(file.path(config, "spec_exceptions.csv")))
  )
})

test_that("build_lineage_input reads A2's layout into the long lineage input", {
  build <- load_data_raw("build_magp_config.R")
  raw <- data.table::data.table(
    V1 = c("Appendix A2", "notes", "need confirmation", "type", "id", "compiled"),
    V2 = c(NA, NA, NA, "magp_table", "magp_sites", "magp_plot_meas"),
    V3 = c(NA, NA, NA, "attribute", "src_site_id", "src_dbh_cutoff"),
    V4 = c(NA, NA, NA, "BC_src", "faib_header.site_identifier", "faib.x"),
    V5 = c(NA, NA, NA, "BC_note", NA, "a note"),
    V6 = c(NA, NA, NA, "ON_src", "tblPlot.plotkey", NA),
    V7 = c(NA, NA, NA, "ON_note", NA, NA)
  )
  long <- build$build_lineage_input(raw)
  expect_named(long, spec_input_schema()$lineage_spec)
  expect_equal(long, data.table::data.table(
    contributor_label = c("BC", "BC", "ON", "ON"),
    table_name = c("magp_sites", "magp_plot_meas", "magp_sites", "magp_plot_meas"),
    attribute_name = c("src_site_id", "src_dbh_cutoff", "src_site_id", "src_dbh_cutoff"),
    spec_type = c("id", "compiled", "id", "compiled"),
    source_text = c("faib_header.site_identifier", "faib.x", "tblPlot.plotkey", NA),
    note = c(NA, "a note", NA, NA),
    source_cell = c("A2!D5", "A2!D6", "A2!F5", "A2!F6")
  ))
})

test_that("build_lineage_input drops a row with a blank first cell, the rest keeping their cells", {
  build <- load_data_raw("build_magp_config.R")
  raw <- data.table::data.table(
    V1 = c("type", "id", NA, "compiled"),
    V2 = c("magp_table", "magp_sites", "a gap", "magp_plot_meas"),
    V3 = c("attribute", "src_site_id", NA, "src_dbh_cutoff"),
    V4 = c("BC_src", "faib_header.site_identifier", "stray", "faib.x")
  )
  long <- build$build_lineage_input(raw, sheet = "sheet one")
  expect_equal(long$attribute_name, c("src_site_id", "src_dbh_cutoff"))
  expect_equal(long$source_cell, c("sheet one!D2", "sheet one!D4"))
  expect_equal(long$note, c(NA_character_, NA_character_))
})

test_that("build_lineage_input stops naming what A2 lacks", {
  build <- load_data_raw("build_magp_config.R")
  raw <- data.table::data.table(
    V1 = c("type", "id"), V2 = c("magp_table", "magp_sites"), V3 = c("attribute", "src_site_id"),
    V4 = c("BC_src", "faib.x")
  )
  no_header <- data.table::copy(raw)
  data.table::set(no_header, i = 1L, j = "V1", value = "kind")
  expect_error(
    build$build_lineage_input(no_header), "A2 has no header row starting with 'type'.",
    fixed = TRUE
  )
  expect_error(
    build$build_lineage_input(raw[, c("V1", "V2", "V3"), with = FALSE]),
    "A2 has no <contributor>_src column.",
    fixed = TRUE
  )
  expect_error(
    build$build_lineage_input(raw[, c("V1", "V4"), with = FALSE]),
    "A2 has no column named magp_table or attribute.",
    fixed = TRUE
  )
  expect_error(
    build$build_lineage_input(raw[, c("V1", "V2", "V4"), with = FALSE]),
    "A2 has no column named attribute.",
    fixed = TRUE
  )
})

test_that("crosswalk_element builds each form of element and stops on rows that disagree", {
  build <- load_data_raw("build_magp_config.R")
  columns <- data.table::data.table(
    crosswalk = "species", attribute_name = NA_character_, code_col = "code",
    filter_col = NA_character_, filter_values = NA_character_, decision = "D12.21"
  )
  expect_equal(build$crosswalk_element("w.csv", columns[0L]), "w.csv")
  expect_equal(build$crosswalk_element("w.csv", columns), list(table = "w.csv", code_col = "code"))
  # One filter for the whole table, its values split on "; ".
  one_filter <- data.table::copy(columns)
  data.table::set(one_filter, j = c("code_col", "filter_col", "filter_values"), value = list(
    NA_character_, "kind", "a; b"
  ))
  expect_equal(
    build$crosswalk_element("w.csv", one_filter),
    list(table = "w.csv", filter_col = "kind", filter_values = c("a", "b"))
  )
  # A filter per attribute: the values are a list named by attribute.
  per_attribute <- data.table::data.table(
    crosswalk = "species", attribute_name = c("x", "y"), code_col = c("code", NA),
    filter_col = "kind", filter_values = c("a; b", "c"), decision = "D12.21"
  )
  expect_equal(
    build$crosswalk_element("w.csv", per_attribute),
    list(
      table = "w.csv", code_col = "code", filter_col = "kind",
      filter_values = list(x = c("a", "b"), y = "c")
    )
  )
  two_codes <- data.table::copy(per_attribute)
  data.table::set(two_codes, j = "code_col", value = c("code", "other"))
  expect_error(
    build$crosswalk_element("w.csv", two_codes),
    "crosswalk_columns.csv gives crosswalk species more than one code_col: code, other.",
    fixed = TRUE
  )
  two_filters <- data.table::copy(per_attribute)
  data.table::set(two_filters, j = "filter_col", value = c("kind", "type"))
  expect_error(
    build$crosswalk_element("w.csv", two_filters),
    "crosswalk_columns.csv gives crosswalk species more than one filter_col: kind, type.",
    fixed = TRUE
  )
  no_filter_col <- data.table::copy(per_attribute)
  data.table::set(no_filter_col, j = "filter_col", value = NA_character_)
  expect_error(
    build$crosswalk_element("w.csv", no_filter_col),
    "crosswalk_columns.csv gives crosswalk species filter_values but no filter_col.",
    fixed = TRUE
  )
})

test_that("the build stops on a hand-kept file that doesn't read cleanly (D12.58)", {
  build <- load_data_raw("build_magp_config.R")
  kept <- testthat::test_path("..", "..", "data-raw", "magp")
  # Stand-ins: the hand-kept files are read before any spec file is opened.
  spec_dir <- withr::local_tempdir()
  stand_ins <- c("DD.xlsx", "Lookup_Tables.xlsx", "datasets.csv", "A2.xlsx")
  file.create(file.path(spec_dir, paste0("20260925_magpv2_", stand_ins)))
  config <- withr::local_tempdir()
  file.copy(list.files(kept, full.names = TRUE), config)
  # A note with an unquoted comma: fread() stops on line 3, and lines 3 and 4 would be lost.
  writeLines(c(
    "exception_id,table_name,attribute_name,dd_column,dd_value,applied_value,decision,note",
    "x,s,a,data_type,numeric,character,D2.17,fine",
    "y,s,b,data_type,numeric,character,D2.17,a note, with a comma",
    "z,s,c,data_type,numeric,character,D2.17,fine"
  ), file.path(config, "spec_exceptions.csv"))
  expect_error(
    build$build_magp_spec(spec_dir, config),
    "spec_exceptions.csv doesn't read cleanly, so nothing was built: fields on line 3.",
    fixed = TRUE
  )
  # An invalid byte in a later file, the earlier ones clean.
  file.copy(file.path(kept, "spec_exceptions.csv"), config, overwrite = TRUE)
  writeBin(c(
    charToRaw("sheet,key_col,attribute_name,winner,blank_rule,decision\ndata"), as.raw(0xe9),
    charToRaw("set,magp_dataset_id,dataset_code,code_lists,wins,D12.20\n")
  ), file.path(config, "precedence.csv"))
  expect_error(
    build$build_magp_spec(spec_dir, config),
    paste(
      "precedence.csv doesn't read cleanly, so nothing was built:",
      "invalid byte on line 2 (data<e9>set)."
    ),
    fixed = TRUE
  )
})

test_that("the hand-kept files have their columns", {
  expect_named(hand_kept("spec_exceptions.csv"), c(
    "exception_id", "table_name", "attribute_name", "dd_column", "dd_value", "applied_value",
    "decision", "note"
  ))
  expect_named(hand_kept("non_code_sheets.csv"), c("sheet", "reason", "decision"))
  expect_named(hand_kept("crosswalk_columns.csv"), c(
    "crosswalk", "attribute_name", "code_col", "filter_col", "filter_values", "decision"
  ))
  expect_named(hand_kept("type_map.csv"), c("data_type", "r_class", "date_format"))
  expect_named(
    hand_kept("sentinels.csv"),
    c("data_type", "role", "value", "allowed_in_pk", "allowed_in_fk")
  )
  expect_named(hand_kept("precedence.csv"), c(spec_input_schema()$precedence, "decision"))
  expect_true(all(hand_kept("non_code_sheets.csv")$reason %in% c("reference", "superseded")))
})

test_that("the hand-kept rows are valid where the build takes them", {
  expect_no_error(validate_sentinels(hand_kept("sentinels.csv")))
  expect_no_error(gpq_type_map(hand_kept("type_map.csv")))
  precedence <- hand_kept("precedence.csv")
  # The reader's own checks: winner and blank_rule from their lists, one row per key.
  expect_no_error(read_precedence(precedence[, spec_input_schema()$precedence, with = FALSE]))
})

test_that("the reader reads the DD workbook in spec/ from its first sheet", {
  testthat::skip_if_not_installed("readxl")
  build <- load_data_raw("build_magp_config.R")
  spec_dir <- testthat::test_path("..", "..", "spec")
  if (!dir.exists(spec_dir)) {
    testthat::skip("spec/ is not in this tree")
  }
  dd <- read_input_table(build$spec_files(spec_dir)[["DD"]], "dictionary")
  expect_true(all(c("table_name", "attribute_name", "data_type") %in% names(dd$data)))
  expect_equal(dd$where$sheet, "DD")
})

test_that("the build reads the tree's spec/ without a stale exception (D12.22)", {
  # A row of spec_exceptions.csv the dated DD no longer matches stops the build.
  expect_no_error(tree_spec())
})

test_that("the species code list is the table's NFI codes less UNKN.SPP (D2.22, D12.73)", {
  species_table <- tree_table("species")
  # The Veg_type filter is how UNKN.SPP, which has no Veg_type, leaves the list. The table
  # lists it, so a later table where the two stop coinciding fails here.
  expect_true("UNKN.SPP" %in% species_table$NFI)
  expected <- setdiff(unique(species_table$NFI[!is.na(species_table$NFI)]), "UNKN.SPP")
  codes <- tree_spec()$codes
  listed <- codes[codes$attribute_name == "species", ]
  expect_gt(length(unique(listed$table_name)), 0L)
  for (table_name in unique(listed$table_name)) {
    species_codes <- listed$code[listed$table_name == table_name]
    expect_setequal(species_codes, expected)
    expect_false(anyDuplicated(species_codes) > 0L)
  }
})

test_that("CLR is in both the disturbance and the treatment lists (D12.73 (5a), (9))", {
  # A re-copy of the treatment table from its master gives CLR "T" again and drops it from
  # the disturbance list, until the master says TD.
  codes <- tree_spec()$codes
  for (attribute in c("disturbance_type", "treatment_type")) {
    expect_true("CLR" %in% codes$code[codes$attribute_name == attribute], info = attribute)
  }
})

test_that("the species copy keeps its fold notes on their rows only (D12.72 (13), (21))", {
  notes <- c(
    BETU.GLA = "Folded code: BETU.GLA covers Betula glandulifera and Betula glandulosa.",
    ALNU.VIR = "Folded code: ALNU.VIR folds Alnus crispa var. mollis into Alnus viridis.",
    POPU.SPP = "Folded code: POPU.SPP folds the unnamed Populus hybrid (Populus X) into the genus."
  )
  species_table <- tree_table("species")
  # A re-copy of the table from its master has no comments column at all.
  expect_identical(names(species_table)[[ncol(species_table)]], "comments")
  expect_equal(as.integer(table(species_table$NFI)[names(notes)]), c(2L, 2L, 2L))
  expect_identical(species_table$comments, unname(notes[species_table$NFI]))
})

test_that("every treat_vs_dist value is in a filter list, blank only on NO and ND (#39)", {
  columns <- hand_kept("crosswalk_columns.csv")
  filters <- columns$filter_values[columns$crosswalk == "treatment_disturbance"]
  listed <- unique(unlist(strsplit(filters, "; ", fixed = TRUE)))
  expect_gt(length(listed), 0L)
  walk <- tree_table("treatment_disturbance")
  # A value in neither list would drop its code from both silently.
  expect_equal(setdiff(walk$treat_vs_dist[!is.na(walk$treat_vs_dist)], listed), character())
  expect_equal(setdiff(walk$magp_codes[is.na(walk$treat_vs_dist)], c("NO", "ND")), character())
})

test_that("the datasets copy and the lookup agree on every src_dataset_id (D12.74)", {
  # A revert would be the owner's 20260830 datasets master, which has 110.05 and 110.06 the
  # other way round: BC_SUP on 110.05 with MIX, FMP, SYS and the comment, BC_VRI on 110.06.
  clashes <- tree_spec()$clashes
  expect_equal(clashes$key_value[clashes$column_name == "src_dataset_id"], character())
  datasets <- tree_table("datasets")
  columns <- c("src_dataset_id", "gp_type", "gp_network", "sampling_design", "comments")
  comment <- "Some visits have TMP as the visit type/code."
  at <- match(c("110.05", "110.06"), datasets$magp_dataset_id)
  expect_false(anyNA(at))
  rows <- datasets[at, columns, with = FALSE]
  expect_equal(
    as.list(rows[1L]),
    list(
      src_dataset_id = "BC_VRI", gp_type = "TMP", gp_network = "CVV", sampling_design = "STR",
      comments = NA_character_
    )
  )
  expect_equal(
    as.list(rows[2L]),
    list(
      src_dataset_id = "BC_SUP", gp_type = "MIX", gp_network = "FMP", sampling_design = "SYS",
      comments = comment
    )
  )
})

test_that("the compiled specification is the one the build makes (D12.14, D12.27)", {
  # Base identical(), not expect_identical(): no index attribute may differ (D12.27).
  expect_true(identical(tree_spec(), magp_spec()))
})
