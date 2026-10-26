# Tests for MAGPlot's evidence for issue classes (plan 3.6, 10.3; D19.29, D19.38, D19.59).

test_that("computed_evidence() is pipeline_computed.csv as the evidence computed (D19.59)", {
  computed <- computed_evidence()
  expect_named(computed, c("table_name", "column_name", "jurisdiction", "derivation"))
  expect_true(all(vapply(computed, is.character, TRUE)))
  path <- system.file("extdata", "magp", "pipeline_computed.csv", package = "groundplotqc")
  file <- read_csv_text(path)$data
  # An empty file would pass the comparisons below.
  expect_gt(nrow(file), 0L)
  expect_identical(computed$table_name, file$table_name)
  expect_identical(computed$column_name, file$attribute_name)
  expect_identical(computed$jurisdiction, file$jurisdiction)
  expect_identical(computed$derivation, file$derivation)
  rows <- paste(computed$table_name, computed$column_name, computed$jurisdiction)
  expect_equal(anyDuplicated(rows), 0L)
  known <- c(
    "magp_sites magp_site_id * id", "magp_sites datum BC fill", "magp_sites n_visits * count",
    "magp_plots design_change * class", "magp_tree_meas crown_length AB formula",
    "magp_datasets gp_type * reference"
  )
  expect_true(all(known %in% paste(rows, computed$derivation)))
})

test_that("crosswalk_evidence() splits, trims, filters and pools on a small spec (D19.59)", {
  # Translation table "walk": a header row, then four rows; `keep` is the filter of "kind",
  # which two tables use and which takes two values, Y and Z (row 4 holds N), and "other" has
  # no filter. Row 5 has no MAGPlot code.
  cells <- function(column, values) {
    data.table::data.table(
      crosswalk = "walk", source_row = seq_along(values), crosswalk_column = column,
      value = values
    )
  }
  # A no-break space before the 9, which space_padding() trims as it trims any Unicode space,
  # and one on its own, which it takes for a blank part.
  nbsp <- intToUtf8(160L)
  spaced <- paste0(nbsp, "9")
  spec <- list(
    code_list_map = data.table::data.table(
      table_name = c("t1", "t2", "t1"), attribute_name = c("kind", "kind", "other"),
      source_type = "crosswalk", source_name = "walk", code_column = "code",
      filter_column = c("keep", "keep", NA), filter_values = c("Y; Z", "Y; Z", NA),
      status = "resolved"
    ),
    crosswalks = rbind(
      cells("code", c("code", "K1", "K2", "K3", NA)),
      cells("keep", c("keep", "Y", "Z", "N", "Y")),
      cells("ab_one", c("ab_one", "1, 2", " 2 ", "4", "5")),
      cells("ab_two", c("ab_two", "2/x", spaced, "6", "7")),
      cells("bc", c("bc", "1,,2", nbsp, "8", "9"))
    )
  )
  # AB's two columns for "kind"; a row whose attribute takes no codes from the table.
  map <- data.table::data.table(
    crosswalk = "walk", attribute_name = c("kind", "kind", "kind", "other", "absent"),
    jurisdiction = c("AB", "AB", "BC", "AB", "AB"),
    provider_column = c("ab_one", "ab_two", "bc", "ab_one", "ab_one")
  )
  testthat::local_mocked_bindings(read_evidence_file = function(name) map)
  evidence <- crosswalk_evidence(spec)
  expect_named(evidence, c("jurisdiction", "provider_code", "code", "column_name"))
  expect_true(all(vapply(evidence, is.character, TRUE)))
  # "2" of AB's two columns is one row on row 2; on row 3 AB's " 2 " gives the same provider
  # code another MAGPlot code, K2, and both stay. Row 3's Z is kept by the filter, row 4's N
  # and row 5's missing code keep them out of "kind"; "other" keeps rows 2 to 4.
  expected <- c(
    "AB 1 K1 kind", "AB 2 K1 kind", "AB 2 K2 kind", "AB x K1 kind", "AB 9 K2 kind",
    "BC 1 K1 kind", "BC 2 K1 kind",
    "AB 1 K1 other", "AB 2 K1 other", "AB 2 K2 other", "AB 4 K3 other"
  )
  got <- paste(evidence$jurisdiction, evidence$provider_code, evidence$code, evidence$column_name)
  expect_setequal(got, expected)
  expect_equal(nrow(evidence), length(expected))
})

test_that("crosswalk_evidence() gives every coded row where no code list has a filter (D19.59)", {
  cells <- function(column, values) {
    data.table::data.table(
      crosswalk = "walk", source_row = seq_along(values), crosswalk_column = column,
      value = values
    )
  }
  spec <- list(
    code_list_map = data.table::data.table(
      table_name = "t1", attribute_name = "kind", source_type = "crosswalk",
      source_name = "walk", code_column = "code", filter_column = NA_character_,
      filter_values = NA_character_, status = "resolved"
    ),
    crosswalks = rbind(
      cells("code", c("code", "K1", "K2", "K3")),
      cells("ab_one", c("ab_one", "1", "2", "2"))
    )
  )
  map <- data.table::data.table(
    crosswalk = "walk", attribute_name = "kind", jurisdiction = "AB", provider_column = "ab_one"
  )
  testthat::local_mocked_bindings(read_evidence_file = function(name) map)
  evidence <- crosswalk_evidence(spec)
  got <- paste(evidence$jurisdiction, evidence$provider_code, evidence$code, evidence$column_name)
  expect_setequal(got, c("AB 1 K1 kind", "AB 2 K2 kind", "AB 2 K3 kind"))
  expect_equal(nrow(evidence), 3L)
})

test_that("crosswalk_evidence() gives no rows where no provider cell pairs with a code (D19.59)", {
  cells <- function(column, values) {
    data.table::data.table(
      crosswalk = "walk", source_row = seq_along(values), crosswalk_column = column,
      value = values
    )
  }
  spec <- list(
    code_list_map = data.table::data.table(
      table_name = "t1", attribute_name = "kind", source_type = "crosswalk",
      source_name = "walk", code_column = "code", filter_column = "keep",
      filter_values = "Y", status = "resolved"
    ),
    crosswalks = rbind(
      cells("code", c("code", "K1", "K2")),
      cells("keep", c("keep", "Y", "N")),
      cells("ab_one", c("ab_one", "1", "2"))
    )
  )
  # A map row whose attribute has no list in `spec`, and a map with no rows.
  maps <- list(
    data.table::data.table(
      crosswalk = "walk", attribute_name = "absent", jurisdiction = "AB",
      provider_column = "ab_one"
    ),
    data.table::data.table(
      crosswalk = character(), attribute_name = character(), jurisdiction = character(),
      provider_column = character()
    )
  )
  for (map in maps) {
    evidence <- testthat::with_mocked_bindings(
      crosswalk_evidence(spec),
      read_evidence_file = function(name) map
    )
    expect_named(evidence, c("jurisdiction", "provider_code", "code", "column_name"))
    expect_true(all(vapply(evidence, is.character, TRUE)))
    expect_equal(nrow(evidence), 0L)
  }
})

test_that("MAGPlot's crosswalk evidence: its columns, distinct rows, listed codes (D19.59)", {
  spec <- magp_spec()
  evidence <- crosswalk_evidence(spec)
  expect_named(evidence, c("jurisdiction", "provider_code", "code", "column_name"))
  expect_true(all(vapply(evidence, is.character, TRUE)))
  expect_gt(nrow(evidence), 0L)
  expect_equal(anyDuplicated(evidence), 0L)
  expect_setequal(
    unique(evidence$column_name),
    c("species", "damage_agent", "treatment_type", "disturbance_type")
  )
  expect_setequal(unique(evidence$jurisdiction), c("AB", "BC", "ON", "QC"))
  # Each provider code is one code, trimmed.
  expect_false(any(grepl("[,/]", evidence$provider_code)))
  padding <- space_padding(evidence$provider_code)
  expect_false(any(padding$blank | padding$padded))
  # Each code is one its attribute's code list keeps, so a row the list's filter leaves out
  # gives none: UNKN.SPP's row, with no Veg_type, holds AB's "Uk".
  listed <- paste(spec$codes$attribute_name, spec$codes$code, sep = "\r")
  expect_true(all(paste(evidence$column_name, evidence$code, sep = "\r") %chin% listed))
  cells <- spec$crosswalks
  unknown_row <- cells$source_row[
    cells$crosswalk == "species" & cells$crosswalk_column == "NFI" & cells$value %in% "UNKN.SPP"
  ]
  ab_cells <- cells$value[
    cells$crosswalk == "species" & cells$crosswalk_column == "AB_code" &
      cells$source_row %in% unknown_row
  ]
  expect_true("Uk" %in% ab_cells)
  species <- evidence[evidence$column_name == "species"]
  expect_false("UNKN.SPP" %in% species$code)
  expect_false("Uk" %in% species$provider_code[species$jurisdiction == "AB"])
})

test_that("MAGPlot's provider cells give each of their codes with their row's code (D19.59)", {
  spec <- magp_spec()
  evidence <- crosswalk_evidence(spec)
  cells <- spec$crosswalks
  # The MAGPlot code on the row whose provider column holds `value`.
  code_at <- function(crosswalk, column, value, code_column) {
    row <- cells$source_row[
      cells$crosswalk == crosswalk & cells$crosswalk_column == column & cells$value %in% value
    ]
    cells$value[
      cells$crosswalk == crosswalk & cells$crosswalk_column == code_column &
        cells$source_row %in% row
    ]
  }
  pairs <- paste(evidence$jurisdiction, evidence$column_name, evidence$provider_code, evidence$code)
  # ON's "1,2" in on_cavtypecode, AB's "Pw / Pa" and one of QC's lists in qc_dic.
  axes <- code_at("condition", "on_cavtypecode", "1,2", "magp_codes")
  expect_length(axes, 1L)
  expect_true(all(paste("ON damage_agent", c("1", "2"), axes) %in% pairs))
  pine <- code_at("species", "AB_code", "Pw / Pa", "NFI")
  expect_length(pine, 1L)
  expect_true(all(paste("AB species", c("Pw", "Pa"), pine) %in% pairs))
  listed <- "DB06A, DB06E, DB06EM, DB06X, DB36X, DB36A"
  quebec <- code_at("condition", "qc_dic", listed, "magp_codes")
  expect_length(quebec, 1L)
  parts <- c("DB06A", "DB06E", "DB06EM", "DB06X", "DB36X", "DB36A")
  expect_true(all(paste("QC damage_agent", parts, quebec) %in% pairs))
  expect_false(any(c("1,2", "Pw / Pa", listed) %in% evidence$provider_code))
  # ON's three damage columns pooled: the codes of each one's first cell are there.
  for (column in c("on_cavtypecode", "on_defmtypecode", "on_defmcausecode")) {
    first <- cells[
      cells$crosswalk == "condition" & cells$crosswalk_column == column &
        cells$source_row > 1L & !is.na(cells$value)
    ][1L]
    code <- cells$value[
      cells$crosswalk == "condition" & cells$crosswalk_column == "magp_codes" &
        cells$source_row == first$source_row
    ]
    codes <- trimws(strsplit(first$value, "[,/]")[[1L]])
    expect_true(all(paste("ON damage_agent", codes, code) %in% pairs), info = column)
  }
})

test_that("MAGPlot's compiled evidence reads as evidence, on its spec's columns (D19.59)", {
  computed <- computed_evidence()
  read <- read_run_inputs(
    list(computed = computed, crosswalk = crosswalk_evidence()), "evidence",
    names(evidence_schema())
  )
  # The reader takes both for the run's group column, each derivation one of the six.
  expect_no_error(read_evidence(read, groups = "jurisdiction"))
  # Each table and column is the dictionary's, each jurisdiction "*" or a code of the sheet.
  spec <- magp_spec()
  expect_true(all(
    paste(computed$table_name, computed$column_name) %chin%
      paste(spec$attributes$table_name, spec$attributes$attribute_name)
  ))
  expect_true(all(
    computed$jurisdiction %chin% c("*", spec$codes$code[spec$codes$source_name == "jurisdiction"])
  ))
})

test_that("read_evidence_file stops on a file missing from the package", {
  # As magp_texts()'s test: pkgload's system.file() under load_all(), base's when installed.
  imports <- parent.env(environment(read_evidence_file))
  if (exists("system.file", envir = imports, inherits = FALSE)) {
    testthat::local_mocked_bindings(system.file = function(...) "")
  } else {
    testthat::local_mocked_bindings(system.file = function(...) "", .package = "base")
  }
  expect_error(computed_evidence(), "pipeline_computed.csv is missing from the package.")
})

test_that("read_evidence_file stops on a file that doesn't read cleanly", {
  file <- withr::local_tempfile(fileext = ".csv")
  real_read <- read_csv_text
  testthat::local_mocked_bindings(
    read_csv_text = function(path) real_read(if (grepl("crosswalk_provider", path)) file else path)
  )
  writeLines(c("crosswalk,attribute_name,jurisdiction,provider_column", "a,b,AB,c,extra"), file)
  expect_error(crosswalk_evidence(list()), "crosswalk_provider_columns.csv doesn't read cleanly.")
})
