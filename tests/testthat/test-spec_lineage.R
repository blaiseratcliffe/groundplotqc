# Tests for the lineage spec (plan 3.6; D7.8, D9.14, D12.14, D12.25).

# The tokens a spec with MAGPlot's character sentinels gets (D12.31).
tokens <- lineage_tokens(gpq_sentinels(character = c(missing = "X", not_applicable = "Z")))

test_that("lineage_tokens takes the tokens from the character sentinels (D12.31)", {
  expect_equal(tokens, c(no_id = "X", not_applicable = "Z"))
  none <- c(no_id = NA_character_, not_applicable = NA_character_)
  expect_equal(lineage_tokens(gpq_sentinels()), none)
})

test_that("parse_lineage_notation reads A2's forms", {
  single <- parse_lineage_notation("faib_header.site_identifier", tokens)
  expect_equal(single$id_status, "single")
  expect_equal(single$part_kind, "source")
  composite <- parse_lineage_notation("src_plot_id + faib_tree_detail.tree_no", tokens)
  expect_equal(composite$part_order, 1:2)
  expect_equal(composite$part_kind, c("derived", "source"))
  expect_equal(unique(composite$id_status), "composite")
  expect_equal(parse_lineage_notation("X", tokens)$id_status, "no_id")
  expect_equal(parse_lineage_notation("Z", tokens)$id_status, "not_applicable")
  expect_equal(parse_lineage_notation(NA_character_, tokens)$id_status, "blank")
})

test_that("without character sentinels, X is an ordinary name (D12.31)", {
  plain <- parse_lineage_notation("X")
  expect_equal(plain$id_status, "single")
  expect_equal(plain$part_kind, "derived")
})

test_that("| separates labelled alternatives and binds looser than +", {
  out <- parse_lineage_notation("PSP: a.x + b.y | non-PSP: c.z")
  expect_equal(out$alternative, c("PSP", "PSP", "non-PSP"))
  expect_equal(out$id_status, c("composite", "composite", "single"))
})

test_that("anything that doesn't split cleanly is unparseable, never guessed", {
  for (text in c("a.x+b.y", "a.x + ", "a.x | b.y", "PSP: ", "a.x + X", ".column", "a b")) {
    expect_equal(parse_lineage_notation(text, tokens)$id_status, "unparseable", info = text)
  }
})

test_that("build_lineage_spec places rows by the dictionary and parses id rows only", {
  attributes <- data.table::data.table(
    table_name = c("magp_subplot_meas", "magp_tally_trees"),
    attribute_name = c("src_subpmeas_id", "src_tally_id"), source_row = c(194L, 300L)
  )
  lineage <- data.table::data.table(
    contributor_label = c("BC", "BC", "BC", "ON"),
    table_name = c("magp_plot_meas", "magp_tally_trees", "magp_tally_trees", "magp_plot_meas"),
    attribute_name = c("src_subpmeas_id", "src_tally_id", "src_tally_id", "src_subpmeas_id"),
    spec_type = c("id", "id", "compiled", "id"),
    source_text = c("src_subplot_id + v.visit_number", "a.b +", "anything, else .x", "t.k"),
    note = NA_character_, source_cell = c("A2!D11", "A2!D17", "A2!D30", "A2!F11")
  )
  built <- build_lineage_spec(lineage, attributes, "a2.xlsx", dictionary_file = "dd.xlsx")
  placed <- built$component$table_name[built$component$attribute_name == "src_subpmeas_id"]
  expect_equal(unique(placed), "magp_subplot_meas")
  expect_equal(built$clashes$kind, "table_placement")
  expect_equal(built$clashes$basis, "fixed_dictionary_placement")
  expect_equal(built$clashes$value_a, "magp_plot_meas")
  # One clash per attribute and table named, citing every lineage row behind it in reading
  # order (D12.33).
  expect_equal(built$clashes$source_cell_a, "A2!D11, A2!F11")
  expect_equal(built$clashes$source_cell_b, "dd.xlsx:194")
  expect_equal(built$findings$source_cell, "A2!D17")
  expect_equal(built$component$id_status[built$component$spec_type == "compiled"], "documented")
  unknown <- build_lineage_spec(lineage, attributes, "a2.xlsx")
  expect_true(is.na(unknown$clashes$source_cell_b))
})

test_that("a lineage input names every missing and extra column at once (D12.33)", {
  lineage <- data.frame(
    contributor_label = "BC", table_name = "t", attribute_name = "a", spec_type = "id",
    source_text = "t.k", note = NA, cell = "A2!D5"
  )
  expect_error(
    read_lineage_input(lineage),
    paste(
      "`lineage_spec` must have exactly the columns contributor_label, table_name,",
      "attribute_name, spec_type, source_text, note, source_cell; missing: source_cell;",
      "not taken: cell."
    ),
    fixed = TRUE
  )
})

test_that("an empty dictionary places no lineage row (D12.26)", {
  attributes <- data.table::data.table(table_name = character(), attribute_name = character())
  lineage <- data.table::data.table(
    contributor_label = "BC", table_name = "plots", attribute_name = "src_plot_id",
    spec_type = "id", source_text = "tbl.key", note = NA_character_, source_cell = "A2!D5"
  )
  built <- build_lineage_spec(lineage, attributes, "a2.xlsx")
  expect_equal(built$component$table_name, "plots")
  expect_equal(built$component$id_status, "single")
  expect_equal(built$clashes, empty_table(spec_schema()$clashes))
})

test_that("a blank type or contributor shows as (blank) in its finding (D12.54)", {
  attributes <- data.table::data.table(table_name = "plots", attribute_name = "src_plot_id")
  lineage <- data.table::data.table(
    contributor_label = NA_character_, table_name = "plots", attribute_name = "src_plot_id",
    spec_type = NA_character_, source_text = "tbl.key", note = NA_character_,
    source_cell = "A2!D5"
  )
  built <- build_lineage_spec(lineage, attributes, "a2.xlsx")
  expect_equal(
    built$findings$detail,
    "(blank) plots.src_plot_id: type (blank) is neither id nor compiled."
  )
})
