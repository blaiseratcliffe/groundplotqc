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

test_that("an attribute is placed by the tables that hold it, not by the rows (D1.15b)", {
  lineage <- data.table::data.table(
    contributor_label = "BC", table_name = "magp_plot_meas", attribute_name = "src_plot_id",
    spec_type = "id", source_text = "t.k", note = NA_character_, source_cell = "A2!D5"
  )
  # Two dictionary rows of one table are one home: the row is placed there.
  twice <- data.table::data.table(
    table_name = "plots", attribute_name = "src_plot_id", source_row = c(10L, 11L)
  )
  built <- build_lineage_spec(lineage, twice, "a2.xlsx", dictionary_file = "dd.xlsx")
  expect_equal(built$component$table_name, "plots")
  expect_equal(built$clashes$value_b, "plots")
  expect_equal(built$clashes$source_cell_b, "dd.xlsx:10")
  # Two tables are two homes: the row stays where A2 puts it, with no clash.
  two_tables <- data.table::data.table(
    table_name = c("plots", "visits"), attribute_name = "src_plot_id", source_row = c(10L, 20L)
  )
  kept <- build_lineage_spec(lineage, two_tables, "a2.xlsx", dictionary_file = "dd.xlsx")
  expect_equal(kept$component$table_name, "magp_plot_meas")
  expect_equal(nrow(kept$clashes), 0L)
})

test_that("a sentinel as a whole alternative or one part of a composite is unparseable", {
  for (text in c("PSP: a.x + X | non-PSP: c.z", "PSP: a.x | non-PSP: Z")) {
    expect_equal(parse_lineage_notation(text, tokens)$id_status, "unparseable", info = text)
    # Without sentinels X and Z are ordinary names, so the same text parses.
    expect_false("unparseable" %in% parse_lineage_notation(text)$id_status, info = text)
  }
})

test_that("read_lineage_input returns the table with its columns in the schema's order", {
  lineage <- data.frame(
    source_cell = "A2!D5", note = NA, source_text = "t.k", spec_type = "id",
    attribute_name = "a", table_name = "t", contributor_label = "BC"
  )
  read <- read_lineage_input(lineage)
  expect_named(read$data, spec_input_schema()$lineage_spec)
  expect_equal(read$data$source_cell, "A2!D5")
  expect_equal(read$data$source_text, "t.k")
  expect_true(is.na(read$data$note))
  expect_equal(read$manifest$input, "lineage_spec")
  expect_equal(read$manifest$file, "in memory")
  expect_equal(nrow(read$findings), 0L)
})

test_that("a type that is neither id nor compiled is unparseable, with its own text (D12.54)", {
  attributes <- data.table::data.table(table_name = "plots", attribute_name = "src_plot_id")
  lineage <- data.table::data.table(
    contributor_label = "BC", table_name = "plots", attribute_name = "src_plot_id",
    spec_type = "other", source_text = "tbl.key", note = NA_character_, source_cell = "A2!D5"
  )
  built <- build_lineage_spec(lineage, attributes, "a2.xlsx")
  expect_equal(built$component$id_status, "unparseable")
  expect_equal(
    built$findings$detail, "BC plots.src_plot_id: type other is neither id nor compiled."
  )
  expect_equal(built$findings$source_cell, "A2!D5")
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

test_that("lineage_when rows go before their part's own row, in the file's order (D19.12)", {
  when <- rbind(
    fx_lineage_when("kind == \"A\"", "field.age_key"),
    fx_lineage_when("kind == \"L\"", "field.live_key")
  )
  spec <- fx_lineage_spec(lineage_when = when)
  lineage <- spec$lineage_spec
  south <- lineage[lineage$contributor_label == "south"]
  expect_equal(south$part_source, c("field.age_key", "field.live_key", "field.tree_key"))
  expect_equal(south$when, c("kind == \"A\"", "kind == \"L\"", NA))
  expect_equal(south$id_status, rep("single", 3L))
  expect_equal(south$part_kind, rep("source", 3L))
  expect_equal(unique(lineage$separator), "_")
  # A2's own rows keep their order.
  expect_equal(lineage$part_source[is.na(lineage$when)], fx_lineage_spec()$lineage_spec$part_source)
})

test_that("each lineage_when row keeps its first problem, a finding at its cell (D19.37, D19.48)", {
  bad <- data.frame(
    contributor_label = c("east", "north", "north", "north", "south", "south", NA),
    table_name = "trees", attribute_name = "src_tree_id",
    part_order = c("1", "3", "x", "1", "1", "1", "1"),
    part_source = c("a.b", "a.b", "a.b", "a.b", "a b", "a.b", "a.b"),
    when = c(
      "kind == \"A\"", "kind == \"A\"", "kind == \"A\"", "kind == \"A\"", "kind == \"A\"",
      "kind = \"A\"", "kind == \"A\""
    ),
    note = NA
  )
  spec <- fx_lineage_spec(lineage_when = bad)
  found <- spec$read_findings[spec$read_findings$rule_id == "lineage_when_invalid"]
  expect_equal(nrow(found), 7L)
  # Kinds by the detail texts' ids: entry (east, and the blank contributor), part (3, x),
  # nested (north's part 1 is src_site_id's parts), source, when.
  ids <- c(
    "entry", "part", "part", "nested", "source", "when", "entry"
  )
  expected <- vapply(seq_along(ids), function(i) {
    report_text(
      paste0("preflight_detail_lineage_when_invalid_", ids[[i]]),
      contributor_label = bad$contributor_label[[i]], table_name = "trees",
      attribute_name = "src_tree_id",
      value = bad[[c(
        entry = "contributor_label", part = "part_order", nested = "part_order",
        source = "part_source", when = "when"
      )[[ids[[i]]]]]][[i]]
    )
  }, character(1))
  expect_setequal(found$detail, expected)
  # No bad row reaches the component.
  expect_true(all(is.na(spec$lineage_spec$when)))
})

test_that("a row on an ID with labelled alternatives is a finding (D19.48)", {
  lineage <- data.frame(
    contributor_label = "south", table_name = "trees", attribute_name = "src_tree_id",
    spec_type = "id", source_text = "PSP: a.x + b.y | other: c.z", note = NA, source_cell = NA
  )
  spec <- fx_lineage_spec(lineage_spec = lineage, lineage_when = fx_lineage_when())
  found <- spec$read_findings[spec$read_findings$rule_id == "lineage_when_invalid"]
  expect_equal(found$detail, report_text(
    "preflight_detail_lineage_when_invalid_alternatives",
    contributor_label = "south", table_name = "trees", attribute_name = "src_tree_id",
    value = "south"
  ))
})

test_that("a lineage_when file's findings cite its lines (D12.54)", {
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(fx_lineage_when("kind == A"), path)
  spec <- fx_lineage_spec(lineage_when = path)
  found <- spec$read_findings[spec$read_findings$rule_id == "lineage_when_invalid"]
  expect_equal(found$source_cell, paste0(basename(path), ":2"))
  expect_equal(spec$manifest$file[spec$manifest$input == "lineage_when"], basename(path))
})

test_that("a lineage_when input without exactly its columns is a caller's error (D12.33)", {
  extra <- cbind(fx_lineage_when(), extra = 1)
  expect_error(fx_lineage_spec(lineage_when = extra), "not taken: extra")
})
