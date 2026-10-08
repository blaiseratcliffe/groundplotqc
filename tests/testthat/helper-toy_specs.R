# Fixture builders for the toy specs and the planted-defect spec (D12.18, D12.27, D12.31),
# and a planted rule set and settings, one defect per check of M3a's (D12.31).

fx_fish_column_map <- function(...) {
  gpq_column_map(
    table = "table", attribute = "field", type = "kind", key_type = "key",
    reference = "parent", lookup = "codes", description = "notes", ...
  )
}

fx_fish_code_lists <- function() {
  list(
    water_body = example_file("fish_water_body.csv"), gear = example_file("fish_gear.csv"),
    species = example_file("fish_species.csv")
  )
}

fx_fish_spec <- function(code_lists = fx_fish_code_lists(), ...) {
  gpq_read_spec(
    dictionary = example_file("fish_dictionary.csv"), code_lists = code_lists,
    column_map = fx_fish_column_map(),
    type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "?", not_applicable = "~"),
      date = c(missing = "?", not_applicable = "~")
    ),
    ...
  )
}

# The fish spec with the `gear` code list holding "GN" twice: the one defect that only
# warns (code_list_duplicate_code), so pre-flight signals one warning and no stop (D12.68).
# fx_fish_code_lists()'s elements are CSV paths, so modifyList() replaces gear whole rather
# than merging into it (D12.27).
fx_fish_gear_twice_spec <- function() {
  lists <- modifyList(fx_fish_code_lists(), list(gear = data.frame(gear = c("GN", "GN"))))
  fx_fish_spec(code_lists = lists)
}

fx_forest_spec <- function() {
  gpq_read_spec(
    dictionary = example_file("forest_dictionary.csv"),
    code_lists = list(
      SPECIES = example_file("forest_species.csv"), STATUS = example_file("forest_status.csv")
    ),
    id_pattern = "_ID$",
    column_map = gpq_column_map(
      table = "TABLE", attribute = "COLUMN", type = "FORMAT", key_type = "KEY",
      reference = "REFERS_TO", lookup = "CODE_LIST", description = "DEFINITION"
    ),
    type_map = gpq_type_map(example_file("forest_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -7, not_applicable = -8),
      character = c(missing = ".", not_applicable = "-"),
      date = c(missing = "00000000", not_applicable = "99999999")
    )
  )
}

# A synthetic spec with one planted defect per pre-flight check (plan 4.2); the expected
# findings are in test-preflight.R. Each defect triggers only its own check (D12.31):
#   plots.plot_id twice: dd_duplicate_attribute 1; trees.tree_id typed "text":
#   dd_type_unknown 1; trees without a PK: dd_pk_missing 1; trees.plot_id to "stands":
#   dd_fk_target_missing 1; soils: code_list_missing 1; cover's sheet without a cover
#   column: code_column_missing 1; shape's repeated SQ, blank row 3 (as R counts rows)
#   and empty use_when: code_list_duplicate_code, code_list_blank_row and
#   code_list_empty_column 1 each; sheet extra: code_list_unreferenced 1; ds against
#   datasets: spec_clash_resolved 1 (name), spec_clash_unresolved 1 (kind),
#   datasets_row_missing 1 (id 3); the bad byte in shape's description:
#   spec_encoding_invalid 1; the non-code sheet notes, a CSV whose line 3 has a field too
#   many: spec_csv_malformed 1; cond's bad byte: crosswalk_unreadable 1.
# Intended overlaps, each counted: site_id_range_invalid 2 (ZZ unknown, and AA and ZZ
# overlapping); lineage_spec_unparseable 2 (BC src_site_id's "a.b+" and ON's blank
# src_plot_id); src_site_id also carries lineage_spec_row_unflagged 1 (an A2 row, no DD
# flag) and lineage_id_unflagged 1 (an ID by name, no DD flag), and "regen" in BC
# src_plot_id lineage_name_unknown 1.
fx_planted_spec <- function() {
  bad_text <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad_text) <- "UTF-8"
  notes <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("note,author", "first,ab", "second, with a comma,cd"), notes)
  dictionary <- data.frame(
    table_name = c(
      "plots", "plots", "plots", "plots", "plots", "trees", "trees", "plots", "plots"
    ),
    attribute_name = c(
      "plot_id", "plot_id", "shape", "soil", "cover", "tree_id", "plot_id", "src_plot_id",
      "src_site_id"
    ),
    key_type = c("PK", "PK", ".", ".", ".", ".", "FK", ".", "."),
    reference_table = c(NA, NA, NA, NA, NA, NA, "stands", NA, NA),
    lookup_table = c(NA, NA, "Y", "soils", "Y", NA, NA, NA, NA),
    description = c(
      "Plot.", "Again.", bad_text, "Soil.", "Cover.", "Tree.", "Plot.", "Source plot.",
      "Source site."
    ),
    appendix = c(NA, NA, NA, NA, NA, NA, NA, "A2", NA),
    data_type = c(rep("character", 5L), "text", rep("character", 3L))
  )
  code_lists <- list(
    shape = data.frame(
      shape = c("SQ", "SQ", NA, "CI"), description = c("square", "square again", NA, "circle"),
      use_when = NA
    ),
    cover = data.frame(code = "X", description = "missing"),
    extra = data.frame(extra = c(" lead", "NA", "01", "say \"hi\"", "café")),
    ranges = data.frame(
      contributor = c("AA", "ZZ"), v2_start = c("1", "50"), v2_end = c("100", "60")
    ),
    contributor = data.frame(name = "AA"),
    ds = data.frame(id = c("1", "2", "3"), name = c("a", "c", "d"), kind = c("p", "r", "s")),
    notes = notes
  )
  bad_walk <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xE9)))
  Encoding(bad_walk) <- "UTF-8"
  gpq_read_spec(
    dictionary = dictionary, code_lists = code_lists,
    non_code_sheets = c("ranges", "contributor", "ds", "notes"),
    datasets = data.frame(id = c("1", "2"), name = c("a", "b"), kind = c("p", "q")),
    lineage_spec = data.frame(
      contributor_label = c("BC", "BC", "ON"), table_name = "plots",
      attribute_name = c("src_plot_id", "src_site_id", "src_plot_id"),
      spec_type = "id", source_text = c("tbl.key + regen", "a.b+", NA),
      note = NA, source_cell = c("A2!D5", "A2!D6", "A2!F5")
    ),
    crosswalks = list(cond = data.frame(code = c("A", bad_walk))),
    id_pattern = "^src_.*_id$",
    id_bands = list(
      sheet = "ranges", label_col = "contributor", start_col = "v2_start", end_col = "v2_end",
      labels_from = c(sheet = "contributor", column = "name"), reserved_pattern = NULL
    ),
    precedence = data.frame(
      sheet = "ds", key_col = "id", attribute_name = "name", winner = "datasets",
      blank_rule = "wins"
    ),
    column_map = gpq_column_map(lineage_flag = c(column = "appendix", value = "A2"))
  )
}

# A rule set and settings with one planted defect per check of M3a's, for fx_planted_spec()
# (D12.31): rows 1 and 3 name unregistered rules, and the severity setting names a third
# (rule_id_unknown 3); row 3 also names the table stands, which the spec lacks
# (rule_set_unknown_column 1, an intended overlap); row 2 names the pre-flight check
# dd_pk_missing (rule_set_override_invalid 1, D14.5); the rule set's language, fr, has no
# rows (text_id_fallback, one finding per registered rule). text_id_missing can't fire on
# the registry's own rules, which all have English text; its test gives the check a
# registry of its own.
fx_planted_rule_set <- function() {
  list(
    meta = data.frame(
      rule_set_name = "planted", version = "1", date = "2026-10-07", spec_version = "20261005"
    ),
    rules = data.frame(
      rule_id = c("no_such_rule", "dd_pk_missing", "other_rule"),
      table_name = c("plots", "*", "stands"), attribute_name = c("plot_id", "*", "*"),
      severity = c(NA, "warning", NA), class = NA_character_, enabled = TRUE
    ),
    settings = data.frame(setting = "lang", value = "fr", type = "character")
  )
}

fx_planted_settings <- function() {
  list(severity = c(also_unknown = "error"))
}

# An empty rule set: every check of the rule set runs and finds nothing.
fx_empty_rule_set <- function() {
  list(rules = data.frame(
    rule_id = character(), table_name = character(), attribute_name = character(),
    severity = character(), class = character(), enabled = logical()
  ))
}
