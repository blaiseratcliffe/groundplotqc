# Fixtures for lineage decoding (D19.30): a two-table spec whose source IDs decode, its tables,
# and the run whose issues they are, or those issues made by hand. Every tree's height fails
# value_below_min, so each tree has one record issue (four or fewer per contributor, below
# column_level_min_n's 10).
#   north: sites.src_site_id is survey.site; trees.src_tree_id is src_site_id + survey.tree,
#     a nested ID of two parts. t1 "41_7" decodes; t2 "41_X" is partial; t3 "41_7_b" is raw;
#     t8 "41_7" decodes to t1's key, so both keep the pk: fingerprint (D19.15).
#   south: trees.src_tree_id is field.tree_key, one part. t4 "k100" decodes; t5 "a200" is of
#     kind A, which a lineage_when row can label field.age_key.
#   east: no entry, so t6 "e5" is no_lineage_spec.
#   west: A2's X, no ID, so t7 is fallback_pk.

fx_lineage_spec <- function(lineage_when = NULL, separator = "_", lineage_spec = NULL) {
  dictionary <- data.frame(
    table_name = c(rep("sites", 3L), rep("trees", 5L)),
    attribute_name = c(
      "site_id", "contributor", "src_site_id", "tree_id", "site_id", "kind", "src_tree_id",
      "height"
    ),
    key_type = c("PK", ".", ".", "PK", "FK", ".", ".", "."),
    reference_table = c(NA, NA, NA, NA, "sites", NA, NA, NA),
    data_type = c(rep("character", 7L), "numeric"),
    min_value = c(rep(NA, 7L), "0")
  )
  if (is.null(lineage_spec)) {
    lineage_spec <- data.frame(
      contributor_label = c("north", "north", "south", "west"),
      table_name = c("sites", "trees", "trees", "trees"),
      attribute_name = c("src_site_id", "src_tree_id", "src_tree_id", "src_tree_id"),
      spec_type = "id",
      source_text = c("survey.site", "src_site_id + survey.tree", "field.tree_key", "X"),
      note = NA, source_cell = NA
    )
  }
  gpq_read_spec(
    dictionary,
    lineage_spec = lineage_spec, lineage_when = lineage_when, lineage_separator = separator,
    id_pattern = "^src_.*_id$",
    sentinels = gpq_sentinels(
      numeric = c(missing = -1, not_applicable = -9),
      character = c(missing = "X", not_applicable = "Z")
    )
  )
}

fx_lineage_tables <- function() {
  tables <- list(
    sites = data.frame(
      site_id = c("s1", "s2", "s3", "s4"), contributor = c("north", "south", "east", "west"),
      src_site_id = c("41", "S9", "E1", "W1")
    ),
    trees = data.frame(
      tree_id = paste0("t", 1:8), site_id = c("s1", "s1", "s1", "s2", "s2", "s3", "s4", "s1"),
      kind = c("L", "L", "L", "L", "A", "L", "L", "L"),
      src_tree_id = c("41_7", "41_X", "41_7_b", "k100", "a200", "e5", "w1", "41_7"),
      height = -5
    )
  )
  # The fixture marker (18.3), set with attr(): the tables stay data.frames, decoding's
  # data.frame path, which fx_mark_fixture() doesn't take.
  for (table in names(tables)) {
    attr(tables[[table]], "gpq_fixture") <- TRUE
  }
  attr(tables, "gpq_fixture") <- TRUE
  tables
}

# The run's issues, one per tree, keyed by tree for the tests.
fx_lineage_issues <- function(spec, tables = fx_lineage_tables()) {
  fx_require_fixture(tables)
  results <- gpq_check_conformance(tables, spec, group_cols = "contributor", layers = 3)
  results$issues
}

# The issues a run gives for one value_below_min failure on each of `rows` of the last table
# in `tables` (the trees), made by hand, so no run is needed: in the columns
# gpq_decode_lineage() reads they are the run's (R60), its fingerprint left to the decoder.
# The table's first column is its PK, written as the run writes record_pk, positional where it
# is missing or repeated; each issue's contributor is its record's site's, where `tables`
# gives the sites.
fx_lineage_made_issues <- function(tables, rows) {
  fx_require_fixture(tables)
  table <- names(tables)[[length(tables)]]
  key <- value_text(tables[[table]][[1L]])
  pk <- key[rows]
  positional <- is.na(key) | duplicated(key) | duplicated(key, fromLast = TRUE)
  issues <- data.table::data.table(
    issue_id = seq_along(rows), rule_id = "value_below_min", table_name = table,
    column_name = "height", record_pk = pk, row_number = rows,
    pk_positional = positional[rows], provider_key = NA_character_,
    provider_key_status = NA_character_, fingerprint = NA_character_,
    fingerprint_pk = data.table::fifelse(
      is.na(pk), NA_character_, paste0("value_below_min|", table, "|height|pk:", pk)
    )
  )
  sites <- tables[["sites"]]
  if (!is.null(sites)) {
    data.table::set(issues,
      j = "contributor",
      value = sites$contributor[match(tables[[table]]$site_id[rows], sites$site_id)]
    )
  }
  data.table::setattr(issues, "gpq_fixture", TRUE)
  issues
}

# One condition: ON-like, the south trees of kind A take another provider column.
fx_lineage_when <- function(when = "kind == \"A\"", part_source = "field.age_key") {
  data.frame(
    contributor_label = "south", table_name = "trees", attribute_name = "src_tree_id",
    part_order = "1", part_source = part_source, when = when, note = NA
  )
}
