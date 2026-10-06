# The column names that data.table expressions use without quotes, declared so
# R CMD check doesn't report them as undefined globals (plan 17.8, D12.29). Each
# task adds its names to this one call.

#' @importFrom utils globalVariables
NULL

utils::globalVariables(c(
  "attribute_name", "blank", "code_column", "contributor_label", "crosswalk",
  "crosswalk_column", "data_type", "description", "file", "filled", "header", "home",
  "i.home", "i.n_home", "i.n_pk", "i.pk", "id_marked", "id_status", "key_part", "key_type",
  "lineage_flag", "lookup", "n", "n_findings", "n_home", "n_pk", "n_ref", "not_run_reason",
  "outcome", "part_source", "pk", "position", "ref_pk", "reference_attribute", "reference_table",
  "rule_id", "sheet", "sheet_column", "source_cell", "source_name", "source_row",
  "source_type", "status", "table_name", "value"
))
