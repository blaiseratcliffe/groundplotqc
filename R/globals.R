# The column names that data.table expressions use without quotes, declared so
# R CMD check doesn't report them as undefined globals (plan 17.8, D12.29). Each
# task adds its names to this one call.

#' @importFrom utils globalVariables
NULL

utils::globalVariables(c(
  "attribute_name", "code_column", "crosswalk", "crosswalk_column", "home", "i.home",
  "i.n_home", "i.n_pk", "i.pk", "id_status", "key_part", "key_type", "lookup", "n_home", "n_pk",
  "n_ref", "pk", "ref_pk", "reference_attribute", "reference_table", "rule_id", "sheet",
  "sheet_column", "source_cell", "source_row", "source_type", "status", "table_name", "value"
))
