# The column names that data.table expressions use without quotes, declared so
# R CMD check doesn't report them as undefined globals (plan 17.8, D12.29). Each
# task adds its names to this one call.

#' @importFrom utils globalVariables
NULL

utils::globalVariables(c(
  "attribute_name", "i.n_pk", "i.pk", "key_part", "key_type", "n_pk", "n_ref", "pk",
  "ref_pk", "reference_attribute", "reference_table", "table_name"
))
