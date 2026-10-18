# Layer 4, keys and relationships (plan 6.1; D2.28 to D2.32, D7.3, D7.7, D7.9, D8.24, D9.26,
# D12.31, D12.75, D16.2, D18.1, D18.3, D18.4, D18.10, D18.14, D18.15, D18.33 to D18.43): each
# table's primary key present and unique, each foreign key naming a parent row, the redundant
# keys key_pairs names agreeing, src_*_id values that repeat, the records no root reaches, the
# parents no child points at, and the three design candidates of Appendix E. A key is read as
# given: NA, a blank (any Unicode space in text that isn't plain ASCII, D16.42, D18.15) and the
# key type's sentinels are kept apart as 3.5's allowed_in_pk and allowed_in_fk say. A value
# that failed Layers 1 to 3 is masked by the run (6.2). The engine names no table, column or
# code: the keys come from the dictionary, the pairs and the walk from the specification, and
# the candidates' tables and columns from the settings (CLAUDE.md "Code").
#
# Task cuts (M5): task 9 writes the file with check_keys() returning an empty list; task 10
# adds key_rows(), pk_holders() and the rules pk_duplicate to fk_not_applicable_mismatch, with
# the locals rows_of(), data_of(), edge_of(), parent_missing() and parent_rows(); task 11
# key_incoherent; task 12 record_labels(), the local units_of() and src_id_duplicate; task 13
# reach_rows(), record_unreachable and parent_without_children; task 14 disagreeing_rows(), the
# local by_groups() and the three design candidates. Each cut keeps the rules in this order, the
# registry's, and each later task inserts its rule at its place.

#' Layer 4's outcomes, by rule
#'
#' Its rules arrive with M5's tasks 10 to 14.
#' @noRd
check_keys <- function(input, context) {
  list()
}
