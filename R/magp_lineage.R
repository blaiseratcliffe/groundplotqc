# MAGPlot's derived flags for lineage decoding (plan 10.1; D19.17, D19.41, D19.42; R1): what a
# lineage condition reads that the delivery doesn't hold, built by the MAGPlot layer and passed
# to gpq_decode_lineage(flags = ). magp_run_qc() calls it from M7.

#' MAGPlot's flags for gpq_decode_lineage(): `age_tree` for each tree (D19.17, D19.41; R1)
#'
#' `age_tree` is "Y" for a tree of magp_trees all of whose measurements in magp_tree_meas
#' have tree_type "A" (an age tree: ON's src_tree_id then names tblAgeTree.agetreekey), and
#' "N" for any other tree, one with no measurement included. One row per distinct
#' magp_tree_id of magp_trees, an NA or blank key left out, so a measurement whose tree key
#' matches no tree adds no row, and its broken link stays the foreign-key rules' to report
#' (R1). Reads magp_trees.magp_tree_id, magp_tree_meas.magp_tree_id and
#' magp_tree_meas.tree_type by reference (16.2); a table or column not given flags no tree
#' "Y". `tables` is a named list of data.frames, as magp_read_delivery() returns it. Returns
#' `list(magp_trees = data.table(magp_tree_id, age_tree))`.
#' @noRd
age_tree_flags <- function(tables) {
  trees <- tables[["magp_trees"]]
  tree_key <- if (!is.null(trees)) trees[["magp_tree_id"]]
  if (is.null(tree_key)) {
    tree_key <- character()
  }
  keys <- unique(tree_key)
  # A key the records can't be matched on gets no row (R1), and so no flag. Only text can be
  # blank, so a number-typed key isn't written as text to be tested (16.2).
  keep <- !is.na(keys)
  if (is.character(keys) || is.factor(keys)) {
    keep <- keep & !is_blank(value_text(keys))
  }
  keys <- keys[keep]
  age_tree <- rep("N", length(keys))
  measurements <- tables[["magp_tree_meas"]]
  if (length(keys) > 0L && all(c("magp_tree_id", "tree_type") %in% names(measurements))) {
    tree <- match(measurements[["magp_tree_id"]], keys)
    known <- !is.na(tree)
    measured <- tabulate(tree[known], length(keys)) > 0L
    other <- tabulate(tree[known & !measurements[["tree_type"]] %in% "A"], length(keys)) > 0L
    age_tree[measured & !other] <- "Y"
  }
  list(magp_trees = data.table(magp_tree_id = keys, age_tree = age_tree))
}
