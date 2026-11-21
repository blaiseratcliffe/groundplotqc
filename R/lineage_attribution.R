# Attribution to scripts (plan 10.4; D4.4 to D4.6, D23.3, D23.27, D23.39, D23.40): each issue's
# processing script and block from the attribution map, and whether that script may have
# changed since it made the delivery. The map is read as pre-flight reads its run inputs
# (read_input_table()); the issues are read by column and set by reference, by one keyed join
# (16.2). The engine names no group column: the map's key column is the caller's.

#' Each issue's script, block and attribution status from the attribution map (10.4; D23.27,
#' D23.39, D23.40)
#'
#' `attribution_map` is a data.frame or a CSV path with run_input_schema()'s
#' `attribution_map` columns and `key_col`, the issues' group column the map is keyed by
#' (MAGPlot: jurisdiction). Caller's errors: a map that doesn't read cleanly, or without
#' exactly those columns. An issue takes the row of its key value, table and column (the map's
#' `attribute_name`), a column "*" meeting a whole table's issue, and its status from the
#' commits: `script_version_unknown` where `script_commit`, the commit of the scripts that made
#' the delivery, is NA (MAGPlot's today: magp_meta records none, request 5); else `current`
#' where the row's `script_commit` is the same, `may_be_stale` where it differs or the row gives
#' none. An issue no row matches is `unmapped`, with the script and block of its key value's
#' row whose table and attribute are both "*", NA without one. Key cells are matched as
#' written, "*" in the key column standing for no value; a row with a blank key cell matches
#' nothing, and of rows repeating a key the first is read (pre-flight stops on both, 4.2).
#' Returns `issues`, invisibly, with `script_file`, `script_block` and `attribution_status`
#' set.
#' @noRd
attribute_issues <- function(issues, attribution_map, key_col, script_commit = NA_character_) {
  read <- read_input_table(attribution_map, "attribution_map")
  stop_if_unclean(read, "attribution_map")
  map <- read$data
  check_input_columns(map, c(key_col, run_input_schema()$attribution_map), "attribution_map")
  keys <- c(key_col, "table_name", "attribute_name")
  keyed <- Reduce(`&`, lapply(keys, function(column) !is.na(map[[column]])))
  whole <- keyed & map$table_name %chin% "*" & map$attribute_name %chin% "*"
  rows <- which(keyed & !whole)
  # One join of every issue's key value, table and column to the map's other rows, each side
  # a new table of the vectors, its columns named here, so no caller's key column meets the
  # join's own; NA where an issue meets none.
  attribute <- setDT(lapply(keys, function(column) map[[column]][rows]))
  setnames(attribute, c("k1", "k2", "k3"))
  asked <- setDT(list(issues[[key_col]], issues$table_name, issues$column_name))
  setnames(asked, c("k1", "k2", "k3"))
  at <- rows[attribute[asked, on = c("k1", "k2", "k3"), mult = "first", which = TRUE]]
  mapped <- !is.na(at)
  # An issue no row maps takes its key value's own row, with "*" as table and attribute.
  wholes <- which(whole)
  from <- at
  from[!mapped] <- wholes[match(issues[[key_col]][!mapped], map[[key_col]][wholes])]
  status <- rep("unmapped", length(at))
  status[mapped] <- if (is.na(script_commit)) {
    "script_version_unknown"
  } else {
    fifelse(map$script_commit[at[mapped]] %chin% script_commit, "current", "may_be_stale")
  }
  set(issues, j = "script_file", value = map$script_file[from])
  set(issues, j = "script_block", value = map$script_block[from])
  set(issues, j = "attribution_status", value = status)
  invisible(issues)
}
