# The rule ID map (plan 9.6, 17.4; D4.2, D5.2, D9.20): inst/extdata/rules/rule_id_map.csv,
# one row per renamed or split rule, so the comparison with a previous run and the
# acknowledgements (M8a) can translate an old rule ID. The package's own file, header only
# until a rule is renamed or split; the add-rule skill adds its rows.

#' The rule ID map, read and checked
#'
#' Columns `old_rule_id`, `new_rule_id`, `change` (`rename`, `split`) and `package_version`
#' (D9.20), all text. A file that doesn't read cleanly or breaks validate_rule_id_map() is
#' an internal error.
#' @noRd
rule_id_map <- function() {
  path <- system.file("extdata", "rules", "rule_id_map.csv", package = "groundplotqc")
  if (!nzchar(path)) {
    stop("The package has no rule_id_map.csv.", call. = FALSE)
  }
  read <- read_csv_text(path)
  if (nrow(read$malformed) > 0L || nrow(read$invalid) > 0L) {
    stop("The package's rule_id_map.csv doesn't read cleanly.", call. = FALSE)
  }
  validate_rule_id_map(read$data)
  read$data
}

#' A rule ID map checked against the registry
#'
#' Exactly its four columns, every cell filled, `change` `rename` or `split`, no row twice;
#' an old rule ID is never registered, since rule IDs are never reused (D5.2), and a new
#' one always is, so a rule renamed twice has its first row rewritten to the latest ID; a
#' renamed rule has one row and a split rule two or more, all of one change. Returns NULL
#' invisibly, or stops naming the rows' old rule IDs.
#' @noRd
validate_rule_id_map <- function(map, registry = rule_registry()) {
  columns <- c("old_rule_id", "new_rule_id", "change", "package_version")
  if (!identical(names(map), columns)) {
    stop(sprintf(
      "The rule ID map has the columns %s, in that order.", paste(columns, collapse = ", ")
    ), call. = FALSE)
  }
  if (nrow(map) == 0L) {
    return(invisible(NULL))
  }
  fail <- function(what, bad) {
    if (any(bad)) {
      stop(sprintf(
        "The rule ID map %s: %s.", what, paste(unique(map$old_rule_id[bad]), collapse = ", ")
      ), call. = FALSE)
    }
  }
  blank <- Reduce(`|`, lapply(map, function(x) is.na(x) | !nzchar(trimws(x))), FALSE)
  fail("has blank cells in the rows of", blank)
  fail("has a change other than rename or split for", !map$change %chin% c("rename", "split"))
  fail("repeats a row for", duplicated(map))
  fail("names registered rules as old rule IDs", map$old_rule_id %chin% registry$rule_id)
  fail("names unregistered rules as new rule IDs for", !map$new_rule_id %chin% registry$rule_id)
  n_rows <- table(map$old_rule_id)[map$old_rule_id]
  kinds <- tapply(map$change, map$old_rule_id, function(x) length(unique(x)))[map$old_rule_id]
  fail("mixes rename and split for", as.vector(kinds) > 1L)
  fail("has more than one rename row for", map$change == "rename" & as.vector(n_rows) > 1L)
  fail("has a split with one row for", map$change == "split" & as.vector(n_rows) < 2L)
  invisible(NULL)
}
