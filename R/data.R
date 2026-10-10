# The package's example data (plan 18.1; D5.11, D15.10), built by
# data-raw/build_magp_example.R with the tests' clean-base builder.

#' MAGPlot 2.0 example tables
#'
#' @description
#' A small synthetic example of MAGPlot 2.0's 17 tables, built from [magp_spec()]: every
#' table and column of the data dictionary, each column of the R class its type maps to,
#' and made-up values that come from no delivery. The help pages' examples use it.
#'
#' @format A named list of 17 data.tables, one per dictionary table, in the dictionary's
#'   order (`magp_contributors` to `magp_tally_trees`), each with the dictionary's columns
#'   in its order. It has two contributors, each with two datasets; every key points to a
#'   row of its parent table, and every coded column holds a code from its list. The list
#'   and each table carry the attribute `gpq_fixture = TRUE`, which marks synthetic data.
#'   There is no `magp_meta` table.
#'
#'   Changing a table by reference (`:=` or `data.table::set()`) can change the example for
#'   the rest of the session; take a copy with `data.table::copy()` first.
#' @source Built by `data-raw/build_magp_example.R` in the package's source repository.
#' @examples
#' names(magp_example)
#' magp_example$magp_plots
#' # A copy to change, leaving the example as it is.
#' plots <- data.table::copy(magp_example$magp_plots)
"magp_example"
