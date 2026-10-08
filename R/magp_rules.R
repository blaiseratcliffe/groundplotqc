# MAGPlot 2.0's rule set (plan 3.5, 3.6; D3.16, D9.20, D14.2, D14.15), read from the
# rules_<component>.csv files that data-raw/build_magp_config.R copies into
# inst/extdata/magp/ from the hand-kept ones in data-raw/magp/.

#' MAGPlot 2.0's rule set
#'
#' @description
#' The rule set that configures the engine for MAGPlot 2.0. Its `rules` rows say where and
#' how registered rules apply, and its `settings` rows give MAGPlot's settings, which take
#' the rule-set place in the settings precedence (see [groundplotqc_options]).
#'
#' @return A named list of data.tables, in this order:
#'   - `meta`, one row: `rule_set_name`, `version` (raised by one at each change),
#'     `date` (of the last change) and `spec_version` (the date of the specification files
#'     it was written for);
#'   - `rules`: `rule_id`, `table_name`, `attribute_name` (`"*"` for all), `severity` and
#'     `class` (blank for the rule's default) and `enabled`;
#'   - `settings`: `setting`, `value` and `type`.
#'
#'   Each carries the attributes `source_file`, the name of the file it was read from, and
#'   `source_lines`, the file line each row starts on, which pre-flight's findings cite.
#' @examples
#' rules <- magp_rules()
#' rules$meta
#' rules$settings
#' @export
magp_rules <- function() {
  dir <- system.file("extdata", "magp", package = "groundplotqc")
  if (!nzchar(dir)) {
    stop("The package has no compiled MAGPlot rule set.", call. = FALSE)
  }
  read_rule_set(dir)
}
