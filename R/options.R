#' Options for groundplotqc
#'
#' @description
#' groundplotqc functions read their settings from function arguments, the rule
#' set and R options. This page lists the R options.
#'
#' @section Option names:
#' Each option is named `groundplotqc.<setting>`, with the setting's name in
#' lower snake case after the dot.
#'
#' @section Where a setting's value comes from:
#' A function takes each setting from the first of these that gives a value:
#'
#' 1. the function's argument;
#' 2. the rule set;
#' 3. the option, set with [options()];
#' 4. the built-in default.
#'
#' @section Options:
#' - `groundplotqc.lang`: the language of report text, a code of two or three
#'   lower-case letters such as `"fr"`. Built-in: `"en"`. A text with no row in
#'   the language is shown in English. In a language other than English,
#'   pre-flight's `text_id_fallback` lists each registered rule's message that
#'   has no row in it.
#' - `groundplotqc.severity`: severities by rule ID, a named character vector
#'   such as `c(some_rule = "warning")`, each rule named once. Built-in: none,
#'   so each rule keeps its registered severity. In a rule set the `rules`
#'   rows' `severity` column gives this tier. Pre-flight checks each entry
#'   (`rule_id_unknown`, `rule_set_override_invalid`); a pre-flight check's own
#'   outcome can't be changed.
#' - `groundplotqc.example_cap`: how many distinct failing values a results row
#'   shows in its `examples` column. Built-in: `5`.
#' - `groundplotqc.column_level_min_n`: the fewest records a column must have in
#'   a group before a rule that fails on all of them is recorded as one
#'   column-level issue. Built-in: `10`.
#' - `groundplotqc.raw_na_policy`: when a raw `NA` is an error (`raw_na`) in a
#'   column outside the primary and foreign keys, whose missing values Layer 4
#'   reports: `"when_sentinels"`, only in a column whose type has missing-value
#'   codes; `"always"`; or `"never"`. Built-in: `"when_sentinels"`.
#' - `groundplotqc.rogue_codes`: values that stand in for a missing value but
#'   aren't the agreed codes (`sentinel_rogue_code`), compared as numbers in a
#'   column stored as numbers, as text otherwise. A value is never one when it
#'   is a code of the column's code list, a missing-value code of the column's
#'   type, an unknown code of the attribute, or another type's missing-value
#'   code that `sentinel_wrong_type` reports. Built-in:
#'   `c("-999", "UNK", "NA", "N/A")`.
#' - `groundplotqc.unknown_codes`: codes for "unknown" by attribute, such as
#'   `c(species = "UNKN.SPP")` (`species_unknown_code`); an attribute may be
#'   named more than once. Built-in: none.
#'
#' In a rule set's settings rows, a whole number has type `integer`, a setting
#' with several values gives them joined by "; ", and a named one gives
#' "name=value" parts. In every tier, a
#' code can't be empty, begin or end with a space, tab or line break, or hold
#' ";". In `groundplotqc.unknown_codes` the same holds for each name, and
#' neither a code nor a name can hold "=".
#'
#' More options are added by the versions that bring the settings they control.
#'
#' @name groundplotqc_options
NULL
