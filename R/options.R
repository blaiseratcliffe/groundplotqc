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
#'
#' More options are added by the versions that bring the settings they control.
#'
#' @name groundplotqc_options
NULL
