# Pre-flight a specification

Checks a specification, never the data, and stops before a run that
would read it wrongly. Given a rule set, it checks the rule set's rows
too; it checks the settings and report text a run would use, and the
run's optional inputs it is given. Each check gives one row per finding,
or one pass or not-run row.

## Usage

``` r
gpq_preflight(
  spec,
  rules = NULL,
  inputs = list(),
  settings = list(),
  text = NULL,
  output_dir = NULL
)
```

## Arguments

- spec:

  A specification from
  [`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md).

- rules:

  `NULL`, or a rule set: a named list of tables with a `rules`
  component, as
  [`magp_rules()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_rules.md)
  returns. Its `rules` rows are checked against the specification and
  the registered rules, and its `settings` rows give settings.

- inputs:

  A named list of the run's optional inputs, each a data.frame or the
  path of a CSV file, read as text: `overrides`, your classes for
  issues, with the columns `fingerprint`, `rule_id`, `table_name`,
  `column_name`, `issue_class`, `who`, `when` and `why`, and any of the
  run's group columns, each an attribute of the specification;
  `lineage`, the provider's raw values of the cells the pipeline changed
  (`table_name`, `record_pk`, `column_name`, `raw_value`, `raw_source`);
  and `pipeline_fill`, the cells the pipeline's missing-value fill wrote
  (`table_name`, `record_pk`, `column_name`, `written_value`).
  Pre-flight stops on an input it can't read cleanly or without exactly
  its columns, and on an override whose `issue_class` isn't `source` or
  `harmonization` or that gives none of `fingerprint`, `rule_id`,
  `table_name`, `column_name` and the group columns
  (`input_unparseable`). It warns on two overrides that give one scope
  two classes (`override_scope_conflict`), and on an override naming a
  rule that isn't registered, or a table or column the dictionary
  doesn't have (`override_unknown_name`); a rule renamed or split since
  is known, as is a column `"*"`, and the delivery's own names in an
  override of `table_unexpected` or `column_unexpected`.

- settings:

  A named list of settings, which come before the rule set's, the
  options' and the built-in values (see
  [groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)).
  Pre-flight reads `lang`, the run language, whose text
  `text_id_fallback` checks; `severity`, severities by rule ID, and
  `fallback_class`, classes by rule ID, whose rule IDs `rule_id_unknown`
  checks; and the settings that name tables and attributes
  (`unknown_codes`, `coordinate_attributes`, `code_set_skip_attributes`
  and the eight settings of `design_geometry_inconsistent`,
  `subplot_label_ambiguous` and `design_duplicate_signature`), whose
  names `setting_unknown_name` and `option_unknown_name` check against
  the specification. It accepts the other settings by name and doesn't
  read them.

- text:

  `NULL`, or a table of report text with the columns `text_id`, `lang`
  and `text`, whose rows stand in for the package's rows of the same
  `text_id` and `lang` when pre-flight checks the registered rules'
  text; pre-flight's own page and table use the package's English text.
  A row that uses a {slot} the package's row of that text lacks stops
  pre-flight (`text_slot_unknown`).

- output_dir:

  `NULL`, or a folder: `metadata/preflight.csv` and
  `reports/preflight.html` are written under it, whether or not
  pre-flight stops. The page's footer names the package version, the
  specification files and, given a rule set with a meta row, that rule
  set.

## Value

The pre-flight table, invisibly: columns `rule_id`, `outcome` (`stop`,
`warn`, `pass`, `not_run`), `file`, `detail`, `n_findings` (the check's
total on each of its rows; 0 on a pass row, `NA` on a `not_run` row),
`not_run_reason` and `source_cell`. A check with findings has one row
per finding.

## Conditions

Any stop row signals an error of class `gpq_preflight_error`, whose
`preflight` element holds the table. Otherwise any warn row signals one
warning of class `gpq_preflight_warning`, with the same element.

## Cells

`source_cell` is where a finding is, when the reader knows: `sheet!B3`
in a workbook, `file:line` in a CSV file, `file:row` for a dictionary
row. A clash's finding gives both sides as `<a>; <b>`, in the order its
detail names them, an unknown side left empty; a side with several cells
lists them with ", ", as in
`lineage!D11, lineage!F11; dictionary.xlsx:59`. A file or sheet name can
itself contain "; " or ", ", so for exact values read the
`source_cell_a` and `source_cell_b` columns of the specification's
`clashes` component.

## Checks

`dd_duplicate_attribute`, `dd_name_missing`, `dd_type_unknown`,
`dd_type_column_ambiguous`, `dd_pk_missing`, `dd_fk_target_missing`,
`code_list_missing`, `code_column_missing`, `code_list_duplicate_code`,
`code_list_blank_row`, `code_list_unreferenced`,
`code_list_empty_column`, `spec_clash_resolved`,
`spec_clash_unresolved`, `datasets_row_missing`,
`spec_encoding_invalid`, `spec_csv_malformed`, `site_id_range_invalid`,
`lineage_spec_unparseable`, `lineage_when_invalid`,
`lineage_name_unknown`, `lineage_spec_row_unflagged`,
`lineage_id_unflagged`, `crosswalk_unreadable`, `dd_format_invalid`,
`key_pair_invalid`, `reach_invalid`, `one_to_many_invalid`; with a rule
set, `rule_set_unknown_column` and `rule_set_duplicate_row`; with a rule
set, a severity setting or a `fallback_class` setting,
`rule_id_unknown`, and with a rule set or a severity setting,
`rule_set_override_invalid`; with a setting that names tables or
attributes, `setting_unknown_name` (given as an argument or in the rule
set) and `option_unknown_name` (set as an option); `text_id_missing`,
`text_id_fallback` and `text_slot_unknown`, on the report text of the
registered rules in English and in the run language and on a caller's
text table; and with run inputs, `input_unparseable`,
`override_scope_conflict` and `override_unknown_name`. No rule set or
setting changes what a check stops or warns on.

## See also

[`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md),
which runs it first, and
[`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md),
which reads the overrides it checks.

## Examples

``` r
example <- function(file) system.file("extdata", "examples", file, package = "groundplotqc")
forest <- gpq_read_spec(
  dictionary = example("forest_dictionary.csv"),
  code_lists = list(
    SPECIES = example("forest_species.csv"), STATUS = example("forest_status.csv")
  ),
  column_map = gpq_column_map(
    table = "TABLE", attribute = "COLUMN", type = "FORMAT", key_type = "KEY",
    reference = "REFERS_TO", lookup = "CODE_LIST", description = "DEFINITION",
    min_value = "MIN_VALUE", max_value = "MAX_VALUE", min_inclusive = "MIN_INCLUSIVE",
    max_inclusive = "MAX_INCLUSIVE", pattern = "PATTERN", max_length = "MAX_LENGTH",
    decimals = "DECIMALS", units = "UNITS"
  ),
  type_map = gpq_type_map(example("forest_types.csv"))
)
results <- gpq_preflight(forest)
table(results$outcome)
#> 
#> not_run    pass 
#>      22      18 

# A dictionary row written twice stops pre-flight.
twice <- data.frame(
  table_name = "t", attribute_name = c("id", "id"), key_type = "PK", data_type = "character"
)
stopped <- tryCatch(gpq_preflight(gpq_read_spec(twice)), gpq_preflight_error = function(e) e)
subset(stopped$preflight, outcome == "stop")
#>                   rule_id outcome      file
#>                    <char>  <char>    <char>
#> 1: dd_duplicate_attribute    stop in memory
#>                                     detail n_findings not_run_reason
#>                                     <char>      <int>         <char>
#> 1: t.id appears 2 times in the dictionary.          1           <NA>
#>    source_cell
#>         <char>
#> 1:        <NA>

# A rule set's row naming a rule no one registered stops pre-flight too.
rules <- list(rules = data.frame(
  rule_id = "no_such_rule", table_name = "*", attribute_name = "*", severity = NA,
  class = NA, enabled = TRUE
))
stopped <- tryCatch(gpq_preflight(forest, rules = rules), gpq_preflight_error = function(e) e)
subset(stopped$preflight, outcome == "stop", c(rule_id, detail))
#>            rule_id
#>             <char>
#> 1: rule_id_unknown
#>                                                                                               detail
#>                                                                                               <char>
#> 1: Row 1 of the rule set's rules (as R counts rows) names rule no_such_rule, which isn't registered.

# An override whose class isn't source or harmonization stops pre-flight too.
overrides <- data.frame(
  fingerprint = NA, rule_id = "raw_na", table_name = "TREES", column_name = NA,
  issue_class = "provider", who = "A. Reviewer", when = "2026-10-22", why = "Checked."
)
stopped <- tryCatch(
  gpq_preflight(forest, inputs = list(overrides = overrides)),
  gpq_preflight_error = function(e) e
)
subset(stopped$preflight, outcome == "stop", c(rule_id, detail))
#>              rule_id
#>               <char>
#> 1: input_unparseable
#>                                                                                                    detail
#>                                                                                                    <char>
#> 1: Row 1 of overrides (as R counts rows) gives the class "provider", which isn't source or harmonization.
```
