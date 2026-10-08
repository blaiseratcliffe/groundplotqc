# Pre-flight a specification

Checks a specification, never the data, and stops before a run that
would read it wrongly. Given a rule set, it checks the rule set's rows
too, and it checks the settings and report text a run would use. Each
check gives one row per finding, or one pass or not-run row.

## Usage

``` r
gpq_preflight(
  spec,
  rules = NULL,
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

- settings:

  A named list of settings, which come before the rule set's, the
  options' and the built-in values (see
  [groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)):
  `lang`, the run language, whose text `text_id_fallback` checks, and
  `severity`, severities by rule ID.

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

`dd_duplicate_attribute`, `dd_type_unknown`, `dd_type_column_ambiguous`,
`dd_pk_missing`, `dd_fk_target_missing`, `code_list_missing`,
`code_column_missing`, `code_list_duplicate_code`,
`code_list_blank_row`, `code_list_unreferenced`,
`code_list_empty_column`, `spec_clash_resolved`,
`spec_clash_unresolved`, `datasets_row_missing`,
`spec_encoding_invalid`, `spec_csv_malformed`, `site_id_range_invalid`,
`lineage_spec_unparseable`, `lineage_name_unknown`,
`lineage_spec_row_unflagged`, `lineage_id_unflagged`,
`crosswalk_unreadable`; with a rule set, `rule_set_unknown_column`; with
a rule set or a severity setting, `rule_id_unknown` and
`rule_set_override_invalid`; and `text_id_missing`, `text_id_fallback`
and `text_slot_unknown`, on the report text of the registered rules in
English and in the run language and on a caller's text table. No rule
set or setting changes what a check stops or warns on.

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
    reference = "REFERS_TO", lookup = "CODE_LIST", description = "DEFINITION"
  ),
  type_map = gpq_type_map(example("forest_types.csv"))
)
results <- gpq_preflight(forest)
table(results$outcome)
#> 
#> not_run    pass 
#>      12      16 

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
```
