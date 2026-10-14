# Check tables against a specification: structure, types and missing-value codes

Runs the conformance stage on a set of related tables: pre-flight of the
specification and configuration, then the checks of the layers asked
for, each table and column read as the specification describes it.
Returns the results summary and one issue per failing record, or one per
column where a rule fails on every record of a column in a group.

## Usage

``` r
gpq_check_conformance(
  tables,
  spec,
  group_cols = NULL,
  problem_unit = NULL,
  rules = NULL,
  layers = 0:5,
  settings = list(),
  run_id = NULL
)
```

## Arguments

- tables:

  A named list of data.frames, one per table, named as the specification
  names its tables. A table holding text marked as bytes is refused.

- spec:

  A specification from
  [`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
  or
  [`magp_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_spec.md).

- group_cols:

  `NULL`, or the grouping columns, report unit first: results and issues
  carry them under these names.

- problem_unit:

  `NULL`, or `list(table, key_col)`: the table and key column of the
  unit problem plots are ranked by; issues carry that key under its
  name.

- rules:

  `NULL`, or a rule set such as
  [`magp_rules()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_rules.md)
  returns.

- layers:

  The layers to run, whole numbers from 0 to 5.

- settings:

  A named list of settings.

- run_id:

  `NULL`, to make one from the run's start, or a run ID such as
  `"20261215T140312Z_3fa9c1"`: the UTC start time and six hex
  characters.

## Value

An object of class `gpq_results`, a list of three tables and the run
metadata:

- `results`: the summary, one row per rule, table, column and group:
  `run_id`, `run_scope`, `rule_id`, `rule_fingerprint`, `stage`,
  `layer`, `severity`, `table_name`, `column_name`, the group columns,
  `n_checked`, `n_fail`, `n_not_evaluated`, `reason_code`,
  `n_distinct_fail_values`, `n_easy_fix`, `examples`, `status` (`pass`,
  `fail` or `not_run`), `not_run_reason` and `message_id`.

- `issues`: one row per failing or not-evaluated record, or per
  column-level issue: `issue_id`, `run_id`, `run_scope`, `rule_id`,
  `rule_fingerprint`, `stage`, `layer`, `severity`, `reason_code`,
  `table_name`, `column_name`, `record_pk`, `row_number`,
  `pk_positional`, `level` (`record` or `column`), `n_records`, `value`,
  `expected`, `entity_key`, `visit_keys`, `provider_key`,
  `provider_key_status`, the group columns, the problem unit's key
  column, `root_issue_id`, `status` (`open` or `not_evaluated`),
  `issue_class`, `class_basis`, `script_file`, `script_block`,
  `attribution_status`, `fingerprint`, `fingerprint_pk`,
  `first_seen_run`, `change` and `change_cause`. The columns a
  conformance run doesn't fill are NA.

- `corrections`: an empty table.

- `metadata`: the tables of `metadata/` in a run folder.

Write it with
[`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md).

## Details

Pre-flight
([`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md))
runs first, on `spec`, `rules` and `settings`; it stops the run where it
would stop, and its warning reaches the caller. The checks never change
`tables`.

Layers and the rules they can raise:

- Layer 0, structure: `table_missing`, `table_unexpected`,
  `column_missing`, `column_unexpected`, `column_name_mismatch`.

- Layer 1, data types: `class_mismatch`, `coercion_loss`,
  `date_unparseable`, `integer_stored_as_double`, `value_not_finite`.

- Layer 2, missing-value codes: `raw_na`, `blank_string`,
  `sentinel_wrong_type`, `sentinel_rogue_code`, `species_unknown_code`,
  `whitespace_padding`, `na_text_in_id`.

Layers 3 to 5 have no rules yet. A column whose name differs from its
dictionary attribute's only in letter case or spaces is reported by
`column_name_mismatch` and checked as that attribute. Layer 0's rules
and `class_mismatch` and `integer_stored_as_double` count tables or
columns, not records, and their rows have `"*"` in every group column,
except where none of a rule's tables or columns could be checked. A
value that failed Layer 1 is not evaluated by Layer 2: its issue there
has status `not_evaluated` and names the earlier issue as its root. When
a rule fails on every checked record of a column in a group, and at
least `column_level_min_n` records were checked, the run records one
column-level issue; masked records sharing one root collapse the same
way.

A record's group values are read from its own table, or from the nearest
table that holds the column, through the foreign keys; a record that
can't reach one, whose keys on the way don't match, or whose value there
is blank or missing, has `"*"` there. A group value marked latin1 is
converted to UTF-8 and runs; one whose bytes aren't valid UTF-8 stops
the run, naming the column, the table it is read from and its first such
row.

Settings (each from the argument, the rule set, the option, or the
built-in value, see
[groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)):
`raw_na_policy`, `rogue_codes`, `unknown_codes`, `column_level_min_n`,
`example_cap`, `lang` and `severity`. A rule set's `rules` rows give a
rule's severity and turn it off (`enabled`) for a table and attribute:
the most specific row that gives a value wins (table and attribute, then
table and `"*"`, then `"*"` and attribute, then `"*"` and `"*"`). A rule
turned off writes no rows there.

## Examples

``` r
# magp_example with one planted defect: a missing value in a column that must hold one.
trees <- data.table::copy(magp_example$magp_trees)
trees$tree_dist[1] <- NA
planted <- magp_example
planted$magp_trees <- trees
# MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
results <- gpq_check_conformance(
  planted, magp_spec(),
  group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
  problem_unit = list("magp_plots", "magp_plot_id"), rules = magp_rules(), layers = 0:2
)
#> Warning: Pre-flight found 32 warning(s); see the returned table.
table(results$results$status)
#> 
#> fail pass 
#>    1 7930 
results$issues[results$issues$status == "open", c("rule_id", "table_name", "column_name")]
#>    rule_id table_name column_name
#>     <char>     <char>      <char>
#> 1:  raw_na magp_trees   tree_dist
```
