# Check tables against a specification: structure, types, missing-value codes, value domains and keys

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
  table_map = NULL,
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

  The tables, one per table of the specification, named as it names
  them: a named list whose elements are data.frames or paths of CSV
  files, mixed as you like, or a named character vector of paths. Every
  column of a CSV file is read as text, so `class_mismatch` and
  `integer_stored_as_double` don't run on its tables. In a CSV file an
  empty cell is a missing value and everything else is read as written,
  so write files with data.table::fwrite(). A cell that isn't valid
  UTF-8 is read as Windows-1252, unless it holds a byte Windows-1252
  doesn't define. A table holding text marked as bytes is refused. Or a
  DBI connection: the specification's tables are read from it under the
  names `table_map` gives, and its other tables are left alone. A column
  stored as integer64 needs the bit64 package: the run loads it, and
  stops if it isn't installed.

- spec:

  A specification from
  [`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
  or
  [`magp_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_spec.md).

- table_map:

  `NULL`, or, when `tables` is a DBI connection, a named character
  vector of the database's names for tables of the specification: names
  the specification's table names, values the database's, such as
  `c(trees = "survey.tree_list")` for the table tree_list in the schema
  survey. A table it doesn't name is read under its own name. A table of
  the specification the database doesn't have is reported as missing
  (`table_missing`).

- group_cols:

  `NULL`, or the grouping columns, report unit first: results and issues
  carry them under these names. A blank group value, or the text `"*"`,
  counts as no value. A record that reaches only such values is grouped
  under `"*"`; in the report-unit column, its records' issues go into no
  provider's report in a run of several units.

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
  `n_checked`, `n_fail`, `n_not_evaluated`, `n_unchecked`,
  `reason_code`, `n_distinct_fail_values`, `n_easy_fix`, `examples`,
  `status` (`pass`, `fail` or `not_run`), `not_run_reason` and
  `message_id`. Each record of a group counts once in `n_checked`,
  `n_not_evaluated` or `n_unchecked`, which add up to the group's
  records: `n_checked` the records the rule checked, `n_fail` of them
  failing; `n_not_evaluated` those it reached but didn't evaluate,
  because an earlier check failed on the same value or the rule can't
  judge it, such as an allowed missing-value code in a foreign key; and
  `n_unchecked` those it never reached, such as every record of a check
  that didn't run. For a rule that checks whole tables or columns, the
  counts are of tables or columns, not records, and `n_unchecked` is NA.

- `issues`: one row per failing or not-evaluated record, or per
  column-level issue: `issue_id`, `run_id`, `run_scope`, `rule_id`,
  `rule_fingerprint`, `stage`, `layer`, `severity`, `reason_code`,
  `table_name`, `column_name`, `record_pk`, `row_number`,
  `pk_positional`, `level` (`record` or `column`), `n_records`, `value`,
  `expected`, `entity_key`, `visit_keys`, `provider_key`,
  `provider_key_status`, the group columns, the problem unit's key
  column, `reaches_other_unit`, `root_issue_id`, `status` (`open` or
  `not_evaluated`), `issue_class`, `class_basis`, `script_file`,
  `script_block`, `attribution_status`, `fingerprint`, `fingerprint_pk`,
  `first_seen_run`, `change` and `change_cause`. The columns a
  conformance run doesn't fill are NA. `reaches_other_unit` is TRUE
  where the issue holds a value read from a record of another report
  unit, or from a record whose report unit is `"*"`. Such a value is a
  group value or the problem unit's key reached through the keys, any
  side of `key_incoherent`'s key pairs on the record (compared or not),
  one of `undeclared_code_set`'s values from records with no unit, or a
  value `design_geometry_inconsistent` compared from a record of another
  unit or with no unit. For a column-level issue it is TRUE when any
  record it covers is. It is FALSE in a run without group columns, and
  isn't part of the fingerprint.

- `corrections`: an empty table.

- `metadata`: the tables of `metadata/` in a run folder.

Write it with
[`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md).

## Details

Pre-flight
([`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md))
runs before any check, on `spec`, `rules` and `settings`; it stops the
run where it would stop, and its warning reaches the caller. The checks
never change `tables`.

A CSV file is read with
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html),
which finds its separator. A file it can't read cleanly, such as one
with a ragged line or a title line shorter than its header, stops the
run before any check, and so does a file that doesn't end with a line
break, which may be cut off. A title line as long as the header, or
above a file of one column, is read as the header, so the checks report
the file's columns as missing. The run's inputs metadata names each file
with its SHA-256, its rows, its separator and how many values were read
as Windows-1252. A table read from a DBI connection is recorded as "DBI
connection, not hashed", with the database's name for it and the
driver's class.

Layers and the rules they can raise:

- Layer 0, structure: `table_missing`, `table_unexpected`,
  `column_missing`, `column_unexpected`, `column_name_mismatch`.

- Layer 1, data types: `class_mismatch`, `coercion_loss`,
  `date_unparseable`, `integer_stored_as_double`, `value_not_finite`.

- Layer 2, missing-value codes: `raw_na`, `blank_string`,
  `sentinel_wrong_type`, `sentinel_rogue_code`, `species_unknown_code`,
  `whitespace_padding`, `na_text_in_id`.

- Layer 3, value domains: `code_not_in_list`, `value_below_min`,
  `value_above_max`, `string_too_long`, `pattern_mismatch`,
  `decimals_exceeded`, `site_id_out_of_range`,
  `coordinate_out_of_bounds`, `undeclared_code_set`.

- Layer 4, keys and relationships: `pk_duplicate`, `pk_missing`,
  `fk_orphan`, `fk_unassigned`, `fk_sentinel_unverified`,
  `fk_not_applicable_mismatch`, `src_id_duplicate`, `key_incoherent`,
  `record_unreachable`, `parent_without_children`,
  `design_geometry_inconsistent`, `subplot_label_ambiguous`,
  `design_duplicate_signature`.

Two of these write no issue at this version: `parent_without_children`
counts, in the results only, the parent records that no child record
names (a parent whose key is missing, blank, a missing-value code or a
value Layer 1 can't read counts as not evaluated), and
`fk_not_applicable_mismatch` writes only `not_run` rows until a version
brings the applicability matrix. `record_unreachable` runs only when
[`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
was given `reach`, and the design checks only when their settings name
their tables and columns; otherwise each writes one `not_run` row
(`no_input`).

Layer 5 has no rules yet. A column whose name differs from its
dictionary attribute's only in letter case or spaces is reported by
`column_name_mismatch` and checked as that attribute. `class_mismatch`,
`integer_stored_as_double`, `undeclared_code_set` and Layer 0's rules
count tables or columns, not records, and their rows have `"*"` in every
group column. `undeclared_code_set` writes one issue per report unit,
each holding only that unit's values; values from records with no unit
go in an issue of their own, under `"*"`. A value that failed Layer 1 is
not evaluated by Layer 2: its issue there has status `not_evaluated` and
names the earlier issue as its root. A value that failed Layer 1 or 2 is
not evaluated by Layer 3's record rules, which leave a missing value, a
blank, a missing-value code and an unknown code to Layer 2. A value that
failed Layer 1 or 2 is not evaluated by Layer 4. Nor is one that failed
Layer 3, unless it is in a primary or foreign key column: such a value
still joins, so Layer 4 checks it as a key. Masking applies only to a
record's own values: `key_incoherent` and the design checks still use
values they read from other records, even ones that failed an earlier
layer in those records. Layer 4 leaves a key that is missing, blank or a
missing-value code the key can't hold to `pk_missing` and
`fk_unassigned`, counts a foreign key's missing-value code that it may
hold as not evaluated in `fk_orphan` (and warns of it in
`fk_sentinel_unverified` where the key is a number), and doesn't
evaluate `record_unreachable` on a record `pk_missing` reports. When a
rule fails on every checked record of a column in a group, and at least
`column_level_min_n` records were checked, the run records one
column-level issue; masked records sharing one root collapse the same
way.

A record's group values are read from its own table or, through the
foreign keys, from a table that holds the column, shortest route first.
Where a key on the way is missing, blank or a missing-value code, a
table or column on the way isn't given, or the value found is blank,
missing, a missing-value code or `"*"`, the next route is tried. A
record no route reaches, or whose key on the way names no parent record,
has `"*"` there. A group value marked latin1 is converted to UTF-8 and
runs; one whose bytes aren't valid UTF-8 stops the run, naming the
column, the table it is read from and its first such row.

Settings (each from the argument, the rule set, the option, or the
built-in value, see
[groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)):
`raw_na_policy`, `rogue_codes`, `unknown_codes`,
`coordinate_attributes`, `code_set_max_values`,
`code_set_skip_attributes`, `code_set_skip_pattern`,
`design_geometry_table`, `design_geometry_by`,
`design_geometry_columns`, `subplot_label_table`,
`subplot_label_columns`, `design_signature_table`,
`design_signature_by`, `design_signature_columns`, `column_level_min_n`,
`example_cap`, `lang` and `severity`. A rule set's `rules` rows give a
rule's severity and turn it off (`enabled`) for a table and attribute:
the most specific row that gives a value wins (table and attribute, then
table and `"*"`, then `"*"` and attribute, then `"*"` and `"*"`). A rule
turned off writes no rows there.

## See also

[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md),
which the run calls first;
[`gpq_decode_lineage()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_decode_lineage.md)
and
[`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md),
which give its issues their provider keys and classes.

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
