# Class a run's issues

Gives each issue its class, `source` (the provider's to fix),
`harmonization` (made by the pipeline that converted the data) or
`unclassified`, and says what decided it. The issues' `issue_class` and
`class_basis` are set by reference.

## Usage

``` r
gpq_classify_issues(
  issues,
  rules,
  evidence = list(),
  overrides = NULL,
  settings = list()
)
```

## Arguments

- issues:

  The `issues` table of a run's results, from
  [`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md):
  a data.table, changed by reference.

- rules:

  `NULL`, or the rule set the run used, such as
  [`magp_rules()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_rules.md)
  returns: its `rules` rows give rules their classes, and its settings
  rows can give `fallback_class`.

- evidence:

  A named list of evidence tables for issues whose class is `depends`,
  each a data.frame or the path of a CSV file: `computed`, `filled`,
  `lineage` and `crosswalk` (see Details).

- overrides:

  `NULL`, or your overrides: a data.frame or the path of a CSV file with
  the columns `fingerprint`, `rule_id`, `table_name`, `column_name`,
  `issue_class` (`source` or `harmonization`), `who`, `when` and `why`,
  and any of the issues' group columns, each row giving at least one of
  the scope cells.
  [`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
  checks them when given as its input `overrides`.

- settings:

  A named list of settings, which come before the rule set's, the
  options' and the built-in values (see
  [groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)).
  It reads `fallback_class`, classes by rule ID, such as
  `c(some_rule = "source")`. The first of those that gives
  `fallback_class` gives all of it: an argument naming one rule replaces
  every entry of the rule set's row or the option, so the other rules
  have none.

## Value

`issues`, invisibly, with `issue_class` and `class_basis` set.

## Details

Each issue first takes its rule's class, with `class_basis` `"default"`:
the class of the most specific row of the rule set's `rules` that gives
one, else the class the rule is registered with. An information rule's
class is `"none"`.

Your overrides then class any issue, whatever its rule's class. An
override gives a class to the issues whose cells match every scope cell
it gives: `fingerprint` (the issue's `fingerprint` or `fingerprint_pk`),
`rule_id`, `table_name`, `column_name` and any group column; a blank
cell matches every issue. Where several overrides match an issue, the
most specific wins, in this order: a fingerprint; a rule, column and
group; a rule and column; a rule and group; a rule; a column and group;
a column; a group; a table alone. Within each, an override naming the
table comes before one that doesn't, and one group column or several
count alike. Two overrides of the winning kind that give different
classes leave the issue `unclassified`, with `class_basis`
`"override conflict"`; otherwise `class_basis` is `"override"`. An
override naming a rule that was renamed or split applies to its new
rules.

Then an issue whose `reason_code` is `harmonization_artifact` is
`harmonization`, with `class_basis` `"reason code"`, unless an override
classed it or left it in conflict.

Then the evidence settles an issue whose class is still `depends` as
`harmonization`, the first of these tables to settle it giving
`class_basis`:

- `computed`: columns the pipeline computes, with the columns
  `table_name`, `column_name`, `derivation` (`id`, `class`, `formula`,
  `count`, `fill` or `reference`) and one group column of the issues,
  which gives the group value the row is for, `"*"` for every value. An
  issue on such a column in that group, of a record or of the whole
  column, is `"computed"`.

- `filled`: cells the pipeline's missing-value fill wrote (`table_name`,
  `record_pk`, `column_name`, `written_value`). An issue on such a cell
  whose value is the one written is `"filled"`.

- `lineage`, the provider's raw values of the cells the pipeline changed
  (`table_name`, `record_pk`, `column_name`, `raw_value`, `raw_source`),
  with `crosswalk`, each provider code and the code it is translated to
  (`provider_code`, `code`, `column_name` and one group column of the
  issues). Together they settle a record's `code_not_in_list` issue,
  whose raw value is its cell's `raw_value`, or its own value where
  `lineage` doesn't list the cell. Where `crosswalk` has rows for the
  issue's column and group value, a raw value listed there as a
  `provider_code` makes it `"lineage"`: the provider's code was valid. A
  raw value not listed there makes it `"crosswalk"` only where the
  pipeline changed the cell, that is where `lineage` lists the cell with
  a raw value other than its value; otherwise the issue stays unsettled,
  as does one whose column and group value `crosswalk` has no rows for.

A cell listed twice in `filled` or `lineage` stops classification.

An issue whose class is still `depends` takes the `fallback_class`
setting's class for its rule (`class_basis` `"fallback"`), else it is
`unclassified` (`class_basis` `"default"`).

## See also

[`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
for the issues,
[`gpq_decode_lineage()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_decode_lineage.md)
for their provider keys, and
[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
for the checks of the overrides.

## Examples

``` r
example <- function(file) system.file("extdata", "examples", file, package = "groundplotqc")
fish <- gpq_read_spec(
  dictionary = example("fish_dictionary.csv"),
  code_lists = list(
    water_body = example("fish_water_body.csv"), gear = example("fish_gear.csv"),
    species = example("fish_species.csv")
  ),
  column_map = gpq_column_map(
    table = "table", attribute = "field", type = "kind", key_type = "key",
    reference = "parent", lookup = "codes", description = "notes",
    min_value = "low", max_value = "high", min_inclusive = "low_in",
    max_inclusive = "high_in", pattern = "format", max_length = "width",
    decimals = "places", units = "unit"
  ),
  type_map = gpq_type_map(example("fish_types.csv")),
  sentinels = gpq_sentinels(
    numeric = c(missing = -99, not_applicable = -88),
    character = c(missing = "?", not_applicable = "~"),
    date = c(missing = "?", not_applicable = "~")
  )
)
# A haul that fished 900 minutes, past the dictionary's 600, and a species code not in
# the species list.
tables <- list(
  stations = data.frame(
    station_id = "S1", station_name = "Long Lake", water_body = "LK", depth = 1.5,
    first_visit = "2020-06-01"
  ),
  hauls = data.frame(
    haul_id = c("H1", "H2"), station_id = "S1", gear = "GN", minutes = c(30L, 900L),
    haul_date = "2020-06-01"
  ),
  catches = data.frame(
    catch_id = c("C1", "C2"), haul_id = "H1", species = c("WAE", "XYZ"),
    fish_count = 3L, weight = 1.2
  )
)
results <- gpq_check_conformance(tables, fish, group_cols = "water_body", layers = 0:3)
# Your override: the long haul is the provider's to fix.
overrides <- data.frame(
  fingerprint = NA, rule_id = "value_above_max", table_name = "hauls",
  column_name = "minutes", issue_class = "source", who = "A. Reviewer",
  when = "2026-10-22", why = "The gear log gives 45 minutes."
)
gpq_classify_issues(results$issues, rules = NULL, overrides = overrides)
results$issues[, c("rule_id", "column_name", "value", "issue_class", "class_basis")]
#>             rule_id column_name  value  issue_class class_basis
#>              <char>      <char> <char>       <char>      <char>
#> 1: code_not_in_list     species    XYZ unclassified     default
#> 2:  value_above_max     minutes    900       source    override

# A fallback class for the code issues nothing else settles.
gpq_classify_issues(
  results$issues,
  rules = NULL, overrides = overrides,
  settings = list(fallback_class = c(code_not_in_list = "source"))
)
results$issues[, c("rule_id", "column_name", "value", "issue_class", "class_basis")]
#>             rule_id column_name  value issue_class class_basis
#>              <char>      <char> <char>      <char>      <char>
#> 1: code_not_in_list     species    XYZ      source    fallback
#> 2:  value_above_max     minutes    900      source    override
```
