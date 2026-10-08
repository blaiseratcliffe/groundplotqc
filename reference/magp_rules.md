# MAGPlot 2.0's rule set

The rule set that configures the engine for MAGPlot 2.0. Its `rules`
rows say where and how registered rules apply, and its `settings` rows
give MAGPlot's settings, which take the rule-set place in the settings
precedence (see
[groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)).

## Usage

``` r
magp_rules()
```

## Value

A named list of data.tables, in this order:

- `meta`, one row: `rule_set_name`, `version` (raised by one at each
  change), `date` (of the last change) and `spec_version` (the date of
  the specification files it was written for);

- `rules`: `rule_id`, `table_name`, `attribute_name` (`"*"` for all),
  `severity` and `class` (blank for the rule's default) and `enabled`;

- `settings`: `setting`, `value` and `type`.

Each carries the attributes `source_file`, the name of the file it was
read from, and `source_lines`, the file line each row starts on, which
pre-flight's findings cite.

## Examples

``` r
rules <- magp_rules()
rules$meta
#>    rule_set_name version       date spec_version
#>           <char>  <char>     <char>       <char>
#> 1:   MAGPlot 2.0       1 2026-10-07     20261005
rules$settings
#>    setting  value      type
#>     <char> <char>    <char>
#> 1:    lang     en character
```
