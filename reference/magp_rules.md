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

- `meta`, one row: `rule_set_name`, `version` (raised by one in each
  pull request that changes a `rules_*.csv`), `date` (the day of the
  last change) and `spec_version` (the date of the specification files
  it was written for);

- `rules`: `rule_id`, `table_name`, `attribute_name` (`"*"` for all),
  `severity` and `class` (blank to leave them to a less specific row,
  then to the registered value; for `severity`, the `severity` setting
  given as an option comes before the registered value, and given as an
  argument overrides every row) and `enabled`;

- `settings`: `setting`, `value` and `type`.

Each carries the attributes `source_file`, the name of the file it was
read from, `source_lines`, the file line each row starts on, and
`source_sha256`, the SHA-256 of its rows; pre-flight's findings cite the
lines while the rows are unchanged.

## Examples

``` r
rules <- magp_rules()
rules$meta
#>    rule_set_name version       date spec_version
#>           <char>  <char>     <char>       <char>
#> 1:   MAGPlot 2.0       2 2026-10-13     20261005
rules$settings
#>               setting              value      type
#>                <char>             <char>    <char>
#> 1:               lang                 en character
#> 2:        example_cap                  5   integer
#> 3: column_level_min_n                 10   integer
#> 4:      raw_na_policy             always character
#> 5:        rogue_codes -999; UNK; NA; N/A character
#> 6:      unknown_codes   species=UNKN.SPP character
```
