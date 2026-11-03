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
#> 1:   MAGPlot 2.0       4 2026-10-30     20261005
rules$settings
#>                      setting
#>                       <char>
#>  1:                     lang
#>  2:              example_cap
#>  3:                  row_cap
#>  4:                 cell_cap
#>  5:       column_level_min_n
#>  6:            raw_na_policy
#>  7:              rogue_codes
#>  8:            unknown_codes
#>  9:    coordinate_attributes
#> 10:      code_set_max_values
#> 11: code_set_skip_attributes
#> 12:    code_set_skip_pattern
#> 13:    design_geometry_table
#> 14:       design_geometry_by
#> 15:  design_geometry_columns
#> 16:      subplot_label_table
#> 17:    subplot_label_columns
#> 18:   design_signature_table
#> 19:      design_signature_by
#> 20: design_signature_columns
#>                      setting
#>                       <char>
#>                                                                                 value
#>                                                                                <char>
#>  1:                                                                                en
#>  2:                                                                                 5
#>  3:                                                                               500
#>  4:                                                                               100
#>  5:                                                                                10
#>  6:                                                                            always
#>  7:                                                                -999; UNK; NA; N/A
#>  8:                                                                  species=UNKN.SPP
#>  9:                                                 latitude; longitude; utm_e; utm_n
#> 10:                                                                                25
#> 11:                     meas_plots; meas_frames; temporal_extent; meas_cycle; subplot
#> 12:                                 _id$|^comments$|name|_date$|^src_|^meas_subplots$
#> 13:                                                                magp_design_frames
#> 14:                                                           magp_design_id; subplot
#> 15:                      plot_area; plot_shape; min_dbh; max_dbh; min_ht; max_ht; baf
#> 16:                                                                     magp_subplots
#> 17:                                                             magp_plot_id; subplot
#> 18:                                                                magp_design_frames
#> 19:                                                                    magp_design_id
#> 20: frame_type; subplot; plot_area; plot_shape; min_dbh; max_dbh; min_ht; max_ht; baf
#>                                                                                 value
#>                                                                                <char>
#>          type
#>        <char>
#>  1: character
#>  2:   integer
#>  3:   integer
#>  4:   integer
#>  5:   integer
#>  6: character
#>  7: character
#>  8: character
#>  9: character
#> 10:   integer
#> 11: character
#> 12: character
#> 13: character
#> 14: character
#> 15: character
#> 16: character
#> 17: character
#> 18: character
#> 19: character
#> 20: character
#>          type
#>        <char>
```
