# Specification files and configuration

groundplotqc checks tables against a specification: a data dictionary,
its code lists and, where they exist, a datasets table, translation
tables and a lineage spec. This article reads the fish survey example
that comes with the package and pre-flights it. It grows with later
versions.

## Reading a specification

The fish survey example names its dictionary columns and types its own
way, so
[`gpq_column_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_column_map.md)
and
[`gpq_type_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_type_map.md)
say what they mean, and
[`gpq_sentinels()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_sentinels.md)
gives the codes the fish tables use for a missing or not-applicable
value.

``` r

library(groundplotqc)
example <- function(file) system.file("extdata", "examples", file, package = "groundplotqc")
fish <- gpq_read_spec(
  dictionary = example("fish_dictionary.csv"),
  code_lists = list(
    water_body = example("fish_water_body.csv"), gear = example("fish_gear.csv"),
    species = example("fish_species.csv")
  ),
  column_map = gpq_column_map(
    table = "table", attribute = "field", type = "kind", key_type = "key",
    reference = "parent", lookup = "codes", description = "notes"
  ),
  type_map = gpq_type_map(example("fish_types.csv")),
  sentinels = gpq_sentinels(
    numeric = c(missing = -99, not_applicable = -88),
    character = c(missing = "?", not_applicable = "~"),
    date = c(missing = "?", not_applicable = "~")
  )
)
names(fish)
#>  [1] "attributes"       "keys"             "code_lists"       "code_list_sheets"
#>  [5] "code_list_map"    "codes"            "non_code_sheets"  "datasets"        
#>  [9] "lineage_spec"     "crosswalks"       "id_bands"         "type_map"        
#> [13] "sentinels"        "clashes"          "read_findings"    "manifest"
fish$keys
#>    table_name attribute_name key_type key_part reference_table
#>        <char>         <char>   <char>    <int>          <char>
#> 1:   stations     station_id       PK        1            <NA>
#> 2:      hauls        haul_id       PK        1            <NA>
#> 3:      hauls     station_id       FK        1        stations
#> 4:    catches       catch_id       PK        1            <NA>
#> 5:    catches        haul_id       FK        1           hauls
#>    reference_attribute
#>                 <char>
#> 1:                <NA>
#> 2:                <NA>
#> 3:          station_id
#> 4:                <NA>
#> 5:             haul_id
```

Every coded attribute resolves to a code list:

``` r

fish$code_list_map
#>    table_name attribute_name lookup source_type source_name code_column
#>        <char>         <char> <char>      <char>      <char>      <char>
#> 1:   stations     water_body      y       sheet  water_body  water_body
#> 2:      hauls           gear      y       sheet        gear        gear
#> 3:    catches        species      y       sheet     species     species
#>    filter_column filter_values   status
#>           <char>        <char>   <char>
#> 1:          <NA>          <NA> resolved
#> 2:          <NA>          <NA> resolved
#> 3:          <NA>          <NA> resolved
```

## Pre-flight

[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
checks the specification before any data is read. A check passes, warns,
stops, or doesn’t run when its input wasn’t given.

``` r

results <- gpq_preflight(fish)
results[, c("rule_id", "outcome", "not_run_reason")]
#>                        rule_id outcome not_run_reason
#>                         <char>  <char>         <char>
#>  1:     dd_duplicate_attribute    pass           <NA>
#>  2:            dd_type_unknown    pass           <NA>
#>  3:   dd_type_column_ambiguous    pass           <NA>
#>  4:              dd_pk_missing    pass           <NA>
#>  5:       dd_fk_target_missing    pass           <NA>
#>  6:          code_list_missing    pass           <NA>
#>  7:        code_column_missing    pass           <NA>
#>  8:   code_list_duplicate_code    pass           <NA>
#>  9:        code_list_blank_row    pass           <NA>
#> 10:     code_list_unreferenced    pass           <NA>
#> 11:     code_list_empty_column    pass           <NA>
#> 12:        spec_clash_resolved not_run       no_input
#> 13:      spec_clash_unresolved not_run       no_input
#> 14:       datasets_row_missing not_run       no_input
#> 15:      spec_encoding_invalid    pass           <NA>
#> 16:         spec_csv_malformed    pass           <NA>
#> 17:      site_id_range_invalid not_run       no_input
#> 18:   lineage_spec_unparseable not_run       no_input
#> 19:       lineage_name_unknown not_run       no_input
#> 20: lineage_spec_row_unflagged not_run       no_input
#> 21:       lineage_id_unflagged not_run       no_input
#> 22:       crosswalk_unreadable not_run       no_input
#>                        rule_id outcome not_run_reason
#>                         <char>  <char>         <char>
```

A defect in the specification is a finding, not a failure to read. Warn
rows are also signalled as one R warning, of class
`gpq_preflight_warning`, and stop rows as an error, of class
`gpq_preflight_error`; each carries the table. The code below mutes the
warning so the table can be shown. Here a code list repeats a code,
which warns:

``` r

repeated <- gpq_read_spec(
  dictionary = example("fish_dictionary.csv"),
  code_lists = list(
    water_body = example("fish_water_body.csv"),
    gear = data.frame(gear = c("GN", "GN", "TN")),
    species = example("fish_species.csv")
  ),
  column_map = gpq_column_map(
    table = "table", attribute = "field", type = "kind", key_type = "key",
    reference = "parent", lookup = "codes", description = "notes"
  ),
  type_map = gpq_type_map(example("fish_types.csv"))
)
warned <- withCallingHandlers(
  gpq_preflight(repeated),
  gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
)
subset(warned, outcome == "warn", c(rule_id, detail))
#>                     rule_id                                                                              detail
#>                      <char>                                                                              <char>
#> 1: code_list_duplicate_code Code "GN" appears 2 times in column gear of sheet gear: rows 1, 2 as R counts rows.
```

## MAGPlot’s specification

The MAGPlot 2.0 layer compiles its specification files into the package
with `data-raw/build_magp_config.R`, which pre-flights them first. If
pre-flight stops, it writes nothing into the package and leaves its
report in `runs/build_magp_config/`.
