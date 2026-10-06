# MAGPlot 2.0's compiled specification

The specification object for MAGPlot 2.0, compiled from the dated
specification files named in its manifest.

## Usage

``` r
magp_spec()
```

## Value

An object of class `gpq_spec`, as
[`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
returns.

## Examples

``` r
spec <- magp_spec()
spec$manifest
#>                                input                                      file
#>                               <char>                                    <char>
#>  1:                       dictionary                   20261005_magpv2_DD.xlsx
#>  2:                       code_lists        20261005_magpv2_Lookup_Tables.xlsx
#>  3:                  non_code_sheets                                 in memory
#>  4:                         datasets              20261005_magpv2_datasets.csv
#>  5:                     lineage_spec                   20261005_magpv2_A2.xlsx
#>  6:             crosswalks:condition             20261005_magpv2_condition.csv
#>  7:              crosswalks:severity              20261005_magpv2_severity.csv
#>  8:               crosswalks:species               20261005_magpv2_species.csv
#>  9: crosswalks:treatment_disturbance 20261005_magpv2_treatment_disturbance.csv
#> 10:                       id_pattern                                 in memory
#> 11:                         id_bands                                 in memory
#> 12:                       precedence              data-raw/magp/precedence.csv
#> 13:            dictionary:exceptions         data-raw/magp/spec_exceptions.csv
#>     file_date                                                           sha256
#>        <char>                                                           <char>
#>  1:  20261005 e3656b48fe7dd3b4f7dd5fdc4c2987d6b1d2dc0c53aee1999357b7bc7261f4fe
#>  2:  20261005 70464946857a6164980deec2da28001cd7ca13565aadba05a8c14632d7ceb71c
#>  3:      <NA>                                                             <NA>
#>  4:  20261005 04f00036aa30315d26aae174a65f7fbeb06a54818c84849ce2b9f70c06224b31
#>  5:  20261005 86bd28ff57baf5371a51900c393e655de25be008ac5753ce456f27e27e006b70
#>  6:  20261005 38ae9975a1becad2eb01122ff5497407f6ec5d22b74b4b4d4faa9e5b075cac7c
#>  7:  20261005 c70841da3a74e8e6f5ea1a2f80734f1a3506c242dc86552ed873c5705cb17675
#>  8:  20261005 8c87871f8d6c9e9a8233eed78879ab33228e6a46b0f2d8079793248ce246c618
#>  9:  20261005 4e9cb223ab103d80e24e99e432347e470f06213fd797c869a2195da3b83c5afe
#> 10:      <NA>                                                             <NA>
#> 11:      <NA>                                                             <NA>
#> 12:      <NA> f3844fe37149405cca45a4d2eb055081d2d582eb8fe705d1ff15c01de87806b3
#> 13:      <NA> 73ba517c7d450d8bd17e0d7630e6080f6bea4ec0e5ff3e034257bd04066e5975
nrow(spec$attributes)
#> [1] 306
```
