# MAGPlot 2.0 example tables

A small synthetic example of MAGPlot 2.0's 17 tables, built from
[`magp_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_spec.md):
every table and column of the data dictionary, each column of the R
class its type maps to, and made-up values that come from no delivery.
The help pages' examples use it.

## Usage

``` r
magp_example
```

## Format

A named list of 17 data.tables, one per dictionary table, in the
dictionary's order (`magp_contributors` to `magp_tally_trees`), each
with the dictionary's columns in its order. It has two contributors,
each with two datasets; every key points to a row of its parent table,
and every coded column holds a code from its list. The list and each
table carry the attribute `gpq_fixture = TRUE`, which marks synthetic
data. There is no `magp_meta` table.

Changing a table by reference (`:=` or
[`data.table::set()`](https://rdrr.io/pkg/data.table/man/assign.html))
can change the example for the rest of the session; take a copy with
[`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html)
first.

## Source

Built by `data-raw/build_magp_example.R` in the package's source
repository.

## Examples

``` r
names(magp_example)
#>  [1] "magp_contributors"  "magp_datasets"      "magp_sites"        
#>  [4] "magp_site_dist"     "magp_site_treat"    "magp_site_visits"  
#>  [7] "magp_plots"         "magp_subplots"      "magp_designs"      
#> [10] "magp_design_frames" "magp_plot_meas"     "magp_subplot_meas" 
#> [13] "magp_trees"         "magp_tree_meas"     "magp_tree_cond"    
#> [16] "magp_tree_age"      "magp_tally_trees"  
magp_example$magp_plots
#>      magp_plot_id src_plot_id magp_site_id  plot plot_elevation plot_slope
#>            <char>      <char>        <int> <int>          <num>      <num>
#>  1:  magp_plots_1          s1       350002     0            1.5        1.5
#>  2:  magp_plots_2          s2       350002     1            2.5        2.5
#>  3:  magp_plots_3          s3       350003     2            3.5        3.5
#>  4:  magp_plots_4          s4       350003     3            4.5        4.5
#>  5:  magp_plots_5          s5       350004     4            5.5        5.5
#>  6:  magp_plots_6          s6       350004     5            6.5        6.5
#>  7:  magp_plots_7          s7       350005     0            7.5        7.5
#>  8:  magp_plots_8          s8       350005     1            8.5        8.5
#>  9:  magp_plots_9          s9       450006     2            9.5        9.5
#> 10: magp_plots_10         s10       450006     3           10.5       10.5
#> 11: magp_plots_11         s11       450007     4           11.5       11.5
#> 12: magp_plots_12         s12       450007     5           12.5       12.5
#> 13: magp_plots_13         s13       450008     0           13.5       13.5
#> 14: magp_plots_14         s14       450008     1           14.5       14.5
#> 15: magp_plots_15         s15       450009     2           15.5       15.5
#> 16: magp_plots_16         s16       450009     3           16.5       16.5
#>     plot_aspect slope_class aspect_class design_change comments
#>           <num>      <char>       <char>        <char>   <char>
#>  1:         1.5           A            N             Y       t1
#>  2:         2.5           B           NE             N       t2
#>  3:         3.5           C            E             Y       t3
#>  4:         4.5           D           SE             N       t4
#>  5:         5.5           E            S             Y       t5
#>  6:         6.5           F           SW             N       t6
#>  7:         7.5           A            W             Y       t7
#>  8:         8.5           B           NW             N       t8
#>  9:         9.5           C            N             Y       t9
#> 10:        10.5           D           NE             N      t10
#> 11:        11.5           E            E             Y      t11
#> 12:        12.5           F           SE             N      t12
#> 13:        13.5           A            S             Y      t13
#> 14:        14.5           B           SW             N      t14
#> 15:        15.5           C            W             Y      t15
#> 16:        16.5           D           NW             N      t16
# A copy to change, leaving the example as it is.
plots <- data.table::copy(magp_example$magp_plots)
```
