# Read a MAGPlot 2.0 delivery

Loads a delivered `.Rdata` file and returns its tables as data.tables,
with the file's name and SHA-256 for the run's metadata.

## Usage

``` r
magp_read_delivery(path)
```

## Arguments

- path:

  The path of one `.Rdata` file.

## Value

A named list: the tables, as data.tables, in the data dictionary's
order; any other data.frames, as data.tables, in the file's order; and
`magp_meta`, unchanged, if the file has one. Its attributes are
`source_file`, the file's name without its folder, and `sha256`, the
file's SHA-256 in lower-case hexadecimal.

## Details

A delivery's file holds each of MAGPlot 2.0's 17 tables as an object of
its own. The tables it holds come first, in the data dictionary's order;
a table it lacks is left out, and the checks report it
(`table_missing`). Any other data.frame in the file follows, in the
file's order, for the checks to report (`table_unexpected`).
`magp_meta`, if the file has one, comes last, unchanged. The file's `dd`
and `lkp` are dropped, since the specification comes from
[`magp_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_spec.md),
and so is any other object that isn't a data.frame.

The file is loaded into an environment of its own, so no object in your
session is replaced.

The call stops if `path` isn't one `.Rdata` file that R can load, if the
file holds none of the 17 tables, or if an object named as one of them
isn't a data.frame.

## Examples

``` r
# magp_example's tables saved as a delivery saves them, each as its own object.
path <- tempfile(fileext = ".Rdata")
save(list = names(magp_example), envir = list2env(magp_example), file = path)
tables <- magp_read_delivery(path)
names(tables)
#>  [1] "magp_contributors"  "magp_datasets"      "magp_sites"        
#>  [4] "magp_site_dist"     "magp_site_treat"    "magp_site_visits"  
#>  [7] "magp_plots"         "magp_subplots"      "magp_designs"      
#> [10] "magp_design_frames" "magp_plot_meas"     "magp_subplot_meas" 
#> [13] "magp_trees"         "magp_tree_meas"     "magp_tree_cond"    
#> [16] "magp_tree_age"      "magp_tally_trees"  
attr(tables, "source_file")
#> [1] "file1e6620d92950.Rdata"
attr(tables, "sha256")
#> [1] "628020ca89267a01483cf4a73c39c8074a212bcc39b763ed27653e014a25bcd3"
unlink(path)
```
