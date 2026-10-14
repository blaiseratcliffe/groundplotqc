# Write a run's results to its folder

Writes a `gpq_results` object to `<output_dir>/<run_id>/`: the results
summary, issues and corrections to `tables/`, and the run metadata to
`metadata/`, one CSV file for each table: `results.csv`, `issues.csv`
and `corrections.csv`, and the eight metadata tables.

## Usage

``` r
gpq_write_results(results, output_dir)
```

## Arguments

- results:

  A `gpq_results` object, such as
  [`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
  returns.

- output_dir:

  The folder that holds run folders, made if it doesn't exist.

## Value

The run folder's path, invisibly.

## Details

Every file has LF line endings, every text field quoted and a missing
value an empty field. Text is written as UTF-8, except a value whose
bytes aren't valid UTF-8, which is written as given. So
[`gpq_read_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_results.md)
reads back the same object. A run's files are written once. Before
writing anything the call stops when the run folder already holds a file
other than that run's own pre-flight files, which
[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
writes: `reports/preflight.html` and `metadata/preflight.csv`. The
second must hold the results' pre-flight table byte for byte, and is
then left as it is.

## Examples

``` r
# MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
results <- gpq_check_conformance(
  magp_example, magp_spec(),
  rules = magp_rules(), layers = 0:2
)
#> Warning: Pre-flight found 32 warning(s); see the returned table.
run_dir <- gpq_write_results(results, tempfile("runs"))
list.files(run_dir, recursive = TRUE)
#>  [1] "metadata/clashes.csv"       "metadata/inputs.csv"       
#>  [3] "metadata/preflight.csv"     "metadata/rules.csv"        
#>  [5] "metadata/run.csv"           "metadata/self_test.csv"    
#>  [7] "metadata/settings.csv"      "metadata/spec_manifest.csv"
#>  [9] "tables/corrections.csv"     "tables/issues.csv"         
#> [11] "tables/results.csv"        
```
