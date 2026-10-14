# Read a run's results back from its folder

Reads the files
[`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md)
wrote back into a `gpq_results` object, each column with its class.

## Usage

``` r
gpq_read_results(run_dir)
```

## Arguments

- run_dir:

  A run folder, as
  [`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md)
  returns it.

## Value

An object of class `gpq_results`, the tables and metadata
[`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
describes.

## Details

The object read is identical to the one written. A run that stopped at
pre-flight has no results to read, and the call stops; so does a missing
file, or one that doesn't read cleanly, naming the file.

## Examples

``` r
# MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
results <- gpq_check_conformance(
  magp_example, magp_spec(),
  rules = magp_rules(), layers = 0:2
)
#> Warning: Pre-flight found 32 warning(s); see the returned table.
run_dir <- gpq_write_results(results, tempfile("runs"))
identical(gpq_read_results(run_dir), results)
#> [1] TRUE
```
