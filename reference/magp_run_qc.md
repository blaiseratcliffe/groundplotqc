# Run MAGPlot's QC on one delivery

Runs MAGPlot 2.0's quality checks on one delivery from start to finish:
pre-flight, the checks of Layers 0 to 4, each issue's provider key and
class, the run's files in `<output_dir>/<run_id>/`, and, by default, a
report and an issues CSV for each contributor.

## Usage

``` r
magp_run_qc(
  tables,
  output_dir,
  overrides = NULL,
  lineage = NULL,
  pipeline_fill = NULL,
  report = TRUE,
  settings = list()
)
```

## Arguments

- tables:

  One delivery's tables:
  [`magp_read_delivery()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_read_delivery.md)'s
  result, the path of the delivery's `.Rdata` file, which the run passes
  to
  [`magp_read_delivery()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_read_delivery.md),
  or a named list of data.frames, one per table. More than one path, or
  a list holding CSV paths, is refused; check CSV files with
  [`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md).

- output_dir:

  The folder that holds run folders, made if it doesn't exist. The run
  writes into `<output_dir>/<run_id>/`.

- overrides:

  `NULL`, or your overrides of issue classes: a data.frame or the path
  of a CSV file, as
  [`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md)
  takes them, whose scope may also name the issues' group columns: the
  run's `magp_contributor_id`, `jurisdiction` and `magp_dataset_id`, and
  the plot's key `magp_plot_id`.

- lineage:

  `NULL`, or the provider's raw values of the cells the pipeline
  changed: a data.frame or the path of a CSV file with the columns
  `table_name`, `record_pk`, `column_name`, `raw_value` and
  `raw_source`. The classes read it as evidence, and each contributor's
  report shows a listed cell's raw value beside its issue among the
  issues to act on, where the record's key is given and, written as
  text, matches no other record's in its table, as
  [`gpq_build_reports()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_build_reports.md)
  does.

- pipeline_fill:

  `NULL`, or the cells the pipeline's missing-value fill wrote: a
  data.frame or the path of a CSV file with the columns `table_name`,
  `record_pk`, `column_name` and `written_value`, which the classes read
  as their evidence `filled`.

- report:

  `TRUE` to build each contributor's report and issues CSV; `FALSE`
  writes the results and run metadata, with no contributor reports or
  issues CSVs.

- settings:

  A named list of settings, which come before the rule set's, the
  options' and the built-in values (see
  [groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)).

## Value

The run's results, invisibly: a `gpq_results` object as
[`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
describes it, each issue with its provider key and class, as written to
the run folder. A run that pre-flight stops returns nothing.

## Details

The run makes its run ID, then loads the tables, then runs pre-flight,
so a run that pre-flight stops still records the contributors its tables
hold. This is the reverse of
[`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md),
which runs pre-flight before it reads any table. Pre-flight checks
MAGPlot's specification, rule set, report text and `settings`, and its
warning reaches you, naming the run's `reports/preflight.html`. When it
stops, the run writes `metadata/run.csv`, with the outcome
`stopped_preflight`, and `metadata/spec_manifest.csv`, as well as
pre-flight's two files, `metadata/preflight.csv` and
`reports/preflight.html`, then stops with an error of class
`gpq_preflight_error`.

Otherwise the run checks Layers 0 to 4, with MAGPlot's group columns
`magp_contributor_id`, `jurisdiction` and `magp_dataset_id` and its
problem unit, the plot (`magp_plot_id` of `magp_plots`). It then gives
each issue its provider key, from its contributor's lineage spec and
Ontario's age-tree flag, and its class, from your `overrides` first,
then from the evidence: the columns MAGPlot's pipeline computes,
`pipeline_fill`, and `lineage` together with MAGPlot's translation
tables' provider codes. It writes the results and the run metadata, and
with `report = TRUE` builds the reports.

Pre-flight checks `overrides`, `lineage` and `pipeline_fill`, and the
run then reads them as
[`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md)
reads them, before any check. The run stops there wherever
[`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md)
would on an input that pre-flight lets pass, such as an `overrides`
column that is an attribute of the specification but not one of the
issues' group columns, or a cell that `lineage` or `pipeline_fill` lists
twice.

In `metadata/run.csv`, `run_start` is when the run started and `run_end`
is set just before the results are written: the time between them covers
loading the tables, pre-flight, the checks, the provider keys and the
classes, not the writing or the reports.

In `metadata/inputs.csv`, each table's row (`tables:<name>`) names the
delivery's `.Rdata` file and its SHA-256, with the table's row count as
its detail. A delivery that holds `magp_meta` adds a row with the input
`magp_meta`, naming the same file and hash; `magp_meta` itself is never
checked. These rows name the file and its hash even for a table you
added to
[`magp_read_delivery()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_read_delivery.md)'s
result, replaced in it or edited in it before the run, so they don't
show which tables you changed. Tables given as a plain named list are
recorded as "in memory", with no hash. Each run input that
classification is given adds a row after these, with the input
`overrides`, `filled` (your `pipeline_fill`), `lineage`, `computed` or
`crosswalk` and the rows classification reads from it as its detail. A
CSV file you gave is recorded by its file name and SHA-256; a data.frame
is recorded as "in memory", with no hash, and so are MAGPlot's own
`computed` and `crosswalk`, which every run has.

The run lets go of the tables once each issue has its provider key,
before the classes and the reports. Their memory is freed only when
nothing else holds them, so give the `.Rdata` file's path to use the
least memory.

`settings` goes to pre-flight, the checks and the classes; its `lang`,
`row_cap`, `example_cap` and `cell_cap` also go to the reports.

A stop after pre-flight passes and before the results are written leaves
pre-flight's two files and no `metadata/run.csv`. A stop while the
reports are built leaves the run's saved results, whose reports
[`gpq_build_reports()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_build_reports.md)
builds again.

## See also

[`magp_read_delivery()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_read_delivery.md),
which loads a delivery;
[`gpq_build_reports()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_build_reports.md),
which builds a saved run's reports again.

## Examples

``` r
# magp_example's tables saved as a delivery saves them, each as its own object.
path <- tempfile(fileext = ".Rdata")
save(list = names(magp_example), envir = list2env(magp_example), file = path)
output <- tempfile("runs")
# MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
results <- magp_run_qc(path, output)
#> Warning: Pre-flight found 32 warning(s); see /tmp/RtmpL0xf7E/runs1e66a8bbe0f/20261103T175534Z_9be761/reports/preflight.html.
run_dir <- file.path(output, results$metadata$run$run_id)
list.files(run_dir, recursive = TRUE)
#>  [1] "metadata/clashes.csv"       "metadata/inputs.csv"       
#>  [3] "metadata/preflight.csv"     "metadata/rules.csv"        
#>  [5] "metadata/run.csv"           "metadata/self_test.csv"    
#>  [7] "metadata/settings.csv"      "metadata/spec_manifest.csv"
#>  [9] "reports/100.html"           "reports/110.html"          
#> [11] "reports/preflight.html"     "tables/100_issues.csv"     
#> [13] "tables/110_issues.csv"      "tables/corrections.csv"    
#> [15] "tables/issues.csv"          "tables/results.csv"        
```
