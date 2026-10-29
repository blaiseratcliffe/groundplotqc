# Build the provider reports and issues CSVs of a saved run

Builds one HTML report and one issues CSV for each report unit of a run,
from its results and never from its data, and writes them into the run's
saved folder: `reports/<unit>.html` and `tables/<unit>_issues.csv`. Each
unit's files hold only what that unit may see.

## Usage

``` r
gpq_build_reports(
  results,
  output_dir,
  group_cols = NULL,
  lineage = NULL,
  lang = NULL,
  text = NULL,
  row_cap = NULL,
  example_cap = NULL,
  cell_cap = NULL
)
```

## Arguments

- results:

  A `gpq_results` object, such as
  [`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
  returns, its issues classed by
  [`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md)
  and its run written to `output_dir` by
  [`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md).

- output_dir:

  The folder that holds the run's folder, as given to
  [`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md).

- group_cols:

  `NULL` for the run's own group columns, or the run's report-unit
  column followed by any of its other group columns, which choose the
  breakdowns the reports show. For a run with no group columns, `NULL`
  or empty. The issues CSVs always hold all the run's group columns.

- lineage:

  `NULL`, or the provider's raw values of the cells the pipeline
  changed: a data.frame or the path of a CSV file with the columns
  `table_name`, `record_pk`, `column_name`, `raw_value` and
  `raw_source`, as
  [`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md)
  takes it in `evidence`. A listed cell's raw value shows beside its
  issue among the issues to act on, in its own unit's report only, and
  only where the record's key is given and, written as text, matches no
  other record's in its table.

- lang, row_cap, example_cap, cell_cap:

  The reports' language (`lang`) and caps: the most rows a table shows
  (`row_cap`), examples per check (`example_cap`) and characters of
  delivered text per cell (`cell_cap`). Each `NULL` takes the run's rule
  set, then the option, then the built-in value (see
  [groundplotqc_options](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md));
  a value given here wins.

- text:

  `NULL`, or a table of report text with the columns `text_id`, `lang`
  and `text`, whose rows stand in for the package's rows of the same
  `text_id` and `lang`.

## Value

The files written, invisibly: a character vector of their paths, each
named by its report unit (by the run ID for a run with no group
columns). A run with group columns whose `metadata/run.csv` lists no
unit gets no file, and an empty vector.

## Details

Every issue must have a class before the results are written: run
[`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md),
which needs no evidence, then
[`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md)
to `output_dir`. A run that stopped at pre-flight is refused, since it
has no reports to build. A build first removes the reports and issues
CSVs an earlier build wrote in the run's folder, never
`reports/preflight.html` or the run's own tables. Each file is written
under a temporary name and renamed when complete, so a build that stops
leaves no partly written file.

One report and one CSV is built for each report unit `metadata/run.csv`
lists, even a unit with no issue. An issue goes to its own unit's
report. An issue of a check that checks whole tables or columns, with
`"*"` as its unit and not marked `reaches_other_unit`, goes to every
unit, and so does a results row that counts no records, such as a check
that couldn't run because a table wasn't given. Any other issue or
results row with `"*"` as its unit, a whole-column issue of records
included, goes to no unit's report, and so does an issue the run marks
`reaches_other_unit` (one holding a value read from another unit's
records or from records with no unit). In a run of one unit, all of
these go to that unit's report. A run with no group columns builds one
report, `reports/report.html`, and one CSV, `tables/report_issues.csv`,
holding every issue.

Each provider's file name is its unit value, with every character other
than an ASCII letter, a digit, "-", "\_" or "." written as % and its
UTF-8 bytes in uppercase hex ("%" itself as %25). A leading or trailing
"." is encoded too, and so is the first character of a value whose part
before its first "." is a Windows reserved name such as CON, NUL or COM1
("CON" becomes "%43ON"). So is the first character of a value that,
whatever its case, is the name of one of the run's own files
("preflight", "national_summary", "report", "results", "issues" or
"corrections"), so a provider's file never replaces one of them. Any
value then makes a safe file name that decodes back.

The CSV has one row per issue sent to the unit, open and not evaluated,
codes as stored. Its columns, in order: the group columns and the
problem unit's key column; the table, column, check, severity and class;
the check's text in the run language; the key's parts as `key_<label>`
columns, named from the lineage spec's labels and empty where a key
can't be split back exactly (a raw, fallback or no-lineage-spec key);
then `provider_key` and `provider_key_status`; the record ID, value and
expected value; `proposed_value` and `easy_fix`, empty until proposals
exist; status, change, fingerprint and record count. Rows are sorted as
the report's tables are. It is UTF-8 with a byte-order mark. In the
provider's CSV, a text cell that starts with =, +, @, a tab or a
carriage return, or with - where the cell isn't a number, is written
with an apostrophe before it, so a spreadsheet shows it as text instead
of running it as a formula. `reaches_other_unit` isn't written.

Tables show at most `row_cap` rows and `example_cap` examples per check,
and text longer than `cell_cap` characters is cut and ends with an
ellipsis. The issues CSV has every issue, with its text in full.

## See also

[`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md),
which saves the run first;
[`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md),
which gives the issues their classes;
[`gpq_decode_lineage()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_decode_lineage.md),
which gives them provider keys.

## Examples

``` r
# MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
results <- gpq_check_conformance(
  magp_example, magp_spec(),
  group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
  problem_unit = list("magp_plots", "magp_plot_id"), rules = magp_rules(), layers = 0:2
)
#> Warning: Pre-flight found 32 warning(s); see the returned table.
gpq_classify_issues(results$issues, magp_rules())
output <- tempfile("runs")
gpq_write_results(results, output)
files <- gpq_build_reports(results, output)
basename(files)
#> [1] "100.html"       "100_issues.csv" "110.html"       "110_issues.csv"
```
