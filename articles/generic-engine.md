# Using the engine on your own dataset

groundplotqc checks any set of related tables against a specification
you write. This article runs the checks on a small fish survey: three
tables of stations, hauls and catches. It reads the survey’s
specification, checks its tables as data frames, then as CSV files and
from a database, and writes the results to a run folder.

## The specification

The specification is the survey’s data dictionary, its code lists, its
type map and its missing-value codes, read as the article “Specification
files and configuration” reads it.

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
    reference = "parent", lookup = "codes", description = "notes",
    min_value = "low", max_value = "high", min_inclusive = "low_in", max_inclusive = "high_in",
    pattern = "format", max_length = "width", decimals = "places", units = "unit"
  ),
  type_map = gpq_type_map(example("fish_types.csv")),
  sentinels = gpq_sentinels(
    numeric = c(missing = -99, not_applicable = -88),
    character = c(missing = "?", not_applicable = "~"),
    date = c(missing = "?", not_applicable = "~")
  )
)
```

## The tables as data frames

The tables are a named list, one data frame per table of the dictionary.
This survey has two mistakes: a catch whose weight is missing, written
as `NA` where the specification’s code for a missing value is -99, and a
gear code with a space after it.

``` r

tables <- list(
  stations = data.frame(
    station_id = c("S1", "S2"), station_name = c("Long Lake", "Stony River"),
    water_body = c("LK", "RV"), depth = c(1.5, 2.5),
    first_visit = c("2020-06-01", "2020-06-02")
  ),
  hauls = data.frame(
    haul_id = c("H1", "H2", "H3"), station_id = c("S1", "S1", "S2"),
    gear = c("GN", "TN ", "EF"), minutes = c(30L, 45L, 20L),
    haul_date = c("2020-06-01", "2020-06-01", "2020-06-02")
  ),
  catches = data.frame(
    catch_id = c("C1", "C2", "C3", "C4"), haul_id = c("H1", "H1", "H2", "H3"),
    species = c("WAE", "NOP", "YEP", "LKT"), fish_count = c(3L, 1L, 4L, 2L),
    weight = c(1.2, 0.4, NA, 0.9)
  )
)
results <- gpq_check_conformance(tables, fish, group_cols = "water_body", layers = 0:2)
```

The results summary has one row for each rule, table, column and group.
Each issue names its rule, its record and the value that failed. The
first table counts the summary’s rows by status; the open issues follow,
each with its group column, `water_body`.

``` r

table(results$results$status)
#> 
#>    fail not_run    pass 
#>       2       1     226
open <- results$issues[results$issues$status == "open", ]
open[, c("rule_id", "table_name", "column_name", "record_pk", "value", "water_body")]
#>               rule_id table_name column_name record_pk  value water_body
#>                <char>     <char>      <char>    <char> <char>     <char>
#> 1:             raw_na    catches      weight        C3   <NA>         LK
#> 2: whitespace_padding      hauls        gear        H2    TN          LK
```

One check doesn’t run: `species_unknown_code` looks for the survey’s
codes for an unknown species, and none are given, so it is recorded as
not run, with the reason `no_input`. The codes are given with the
setting `unknown_codes` (see
[`?groundplotqc_options`](https://blaiseratcliffe.github.io/groundplotqc/reference/groundplotqc_options.md)).

## The tables as CSV files

The same tables can be given as paths of CSV files, here written to a
temporary folder. Every column of a CSV file is read as text, as
written, so the checks of how a column is stored don’t run on it; they
are recorded as not run, with the reason `text_input`.

``` r

folder <- tempfile("fish")
dir.create(folder)
paths <- file.path(folder, paste0(names(tables), ".csv"))
names(paths) <- names(tables)
for (name in names(tables)) {
  data.table::fwrite(tables[[name]], paths[[name]])
}
from_csv <- gpq_check_conformance(paths, fish, group_cols = "water_body", layers = 0:2)
text_input <- from_csv$results$not_run_reason %in% "text_input"
from_csv$results[text_input, c("rule_id", "table_name", "column_name", "not_run_reason")]
#>                      rule_id table_name  column_name not_run_reason
#>                       <char>     <char>       <char>         <char>
#>  1:           class_mismatch   stations   station_id     text_input
#>  2:           class_mismatch   stations station_name     text_input
#>  3:           class_mismatch   stations   water_body     text_input
#>  4:           class_mismatch   stations        depth     text_input
#>  5:           class_mismatch   stations  first_visit     text_input
#>  6:           class_mismatch      hauls      haul_id     text_input
#>  7:           class_mismatch      hauls   station_id     text_input
#>  8:           class_mismatch      hauls         gear     text_input
#>  9:           class_mismatch      hauls      minutes     text_input
#> 10:           class_mismatch      hauls    haul_date     text_input
#> 11:           class_mismatch    catches     catch_id     text_input
#> 12:           class_mismatch    catches      haul_id     text_input
#> 13:           class_mismatch    catches      species     text_input
#> 14:           class_mismatch    catches   fish_count     text_input
#> 15:           class_mismatch    catches       weight     text_input
#> 16: integer_stored_as_double   stations        depth     text_input
#> 17: integer_stored_as_double      hauls      minutes     text_input
#> 18: integer_stored_as_double    catches   fish_count     text_input
#> 19: integer_stored_as_double    catches       weight     text_input
```

The run’s metadata records each file by its name and SHA-256, with its
rows and the separator that was found.

``` r

from_csv$metadata$inputs
#>              input         file                                                           sha256                  detail
#>             <char>       <char>                                                           <char>                  <char>
#> 1: tables:stations stations.csv 2624c1e274b91379a059858541673771072af813a7fcab6d15a2a76a56b4b0c4 2 rows; separator comma
#> 2:    tables:hauls    hauls.csv 1e6ddd9a42499a7f0b0f255a076fc44b0d29d59153e1ff7122fe6fa7924cd90e 3 rows; separator comma
#> 3:  tables:catches  catches.csv 23f052b4568fd7dd829c6f71f27cdf7a41566a5fe26a58b01b185b233e511499 4 rows; separator comma
```

## The tables in a database

Tables in a database are read through a DBI connection. Where a table
has another name in the database, `table_map` gives it, such as
`c(hauls = "survey.hauls")` for a table in the schema survey. This part
runs where the packages DBI and RSQLite are installed.

``` r

connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(connection, "stations", tables$stations)
DBI::dbWriteTable(connection, "fish_hauls", tables$hauls)
DBI::dbWriteTable(connection, "fish_catches", tables$catches)
from_db <- gpq_check_conformance(
  connection, fish,
  table_map = c(hauls = "fish_hauls", catches = "fish_catches"),
  group_cols = "water_body", layers = 0:2
)
from_db$metadata$inputs
#>              input                       file sha256                                              detail
#>             <char>                     <char> <char>                                              <char>
#> 1: tables:stations DBI connection, not hashed   <NA>     2 rows; table stations; driver SQLiteConnection
#> 2:    tables:hauls DBI connection, not hashed   <NA>   3 rows; table fish_hauls; driver SQLiteConnection
#> 3:  tables:catches DBI connection, not hashed   <NA> 4 rows; table fish_catches; driver SQLiteConnection
DBI::dbDisconnect(connection)
```

The database’s tables give the same results as the data frames, and the
metadata names each table as the database names it, with the driver’s
class.

## The run folder

[`gpq_write_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_write_results.md)
writes a run’s results to a folder named by its run ID, and
[`gpq_read_results()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_results.md)
reads them back.

``` r

output <- tempfile("runs")
gpq_write_results(results, output)
run_dir <- file.path(output, results$metadata$run$run_id)
list.files(run_dir, recursive = TRUE)
#>  [1] "metadata/clashes.csv"       "metadata/inputs.csv"       
#>  [3] "metadata/preflight.csv"     "metadata/rules.csv"        
#>  [5] "metadata/run.csv"           "metadata/self_test.csv"    
#>  [7] "metadata/settings.csv"      "metadata/spec_manifest.csv"
#>  [9] "tables/corrections.csv"     "tables/issues.csv"         
#> [11] "tables/results.csv"
again <- gpq_read_results(run_dir)
identical(again$issues, results$issues)
#> [1] TRUE
```
