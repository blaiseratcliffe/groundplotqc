# Map dictionary types to R classes

The type map says which R class each type of the data dictionary
expects, and which types hold dates.

## Usage

``` r
gpq_type_map(map = NULL)
```

## Arguments

- map:

  `NULL` for the four built-in rows, or a data.frame or the path of a
  CSV file, ending in `.csv`, with columns `data_type`, `r_class` and
  `date_format`, which replaces them.

## Value

A data.table with columns `data_type` (the dictionary's type name),
`r_class` (`"character"`, `"integer"` or `"double"`) and `date_format`
(a [`strptime()`](https://rdrr.io/r/base/strptime.html) format that
non-sentinel values must parse with, or `NA`). Built in: `character`,
`integer`, `numeric` (double) and `date` (character, `"%Y-%m-%d"`). A
map read from a file carries what was found reading it, an invalid byte
or a malformed line, as its attribute `gpq_read_findings`, which
[`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
adds to its `read_findings`. Nothing is signalled here:
[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
reports them, and `attr(map, "gpq_read_findings")` shows them.

## Examples

``` r
gpq_type_map()
#>    data_type   r_class date_format
#>       <char>    <char>      <char>
#> 1: character character        <NA>
#> 2:   integer   integer        <NA>
#> 3:   numeric    double        <NA>
#> 4:      date character    %Y-%m-%d
path <- system.file("extdata", "examples", "fish_types.csv", package = "groundplotqc")
fish <- gpq_type_map(path)
attr(fish, "gpq_read_findings")
#> Empty data.table (0 rows and 5 cols): rule_id,input,file,detail,source_cell
```
