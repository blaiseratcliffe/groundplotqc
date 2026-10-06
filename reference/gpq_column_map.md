# Name the dictionary's columns

Tells
[`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
which column of the data dictionary holds each piece of information.
Each default is the column name shown in the usage above.

## Usage

``` r
gpq_column_map(
  table = "table_name",
  attribute = "attribute_name",
  type = c("data_type", "datatype"),
  key_type = "key_type",
  reference = "reference_table",
  lookup = "lookup_table",
  description = "description",
  lineage_flag = NULL
)
```

## Arguments

- table, attribute, key_type, reference, lookup, description:

  One column name each: the table, the attribute, the key type (`"PK"`
  or `"FK"`, in any case), the table a foreign key refers to, the code
  list (a sheet or translation table by name, or `"Y"`, in any case, for
  the sheet named after the attribute), and the description.

- type:

  One or more names the type column may have. Exactly one of them must
  be in the dictionary; otherwise pre-flight stops with
  `dd_type_column_ambiguous`.

- lineage_flag:

  `NULL`, or `c(column = , value = )`: the dictionary column, and the
  value in it, that mark the attributes needing a row in the lineage
  spec.

## Value

A list of class `gpq_column_map`, with elements `table`, `attribute`,
`type`, `key_type`, `reference`, `lookup`, `description` and
`lineage_flag`.

## Examples

``` r
# The fish survey example names its dictionary columns its own way.
fish_map <- gpq_column_map(
  table = "table", attribute = "field", type = "kind", key_type = "key",
  reference = "parent", lookup = "codes", description = "notes"
)
fish_map$attribute
#> [1] "field"
```
