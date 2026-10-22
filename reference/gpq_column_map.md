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
  lineage_flag = NULL,
  min_value = "min_value",
  max_value = "max_value",
  min_inclusive = "min_inclusive",
  max_inclusive = "max_inclusive",
  pattern = "pattern",
  max_length = "max_length",
  decimals = "decimals",
  units = "units"
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

- min_value, max_value, min_inclusive, max_inclusive, pattern,
  max_length, decimals, units:

  One column name each: the lowest and highest value an attribute
  allows, whether each bound is itself allowed (TRUE or FALSE, in any
  case; a blank where its bound is given counts as TRUE), the regular
  expression its values must match, the most characters they may have,
  the most decimal places they may have, and their units. A bound,
  pattern, length or number of decimal places that the dictionary gives
  no attribute, its column missing or blank in every row, leaves the
  rules that read it unrun: each writes one `not_run` row. A missing
  flag column counts as TRUE.

## Value

A list of class `gpq_column_map`, with elements `table`, `attribute`,
`type`, `key_type`, `reference`, `lookup`, `description`,
`lineage_flag`, `min_value`, `max_value`, `min_inclusive`,
`max_inclusive`, `pattern`, `max_length`, `decimals` and `units`.

## Examples

``` r
# The fish survey example names its dictionary columns its own way.
fish_map <- gpq_column_map(
  table = "table", attribute = "field", type = "kind", key_type = "key",
  reference = "parent", lookup = "codes", description = "notes",
  min_value = "low", max_value = "high", min_inclusive = "low_in",
  max_inclusive = "high_in", pattern = "format", max_length = "width",
  decimals = "places", units = "unit"
)
fish_map$attribute
#> [1] "field"
```
