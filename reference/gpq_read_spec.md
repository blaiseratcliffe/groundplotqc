# Read a specification

Reads a data dictionary and its companion inputs into one specification
object, which
[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
checks and the checks of later layers read. Reading never stops on a
defect in the specification: each one is recorded in the `read_findings`
component, and
[`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
decides whether it stops.

It does stop, with an error, on a mistake in the call: an argument in
the wrong form; a file that doesn't exist, or a CSV file that can't be
read, such as one in UTF-16, unless it is a translation table, which is
then a finding; a dictionary without its table or attribute column; a
`precedence` or `lineage_spec` table without exactly its columns; a
`precedence` table with an unknown `winner` or `blank_rule`, or with two
rows for one sheet, key and attribute; a name given in R that isn't
valid UTF-8; a translation table's filter on a column it lacks, or with
no values for an attribute that uses it; or an origin that doesn't fit
its input.

## Usage

``` r
gpq_read_spec(
  dictionary,
  code_lists = NULL,
  non_code_sheets = NULL,
  datasets = NULL,
  lineage_spec = NULL,
  crosswalks = NULL,
  id_pattern = NULL,
  id_bands = NULL,
  precedence = NULL,
  origins = NULL,
  column_map = gpq_column_map(),
  type_map = gpq_type_map(),
  sentinels = gpq_sentinels()
)
```

## Arguments

- dictionary:

  The data dictionary: a data.frame, or the path of a CSV file or of an
  `.xlsx` workbook, read from its first sheet. One row per table and
  attribute.

- code_lists:

  `NULL`, the path of an `.xlsx` workbook (every sheet is a code list or
  a reference table), or a named list of data.frames or CSV paths, named
  by sheet.

- non_code_sheets:

  `NULL`, or the names of sheets that aren't code lists; the
  `code_list_*` checks skip them. A blank name is dropped, as a repeated
  one is.

- datasets:

  `NULL`, or the datasets table (a data.frame or a CSV path), read as
  text.

- lineage_spec:

  `NULL`, or the lineage spec in long form (a data.frame or a CSV path),
  with exactly the columns `contributor_label`, `table_name`,
  `attribute_name`, `spec_type` (`id` or `compiled`), `source_text`,
  `note` and `source_cell`.

- crosswalks:

  `NULL`, or a named list of translation tables: each a data.frame, a
  CSV path, or
  `list(table = , code_col = , filter_col = , filter_values = )`, where
  `filter_values` may be a list named by attribute.

- id_pattern:

  `NULL`, or one regular expression marking ID attributes by name.

- id_bands:

  `NULL`, or
  `list(sheet, label_col, start_col, end_col, labels_from = c(sheet = , column = ), reserved_pattern)`
  naming the code-list sheet of ID bands.

- precedence:

  `NULL`, or a table (data.frame or CSV path) with exactly the columns
  `sheet`, `key_col`, `attribute_name`, `winner` (`datasets` or
  `code_lists`) and `blank_rule` (`yields` or `wins`), saying which
  input wins where the datasets table and a sheet overlap.

- origins:

  `NULL`, or, for inputs given as data.frames that came from files, a
  list named by input, each once (`dictionary`, `datasets`,
  `lineage_spec`, `precedence`, `code_lists:<sheet>`,
  `crosswalks:<name>`) of
  `list(path = , sheet = , rows = , columns = )`: the file, named in
  findings and hashed in the manifest; a workbook's sheet, or `NULL` for
  a CSV file; the file row of the first data row, later rows following
  on, or the file row of every data row, or `NULL` to count rows as R
  counts them; and the file column, by number or letter, of every column
  of the data.frame, or `NULL` for the file's order. Findings in such an
  input then name the file's cells.

- column_map:

  The dictionary's column names, from
  [`gpq_column_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_column_map.md).

- type_map:

  The type map, from
  [`gpq_type_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_type_map.md).
  What was found reading its file joins `read_findings`.

- sentinels:

  The sentinel table, from
  [`gpq_sentinels()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_sentinels.md).

## Value

An object of class `gpq_spec`: a named list of data.tables, the
components `attributes`, `keys`, `code_lists`, `code_list_sheets`,
`code_list_map`, `codes`, `non_code_sheets`, `datasets`, `lineage_spec`,
`crosswalks`, `id_bands`, `type_map`, `sentinels`, `clashes`,
`read_findings` and `manifest`. An input not given leaves its component
empty, with its columns. A blank cell is `NA`; text is kept exactly as
written.

## Components

Each component is a data.table with these columns:

- `attributes`: `table_name`, `attribute_name`, `data_type`, `r_class`,
  `key_type`, `reference_table`, `lookup`, `description`,
  `lineage_flag`, `id_marked`, `source_row`

- `keys`: `table_name`, `attribute_name`, `key_type`, `key_part`,
  `reference_table`, `reference_attribute`

- `code_lists`: `sheet`, `source_row`, `sheet_column`, `value`,
  `source_cell`

- `code_list_sheets`: `sheet`

- `code_list_map`: `table_name`, `attribute_name`, `lookup`,
  `source_type`, `source_name`, `code_column`, `filter_column`,
  `filter_values`, `status`

- `codes`: `table_name`, `attribute_name`, `code`, `source_type`,
  `source_name`, `source_row`

- `non_code_sheets`: `sheet`

- `datasets`: the input's own columns, all text

- `lineage_spec`: `contributor_label`, `table_name`, `attribute_name`,
  `spec_type`, `alternative`, `part_order`, `part_source`, `part_kind`,
  `id_status`, `source_text`, `note`, `source_cell`

- `crosswalks`: `crosswalk`, `source_row`, `crosswalk_column`, `value`,
  `source_cell`

- `id_bands`: `label`, `band_start`, `band_end`, `reserved`,
  `source_row`, `source_cell`

- `type_map`: `data_type`, `r_class`, `date_format`

- `sentinels`: `data_type`, `role`, `value`, `allowed_in_pk`,
  `allowed_in_fk`

- `clashes`: `rule_id`, `kind`, `key_col`, `key_value`, `column_name`,
  `source_a`, `value_a`, `source_b`, `value_b`, `winner`, `basis`,
  `source_cell_a`, `source_cell_b`

- `read_findings`: `rule_id`, `input`, `file`, `detail`, `source_cell`

- `manifest`: `input`, `file`, `file_date`, `sha256`

In `clashes`, `basis` says how a clash was settled: `precedence`, by the
`precedence` input; or `fixed_dictionary_placement`, where a
lineage-spec row names another table for an attribute and the
dictionary's table is used. `source_cell_a` and `source_cell_b` are the
two sides' cells, `NA` where unknown; a side with several cells, such as
every lineage-spec row behind one placement clash, lists them with ", ".

## Examples

``` r
example <- function(file) system.file("extdata", "examples", file, package = "groundplotqc")
fish <- gpq_read_spec(
  dictionary = example("fish_dictionary.csv"),
  code_lists = list(
    water_body = example("fish_water_body.csv"), gear = example("fish_gear.csv"),
    species = example("fish_species.csv")
  ),
  column_map = gpq_column_map(
    table = "table", attribute = "field", type = "kind", key_type = "key",
    reference = "parent", lookup = "codes", description = "notes"
  ),
  type_map = gpq_type_map(example("fish_types.csv")),
  sentinels = gpq_sentinels(
    numeric = c(missing = -99, not_applicable = -88),
    character = c(missing = "?", not_applicable = "~"),
    date = c(missing = "?", not_applicable = "~")
  )
)
fish$keys
#>    table_name attribute_name key_type key_part reference_table
#>        <char>         <char>   <char>    <int>          <char>
#> 1:   stations     station_id       PK        1            <NA>
#> 2:      hauls        haul_id       PK        1            <NA>
#> 3:      hauls     station_id       FK        1        stations
#> 4:    catches       catch_id       PK        1            <NA>
#> 5:    catches        haul_id       FK        1           hauls
#>    reference_attribute
#>                 <char>
#> 1:                <NA>
#> 2:                <NA>
#> 3:          station_id
#> 4:                <NA>
#> 5:             haul_id
subset(fish$codes, attribute_name == "gear")
#>    table_name attribute_name   code source_type source_name source_row
#>        <char>         <char> <char>      <char>      <char>      <int>
#> 1:      hauls           gear     GN       sheet        gear          2
#> 2:      hauls           gear     TN       sheet        gear          3
#> 3:      hauls           gear     EF       sheet        gear          4
```
