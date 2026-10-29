# Decode the provider's keys of a run's issues

Shows each record-level issue in the provider's own terms: the record's
source ID (`src_*_id`), read from `tables` at the issue's row, is split
into the parts its contributor's lineage spec expects, and each part is
labelled with the provider's table and column. The issues'
`provider_key`, `provider_key_status` and `fingerprint` are set by
reference.

## Usage

``` r
gpq_decode_lineage(
  issues,
  tables,
  spec,
  key_col,
  table_map = NULL,
  flags = NULL
)
```

## Arguments

- issues:

  The `issues` table of a run's results, from
  [`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md):
  a data.table, changed by reference.

- tables:

  The tables the issues came from, as
  [`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
  takes them. Decoding stops if an issue's row isn't in its table or
  holds another primary key there.

- spec:

  The specification the run used, from
  [`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
  or
  [`magp_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_spec.md).

- key_col:

  The issues column holding each record's contributor, such as a group
  column of the run.

- table_map:

  `NULL`, or, when `tables` is a DBI connection, the database's names
  for the specification's tables, as
  [`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
  takes it.

- flags:

  `NULL`, or a list named by table of data.frames, each holding the
  table's primary-key columns and one or more flag columns, which
  conditions read as `flag.<name>`. A record whose key the data.frame
  doesn't give has no flag there.

## Value

`issues`, invisibly, with `provider_key`, `provider_key_status` and
`fingerprint` set.

## Details

A record's contributor comes from the issues' `key_col` value: where
`key_col` is the last attribute of the site-ID bands' key chain, the
label of that value in the bands' labels sheet, else the value itself.
The ID decoded is the record's table's first ID attribute that the
lineage spec gives an entry.

`provider_key_status`:

- `decoded`: the ID split into exactly the parts the entry expects, each
  written as `table.column=value`, or for a part the pipeline derives,
  its attribute name and value, joined by "; ". An ID of one part isn't
  split.

- `partial`: as `decoded`, but a part is blank or a missing-value code.

- `raw`: the ID as delivered: it doesn't split into the expected parts
  (a value holding the separator), its entry has labelled alternatives,
  there is no separator, or one of its parts or labels holds "; " or
  "=". The last case keeps every `decoded` or `partial` key splitting
  back into its parts exactly.

- `no_lineage_spec`: the ID as delivered: the record's contributor has
  no entry for it.

- `fallback_pk`: no key: a column-level issue, a table with no ID to
  decode, an ID that is missing, blank or a missing-value code, or an
  entry that says the contributor has no ID.

A part's label can depend on the record: the lineage spec's `when`
conditions are tried in order before the entry's own part. A condition
reads a column of the record, a parent's column through a chain of
foreign keys joined by " \> ", or a flag given in `flags` as
`flag.<name>`.

The fingerprint becomes `rule_id|table_name|column_name|<provider_key>`
where a key is given; where another record of the table holds the same
key, and where there is no key, it stays the `pk:` form.

## See also

[`gpq_check_conformance()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_check_conformance.md)
for the issues, and
[`gpq_classify_issues()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_classify_issues.md),
which classes them.

## Examples

``` r
# One table of plots whose source ID is the provider's site and plot joined by "_".
dictionary <- data.frame(
  table_name = "plots",
  attribute_name = c("plot_id", "contributor", "src_plot_id", "slope"),
  key_type = c("PK", ".", ".", "."),
  data_type = c("character", "character", "character", "numeric"),
  min_value = c(NA, NA, NA, "0")
)
spec <- gpq_read_spec(
  dictionary,
  lineage_spec = data.frame(
    contributor_label = "north", table_name = "plots", attribute_name = "src_plot_id",
    spec_type = "id", source_text = "survey.site + survey.plot", note = NA,
    source_cell = NA
  ),
  lineage_separator = "_",
  id_pattern = "^src_.*_id$",
  sentinels = gpq_sentinels(numeric = c(missing = -1), character = c(missing = "X"))
)
plots <- data.frame(
  plot_id = c("p1", "p2", "p3"), contributor = "north",
  src_plot_id = c("41_2", "41_X", "41_2_b"), slope = c(-5, -6, -7)
)
results <- gpq_check_conformance(
  list(plots = plots), spec,
  group_cols = "contributor", layers = 3
)
gpq_decode_lineage(results$issues, list(plots = plots), spec, key_col = "contributor")
results$issues[, c("record_pk", "provider_key", "provider_key_status")]
#>    record_pk                  provider_key provider_key_status
#>       <char>                        <char>              <char>
#> 1:        p1 survey.site=41; survey.plot=2             decoded
#> 2:        p2 survey.site=41; survey.plot=X             partial
#> 3:        p3                        41_2_b                 raw
```
