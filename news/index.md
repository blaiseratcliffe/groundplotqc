# Changelog

## groundplotqc 0.0.0.9001 (development version)

- First version: the package skeleton, with its help pages, tests, lint
  configuration, continuous integration on Windows, macOS and Linux, and
  the pkgdown site.
- [`gpq_column_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_column_map.md),
  [`gpq_type_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_type_map.md)
  and
  [`gpq_sentinels()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_sentinels.md)
  describe a specification’s dictionary columns, its types and its
  missing-value codes. Two example specifications, a fish survey and
  forest ground plots, come with the package.
- Every specification CSV, read through
  [`gpq_type_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_type_map.md)
  or
  [`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md),
  goes through one reader that records what it finds as findings about
  the file, an invalid byte or a malformed line.
  [`gpq_type_map()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_type_map.md)
  given a file, whose path must end in `.csv`, carries them as the
  attribute `gpq_read_findings`.
- The CSV reader also reports, as findings about the file, a warning
  from
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
  that the package doesn’t recognise, rows that were read short of the
  file’s records after its header, and a quote that `fread()` stops on
  in a file of one column. It reports them whatever the session’s `warn`
  option, so with `options(warn = 2)` a malformed CSV doesn’t stop the
  read. The package needs data.table 1.18.2.1 or later.
- [`gpq_read_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_read_spec.md)
  reads a data dictionary, its code lists, translation tables, datasets
  table and lineage spec into one specification object, recording every
  defect it finds for pre-flight. A blank name in `non_code_sheets` is
  dropped, as a repeated one is.
- [`gpq_preflight()`](https://blaiseratcliffe.github.io/groundplotqc/reference/gpq_preflight.md)
  checks a specification before any data is read: 22 checks that stop or
  warn, with a pre-flight CSV and a self-contained HTML report.
- A first article, “Specification files and configuration”, reads and
  pre-flights the fish survey example.
- [`magp_spec()`](https://blaiseratcliffe.github.io/groundplotqc/reference/magp_spec.md)
  returns MAGPlot 2.0’s specification, compiled from
  20261005_magpv2_DD.xlsx, 20261005_magpv2_Lookup_Tables.xlsx,
  20261005_magpv2_A2.xlsx, 20261005_magpv2_datasets.csv,
  20261005_magpv2_species.csv, 20261005_magpv2_condition.csv,
  20261005_magpv2_treatment_disturbance.csv and
  20261005_magpv2_severity.csv.
