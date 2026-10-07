# groundplotqc naming rules

Names that follow this file count as decided. For a case it doesn't cover, ask me
(CLAUDE.md).

## Prefixes (D5.1)

| Prefix | Used for | Example |
|---|---|---|
| `gpq_` | exported engine functions and tools | `gpq_check_conformance()`, `gpq_add_dem_elevation()` |
| `magp_` | exported MAGPlot 2.0 layer functions | `magp_run_qc()`, `magp_spec()` |
| none | internal helpers, plain snake_case, never exported | `html_escape()`, `mask_sentinels()` |
| `fx_` | test fixture builders in `tests/testthat/helper-*.R` (approved, D8.21) | `fx_magp_clean()` |

## Function names

- `prefix_verb_noun()`, lower snake case.
- Engine verbs: `read`, `check`, `lookup`, `propose`, `apply`, `decode`, `classify`, `compare`, `build`, `write`, `list`, `register`, `fit` (calibration, plan section 3.6).
- Tool verbs (D5.1): `add` (append derived columns), `assign` (codes or classes from a reference), `convert` (units or coordinate systems), `displace`.
- MAGPlot-layer verbs: the engine verbs, plus `run` (a whole QC run end to end: `magp_run_qc()`, `magp_run_national()`) (D9.8).
- Constructors for configuration objects are nouns: `gpq_column_map()`, `gpq_type_map()`, `gpq_sentinels()`.

## Argument vocabulary

| Argument | Meaning |
|---|---|
| `tables` | named list of data.tables, names are table names; in the engine also a DBI connection (plan section 15, D8.24) |
| `table_map` | for a DBI connection as `tables`, the database's name for each spec table (plan section 15, D9.23) |
| `data` | one data.table |
| `spec`, `rules`, `settings`, `thresholds` | spec object, rule set, settings list, threshold table or a named list of them (D9.8) |
| `results`, `issues`, `corrections` | result objects or their tables |
| `*_col` / `*_cols` | one column name / several column names (`value_col`, `entity_cols`, `time_cols`, `lat_col`) |
| `*_dir` / `*_path` | a directory / a file (`output_dir`, `run_dir`, `audit_path`) |
| `previous`, `acknowledgements`, `overrides`, `lineage`, `attribution_map` | optional run inputs |
| `group_cols`, `key_col`, `matrix_levels`, `problem_unit` | caller-named grouping, key and level columns (D7.5) |
| `in_place`, `report` | logical switches, named as the thing they turn on |
| `lang` | report language code |
| `*_cap` | display caps (`row_cap`, `example_cap`) |
| `min_n`, `min_support`, `tolerance`, `strategy`, `tie_break` | plausibility and correction settings |

Required arguments come first, starting with the data (so `output_dir` is second where it is required, as in `gpq_build_reports()` and `magp_run_qc()`). Optional arguments follow in the order data, specification, settings, output.

## Columns, rule IDs and codes

- Result and input columns: lower snake; counts `n_*`; identifiers `*_id`; MAGPlot's own attribute names are used unchanged.
- A table's column is `attribute_name` in specification and configuration files and `column_name` in outputs and run inputs; grouping and level columns keep the caller's names (D7.5, plan section 3.5).
- Rule IDs (D5.2): readable lower snake, no layer prefix, unique across the package, never reused. Renames and splits go in `rule_id_map.csv`.
- Reason codes (D5.2): lower snake (`misidentification`, `refinement`, `coarsening`, `harmonization_artifact`, `tag_id_error`, `dead_to_live`, ...); a rule with one reason uses its rule ID (D9.33).
- Severities: `error`, `warning`, `flag`, `info` (`info` approved, D8.12; plan section 9.2). Issue classes: `source`, `harmonization`, `unclassified`. Information rules have the default class "none" in the registry (approved, D8.12) and write results rows only, never issue rows, so "none" never reaches `issue_class` (D9.21). The exception is an `info` reason code inside a rule that writes issues (today `refinement`, whose proposals need an issue row, plan section 9.4): its rows carry class "none" (D9.33). Statuses: `pass`, `fail`, `not_run`; `open`, `not_evaluated`, `acknowledged`.
- Classed conditions: `gpq_<what>_error`, for example `gpq_preflight_error`, `gpq_network_error`, `gpq_missing_package_error`, and `gpq_<what>_warning`, for example `gpq_preflight_warning` (D12.16) (approved, D8.21).

## Options (D5.3)

`groundplotqc.<setting>`, lower snake after the dot, all documented on one help page (`?groundplotqc_options`, approved, D8.21). M1 starts the page with the naming pattern and the resolution order (argument > rule set > option > built-in, plan section 3.5) and no options; each milestone that implements a setting adds its option to the page, M3's settings code first. `R/options.R` holds documentation only: no code sets options when the package loads (D11.4).

## Files

| Kind | Pattern |
|---|---|
| R source | `R/<family>_<topic>.R`, families `spec`, `preflight`, `rules`, `results`, `check`, `fix`, `lineage` (lineage, issue classes, attribution), `report`, `tool`, `magp`, `utils`. Package-level exceptions, named by R convention: `groundplotqc-package.R`, `globals.R`, `options.R`, `data.R`; and `preflight.R`, the pre-flight checks (D9.8) |
| Tests | `tests/testthat/test-<R file stem>.R`; helpers `helper-<topic>.R`. Exceptions: `test-naming.R`, which tests package-wide naming and documentation (Enforcement, below), not one R file; `test-dev_scripts.R`, which tests the scripts in `.github/scripts/` from the source tree (D11.8); `test-data_raw.R`, which tests the scripts in `data-raw/` from the source tree (D12.23). Fixtures in `tests/testthat/fixtures/<name>.<ext>`, lower snake; a binary fixture, or any fixture produced by code, has a generating script `tests/testthat/fixtures/make_<name>.R`, small hand-written text fixtures needing none; `.Rbuildignore` excludes only the scripts (`^tests/testthat/fixtures/make_.*\.R$`), never the fixtures, which ship for R CMD check's tests (D12.27) |
| data-raw | `build_<thing>.R`; the folder `data-raw/spec_edits/` holds the edit list of a dated spec set, `edits_YYYYMMDD.R`, and the scripts and record that match neither pattern, `patch_workbooks.R`, `verify_spec_edits.R`, `edit_csv_cell.R` and `README.md` (approved, D12.73 (10), D12.76 (2)) |
| Hand-kept configuration | `data-raw/magp/<name>.csv`, lower snake; read by the build, never written by it (D12.15) |
| Development scripts | `.github/scripts/check_<thing>.R`, run by CI with Rscript, outside the package build (D11.8, D11.19) |
| Bundled configuration | `inst/extdata/<group>/<name>.csv`, lower snake; groups `magp` (MAGPlot configuration), `text` (report text), `examples` (tool, help-page and vignette inputs, the toy spec among them), `rules` (package-wide rule files such as `rule_id_map.csv`) (groups approved, D8.5) |
| Spec files | `spec/YYYYMMDD_magpv2_<name>.<ext>` |
| Run folders | `<output_dir>/<run_id>/` (D4.16) |
| Agents and skills | `.claude/agents/<kebab-name>.md`, `.claude/skills/<kebab-name>/SKILL.md` |

## Abbreviations

Allowed: `id`, `pk`, `fk`, `dd`, `qc`, `dt` (internal variables only), `n`, `html`, `csv`, `utm`, `dem`, `crs`, `rd` (R's Rd help files) and `db` (as in `tools::Rd_db()`) (D11.12), `dev`, `env` and `args` (D11.19); the short forms approved names already use: `spec`, `lang`, `lat`, `lon`, `latlon`, `hd`, `min`, `max`, `abs`, `rel`, `dir`, `col`, `cols`, `fun`, `coords`, `meta`, `stat`, `config`, `info`, `utils` (D9.8); `gpq` and `magp` (the prefixes, D5.1), `r` (R's class, `r_class`), `a` and `b` (a clash's two sides, D12.13), `sha256` (the hash's name, `tools::sha256sum()`) and `meas` (as MAGPlot's DD spells it, and the forest toy's `forest_tree_meas.csv`) (D12.23); `utf8`, `css`, `xlsx` (format names) and `na` (R's missing value) (D12.32); and MAGPlot attribute names as the DD spells them. Everything else is spelled out.

M2's naming test lists any other short form found in an approved name, for my approval: it splits every export, export argument, component name, component and input column, `preflight.csv` column, and `R/` and `inst/extdata/` file name on "_" and "-" and checks each token against this list and the "Approved words" section below, both read from this file; the component names come from the schema, so the compiled files' names are checked as the schema gives them (D12.32); an unknown token fails, and the implementer asks me whether it is a word or a short form, which then joins the matching list (D12.18). The list governs the names of functions, arguments, columns and files; local variables inside a function are not covered (D11.19).

Tokens: `id`, `pk`, `fk`, `dd`, `qc`, `dt`, `n`, `html`, `csv`, `utm`, `dem`, `crs`, `rd`, `db`, `dev`, `env`, `args`, `spec`, `lang`, `lat`, `lon`, `latlon`, `hd`, `min`, `max`, `abs`, `rel`, `dir`, `col`, `cols`, `fun`, `coords`, `meta`, `stat`, `config`, `info`, `utils`, `gpq`, `magp`, `r`, `a`, `b`, `sha256`, `meas`, `utf8`, `css`, `xlsx`, `na`

## Approved words

Whole words used in approved names (D12.18). The naming test splits every export, export argument, component name (from the schema, so the compiled files are covered as the schema gives them), component and input column, `preflight.csv` column, and file name under `R/` and `inst/extdata/` on "_" and "-", and fails on a token in neither list (D12.32); a new word joins this line with my approval.

Tokens: `allowed`, `alternative`, `attribute`, `attributes`, `band`, `bands`, `basis`, `blank`, `body`, `buffers`, `cell`, `character`, `clashes`, `class`, `code`, `codes`, `column`, `compiled`, `contributor`, `crossfield`, `crosswalk`, `crosswalks`, `data`, `datasets`, `date`, `defects`, `description`, `detail`, `dictionary`, `enabled`, `end`, `engine`, `file`, `filter`, `findings`, `fish`, `flag`, `forest`, `format`, `from`, `gear`, `globals`, `groundplotqc`, `in`, `input`, `key`, `keys`, `kind`, `label`, `labels`, `lineage`, `list`, `lists`, `lookup`, `manifest`, `map`, `marked`, `name`, `non`, `not`, `note`, `numeric`, `options`, `order`, `origins`, `outcome`, `output`, `package`, `part`, `pattern`, `plots`, `precedence`, `preflight`, `read`, `reason`, `reference`, `registry`, `report`, `reserved`, `role`, `row`, `rule`, `rules`, `run`, `sentinels`, `set`, `setting`, `settings`, `severity`, `sheet`, `sheets`, `source`, `species`, `start`, `status`, `strategies`, `strings`, `table`, `text`, `tolerances`, `tree`, `trees`, `type`, `types`, `value`, `values`, `version`, `water`, `winner`

## Enforcement (D5.7, D5.8)

- `.lintr`: tidyverse style, line length 100, `object_name_linter` snake_case, and a custom check that exports carry the `gpq_` or `magp_` prefix (D5.7), a linter defined inside `.lintr`'s `linters:` field that flags a function under a roxygen `@export` tag whose name lacks the prefix (D11.4), skipping a dotted name, which is an S3 method since function names are snake_case (D11.7); `object_usage_linter` off (R CMD check covers it). NSE column names go in `R/globals.R` via `utils::globalVariables()`.
- `tests/testthat/test-naming.R` (D5.8: a test checks exported names and documentation): every export matches `^(gpq|magp)_[a-z0-9_]+$`, every export has a help page with an example, every rule ID in the registry is unique lower snake and has report text, and every reason code the registry lists is lower snake and has report text (D9.33); every token of an export, export argument, component name, component or input column, `preflight.csv` column, and `R/` or `inst/extdata/` file name, split on "_" and "-", is in Abbreviations or Approved words, the component names taken from the schema (D12.18, D12.32); a help page whose only example is in `\dontrun{}` or only comments counts as having none (D11.12, D11.19, D12.41); internal function names are the reviewers' to check (D12.32).
- The lint CI job runs lintr and fails on any lint.
- The datatable-reviewer and spec-conformance reviewer agents check new names against NAMING.md.
