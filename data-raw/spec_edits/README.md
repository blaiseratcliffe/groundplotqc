# Spec edits

This folder is the record of Y8's edits to the specification set, which turned the 20260925
files into the 20261005 ones (M2, task 18), and the way to re-apply the two deliberate edits
to the translation-table copies after a refresh from their masters (plan 24.1 #46).

The scripts are tools for whoever runs `update-spec`. They are not part of the package
(`data-raw/` is build-ignored), and they take every folder as an argument: no path to a
machine is written in them. They never write into `spec/`; give them an output folder
outside the tree, or a temporary one. `zip` (patching a workbook's package) and `xml2` (the
well-formedness check) are used here only, never in `R/` or `DESCRIPTION` (D12.73 (10)).
`readxl` and `data.table` are used as elsewhere.

## The scripts

Each prints its usage line when run with no arguments.

| Script | What it does | Decision |
|---|---|---|
| `edits_20261005.R` | The edit list and the names of the files, as data. The three scripts below `source()` it from their own folder. Each cell edit names a guard: cells that must hold the expected text first, so a wrong row stops the run. | The checklist `plans/M2_spec_fixes.md` and D12.72 (5) to (21) |
| `patch_workbooks.R` | Writes all eight 20261005 files into a new folder: the DD, lookup and A2 workbooks (copies of the 20260925 packages with only the touched sheets' XML changed), the datasets CSV, and the four translation tables converted to UTF-8 with the species copy's `comments` column added. It refuses a folder that exists. | D12.72 (1), (18) for the workbooks and the conversion; (13), (21) for the species `comments` column |
| `edit_csv_cell.R` | Changes CLR's `treat_vs_dist` from T to TD in the treatment/disturbance copy, keeping every other byte, and writes the result to a second folder. It refuses to overwrite a file. | D12.73 (5a) |
| `verify_spec_edits.R` | Checks the eight files in a folder against the 20260925 baseline and the edit list, by its own method: every cell of every sheet, the package parts, the CSV encodings and each CSV's difference from its source. Prints PASS or FAIL and exits with status 1 on a failure. | The added validation in D12.72 |

## Re-applying the two copy edits after a master refresh

The species copy carries a `comments` column with six fold notes, and the treatment/disturbance
copy carries CLR as TD. Their masters have neither. A plain re-copy from the master folder
undoes both, and `tests/testthat/test-data_raw.R` fails on that. Edit the existing copy, or
re-apply the edits to the refreshed master like this:

1. Put the four 20260925 baseline files (DD, Lookup_Tables and A2 workbooks, datasets CSV)
   in one folder, for example from the commit before task 18, and the refreshed master tables
   in another, under the master names the script's `csv_sources` gives.
2. `Rscript data-raw/spec_edits/patch_workbooks.R <baseline_dir> <translation_dir> <staging_dir>`
   converts each master to UTF-8 and adds the species `comments` column.
3. `Rscript data-raw/spec_edits/edit_csv_cell.R <staging_dir> <edited_dir>` writes the
   treatment/disturbance copy with CLR as TD. Put that file in place of the staging folder's
   copy. If the master already says TD, the script stops at its check that the cell holds T:
   remove the entry from `csv_cell_edits`.
4. `Rscript data-raw/spec_edits/verify_spec_edits.R <baseline_dir> <translation_dir> <staging_dir>`
   must print PASS.
5. The workbooks in the staging folder come out part for part the same as the ones in `spec/`,
   so a translation-table refresh takes only the CSV copies from it, as a new dated file
   through `update-spec`. `fold_lines` in `edits_20261005.R` holds the species table's line
   numbers of the six fold rows: a refreshed master whose fold rows are elsewhere stops the
   run, and the list is edited to match.

Run the verification on the tree's own `spec/` the same way (`<staging_dir>` replaced by
`spec/`) to check that the committed files still match the record.

A later dated set gets its own edit list beside this one. These scripts stay as the record of
what 20261005 was made from.
