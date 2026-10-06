# Spec edits

This folder is the record of Y8's edits to the specification set, which turned the 20260925
files into the 20261005 ones (M2, task 18), of Y9's fix of the datasets copy's 110.05 and
110.06 (D12.74), and the way to re-apply the two deliberate edits to the translation-table
copies after a refresh from their masters (plan 24.1 #46).

The scripts are tools for whoever runs `update-spec`. They are not part of the package
(`data-raw/` is build-ignored), and they take every folder as an argument: no path to a
machine is written in them. Give them an output folder outside the tree, or a temporary one:
nothing in them stops an output folder named `spec/`. Give folders as absolute paths.
`patch_workbooks.R` makes its output folder absolute itself, since `zip::zip()` works from
the folder it packs and would look for a relative path there; `edit_csv_cell.R` writes where
it is told. `zip` (patching a workbook's package) and `xml2` (the well-formedness check) are
used here only, never in `R/` or `DESCRIPTION` (D12.73 (10)). `readxl` and `data.table` are
used as elsewhere.

## The scripts

The three scripts below `edits_20261005.R` print their usage line when run with no
arguments; `edits_20261005.R` is data, sourced by the others, and runs nothing.

| Script | What it does | Decision |
|---|---|---|
| `edits_20261005.R` | The edit list and the names of the files, as data. The three scripts below `source()` it from their own folder. Each cell edit names a guard: cells that must hold the expected text first, so a wrong row stops the run. | The checklist `plans/M2_spec_fixes.md` and D12.72 (5) to (21); D12.73 (5a); D12.74 (4) |
| `patch_workbooks.R` | Writes all eight 20261005 files into a new folder: the DD, lookup and A2 workbooks (copies of the 20260925 packages with only the touched sheets' XML changed), the datasets CSV, and the four translation tables converted to UTF-8 with the species copy's `comments` column added. It refuses a folder that exists. | D12.72 (1), (18) for the workbooks and the conversion; (13), (21) for the species `comments` column |
| `edit_csv_cell.R` | Applies the `csv_cell_edits` entries to the CSV copies they name, each file written once to a second folder, keeping every other byte: CLR's `treat_vs_dist` from T to TD in the treatment/disturbance copy, and the ten cells of the datasets copy's 110.05 and 110.06 that exchange their descriptors (`src_dataset_id`, `gp_type`, `gp_network`, `sampling_design`, `comments`), so that 110.05 = BC_VRI and 110.06 = BC_SUP as the lookup has them. It checks each edited line and the parsed tables of every file before it writes any, and refuses to overwrite a file. | D12.73 (5a); D12.74 (4) |
| `verify_spec_edits.R` | Checks the eight files in a folder against the 20260925 baseline and the edit list, by its own method: every cell of every sheet, the package parts, the CSV encodings and each CSV's difference from its source. Prints PASS or FAIL and exits with status 1 on a failure. | The added validation in D12.72 |

## Re-applying the copy edits after a master refresh

The species copy carries a `comments` column with six fold notes, and the treatment/disturbance
copy carries CLR as TD. Their masters have neither. A plain re-copy from the master folder
undoes both, and `tests/testthat/test-data_raw.R` fails on that.

The datasets copy is not a master's: its source is the 20260925 baseline's own file, which
has the AB rows and the `meas_interval` column that the owner's datasets master lacks (plan
24.1 #49), so a re-copy of that master would drop both. Its swapped 110.05 and 110.06 are
made by step 3 below, and the master's other fills wait for a later update-spec.

Edit the existing copy, or re-apply the edits to the refreshed master like this:

1. Put the four 20260925 baseline files (DD, Lookup_Tables and A2 workbooks, datasets CSV)
   in one folder, for example from the commit before task 18, and the refreshed master tables
   in another, under the master names the script's `csv_sources` gives.
2. `Rscript data-raw/spec_edits/patch_workbooks.R <baseline_dir> <translation_dir> <staging_dir>`
   converts each master to UTF-8 and adds the species `comments` column.
3. `Rscript data-raw/spec_edits/edit_csv_cell.R <staging_dir> <edited_dir>` writes the
   treatment/disturbance copy with CLR as TD and the datasets copy with 110.05 and 110.06
   swapped. Put those two files in place of the staging folder's copies. If the master already
   says TD, the script stops, naming the file, key and column, at its check that the cell
   holds T: remove the entry from `csv_cell_edits`. The datasets entries stop the same way on
   a baseline that already has the swap fixed. A stop leaves nothing in `<edited_dir>`.
4. `Rscript data-raw/spec_edits/verify_spec_edits.R <baseline_dir> <translation_dir> <staging_dir>`
   must print PASS.
5. The workbooks in the staging folder come out part for part the same as the ones in `spec/`,
   so a translation-table refresh takes only the CSV copies from it, as a new dated file
   through `update-spec`. `fold_lines` in `edits_20261005.R` holds the species table's line
   numbers of the six fold rows, and each `csv_cell_edits` entry there has a `line`, its
   row's line in its file (CLR's in the treatment/disturbance copy, the datasets copy's two
   rows' in that copy; the header is line 1). A refreshed master whose rows are elsewhere
   stops the run, `patch_workbooks.R` on a fold row, `edit_csv_cell.R` on an entry's row, and
   the list or the entry is edited to match. `patch_workbooks.R` stops after it has written
   the workbooks, and the next run refuses the folder it left: delete it first.

Run the verification on the tree's own `spec/` the same way (`<staging_dir>` replaced by
`spec/`) to check that the committed files still match the record.

A later dated set gets its own edit list beside this one. These scripts stay as the record of
what 20261005 was made from.
