# The edits that make the 20261015 treatment/disturbance copy from the 20261005 one (D18.11,
# D18.23, D18.24; plan 24.1 #3, #39): NO becomes a treatment and ND a disturbance, so both
# join their code lists. The edit list as data, nothing else: edit_csv_cell.R sources it from
# its own folder when named as its third argument. It defines `csv_cell_edits` in the form
# edits_20261005.R gives it.
#
# The staging folder holds 20261005_magpv2_treatment_disturbance.csv copied byte for byte
# under the name 20261015_magpv2_treatment_disturbance.csv (file.copy() or a plain copy,
# never opened and saved in Excel or rewritten by fread() or fwrite()), since
# edit_csv_cell.R checks whole CRLF lines and byte identity (D18.24). Every other cell of the
# copy stays as 20261005 has it, CLR's TD included (D12.73 (5a)); no other spec file changes.
csv_cell_edits <- list(
  list(
    file = "20261015_magpv2_treatment_disturbance.csv", key_col = "magp_codes", key = "NO",
    col = "treat_vs_dist", from = NA_character_, to = "T", line = 102L
  ),
  list(
    file = "20261015_magpv2_treatment_disturbance.csv", key_col = "magp_codes", key = "ND",
    col = "treat_vs_dist", from = NA_character_, to = "D", line = 103L
  )
)
