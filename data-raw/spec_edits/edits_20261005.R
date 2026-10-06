# The edits that make the 20261005 specification set from the 20260925 one (Y8; the
# checklist plans/M2_spec_fixes.md, D12.72 (5) to (21), D12.73 (5a), D12.74 (4)). The edit
# list and the names of the files it makes, nothing else: patch_workbooks.R,
# verify_spec_edits.R and edit_csv_cell.R each source() this file from their own folder. It
# defines `edits`, `workbooks`, `new_name()`, `csv_sources`, `csv_source_path()`,
# `fold_notes`, `fold_lines`, `csv_cell_edits`, `datasets_edit()` and `swap_note`.
#
# Coordinates are the 20260925 files' cells (Excel column letter and row). Ops on one cell
# apply in list order. Every op names a guard: cells that must hold the given text before the
# edit (the row's attribute, code or the cell itself), so a wrong row stops the run.
#   set     ref, value, type ("text", "num", "bool", "blank")
#   append  ref, value (appended to the current text)
#   sub     ref, from, value (fixed text; `from` must occur exactly once)
#   delete  row
#   insert  after (a 20260925 row; 0 = before row 1), values (named by column letter)
#   move    row, after
collected <- new.env()
collected$edits <- list()
add <- function(file, sheet, op, ref = NA, value = NA, type = "text", from = NA, guard,
                row = NA, after = NA, values = NULL, clone = NA) {
  collected$edits[[length(collected$edits) + 1L]] <- list(
    file = file, sheet = sheet, op = op, ref = ref, value = value, type = type,
    from = from, guard = guard, row = row, after = after, values = values, clone = clone
  )
}
row_of <- function(ref) as.integer(sub("^[A-Z]+", "", ref))
dd_guard <- function(ref, key) setNames(key, paste0("C", row_of(ref)))
dd_set <- function(ref, key, value, type = "text") {
  add("DD", "DD", "set", ref, value, type, guard = dd_guard(ref, key))
}
dd_app <- function(ref, key, value) {
  add("DD", "DD", "append", ref, value, guard = dd_guard(ref, key))
}
dd_sub <- function(ref, key, from, value) {
  add("DD", "DD", "sub", ref, value, from = from, guard = dd_guard(ref, key))
}

# ---- DD (20261005_magpv2_DD.xlsx, sheet DD) ----
# Section 1, R1 (required)
dd_set("H59", "coord_src", "Y")
dd_set("H141", "src_frame_name", "Y")
# Section 2.1: eight new columns after L
new_cols <- c(
  M = "min_value", N = "max_value", O = "min_inclusive", P = "max_inclusive",
  Q = "pattern", R = "max_length", S = "decimals", T = "units"
)
for (col in names(new_cols)) {
  dd_set(paste0(col, 1), "attribute_name", new_cols[[col]])
}
ranges <- list(
  list(234, "dbh", 0.01, 500, "cm"), list(235, "height", 0.01, 110, "m"),
  list(56, "aspect", 0, 360, "degrees"), list(116, "plot_aspect", 0, 360, "degrees"),
  list(52, "slope", 0, 1143, "percent"), list(115, "plot_slope", 0, 1143, "percent"),
  list(50, "elevation", 0, 5000, "m"), list(114, "plot_elevation", 0, 5000, "m")
)
for (r in ranges) {
  dd_set(paste0("M", r[[1]]), r[[2]], r[[3]], "num")
  dd_set(paste0("N", r[[1]]), r[[2]], r[[4]], "num")
  dd_set(paste0("O", r[[1]]), r[[2]], TRUE, "bool")
  dd_set(paste0("P", r[[1]]), r[[2]], TRUE, "bool")
  dd_set(paste0("T", r[[1]]), r[[2]], r[[5]])
}
dd_set("Q63", "ec_zone", r"(^[0-9]{1,2}$)")
dd_set("Q64", "ec_province", r"(^[0-9]{1,2}\.[0-9]{1,2}$)")
dd_set("Q65", "ec_region", r"(^[0-9]{2}\.[0-9]\.[0-9]{3}$)")
dd_set("Q66", "ec_district", r"(^[0-9]{2}\.[0-9]\.[0-9]{3}\.[0-9]{4}$)")
# Section 2.2
for (r in list(c(63, "ec_zone"), c(64, "ec_province"), c(65, "ec_region"), c(66, "ec_district"))) {
  dd_set(paste0("J", r[[1]]), r[[2]], "character")
}
dd_set("F36", "estab_date", "Establishment date (YYYY-MM-DD), if given.")
dd_set("D9", "magp_contributor_id", "FK")
dd_set("D236", "ht_sampling_meth", ".")
dd_set("D241", "lcr_meas_meth", ".")
dd_set("D254", "src_tree_ba", ".")
dd_set("H119", "design_change", "yes_no")
dd_set("H129", "ef_change", "yes_no")
species_text <- paste0(
  "NFI species code GENUS.SPE[.VAR] from the species translation table. UNKN.SPP is not ",
  "valid; a code with a component equal to NA is invalid; X = not recorded."
)
species_examples <- paste0(
  "ABIE.BAL = Abies balsamea, PICE.MAR = Picea mariana, ",
  "PINU.CON.LAT = Pinus contorta var. latifolia"
)
for (r in c(233, 297)) {
  dd_set(paste0("H", r), "species", "species")
  dd_set(paste0("F", r), "species", species_text)
  dd_set(paste0("I", r), "species", species_examples)
}
dd_set("H76", "disturbance_type", "treatment_disturbance")
dd_set("H88", "treatment_type", "treatment_disturbance")
dd_set("H266", "damage_agent", "condition")
dd_set("F276", "bore_ht", paste0(
  "Height of the sampling point (core/cookie), in metres. Where the height was not ",
  "measured, a code is used instead: B = bored at the base, D = bored at DBH. X = missing, ",
  "Z = not applicable."
))
# #15 before #10 on F220 and F292
for (r in c(220, 292)) {
  dd_sub(
    paste0("F", r), "magp_frame_id",
    "Primary key in magp_designs and foreign key in related MAGPlot tables.",
    "Foreign key to magp_design_frames."
  )
}
id_rows <- list(
  c(71, "magp_dist_id"), c(83, "magp_treat_id"), c(93, "magp_visit_id"), c(110, "magp_plot_id"),
  c(121, "magp_subplot_id"), c(139, "magp_frame_id"), c(160, "magp_plotmeas_id"),
  c(193, "magp_subpmeas_id"), c(201, "magp_tree_id"), c(216, "magp_treemeas_id"),
  c(217, "magp_subpmeas_id"), c(218, "magp_tree_id"), c(220, "magp_frame_id"),
  c(261, "magp_cond_id"), c(263, "magp_treemeas_id"), c(270, "magp_age_id"),
  c(272, "magp_treemeas_id"), c(288, "magp_tally_id"), c(290, "magp_subpmeas_id"),
  c(292, "magp_frame_id")
)
for (r in id_rows) {
  dd_app(paste0("F", r[[1]]), r[[2]], " Parts joined with '_', no zero padding.")
}
dd_app("F131", "magp_design_id", " Parts joined with '.'.")
for (r in c(128, 144, 208)) {
  dd_set(paste0("F", r), "subplot", paste0(
    "Subplot label within a plot, formed by concatenating frame_type and frame_num with no ",
    "separator."
  ))
}
excludes_o <- list(
  c(168, "sph1_tree"), c(169, "baph1_tree"), c(171, "sph9_tree"), c(172, "baph9_tree"),
  c(167, "nrow_sph1_tree"), c(170, "nrow_sph9_tree")
)
for (r in excludes_o) {
  dd_app(paste0("F", r[[1]]), r[[2]], " Excludes trees in frame_type O (outside-plot) frames.")
}
dd_set("F191", "n_species", paste0(
  "Number of live tree species recorded across all frames of this plot measurement, ",
  "excluding frame_type O trees."
))
dd_set("F135", "n_subplots", paste0(
  "Number of frames in the design: the count of magp_design_frames rows for this ",
  "magp_design_id."
))
dd_set("F136", "meas_subplots", paste0(
  "Comma-separated list of the subplot labels of the design's frames (magp_design_frames), ",
  "for example M1,S1,R1."
))
dd_app(
  "F193", "magp_subpmeas_id", " One row per frame measured at the visit, empty frames included."
)
for (r in c(113, 125, 205)) {
  dd_app(
    paste0("F", r), "plot", " 0 = outside the plot (a tree measured outside the plot boundary)."
  )
}
for (r in c(127, 143)) {
  dd_sub(paste0("F", r), "frame_num", "L1 and L2 in QC", "M1 and M2 in QC")
}
dd_sub("I293", "tally_type", "RG = Regeneration", "RE = Regeneration")
dd_sub("F298", "count", "regen_type", "tally_type")
dd_sub("F294", "ht_class", "regen type", "tally_type")
dd_set("I8", "magp_dataset_id", "100.01, 110.01, 110.02, etc.")
for (r in list(c(257, "src_vol_nmer"), c(258, "src_vol_gmer"), c(259, "src_vol_wsv"))) {
  dd_sub(
    paste0("F", r[[1]]), r[[2]], "for live trees",
    "for standing trees, live or dead; not applicable to cut or gone trees"
  )
}
dd_set("G10", "src_dataset_id", "A2")
dd_set("G179", "src_dbh_cutoff", "A2")
dd_set("G254", "src_tree_ba", "A2")
dd_set("I5", "src_database_name", "faibGP, PEI_PSP, etc.")

# ---- Lookup (20261005_magpv2_Lookup_Tables.xlsx) ----
lk <- function(sheet, op, ref, value, guard, type = "text") {
  add("Lookup", sheet, op, ref, value, type, guard = guard)
}
lk("contributor", "set", "B11", "PEI", c(A11 = "190", B11 = "PE"))
lk("contributor", "set", "D1", "src_database_name", c(D1 = "database_name_src"))
lk("contributor", "set", "E1", "magp_database_name", c(E1 = "database_name"))
lk("contributor", "set", "E8", "NT_PSP", c(A8 = "160", E8 = " NT_PSP"))
lk("contributor", "set", "E9", "ON_GP", c(A9 = "170", E9 = " ON_GP"))
for (k in 1:5) {
  lk(
    "magp_site_id_range", "set", paste0("A", 15 + k), paste0("reserved_", k),
    setNames(as.character(k), paste0("A", 15 + k))
  )
}
lk("magp_site_id_range", "set", "D16", 950001, c(A16 = "1", D16 = "750001"), "num")
lk("magp_site_id_range", "set", "E16", 1000000, c(A16 = "1", E16 = "750000"), "num")
lk("tally_type", "set", "D2", "Z", c(A2 = "SE", D2 = "-1"))
lk("tally_type", "set", "D3", "Z", c(A3 = "RE", D3 = "-1"))
lk("tally_type", "set", "C5", "X", c(A5 = "X", C5 = "-9"))
lk("tally_type", "set", "D5", "X", c(A5 = "X", D5 = "-9"))
lk(
  "tree_type", "set", "C12",
  paste0(
    "Use when the tree was measured outside the main plot boundary or outside the standard ",
    "measurement area."
  ),
  c(A12 = "O")
)
lk(
  "tree_type", "set", "C13",
  "Use when the tree type is known but does not fit the listed categories.",
  c(A13 = "OTH")
)
lk(
  "tree_type", "set", "C14",
  paste0(
    "Use when a non-residual and non-random tree cannot be classified into the standard ",
    "tree-type categories (TH, SS, O, LS and X)."
  ),
  c(A14 = "N")
)
lk("src_frame_name", "set", "B7", "Regeneration plot", c(A7 = "RP", B7 = "Regenration plot"))
lk(
  "src_frame_name", "set", "B9", "Regeneration vegetation plot",
  c(A9 = "RVP", B9 = "Regenration vegetation plot")
)
lk(
  "visit_type", "set", "C11",
  paste0(
    "Use for young-stand monitoring remeasurements on a standardized intensified NFI grid, ",
    "generally 5 km \u{00d7} 10 km, in the young-stand population the VegCompR1 inventory ",
    "defines as stands 15 to 50 years old. In BC it can be used for the CMI, SUP and YSM ",
    "programs."
  ),
  c(A11 = "REY")
)
add("Lookup", "visit_type", "delete", row = 13, guard = c(A13 = NA, B13 = NA, C13 = NA, A14 = "X"))
meas_dups <- c("TRE", "AGE", "VEG", "STN")
for (k in seq_along(meas_dups)) {
  r <- 8 + k
  add("Lookup", "meas_type", "delete",
    row = r,
    guard = setNames(c(meas_dups[k], NA, NA), paste0(c("A", "B", "C"), r))
  )
}
for (r in 2:36) {
  add("Lookup", "dataset", "set", paste0("E", r), NA, "blank", guard = c(E1 = "plot_layout_fig"))
}

# ---- A2 (20261005_magpv2_A2.xlsx) ----
add("A2", "A2", "sub", "A2",
  "Rows are grouped, id rows first, then compiled rows, each group in data dictionary order.",
  from = "Rows follow MAGPlot v2 data dictionary order.", guard = c(A4 = "type")
)
add("A2", "A2", "set", "B11", "magp_subplot_meas",
  guard = c(C11 = "src_subpmeas_id", B11 = "magp_plot_meas")
)
add("A2", "A2", "sub", "D17", "tally_type", from = "regen_type", guard = c(C17 = "src_tally_id"))
add("A2", "A2", "sub", "E17", "tally_type", from = "regen_type", guard = c(C17 = "src_tally_id"))
add("A2", "A2", "set", "E30",
  paste0(
    "Both are net merchantable volume (stump, top, decay, waste and breakage deducted). PSP ",
    "deducts decay, waste and breakage with BEC-based loss factors, non-PSP with ",
    "cruiser-called decay, waste and breakage. Not the same definition."
  ),
  guard = c(C30 = "src_nmer_volume")
)
add("A2", "A2", "sub", "G28", "Set to -9 for",
  from = "Set to -1 for", guard = c(C28 = "src_lorey_ht")
)
add("A2", "A2", "sub", "G29", "Set to -9 for",
  from = "Set to -1 for", guard = c(C29 = "src_qmd")
)
a2_x <- c(
  "D20", "F24", "F30", "F31", "F32", "D33", "F33", "D34", "F34", "F35", "D36", "F36",
  "F37", "F38", "F39", "D40", "F40", "D41"
)
for (ref in a2_x) {
  add("A2", "A2", "set", ref, "-1", guard = setNames("X", ref))
}
for (ref in c("D25", "F25")) {
  add("A2", "A2", "set", ref, "-9", guard = c(setNames("Z", ref), C25 = "src_baph_tally"))
}
add("A2", "A2", "sub", "E24", "-1 where BC published no rate",
  from = "NA where BC published no rate", guard = c(C24 = "src_sph_tally")
)
add("A2", "A2", "insert", after = 4, guard = c(C4 = "attribute", C5 = "src_site_id"), values = c(
  A = "id", B = "magp_datasets", C = "src_dataset_id",
  D = "faib_header.sample_establishment_type",
  E = paste0(
    "MAGPlot prefixes the programme code with 'BC_' (PSP_G becomes BC_PSP_G). Programmes ",
    "absent from the dataset lookup, such as NFI, are dropped."
  ),
  F = "tblPlot.plotname",
  G = "The name's last three characters (PSP, PGP, PIP) give ON_PSP, ON_PGP and ON_PGP_PIP."
))
add("A2", "A2", "insert",
  after = 35, guard = c(C35 = "src_tree_ef_adj", C36 = "src_baph_adj"),
  values = c(
    A = "compiled", B = "magp_tree_meas", C = "src_tree_ba", D = "faib_tree_detail.ba_tree",
    E = "Same column name in both packages.", F = "-1", G = "Not supplied at tree level."
  )
)
add("A2", "A2", "move",
  row = 40, after = 36,
  guard = c(B40 = "magp_tree_meas", C40 = "src_biomass", C36 = "src_baph_adj", C37 = "src_vol_nmer")
)
add("A2", "legend", "append", "B12",
  paste0(
    " '|' separates alternatives and binds looser than '+': 'PSP: a + b | non-PSP: c' is two ",
    "alternatives, the first with two parts."
  ),
  guard = c(A12 = "PSP: ... | non-PSP: ...")
)
# D12.72 (20): a legend row for -9 after the -1 row (styled as it), and B6
add("A2", "legend", "insert",
  after = 15, clone = 15, guard = c(A15 = "-1", A16 = NA, A17 = "BC source tables"),
  values = c(A = "-9", B = "Not-applicable numeric sentinel written by the script.")
)
add("A2", "legend", "set", "B6",
  "Exception, assumption, or reason the value is X, Z, -1 or -9. Blank is the normal case.",
  guard = c(
    A6 = "BC_note / ON_note",
    B6 = "Exception, assumption, or reason the value is X or Z. Blank is the normal case."
  )
)

edits <- collected$edits

# ---- The files ----
# The workbooks are patched copies of the 20260925 ones, found by name in the baseline folder.
workbooks <- c(
  DD = "20260925_magpv2_DD.xlsx", Lookup = "20260925_magpv2_Lookup_Tables.xlsx",
  A2 = "20260925_magpv2_A2.xlsx"
)
new_name <- function(x) sub("^20260925_", "20261005_", x)
# The CSV copies: the datasets table is the baseline's own file; each translation table is the
# owner's master, found by its master name in the translation folder (read, never written).
csv_sources <- list(
  "20261005_magpv2_datasets.csv" = list(from = "baseline", file = "20260925_magpv2_datasets.csv"),
  "20261005_magpv2_species.csv" = list(
    from = "translation", file = "1 MAGPlot_species_codes_translation.csv"
  ),
  "20261005_magpv2_condition.csv" = list(from = "translation", file = "2 MAGPlot_condition.csv"),
  "20261005_magpv2_treatment_disturbance.csv" = list(
    from = "translation", file = "3 MAGPlot_treatment_disturbance.csv"
  ),
  "20261005_magpv2_severity.csv" = list(from = "translation", file = "MAGPlot_severity.csv")
)
csv_source_path <- function(name, baseline_dir, translation_dir) {
  source_entry <- csv_sources[[name]]
  file.path(
    if (source_entry$from == "baseline") baseline_dir else translation_dir, source_entry$file
  )
}
# The species copy gains a last column, `comments`, filled on the six fold rows only
# (D12.72 (13), (21)); every other byte of the master is kept.
fold_notes <- c(
  BETU.GLA = "Folded code: BETU.GLA covers Betula glandulifera and Betula glandulosa.",
  ALNU.VIR = "Folded code: ALNU.VIR folds Alnus crispa var. mollis into Alnus viridis.",
  POPU.SPP = "Folded code: POPU.SPP folds the unnamed Populus hybrid (Populus X) into the genus."
)
fold_lines <- c(36L, 45L, 68L, 69L, 246L, 247L)
# Cell edits of the CSV copies, applied by edit_csv_cell.R in table order, each file written
# once. An entry names the file, the key column and value of its row, the column, the cell's
# text before (from) and after (to), and the row's file line (header = line 1). A blank cell is
# NA_character_, as fread(na.strings = "") reads it. verify_spec_edits.R checks the same list.
# D12.73 (5a): CLR's treat_vs_dist in the treatment/disturbance copy, T to TD.
csv_cell_edits <- list(list(
  file = "20261005_magpv2_treatment_disturbance.csv", key_col = "magp_codes", key = "CLR",
  col = "treat_vs_dist", from = "T", to = "TD", line = 61L
))
# D12.74 (4): the datasets copy's 110.05 and 110.06 exchange their descriptors, ten cells, so
# that 110.05 = BC_VRI and 110.06 = BC_SUP as the lookup's dataset sheet has them. The copy is
# the 20260830 master's, which still had the swap; the owner fixed it from 20260915. The
# master's other fills are not taken in (plan 24.1 #49).
datasets_edit <- function(key, line, col, from, to) {
  list(
    file = "20261005_magpv2_datasets.csv", key_col = "magp_dataset_id", key = key, col = col,
    from = from, to = to, line = line
  )
}
swap_note <- "Some visits have TMP as the visit type/code."
csv_cell_edits <- c(csv_cell_edits, list(
  datasets_edit("110.05", 6L, "src_dataset_id", "BC_SUP", "BC_VRI"),
  datasets_edit("110.05", 6L, "gp_type", "MIX", "TMP"),
  datasets_edit("110.05", 6L, "gp_network", "FMP", "CVV"),
  datasets_edit("110.05", 6L, "sampling_design", "SYS", "STR"),
  datasets_edit("110.05", 6L, "comments", swap_note, NA_character_),
  datasets_edit("110.06", 7L, "src_dataset_id", "BC_VRI", "BC_SUP"),
  datasets_edit("110.06", 7L, "gp_type", "TMP", "MIX"),
  datasets_edit("110.06", 7L, "gp_network", "CVV", "FMP"),
  datasets_edit("110.06", 7L, "sampling_design", "STR", "SYS"),
  datasets_edit("110.06", 7L, "comments", NA_character_, swap_note)
))
