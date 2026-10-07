# Builds the applicability matrix's first working copy (plan 5.6; D2.6, D7.16, D7.26, D8.4,
# D13.1 to D13.5): one national row per dictionary attribute, rows seeded from A2, from the
# pipeline scripts' registers, frame rules and absent links, and from the AB mapping workbook,
# and a review guide. It writes applicability_working.csv and review_guide.md to the output
# folder, never over an existing file, and never writes spec/.
# Run from the repo root, with the paths given in the session and never committed (D5.31):
#   Rscript data-raw/build_matrix_template.R "--pipeline-dir=<folder>" "--workbook=<file>"
# Without --output-dir the folder is matrix/ in GPQ_PLANS_DIR. Without --pipeline-dir or
# --workbook those sources aren't seeded, and the guide says so (D7.16, D13.5 (1)).

# The matrix's columns, its four level columns in MAGPlot's order (plan 5.2), and its files.
matrix_levels <- c("jurisdiction", "magp_dataset_id", "frame_type", "meas_type")
matrix_columns <- c(
  "table_name", "attribute_name", matrix_levels, "applicability", "evidence", "status", "note"
)
output_files <- c(matrix = "applicability_working.csv", guide = "review_guide.md")

# The lookup sheets that list the contributors and their datasets (D13.2 (1), (4)).
contributor_sheet <- c(
  sheet = "contributor", id_col = "magp_contributor_id", label_col = "abbreviated_name"
)
dataset_sheet <- c(
  sheet = "dataset", id_col = "magp_dataset_id", contributor_col = "magp_contributor_id"
)

# Tables that reach no site, so take no contributor's rows (plan 5.3; D2.3, D9.24, D13.4 (1)).
no_dataset_tables <- c("magp_contributors", "magp_designs", "magp_design_frames")

# A2's markers (legend!A13:A16): X and -1 not collected, O; Z and -9 not applicable, N (D2.27,
# D13.2 (3)).
a2_markers <- c("X" = "O", "-1" = "O", "Z" = "N", "-9" = "N")

# The sources' names in a clash's detail.
source_labels <- c(
  dd = "DD", a2 = "A2", register = "register", mapping = "AB mapping", rule = "rule"
)

# Contributors and their datasets, from the lookup's contributor and dataset sheets (D13.2 (1),
# (4)): one row per dataset, with its contributor's abbreviated name.
contributor_datasets <- function(code_lists) {
  # A sheet's rows as a table of the wanted columns; the names don't shadow code_lists' columns.
  sheet_table <- function(name, wanted) {
    cells <- code_lists[code_lists$sheet == name & code_lists$source_row > 1L]
    absent <- setdiff(wanted, cells$sheet_column)
    if (nrow(cells) == 0L || length(absent) > 0L) {
      stop(sprintf(
        "The lookup's %s sheet lacks %s (D13.2 (1)).", name,
        if (nrow(cells) == 0L) "rows" else paste(absent, collapse = ", ")
      ), call. = FALSE)
    }
    cells <- cells[cells$sheet_column %chin% wanted]
    data.table::dcast(cells, source_row ~ sheet_column, value.var = "value")
  }
  columns <- unname(contributor_sheet[c("id_col", "label_col")])
  contributors <- sheet_table(contributor_sheet[["sheet"]], columns)
  data.table::setnames(contributors, columns, c("contributor_id", "contributor"))
  columns <- unname(dataset_sheet[c("id_col", "contributor_col")])
  datasets <- sheet_table(dataset_sheet[["sheet"]], columns)
  data.table::setnames(datasets, columns, c("magp_dataset_id", "contributor_id"))
  repeated <- unique(contributors$contributor[duplicated(contributors$contributor)])
  if (length(repeated) > 0L) {
    stop(
      "The lookup's contributor sheet repeats the abbreviated name ",
      paste(repeated, collapse = ", "), " (D13.2 (4)).",
      call. = FALSE
    )
  }
  datasets <- contributors[datasets, on = "contributor_id", nomatch = NULL]
  datasets[, c("contributor", "magp_dataset_id"), with = FALSE]
}

# The datasets of one contributor, by its abbreviated name, or a stop (D13.2 (4)).
datasets_of <- function(datasets, contributor) {
  ids <- datasets$magp_dataset_id[datasets$contributor %chin% contributor]
  if (length(ids) == 0L) {
    stop(sprintf(
      "The lookup lists no dataset of contributor %s (D13.2 (4)).", contributor
    ), call. = FALSE)
  }
  ids
}

# The national rows (plan 5.2, 5.6): R for a primary or foreign key, with the DD's key type and
# row, and the default O without evidence for every other attribute (D8.10).
national_rows <- function(attributes, dd_file) {
  keyed <- attributes$key_type %chin% c("PK", "FK")
  data.table::data.table(
    table_name = attributes$table_name, attribute_name = attributes$attribute_name,
    contributor = NA_character_, frame_type = "*", meas_type = "*",
    applicability = data.table::fifelse(keyed, "R", "O"),
    evidence = data.table::fifelse(
      keyed,
      sprintf("DD key_type %s (%s:%d)", attributes$key_type, dd_file, attributes$source_row),
      NA_character_
    ),
    note = NA_character_, source = "dd"
  )
}

# A2's marked cells as contributor rows (D2.27, D9.20, D13.2 (3)).
a2_rows <- function(lineage_spec, a2_file) {
  marked <- unique(
    lineage_spec[lineage_spec$source_text %chin% names(a2_markers)],
    by = c("contributor_label", "table_name", "attribute_name")
  )
  data.table::data.table(
    table_name = marked$table_name, attribute_name = marked$attribute_name,
    contributor = marked$contributor_label, frame_type = "*", meas_type = "*",
    applicability = unname(a2_markers[marked$source_text]),
    evidence = sprintf("A2 %s (%s, %s)", marked$source_text, a2_file, marked$source_cell),
    note = data.table::fifelse(is.na(marked$note), NA_character_, paste0("A2: ", marked$note)),
    source = "a2"
  )
}

# Why a contributor's row can't be seeded, or NA where it can: a table that takes no dataset
# rows, a pair the DD lacks, or a key (plan 5.3; D13.3 (4), D13.4 (1)).
placement <- function(rows, attributes) {
  pairs <- paste(rows$table_name, rows$attribute_name)
  keyed <- attributes$key_type %chin% c("PK", "FK")
  dd_pairs <- paste(attributes$table_name, attributes$attribute_name)
  data.table::fcase(
    rows$table_name %chin% no_dataset_tables, "design_table",
    !pairs %chin% dd_pairs, "not_in_dd",
    pairs %chin% dd_pairs[keyed], "key",
    default = NA_character_
  )
}

# Rows of one key merged into one (D13.2 (2)): A2's value kept where the sources disagree, the
# evidence joined with "; " and the notes with " | "; each disagreement returned for review.
merge_rows <- function(rows) {
  key_cols <- c("table_name", "attribute_name", "contributor", "frame_type", "meas_type")
  joined <- function(x, sep) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) NA_character_ else paste(x, collapse = sep)
  }
  kept <- function(value, source) {
    if (any(source == "a2")) value[source == "a2"][[1L]] else value[[1L]]
  }
  groups <- rows[, list(
    n_values = data.table::uniqueN(applicability), has_a2 = any(source == "a2"),
    applicability = kept(applicability, source),
    evidence = joined(evidence, "; "), note = joined(note, " | "),
    detail = paste(source_labels[source], applicability, collapse = ", ")
  ), by = key_cols]
  if (any(groups$n_values > 1L & !groups$has_a2)) {
    stop("Internal error: sources other than A2 disagree on a matrix key.", call. = FALSE)
  }
  clash <- groups[groups$n_values > 1L]
  list(
    rows = groups[, c(key_cols, "applicability", "evidence", "note"), with = FALSE],
    review = clash[, list(
      topic = "clash", contributor, table_name, attribute_name,
      detail = paste0(detail, "; A2 kept"), evidence, note
    )]
  )
}

# Contributor rows as dataset rows (D13.2 (1)): jurisdiction "*", one row per dataset; the
# national rows get dataset "*". Every row is "proposed".
dataset_rows <- function(rows, datasets) {
  ids <- lapply(rows$contributor, function(label) {
    if (is.na(label)) "*" else datasets_of(datasets, label)
  })
  rows <- rows[rep(seq_len(nrow(rows)), lengths(ids))]
  rows[, `:=`(
    jurisdiction = "*", magp_dataset_id = unlist(ids, use.names = FALSE), status = "proposed"
  )]
  rows[]
}

# The M2a gate (plan 21 M2a; D7.16, D7.26): every DD attribute has one national row and every
# row names a DD attribute, every value is R, O or N, every row but a default national O has
# evidence, no key repeats, and no text of the matrix, its review rows or the guide holds a
# machine path. Problems stop the build before anything is written.
check_template <- function(template, attributes, paths = character(), guide = character()) {
  matrix <- template$matrix
  national <- matrix$jurisdiction == "*" & matrix$magp_dataset_id == "*" &
    matrix$frame_type == "*" & matrix$meas_type == "*"
  dd_pairs <- paste(attributes$table_name, attributes$attribute_name)
  pairs <- paste(matrix$table_name, matrix$attribute_name)
  counts <- table(factor(pairs[national], levels = dd_pairs))
  bad_value <- which(!matrix$applicability %chin% c("R", "O", "N"))
  bare <- which(!(national & matrix$applicability %chin% "O") & is.na(matrix$evidence))
  repeated <- which(duplicated(matrix, by = c("table_name", "attribute_name", matrix_levels)))
  text <- c(
    matrix$evidence, matrix$note, template$review$detail, template$review$evidence,
    template$review$note, guide
  )
  text <- unique(text[!is.na(text)])
  machine <- grepl("(^|[^A-Za-z])[A-Za-z]:[\\\\/]|\\\\\\\\|/Users/|/home/|~/", text)
  for (path in paths[!is.na(paths) & nzchar(paths)]) {
    for (form in unique(c(path, gsub("\\\\", "/", path), gsub("/", "\\\\", path)))) {
      machine <- machine | grepl(form, text, fixed = TRUE)
    }
  }
  problems <- c(
    sprintf("%s has %d national rows", names(counts)[counts != 1L], counts[counts != 1L]),
    sprintf("%s isn't a DD attribute", unique(pairs[!pairs %chin% dd_pairs])),
    sprintf("row %d has applicability %s", bad_value, matrix$applicability[bad_value]),
    sprintf("row %d has no evidence", bare),
    sprintf("row %d repeats a key", repeated),
    sprintf("a machine path in: %s", text[machine])
  )
  if (length(problems) > 0L) {
    stop(
      "The template fails the M2a gate, so nothing was written:\n  ",
      paste(utils::head(problems, 20L), collapse = "\n  "),
      if (length(problems) > 20L) sprintf("\n  and %d more", length(problems) - 20L),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
