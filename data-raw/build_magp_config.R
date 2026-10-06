# Compiles MAGPlot's specification (plan 3.6, 4.4; D5.12, D12.15, D12.20, D12.22, D12.25).
# Reads the one dated file of each kind in spec/ and the hand-kept files in
# data-raw/magp/, applies spec_exceptions.csv to the dictionary, runs pre-flight (its
# report under runs/build_magp_config/), and writes inst/extdata/magp/ only when
# pre-flight doesn't stop. It never writes data-raw/magp/ or spec/.
# Run from the repo root: Rscript data-raw/build_magp_config.R

# MAGPlot's settings that aren't tables (D12.20), named after the arguments they fill
# (D12.35).
# The ID marker: the 13 src_*_id attributes (D9.18, D12.13).
id_pattern <- "^src_.*_id$"
# The site-ID bands, v2 only (D12.9, D12.10, D12.13).
id_bands <- list(
  sheet = "magp_site_id_range", label_col = "contributor", start_col = "v2_start",
  end_col = "v2_end", labels_from = c(sheet = "contributor", column = "abbreviated_name"),
  reserved_pattern = "^reserved_[0-9]+$"
)
# The DD column and value flagging A2 attributes (D12.13).
lineage_flag <- c(column = "appendix", value = "A2")

spec_file_pattern <- "^([0-9]{8})_magpv2_(.+)[.](xlsx|csv)$"

spec_files <- function(spec_dir = "spec") {
  files <- list.files(spec_dir, pattern = spec_file_pattern)
  kinds <- sub(spec_file_pattern, "\\2", files)
  repeated <- unique(kinds[duplicated(kinds)])
  if (length(repeated) > 0L) {
    stop(
      "spec/ holds more than one dated file of: ", paste(repeated, collapse = ", "),
      call. = FALSE
    )
  }
  for (kind in c("DD", "Lookup_Tables", "datasets", "A2")) {
    if (!kind %in% kinds) {
      stop("spec/ has no ", kind, " file.", call. = FALSE)
    }
  }
  paths <- file.path(spec_dir, files)
  names(paths) <- kinds
  paths
}

apply_spec_exceptions <- function(dictionary, exceptions) {
  for (i in seq_len(nrow(exceptions))) {
    row <- exceptions[i]
    target <- which(
      dictionary$table_name == row$table_name & dictionary$attribute_name == row$attribute_name
    )
    if (length(target) != 1L || !row$dd_column %in% names(dictionary)) {
      stop(sprintf("spec_exceptions.csv row %s names no single DD cell.", row$exception_id),
        call. = FALSE
      )
    }
    current <- dictionary[[row$dd_column]][[target]]
    if (!identical(current, row$dd_value)) {
      stop(sprintf(
        "spec_exceptions.csv row %s is stale: the DD holds %s, not %s; remove the row (D12.22).",
        row$exception_id, current, row$dd_value
      ), call. = FALSE)
    }
    data.table::set(dictionary, i = target, j = row$dd_column, value = row$applied_value)
  }
  dictionary
}

# MAGPlot's lineage spec is Appendix A2, a wide sheet with one <contributor>_src and
# <contributor>_note column pair per contributor; this builds the engine's long
# lineage_spec input from it (plan 3.6). Named for what it builds (D12.32).
build_lineage_input <- function(raw, sheet = "A2") {
  header_row <- match("type", raw[[1L]])
  if (is.na(header_row)) {
    stop("A2 has no header row starting with 'type'.", call. = FALSE)
  }
  header <- unlist(raw[header_row], use.names = FALSE)
  body <- raw[-seq_len(header_row)]
  rows <- header_row + seq_len(nrow(body))
  keep <- !is.na(body[[1L]])
  sources <- grep("_src$", header)
  if (length(sources) == 0L) {
    stop("A2 has no <contributor>_src column.", call. = FALSE)
  }
  column <- function(name) body[[match(name, header)]]
  data.table::rbindlist(lapply(sources, function(j) {
    label <- sub("_src$", "", header[[j]])
    note <- match(paste0(label, "_note"), header)
    data.table::data.table(
      contributor_label = label, table_name = column("magp_table"),
      attribute_name = column("attribute"), spec_type = column("type"),
      source_text = body[[j]], note = if (is.na(note)) NA_character_ else body[[note]],
      source_cell = paste0(sheet, "!", column_letters(j), rows)
    )[keep]
  }))
}

crosswalk_element <- function(path, columns) {
  if (nrow(columns) == 0L) {
    return(path)
  }
  split_values <- function(x) strsplit(x, "; ", fixed = TRUE)[[1L]]
  filters <- columns[!is.na(columns$filter_values)]
  values <- if (nrow(filters) == 0L) {
    NULL
  } else if (all(is.na(filters$attribute_name))) {
    split_values(filters$filter_values[[1L]])
  } else {
    stats::setNames(lapply(filters$filter_values, split_values), filters$attribute_name)
  }
  element <- list(table = path)
  code_col <- stats::na.omit(columns$code_col)
  # No declared code column: the element has none, and D5.28's names decide (D12.26).
  if (length(code_col) > 0L) {
    element$code_col <- code_col[[1L]]
  }
  if (!is.null(values)) {
    element$filter_col <- stats::na.omit(filters$filter_col)[[1L]]
    element$filter_values <- values
  }
  element
}

# The manifest names a hand-kept file by its repo path, a bare name meaning spec/
# (D12.33): precedence.csv's row, which its origin gives, and a row for
# spec_exceptions.csv, input dictionary:exceptions, since the build applies it to the DD
# before the read and the engine doesn't know.
record_hand_kept <- function(spec, config_dir) {
  repo_path <- function(name) paste0("data-raw/magp/", name)
  row <- match("precedence", spec$manifest$input)
  data.table::set(spec$manifest, i = row, j = "file", value = repo_path("precedence.csv"))
  exceptions <- manifest_row(
    "dictionary:exceptions", file.path(config_dir, "spec_exceptions.csv")
  )
  data.table::set(exceptions, j = "file", value = repo_path("spec_exceptions.csv"))
  spec$manifest <- data.table::rbindlist(list(spec$manifest, exceptions))
  new_gpq_spec(spec)
}

build_magp_spec <- function(spec_dir = "spec", config_dir = file.path("data-raw", "magp")) {
  files <- spec_files(spec_dir)
  config <- function(name) read_csv_text(file.path(config_dir, name))$data
  dd_sheet <- workbook_sheets(files[["DD"]])[[1L]]
  dictionary <- raw_to_table(read_xlsx_raw(files[["DD"]], dd_sheet))
  # Applied in memory before the read, so findings see the corrected values; the DD's
  # origin then names its workbook's cells (D12.33).
  dictionary <- apply_spec_exceptions(dictionary, config("spec_exceptions.csv"))
  translation <- setdiff(names(files), c("DD", "Lookup_Tables", "datasets", "A2"))
  columns <- config("crosswalk_columns.csv")
  crosswalks <- lapply(translation, function(name) {
    crosswalk_element(files[[name]], columns[columns$crosswalk == name])
  })
  names(crosswalks) <- translation
  precedence <- config("precedence.csv")
  taken <- spec_input_schema()$precedence
  precedence_columns <- match(taken, names(precedence))
  names(precedence_columns) <- taken
  spec <- gpq_read_spec(
    dictionary = dictionary, code_lists = files[["Lookup_Tables"]],
    non_code_sheets = config("non_code_sheets.csv")$sheet, datasets = files[["datasets"]],
    lineage_spec = build_lineage_input(read_xlsx_raw(files[["A2"]], "A2")),
    crosswalks = if (length(crosswalks) > 0L) crosswalks,
    id_pattern = id_pattern, id_bands = id_bands,
    precedence = precedence[, taken, with = FALSE],
    origins = list(
      # The DD's data rows are its workbook's from row 2, the header above: no exception
      # adds or removes a row. One that did would need one file row per data row.
      dictionary = list(path = files[["DD"]], sheet = dd_sheet, rows = 2L),
      # A2's rows carry their cells in source_cell; the origin names A2's file.
      lineage_spec = list(path = files[["A2"]]),
      # decision is dropped before the read, so columns says where each one is.
      precedence = list(
        path = file.path(config_dir, "precedence.csv"), rows = 2L,
        columns = precedence_columns
      )
    ),
    column_map = gpq_column_map(lineage_flag = lineage_flag),
    type_map = gpq_type_map(file.path(config_dir, "type_map.csv")),
    sentinels = validate_sentinels(config("sentinels.csv"))
  )
  record_hand_kept(spec, config_dir)
}

main <- function() {
  library(data.table)
  pkgload::load_all(".", quiet = TRUE)
  spec <- build_magp_spec()
  stopped <- tryCatch(
    {
      gpq_preflight(spec, output_dir = file.path("runs", "build_magp_config"))
      NULL
    },
    gpq_preflight_error = function(e) e
  )
  if (!is.null(stopped)) {
    cat(conditionMessage(stopped), "\nNothing was written to inst/extdata/magp/.\n", sep = "")
    quit(save = "no", status = 1L)
  }
  write_compiled_spec(spec, file.path("inst", "extdata", "magp"))
  cat("Compiled the specification into inst/extdata/magp/.\n")
}

if (sys.nframe() == 0L) {
  main()
}
