# Compiles MAGPlot's specification and rule set (plan 3.5, 3.6, 4.4; D5.12, D9.20, D12.15,
# D12.20, D12.22, D12.25, D14.2, D14.15). Reads the one dated file of each kind in spec/ and
# the hand-kept files in data-raw/magp/, applies spec_exceptions.csv to the dictionary, runs
# pre-flight on the spec and the rule set (its report under runs/build_magp_config/), and
# writes inst/extdata/magp/ only when pre-flight doesn't stop: the compiled spec, and the
# rule set's rules_<component>.csv files copied unchanged. It never writes data-raw/magp/
# or spec/.
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
  # The rows are set in a copy, so the caller's table stays as it was, a stop on a stale row
  # included; the DD is small.
  dictionary <- data.table::copy(dictionary)
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
  absent <- setdiff(c("type", "magp_table", "attribute"), header)
  if (length(absent) > 0L) {
    stop("A2 has no column named ", paste(absent, collapse = " or "), ".", call. = FALSE)
  }
  sources <- grep("_src$", header)
  if (length(sources) == 0L) {
    stop("A2 has no <contributor>_src column.", call. = FALSE)
  }
  body <- raw[-seq_len(header_row)]
  rows <- header_row + seq_len(nrow(body))
  # A row with a blank first cell is no attribute's: dropped once, each kept row with its
  # own sheet row.
  keep <- !is.na(body[[1L]])
  body <- body[keep]
  rows <- rows[keep]
  column <- function(name) body[[match(name, header)]]
  data.table::rbindlist(lapply(sources, function(j) {
    label <- sub("_src$", "", header[[j]])
    note <- match(paste0(label, "_note"), header)
    data.table::data.table(
      contributor_label = label, table_name = column("magp_table"),
      attribute_name = column("attribute"), spec_type = column("type"),
      source_text = body[[j]], note = if (is.na(note)) NA_character_ else body[[note]],
      source_cell = paste0(sheet, "!", column_letters(j), rows)
    )
  }))
}

crosswalk_element <- function(path, columns) {
  if (nrow(columns) == 0L) {
    return(path)
  }
  # A crosswalk's rows agree on its code column and its filter column, or the build stops
  # rather than take the first row's (D12.21).
  name <- columns$crosswalk[[1L]]
  one_value <- function(x, what) {
    x <- unique(x[!is.na(x)])
    if (length(x) > 1L) {
      stop(sprintf(
        "crosswalk_columns.csv gives crosswalk %s more than one %s: %s.", name, what,
        paste(x, collapse = ", ")
      ), call. = FALSE)
    }
    x
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
  code_col <- one_value(columns$code_col, "code_col")
  # No declared code column: the element has none, and D5.28's names decide (D12.26).
  if (length(code_col) > 0L) {
    element$code_col <- code_col
  }
  if (!is.null(values)) {
    filter_col <- one_value(filters$filter_col, "filter_col")
    if (length(filter_col) == 0L) {
      stop(sprintf(
        "crosswalk_columns.csv gives crosswalk %s filter_values but no filter_col.", name
      ), call. = FALSE)
    }
    element$filter_col <- filter_col
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
  exceptions <- manifest_row(
    "dictionary:exceptions", file.path(config_dir, "spec_exceptions.csv")
  )
  data.table::set(exceptions, j = "file", value = repo_path("spec_exceptions.csv"))
  # rbindlist() makes a new table, set here, so the caller's spec keeps its manifest.
  manifest <- data.table::rbindlist(list(spec$manifest, exceptions))
  row <- match("precedence", manifest$input)
  data.table::set(manifest, i = row, j = "file", value = repo_path("precedence.csv"))
  spec$manifest <- manifest
  new_gpq_spec(spec)
}

# A hand-kept file reads clean or the build stops, naming the file and each problem: a
# malformed line or an invalid byte would drop or change rows with no finding, since of
# these files only the type map reaches pre-flight as a file (D12.58).
read_hand_kept <- function(path) {
  read <- read_csv_text(path)
  at_line <- function(line) ifelse(is.na(line), "", paste0(" on line ", line))
  malformed <- read$malformed
  invalid <- read$invalid
  # A "fields" problem gives the header's number of fields, a "short" one its record count
  # and the rows read; any other the text fread() gave, where it gave one.
  counts <- ifelse(
    malformed$kind == "fields", paste0(" (the header has ", malformed$fields, " fields)"),
    ifelse(
      malformed$kind == "short",
      paste0(
        " (", malformed$n_records, " records after the header, ", malformed$n_read, " read)"
      ),
      ifelse(is.na(malformed$value), "", paste0(" (", malformed$value, ")"))
    )
  )
  problems <- c(
    paste0(malformed$kind, at_line(malformed$line), counts, recycle0 = TRUE),
    # An invalid byte's row 0 is the header, line 1; row r starts on lines[r].
    paste0(
      "invalid byte", at_line(c(1L, read$lines)[invalid$row + 1L]), " (", invalid$value, ")",
      recycle0 = TRUE
    )
  )
  if (length(problems) > 0L) {
    stop(
      path, " doesn't read cleanly, so nothing was built: ", paste(problems, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  read
}

build_magp_spec <- function(spec_dir = "spec", config_dir = file.path("data-raw", "magp")) {
  files <- spec_files(spec_dir)
  # The five hand-kept files read through config() are all read before any spec file is
  # opened; type_map.csv is read last, through gpq_type_map(path), which carries its findings
  # on to pre-flight.
  config <- function(name) read_hand_kept(file.path(config_dir, name))
  exceptions <- config("spec_exceptions.csv")$data
  non_code_sheets <- config("non_code_sheets.csv")$data$sheet
  columns <- config("crosswalk_columns.csv")$data
  precedence <- config("precedence.csv")
  sentinels <- validate_sentinels(config("sentinels.csv")$data)
  dd_sheet <- workbook_sheets(files[["DD"]])[[1L]]
  dictionary <- raw_to_table(read_xlsx_raw(files[["DD"]], dd_sheet))
  # Applied in memory before the read, so findings see the corrected values; the DD's
  # origin then names its workbook's cells (D12.33).
  dictionary <- apply_spec_exceptions(dictionary, exceptions)
  translation <- setdiff(names(files), c("DD", "Lookup_Tables", "datasets", "A2"))
  crosswalks <- lapply(translation, function(name) {
    crosswalk_element(files[[name]], columns[columns$crosswalk == name])
  })
  names(crosswalks) <- translation
  taken <- spec_input_schema()$precedence
  precedence_columns <- match(taken, names(precedence$data))
  names(precedence_columns) <- taken
  # One sheet name for A2's read and for the cells its rows carry.
  sheet <- "A2"
  spec <- gpq_read_spec(
    dictionary = dictionary, code_lists = files[["Lookup_Tables"]],
    non_code_sheets = non_code_sheets, datasets = files[["datasets"]],
    lineage_spec = build_lineage_input(read_xlsx_raw(files[["A2"]], sheet), sheet),
    crosswalks = if (length(crosswalks) > 0L) crosswalks,
    id_pattern = id_pattern, id_bands = id_bands,
    precedence = precedence$data[, taken, with = FALSE],
    origins = list(
      # The DD's data rows are its workbook's from row 2, the header above: no exception
      # adds or removes a row. One that did would need one file row per data row.
      dictionary = list(path = files[["DD"]], sheet = dd_sheet, rows = 2L),
      # A2's rows carry their cells in source_cell; the origin names A2's file.
      lineage_spec = list(path = files[["A2"]]),
      # decision is dropped before the read, so columns says where each one is; each row
      # is located by the file line it starts on (D12.54), a file of no rows by its header.
      precedence = list(
        path = file.path(config_dir, "precedence.csv"),
        rows = if (length(precedence$lines) > 0L) precedence$lines else 2L,
        columns = precedence_columns
      )
    ),
    column_map = gpq_column_map(lineage_flag = lineage_flag),
    type_map = gpq_type_map(file.path(config_dir, "type_map.csv")),
    sentinels = sentinels
  )
  record_hand_kept(spec, config_dir)
}

# MAGPlot's rule set (D9.20, D14.2): every rules_<component>.csv in data-raw/magp/ read clean,
# rules_rules.csv among them and none outside the rule set's components, checked by the
# engine's validator, and written for the spec set it is compiled with: rules_meta.csv's
# spec_version is the DD's file date, or the build stops (D14.15). Each component keeps its
# file's name and each row's line, as magp_rules() reads them from the copies, so the two
# rule sets are identical (D14.19).
build_magp_rules <- function(spec, config_dir = file.path("data-raw", "magp")) {
  components <- names(rule_set_schema())
  expected <- paste0("rules_", components, ".csv")
  found <- list.files(config_dir, pattern = "^rules_.*[.]csv$")
  stray <- setdiff(found, expected)
  if (length(stray) > 0L) {
    stop(
      "data-raw/magp/ has rule-set files for components a rule set doesn't have: ",
      paste(stray, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!"rules_rules.csv" %in% found) {
    stop(
      config_dir, " has no rules_rules.csv, and a rule set needs its rules component.",
      call. = FALSE
    )
  }
  given <- expected[expected %in% found]
  rules <- lapply(given, function(name) {
    read <- read_hand_kept(file.path(config_dir, name))
    set_origin(read$data, name, read$lines)
  })
  names(rules) <- sub("^rules_(.*)[.]csv$", "\\1", given)
  rules <- validate_rule_set(rules)
  dd_date <- spec$manifest$file_date[match("dictionary", spec$manifest$input)]
  written_for <- if (is.null(rules$meta)) NA_character_ else rules$meta$spec_version
  if (!identical(written_for, dd_date)) {
    stop(sprintf(
      paste0(
        "rules_meta.csv's spec_version is %s, but the specification files are dated %s: ",
        "review the rule set against them and update rules_meta.csv (update-spec)."
      ),
      written_for, dd_date
    ), call. = FALSE)
  }
  rules
}

# The rule set's files copied unchanged into the compiled folder, beside the spec.
copy_rule_set <- function(config_dir, out_dir) {
  files <- list.files(config_dir, pattern = "^rules_.*[.]csv$", full.names = TRUE)
  copied <- file.copy(files, out_dir, overwrite = TRUE)
  if (!all(copied)) {
    stop("Couldn't copy ", paste(basename(files[!copied]), collapse = ", "), ".", call. = FALSE)
  }
  invisible(file.path(out_dir, basename(files)))
}

main <- function() {
  library(data.table)
  pkgload::load_all(".", quiet = TRUE)
  config_dir <- file.path("data-raw", "magp")
  spec <- build_magp_spec(config_dir = config_dir)
  rules <- build_magp_rules(spec, config_dir)
  stopped <- tryCatch(
    {
      gpq_preflight(spec, rules = rules, output_dir = file.path("runs", "build_magp_config"))
      NULL
    },
    gpq_preflight_error = function(e) e
  )
  if (!is.null(stopped)) {
    cat(conditionMessage(stopped), "\nNothing was written to inst/extdata/magp/.\n", sep = "")
    quit(save = "no", status = 1L)
  }
  out_dir <- file.path("inst", "extdata", "magp")
  write_compiled_spec(spec, out_dir)
  copy_rule_set(config_dir, out_dir)
  cat("Compiled the specification and the rule set into inst/extdata/magp/.\n")
}

if (sys.nframe() == 0L) {
  main()
}
