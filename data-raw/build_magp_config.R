# Compiles MAGPlot's specification and rule set (plan 3.5, 3.6, 4.4; D5.12, D9.20, D12.15,
# D12.20, D12.22, D12.25, D14.2, D14.15). Reads the one dated file of each kind in spec/ and
# the hand-kept files in data-raw/magp/, applies spec_exceptions.csv to the dictionary, runs
# pre-flight on the spec and the rule set (its report under runs/build_magp_config/), and
# writes inst/extdata/magp/ only when pre-flight doesn't stop: the compiled spec, the rule
# set's rules_<component>.csv files copied unchanged, and MAGPlot's evidence files,
# pipeline_computed.csv and crosswalk_provider_columns.csv, copied unchanged once every row
# checks against the spec (D19.59). It never writes data-raw/magp/ or spec/.
# Run from the repo root: Rscript data-raw/build_magp_config.R

# MAGPlot's settings that aren't tables (D12.20), named after the arguments they fill
# (D12.35).
# The ID marker: the 13 src_*_id attributes (D9.18, D12.13).
id_pattern <- "^src_.*_id$"
# The site-ID bands, v2 only (D12.9, D12.10, D12.13), and the column they band: a site
# reaches its contributor's ID through its dataset (D18.2).
id_bands <- list(
  sheet = "magp_site_id_range", label_col = "contributor", start_col = "v2_start",
  end_col = "v2_end", labels_from = c(sheet = "contributor", column = "abbreviated_name"),
  reserved_pattern = "^reserved_[0-9]+$", table = "magp_sites", attribute = "magp_site_id",
  key = "magp_dataset_id > magp_contributor_id"
)
# The DD column and value flagging A2 attributes (D12.13).
lineage_flag <- c(column = "appendix", value = "A2")
# What A2's composite IDs join their parts with (D2.14, D19.12).
lineage_separator <- "_"

spec_file_pattern <- "^([0-9]{8})_magpv2_(.+)[.](xlsx|csv)$"

# MAGPlot's evidence files and their columns (plan 3.6; D19.38, D19.59): the build checks each
# against the spec and copies it unchanged into the compiled folder, where R/magp_evidence.R
# reads it.
evidence_files <- list(
  pipeline_computed.csv = c(
    "table_name", "attribute_name", "jurisdiction", "derivation", "note"
  ),
  crosswalk_provider_columns.csv = c(
    "crosswalk", "attribute_name", "jurisdiction", "provider_column"
  )
)

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

# MAGPlot's documented one-to-many IDs are A2's sheet one_to_many, once the workbook has one
# (D18.3, D18.36): its columns contributor, table_name, attribute_name and reason become the
# engine's contributor_label, table_name, attribute_name and note. NULL without the sheet;
# else list(data, origin), the origin placing each row on its sheet row and column.
build_one_to_many_input <- function(path, sheet = "one_to_many") {
  if (!sheet %in% workbook_sheets(path)) {
    return(NULL)
  }
  raw <- raw_to_table(read_xlsx_raw(path, sheet))
  taken <- c(
    contributor = "contributor_label", table_name = "table_name",
    attribute_name = "attribute_name", reason = "note"
  )
  absent <- setdiff(names(taken), names(raw))
  if (length(absent) > 0L) {
    stop(
      "A2's sheet ", sheet, " has no column named ", paste(absent, collapse = " or "), ".",
      call. = FALSE
    )
  }
  columns <- match(names(taken), names(raw))
  names(columns) <- unname(taken)
  data <- raw[, names(taken), with = FALSE]
  data.table::setnames(data, unname(taken))
  list(data = data, origin = list(path = path, sheet = sheet, rows = 2L, columns = columns))
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
# (D12.33): the rows of precedence.csv, key_pairs.csv, reach.csv and lineage_when.csv, which
# their origins give, and a row for spec_exceptions.csv, input dictionary:exceptions, since
# the build applies it to the DD before the read and the engine doesn't know.
record_hand_kept <- function(spec, config_dir) {
  repo_path <- function(name) paste0("data-raw/magp/", name)
  exceptions <- manifest_row(
    "dictionary:exceptions", file.path(config_dir, "spec_exceptions.csv")
  )
  data.table::set(exceptions, j = "file", value = repo_path("spec_exceptions.csv"))
  # rbindlist() makes a new table, set here, so the caller's spec keeps its manifest.
  manifest <- data.table::rbindlist(list(spec$manifest, exceptions))
  for (input in intersect(c("precedence", "key_pairs", "reach", "lineage_when"), manifest$input)) {
    row <- match(input, manifest$input)
    data.table::set(manifest, i = row, j = "file", value = repo_path(paste0(input, ".csv")))
  }
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
  # The eight hand-kept files read through config() are all read before any spec file is
  # opened; type_map.csv is read last, through gpq_type_map(path), which carries its findings
  # on to pre-flight.
  config <- function(name) read_hand_kept(file.path(config_dir, name))
  exceptions <- config("spec_exceptions.csv")$data
  non_code_sheets <- config("non_code_sheets.csv")$data$sheet
  columns <- config("crosswalk_columns.csv")$data
  precedence <- config("precedence.csv")
  key_pairs <- config("key_pairs.csv")
  reach <- config("reach.csv")
  lineage_when <- config("lineage_when.csv")
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
  # A2's documented one-to-many IDs, NULL until A2 has the sheet (D18.3).
  one_to_many <- build_one_to_many_input(files[["A2"]])
  spec <- gpq_read_spec(
    dictionary = dictionary, code_lists = files[["Lookup_Tables"]],
    non_code_sheets = non_code_sheets, datasets = files[["datasets"]],
    lineage_spec = build_lineage_input(read_xlsx_raw(files[["A2"]], sheet), sheet),
    lineage_when = lineage_when$data, lineage_separator = lineage_separator,
    crosswalks = if (length(crosswalks) > 0L) crosswalks,
    id_pattern = id_pattern, id_bands = id_bands,
    precedence = precedence$data[, taken, with = FALSE], key_pairs = key_pairs$data,
    one_to_many = one_to_many$data, reach = reach$data,
    origins = c(list(
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
      ),
      # Each key pair is located by the line it starts on, as the precedence rows are.
      key_pairs = list(
        path = file.path(config_dir, "key_pairs.csv"),
        rows = if (length(key_pairs$lines) > 0L) key_pairs$lines else 2L
      ),
      reach = list(
        path = file.path(config_dir, "reach.csv"),
        rows = if (length(reach$lines) > 0L) reach$lines else 2L
      ),
      lineage_when = list(
        path = file.path(config_dir, "lineage_when.csv"),
        rows = if (length(lineage_when$lines) > 0L) lineage_when$lines else 2L
      )
    ), if (!is.null(one_to_many)) list(one_to_many = one_to_many$origin)),
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
      config_dir, " has rule-set files for components a rule set doesn't have: ",
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

# The rule set's files copied unchanged into the compiled folder, beside the spec. A copy
# there whose file has left the config folder is removed first, so magp_rules() never reads
# a component the build didn't validate and pre-flight.
copy_rule_set <- function(config_dir, out_dir) {
  files <- list.files(config_dir, pattern = "^rules_.*[.]csv$", full.names = TRUE)
  copies <- list.files(out_dir, pattern = "^rules_.*[.]csv$", full.names = TRUE)
  stale <- copies[!basename(copies) %in% basename(files)]
  unlink(stale)
  if (any(file.exists(stale))) {
    stop(
      "Couldn't remove ", paste(basename(stale[file.exists(stale)]), collapse = ", "),
      " from ", out_dir, ".",
      call. = FALSE
    )
  }
  copied <- file.copy(files, out_dir, overwrite = TRUE)
  if (!all(copied)) {
    stop("Couldn't copy ", paste(basename(files[!copied]), collapse = ", "), ".", call. = FALSE)
  }
  invisible(file.path(out_dir, basename(files)))
}

# MAGPlot's evidence files (plan 3.6, 10.3; D19.38, D19.59), each read clean and every row
# checked against the spec: in pipeline_computed.csv a table and attribute of the dictionary,
# a derivation of plan 3.6's six and a jurisdiction "*" or a code of the jurisdiction sheet;
# in crosswalk_provider_columns.csv a translation table the spec reads, an attribute whose
# code list is that table, a column of that table and a code of the jurisdiction sheet; in
# neither a row given twice. A file without exactly its columns stops the build, and so does
# a bad row, each problem named by the file line its row starts on, before anything is
# written. Returns the two tables, named by file.
build_magp_evidence <- function(spec, config_dir = file.path("data-raw", "magp")) {
  # Several columns as one key, as the engine joins names (R/check_input.R).
  key <- function(...) paste(..., sep = "\r")
  codes <- spec$codes
  jurisdictions <- codes$code[
    codes$source_type %in% "sheet" & codes$source_name %in% "jurisdiction"
  ]
  # One kind of problem: the line of each row that has it, and its text.
  problem <- function(lines, failed, text) {
    data.table::data.table(line = lines[failed], text = text[failed])
  }
  # A row whose key an earlier row has, its text naming that row's line ("%d").
  repeated <- function(lines, row_key, text) {
    first <- match(row_key, row_key)
    problem(lines, first < seq_along(row_key), sprintf(text, lines[first]))
  }
  read_checked <- function(name, problems) {
    path <- file.path(config_dir, name)
    read <- read_hand_kept(path)
    columns <- evidence_files[[name]]
    if (!identical(names(read$data), columns)) {
      stop(
        path, " must have exactly the columns ", paste(columns, collapse = ", "),
        ", so nothing was built.",
        call. = FALSE
      )
    }
    found <- problems(read$data, read$lines)
    if (nrow(found) > 0L) {
      found <- found[order(found$line)]
      stop(
        path, " has rows the build can't take, so nothing was built: ",
        paste0("line ", found$line, ": ", found$text, collapse = "; "), ".",
        call. = FALSE
      )
    }
    read$data
  }
  computed <- read_checked("pipeline_computed.csv", function(rows, lines) {
    attributes <- spec$attributes
    derivations <- c("id", "class", "formula", "count", "fill", "reference")
    data.table::rbindlist(list(
      problem(
        lines,
        !key(rows$table_name, rows$attribute_name) %in%
          key(attributes$table_name, attributes$attribute_name),
        sprintf(
          "the dictionary has no attribute %s in table %s",
          quote_value(rows$attribute_name), quote_value(rows$table_name)
        )
      ),
      problem(
        lines, !rows$derivation %in% derivations,
        sprintf(
          "derivation %s isn't %s or %s", quote_value(rows$derivation),
          paste(derivations[-length(derivations)], collapse = ", "),
          derivations[[length(derivations)]]
        )
      ),
      problem(
        lines, !rows$jurisdiction %in% c("*", jurisdictions),
        sprintf(
          "jurisdiction %s is neither \"*\" nor a code of the jurisdiction sheet",
          quote_value(rows$jurisdiction)
        )
      ),
      repeated(
        lines, key(rows$table_name, rows$attribute_name, rows$jurisdiction),
        "line %d already gives this table, attribute and jurisdiction"
      )
    ))
  })
  provider <- read_checked("crosswalk_provider_columns.csv", function(rows, lines) {
    # A translation table's header cells name its columns.
    headers <- spec$crosswalks[spec$crosswalks$source_row %in% 1L]
    lists <- spec$code_list_map[spec$code_list_map$source_type %in% "crosswalk"]
    read_by_spec <- rows$crosswalk %in% headers$crosswalk
    data.table::rbindlist(list(
      problem(
        lines, !read_by_spec,
        sprintf(
          "the specification reads no translation table %s", quote_value(rows$crosswalk)
        )
      ),
      # An attribute and a column are looked for only in a table the specification reads.
      problem(
        lines,
        read_by_spec & !key(rows$crosswalk, rows$attribute_name) %in%
          key(lists$source_name, lists$attribute_name),
        sprintf(
          "attribute %s doesn't take its codes from translation table %s",
          quote_value(rows$attribute_name), quote_value(rows$crosswalk)
        )
      ),
      problem(
        lines,
        read_by_spec & !key(rows$crosswalk, rows$provider_column) %in%
          key(headers$crosswalk, headers$crosswalk_column),
        sprintf(
          "translation table %s has no column %s", quote_value(rows$crosswalk),
          quote_value(rows$provider_column)
        )
      ),
      problem(
        lines, !rows$jurisdiction %in% jurisdictions,
        sprintf(
          "jurisdiction %s isn't a code of the jurisdiction sheet", quote_value(rows$jurisdiction)
        )
      ),
      repeated(
        lines, key(rows$crosswalk, rows$attribute_name, rows$jurisdiction, rows$provider_column),
        "the row repeats line %d"
      )
    ))
  })
  list(pipeline_computed = computed, crosswalk_provider_columns = provider)
}

# The evidence files copied unchanged into the compiled folder, beside the spec.
copy_evidence <- function(config_dir, out_dir) {
  files <- file.path(config_dir, names(evidence_files))
  copied <- file.copy(files, out_dir, overwrite = TRUE)
  if (!all(copied)) {
    stop("Couldn't copy ", paste(basename(files[!copied]), collapse = ", "), ".", call. = FALSE)
  }
  invisible(file.path(out_dir, names(evidence_files)))
}

main <- function() {
  library(data.table)
  pkgload::load_all(".", quiet = TRUE)
  config_dir <- file.path("data-raw", "magp")
  spec <- build_magp_spec(config_dir = config_dir)
  rules <- build_magp_rules(spec, config_dir)
  build_magp_evidence(spec, config_dir)
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
  copy_evidence(config_dir, out_dir)
  cat("Compiled the specification, the rule set and the evidence files into inst/extdata/magp/.\n")
}

if (sys.nframe() == 0L) {
  main()
}
