# Builds the applicability matrix's first working copy (plan 5.6; D2.6, D7.16, D7.26, D8.4,
# D13.1 to D13.5): one national row per dictionary attribute, rows seeded from A2, from the
# pipeline scripts' registers, frame rules and absent links, and from the AB mapping workbook,
# and a review guide. It writes applicability_working.csv and review_guide.md to the output
# folder, never over an existing file. The default output folder is outside the repo, and the
# output folder must never be spec/.
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
  repeated_ids <- unique(contributors$contributor_id[duplicated(contributors$contributor_id)])
  if (length(repeated_ids) > 0L) {
    stop(
      "The lookup's contributor sheet repeats the contributor id ",
      paste(repeated_ids, collapse = ", "), " (D13.2 (4)).",
      call. = FALSE
    )
  }
  lost <- !datasets$contributor_id %chin% contributors$contributor_id
  if (any(lost)) {
    named <- paste0(datasets$magp_dataset_id[lost], " (", datasets$contributor_id[lost], ")")
    stop(
      "The lookup's contributor sheet lacks the contributor id of datasets ",
      paste(named, collapse = ", "), " (D13.2 (4)).",
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
  # The evidence is formatted for the keyed rows only.
  at <- which(keyed)
  evidence <- rep(NA_character_, nrow(attributes))
  evidence[at] <- sprintf(
    "DD key_type %s (%s:%d)", attributes$key_type[at], dd_file, attributes$source_row[at]
  )
  data.table::data.table(
    table_name = attributes$table_name, attribute_name = attributes$attribute_name,
    contributor = NA_character_, frame_type = "*", meas_type = "*",
    applicability = data.table::fifelse(keyed, "R", "O"), evidence = evidence,
    note = NA_character_, source = "dd"
  )
}

# A2's marked cells as contributor rows (D2.27, D9.20, D13.2 (3)).
a2_rows <- function(lineage_spec, a2_file) {
  marked <- unique(
    lineage_spec[lineage_spec$source_text %chin% names(a2_markers)],
    by = c("contributor_label", "table_name", "attribute_name")
  )
  # The note is formatted where A2 gives one only.
  noted <- which(!is.na(marked$note))
  note <- rep(NA_character_, nrow(marked))
  note[noted] <- paste0("A2: ", marked$note[noted])
  data.table::data.table(
    table_name = marked$table_name, attribute_name = marked$attribute_name,
    contributor = marked$contributor_label, frame_type = "*", meas_type = "*",
    applicability = unname(a2_markers[marked$source_text]),
    evidence = sprintf("A2 %s (%s, %s)", marked$source_text, a2_file, marked$source_cell),
    note = note, source = "a2"
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
    has_a2 = any(source == "a2"), applicability = kept(applicability, source),
    evidence = joined(evidence, "; "), note = joined(note, " | ")
  ), by = key_cols]
  # The distinct values of a key are counted from the distinct pairs of key and value.
  distinct <- unique(rows[, c(key_cols, "applicability"), with = FALSE])
  counts <- distinct[, list(n_values = .N), by = key_cols]
  groups[counts, n_values := i.n_values, on = key_cols]
  if (any(groups$n_values > 1L & !groups$has_a2)) {
    stop("Internal error: sources other than A2 disagree on a matrix key.", call. = FALSE)
  }
  clash <- groups[groups$n_values > 1L]
  # The sources' values are written out for the clashing keys only, in the rows' order.
  clashing <- rows[clash[, key_cols, with = FALSE], on = key_cols, nomatch = NULL]
  details <- clashing[, list(
    detail = paste(source_labels[source], applicability, collapse = ", ")
  ), by = key_cols]
  clash[, detail := NA_character_]
  clash[details, on = key_cols, detail := i.detail]
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
  # Each distinct label is looked up once; a row with no contributor takes "*", the last entry.
  labels <- unique(rows$contributor[!is.na(rows$contributor)])
  found <- c(lapply(labels, datasets_of, datasets = datasets), list("*"))
  ids <- found[match(rows$contributor, labels, nomatch = length(labels) + 1L)]
  rows <- rows[rep(seq_len(nrow(rows)), lengths(ids))]
  rows[, `:=`(
    jurisdiction = "*", magp_dataset_id = as.character(unlist(ids, use.names = FALSE)),
    status = "proposed"
  )]
  rows[]
}

# The M2a gate (plan 21 M2a; D7.16, D7.26): every DD attribute has one national row and every
# row names a DD attribute, every value is R, O or N, every row but a default national O has
# evidence, no key repeats, and no text of the matrix, its review rows or the guide holds a
# machine path. Problems stop the build before anything is written.
check_template <- function(template, attributes, paths = character(), guide = character()) {
  matrix <- template$matrix
  # An NA in a level column is not "*", so that row isn't national.
  national <- matrix$jurisdiction %chin% "*" & matrix$magp_dataset_id %chin% "*" &
    matrix$frame_type %chin% "*" & matrix$meas_type %chin% "*"
  dd_pairs <- paste(attributes$table_name, attributes$attribute_name)
  pairs <- paste(matrix$table_name, matrix$attribute_name)
  counts <- table(factor(pairs[national], levels = unique(dd_pairs)))
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

# ---- The pipeline scripts (task 2) ----

# The pipeline files the build reads (D13.1), each with its contributor's abbreviated name, its
# version and its register's row count verified on 2026-10-06 (D13.3 (1)). A file missing from
# --pipeline-dir stops the build; a new script version needs its row here first.
pipeline_files <- data.table::data.table(
  file = c(
    "magpv2_blocks_1-4_BC_2026data_v7.3_candidate.R",
    "magpv2_blocks_1-4_ON_2026data_v7.3_candidate.R",
    "magpv2_blocks_1-4_QUE_2026data_v7.3_candidate.R",
    "magpv2_functions_v7.2_candidate.R"
  ),
  contributor = c("BC", "ON", "QC", NA),
  version = c("v7.3", "v7.3", "v7.3", "v7.2"),
  register_rows = c(91L, 22L, 85L, NA)
)

# Where each register is built (D13.3 (1)): its top-level assignments, in bind order. A "rows"
# part is a data.table() literal or an rbindlist() of them; a "tables" part names tables whose
# every DD attribute the register lists, as BC's join at its lines 1167-1170 does.
register_parts <- data.table::data.table(
  file = pipeline_files$file[c(1L, 1L, 1L, 2L, 3L)],
  part = c(
    "bc_empty_tables", "reg_unavailable_single", "reg_unavailable_c6", "reg_unavailable",
    "reg_unavailable"
  ),
  kind = c("tables", "rows", "rows", "rows", "rows")
)

# Register reasons that don't seed O, each with the topic it's listed under (D13.3 (2)).
unseeded_reasons <- c("computed later" = "computed_later", "open" = "open")

# The frame rules of plan 5.6 (D13.4 (2), (3)): set_design_sentinels() in the functions file
# writes -9 on O and V frames for a frame's area, dimensions and limits, and -9 for baf on every
# frame but V. Each rule's line is the one line of its file holding its anchor.
frame_rules <- data.table::data.table(
  table_name = "magp_design_frames",
  attribute_name = c(
    "plot_area", "radius", "length", "width", "min_dbh", "max_dbh", "min_ht", "max_ht", "baf"
  ),
  frames = c(rep("O and V", 8L), "all but V"),
  file = pipeline_files$file[[4L]],
  anchor = c(
    rep("dt[no_area  & is.na(get(col)), (col) := -9]", 8L),
    "dt[frame_type != \"V\" & is.na(baf), baf := -9]"
  )
)

# The links absent by design (plan 5.6, 6.7; D2.28): ON's age-sample trees, tree_type "A" and
# so meas_type AGE (D2.3b), carry no subplot measurement and no frame; each rule says which
# (D13.6 (2)).
absent_links <- data.table::data.table(
  table_name = "magp_tree_meas", attribute_name = c("magp_subpmeas_id", "magp_frame_id"),
  contributor = "ON", meas_type = "AGE", file = pipeline_files$file[[2L]],
  anchor = c(
    "on_tree_meas[msr_typ == \"age\", `:=`(magp_subpmeas_id = NA_character_,",
    "AGE ROWS CARRY NO DESIGN, AND THAT IS CORRECT"
  ),
  rule = paste(
    "N for ON's age-sample trees (tree_type \"A\"), which have no",
    c("subplot measurement", "frame"), "(absent by design)"
  )
)

# A file's parse data, keyed so a node's children come in source order; nothing is run. The
# lines are parsed under the file's base name, so R's message for a script that doesn't parse
# never holds the folder (R cuts a longer file name in a message, so a swap wouldn't be safe).
parse_data <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  exprs <- parse(
    text = lines, keep.source = TRUE, srcfile = srcfilecopy(basename(path), lines),
    encoding = "UTF-8"
  )
  data <- data.table::as.data.table(utils::getParseData(exprs, includeText = TRUE))
  data.table::setkeyv(data, c("parent", "line1", "col1"))
  data
}

# A node's children, in source order.
children <- function(data, node) {
  data[list(node), nomatch = NULL]
}

# The node holding the value of the file's one top-level `name <- value`, or a stop.
assignment_value <- function(data, name, file) {
  wraps <- data$parent[data$token == "SYMBOL" & data$text == name]
  where <- match(wraps, data$id)
  tops <- data$parent[where]
  # An assignment at the top level has the file's root, 0, as its parent. The name's wrapper is
  # an expr holding the SYMBOL alone, so the name after a `$` or `@` isn't taken for it.
  distinct <- unique(wraps)
  alone <- tabulate(match(data$parent, distinct), length(distinct))[match(wraps, distinct)] == 1L
  top_level <- which(
    tops != 0L & data$parent[match(tops, data$id)] == 0L & data$token[where] == "expr" & alone
  )
  found <- integer()
  for (i in top_level) {
    kids <- children(data, tops[[i]])
    ok <- nrow(kids) == 3L && kids$id[[1L]] == wraps[[i]] && kids$text[[2L]] %chin% c("<-", "=")
    if (ok) {
      found <- c(found, kids$id[[3L]])
    }
  }
  if (length(found) != 1L) {
    stop(sprintf(
      "%s has %d top-level assignments to %s, not one (D13.3 (1)).", file, length(found), name
    ), call. = FALSE)
  }
  found
}

# A literal's value with the line of each element, read from the parse data, never run
# (D13.3 (1)): strings, numbers and NULL; c(), rep(), list(), data.table() and rbindlist().
# A vector is list(value, line); a table is a data.table with a <column>_line column for each
# column; a list is a list of those. Any other call, or a name, stops the build.
literal_value <- function(data, node, file) {
  kids <- children(data, node)
  constant <- nrow(kids) == 1L &&
    kids$token[[1L]] %chin% c("STR_CONST", "NUM_CONST", "NULL_CONST")
  if (constant) {
    text <- kids$text[[1L]]
    # The parse data holds a string of about 1000 characters or more as a placeholder, so its
    # full text is read from the source, as utils::getParseText() does from a one-row frame.
    if (kids$token[[1L]] == "STR_CONST" && startsWith(text, "[")) {
      one <- data.frame(
        line1 = kids$line1[[1L]], col1 = kids$col1[[1L]], line2 = kids$line2[[1L]],
        col2 = kids$col2[[1L]], token = "STR_CONST", text = text,
        row.names = as.character(kids$id[[1L]])
      )
      attr(one, "srcfile") <- attr(data, "srcfile")
      text <- utils::getParseText(one, kids$id[[1L]])
    }
    value <- str2lang(text)
    return(if (is.null(value)) NULL else list(value = value, line = kids$line1[[1L]]))
  }
  call <- if (nrow(kids) >= 3L) children(data, kids$id[[1L]]) else kids[0L]
  ok <- nrow(call) == 1L && call$token[[1L]] == "SYMBOL_FUNCTION_CALL" &&
    kids$token[[2L]] == "'('"
  where <- match(node, data$id)
  if (!ok) {
    held <- data$text[[where]]
    if (nchar(held) > 60L) held <- paste0(substr(held, 1L, 60L), "...")
    stop(sprintf(
      "%s line %d holds %s, which the build can't read without running it (D13.3 (1)).",
      file, data$line1[[where]], held
    ), call. = FALSE)
  }
  fun <- call$text[[1L]]
  if (!fun %chin% c("c", "rep", "list", "data.table", "rbindlist")) {
    stop(sprintf(
      "%s line %d calls %s(), which the build can't read without running it (D13.3 (1)).",
      file, call$line1[[1L]], fun
    ), call. = FALSE)
  }
  args <- call_arguments(kids, data, file)
  switch(fun,
    c = list(
      value = unlist(lapply(args, `[[`, "value"), use.names = FALSE),
      line = unlist(lapply(args, `[[`, "line"), use.names = FALSE)
    ),
    rep = literal_rep(args, call$line1[[1L]], file),
    list = Filter(Negate(is.null), unname(args)),
    data.table = literal_table(args, call$line1[[1L]], file),
    rbindlist = data.table::rbindlist(args[[1L]], use.names = TRUE, fill = TRUE)
  )
}

# A call's arguments, by name where named, each read as a literal; rbindlist()'s use.names and
# fill are dropped, since the parts are bound by name and filled anyway.
call_arguments <- function(kids, data, file) {
  inside <- kids[-c(1L, 2L, nrow(kids))]
  args <- list()
  name <- ""
  for (i in seq_len(nrow(inside))) {
    if (inside$token[[i]] == "SYMBOL_SUB") {
      name <- inside$text[[i]]
    } else if (inside$token[[i]] == "expr") {
      if (!name %chin% c("use.names", "fill")) {
        args[length(args) + 1L] <- list(literal_value(data, inside$id[[i]], file))
        names(args)[length(args)] <- name
      }
      name <- ""
    }
  }
  args
}

# rep(x, times) and rep(x, times = n), the only forms read.
literal_rep <- function(args, line, file) {
  ok <- length(args) == 2L && names(args)[[1L]] == "" && names(args)[[2L]] %chin% c("", "times")
  if (!ok) {
    stop(sprintf("%s line %d: rep() is read only as rep(x, times).", file, line), call. = FALSE)
  }
  positions <- rep(seq_along(args[[1L]]$value), times = args[[2L]]$value)
  list(value = args[[1L]]$value[positions], line = args[[1L]]$line[positions])
}

# data.table()'s named columns, a column of length 1 recycled, as the call itself would.
literal_table <- function(args, line, file) {
  lengths <- vapply(args, function(column) length(column$value), integer(1L))
  n <- max(lengths)
  if (any(names(args) == "") || any(lengths != n & lengths != 1L)) {
    stop(sprintf(
      "%s line %d: data.table() has an unnamed column or columns of unequal lengths.", file, line
    ), call. = FALSE)
  }
  columns <- list()
  for (column in names(args)) {
    columns[[column]] <- rep_len(args[[column]]$value, n)
    columns[[paste0(column, "_line")]] <- rep_len(args[[column]]$line, n)
  }
  data.table::as.data.table(columns)
}

# One script's register (D13.3 (1)): its parts read as literals and bound in order, one row per
# table and attribute, the first kept, as BC's line 1288 does. A row's line is its attribute's,
# or for a "tables" part its table's.
read_register <- function(path, parts, attributes) {
  data <- parse_data(path)
  file <- basename(path)
  rows <- lapply(seq_len(nrow(parts)), function(i) {
    value <- literal_value(data, assignment_value(data, parts$part[[i]], file), file)
    wanted <- c(if (parts$kind[[i]] == "rows") "attribute_name", "table_name", "reason")
    ok <- data.table::is.data.table(value) && all(wanted %chin% names(value))
    if (!ok) {
      stop(sprintf(
        "%s: %s isn't a table with columns %s.", file, parts$part[[i]],
        paste(wanted, collapse = ", ")
      ), call. = FALSE)
    }
    if (!"note" %chin% names(value)) {
      value[, note := NA_character_]
    }
    if (parts$kind[[i]] == "tables") {
      return(attributes[
        value,
        on = "table_name", nomatch = NULL,
        list(
          table_name, attribute_name,
          reason = i.reason, note = i.note, line = i.table_name_line
        )
      ])
    }
    value[, list(table_name, attribute_name, reason, note, line = attribute_name_line)]
  })
  rows <- data.table::rbindlist(rows, use.names = TRUE)
  unique(rows, by = c("table_name", "attribute_name"))
}

# The one line of a file holding a rule's anchor, or a stop (D13.4 (3)).
anchor_line <- function(lines, anchor, file) {
  found <- which(grepl(anchor, lines, fixed = TRUE, useBytes = TRUE))
  if (length(found) != 1L) {
    stop(sprintf(
      "%s has %d lines holding the anchor %s, not one (D13.4 (3)).", file, length(found),
      encodeString(anchor, quote = "\"")
    ), call. = FALSE)
  }
  found
}

# A pipeline evidence value: file, version, SHA-256 and line (plan 5.2, D7.16).
pipeline_evidence <- function(file, version, sha256, line, what) {
  sprintf("pipeline: %s %s sha256:%s line %d (%s)", file, version, sha256, line, what)
}

# The pipeline files in the folder, with their paths and SHA-256, or a stop naming one missing.
pipeline_sources <- function(pipeline_dir) {
  paths <- file.path(pipeline_dir, pipeline_files$file)
  absent <- pipeline_files$file[!file.exists(paths)]
  if (length(absent) > 0L) {
    stop("The pipeline folder lacks ", paste(absent, collapse = ", "), " (D13.1).", call. = FALSE)
  }
  sources <- data.table::copy(pipeline_files)
  sources[, `:=`(path = paths, sha256 = unname(tools::sha256sum(paths)))]
  sources[]
}

# Review rows: what wasn't seeded, why, and its evidence. A pair the DD lacks names the table the
# DD has the attribute in, a design table where there is one (D13.3, D13.4 (1)).
review_rows <- function(topic, contributor, rows, evidence, note, attributes) {
  absent <- !paste(rows$table_name, rows$attribute_name) %chin%
    paste(attributes$table_name, attributes$attribute_name)
  # The detail is worded for the absent pairs only, once for each distinct attribute name.
  detail <- rep(NA_character_, nrow(rows))
  at <- which(absent)
  if (length(at) > 0L) {
    names_at <- unique(rows$attribute_name[at])
    wanted <- attributes$attribute_name %chin% names_at
    listed <- split(attributes$table_name[wanted], attributes$attribute_name[wanted])
    text <- vapply(names_at, function(name) {
      found <- listed[[name]]
      if (is.null(found)) {
        return("the DD has no attribute of that name")
      }
      design <- found[found %chin% no_dataset_tables]
      if (length(design) > 0L) found <- design
      paste0("the DD has it in ", paste(found, collapse = ", "))
    }, character(1L), USE.NAMES = FALSE)
    detail[at] <- text[match(rows$attribute_name[at], names_at)]
  }
  data.table::data.table(
    topic = topic, contributor = contributor, table_name = rows$table_name,
    attribute_name = rows$attribute_name, detail = detail, evidence = evidence, note = note
  )
}

# A register's rows as seeded contributor rows and review rows (D13.3 (2), (4), D13.4 (1)).
register_seed <- function(register, source, attributes) {
  topic <- placement(register, attributes)
  topic[is.na(topic)] <- unname(unseeded_reasons[register$reason[is.na(topic)]])
  evidence <- pipeline_evidence(
    source$file, source$version, source$sha256, register$line,
    paste0("reg_unavailable: ", register$reason)
  )
  note <- blank_to_na(register$note)
  note <- data.table::fifelse(
    is.na(note), sprintf("%s register, %s", source$contributor, register$reason),
    sprintf("%s register, %s: %s", source$contributor, register$reason, note)
  )
  seeded <- is.na(topic)
  list(
    rows = data.table::data.table(
      table_name = register$table_name[seeded], attribute_name = register$attribute_name[seeded],
      contributor = source$contributor, frame_type = "*", meas_type = "*", applicability = "O",
      evidence = evidence[seeded], note = note[seeded], source = "register"
    ),
    review = review_rows(
      topic[!seeded], source$contributor, register[!seeded], evidence[!seeded], note[!seeded],
      attributes
    )
  )
}

# A rule's rows (D13.4 (3), D13.5 (1)): N with the anchored line when the pipeline folder was
# given, else the default O marked "not seeded", the rule's value in the note.
rule_rows <- function(rows, sources, what) {
  base <- data.table::data.table(
    table_name = rows$table_name, attribute_name = rows$attribute_name,
    contributor = rows$contributor, frame_type = rows$frame_type, meas_type = rows$meas_type
  )
  if (is.null(sources)) {
    return(base[, `:=`(
      applicability = "O", evidence = "not seeded: pipeline scripts not given",
      note = paste0("rule: ", rows$rule), source = "rule"
    )][])
  }
  origin <- sources[match(rows$file, sources$file)]
  text <- lapply(unique(origin$path), readLines, warn = FALSE, encoding = "UTF-8")
  names(text) <- unique(origin$path)
  line <- vapply(seq_len(nrow(rows)), function(i) {
    anchor_line(text[[origin$path[[i]]]], rows$anchor[[i]], rows$file[[i]])
  }, integer(1L))
  base[, `:=`(
    applicability = "N",
    evidence = pipeline_evidence(origin$file, origin$version, origin$sha256, line, what),
    note = rows$rule, source = "rule"
  )][]
}

# The frame-rule rows (plan 5.6, D13.4 (2), (3)), one per attribute and frame type, the frame
# types of "all but V" taken from the code list.
frame_rule_rows <- function(frame_codes, sources) {
  types <- lapply(frame_rules$frames, function(frames) {
    if (frames == "O and V") c("O", "V") else setdiff(frame_codes, "V")
  })
  rows <- frame_rules[rep(seq_len(nrow(frame_rules)), lengths(types))]
  rows[, `:=`(
    frame_type = unlist(types), contributor = NA_character_, meas_type = "*",
    rule = sprintf("N on %s frames (set_design_sentinels())", frames)
  )]
  rule_rows(rows, sources, "set_design_sentinels(): -9")
}

# The absent-link rows (plan 5.6, 6.7; D2.28).
absent_link_rows <- function(sources) {
  rows <- data.table::copy(absent_links)
  rows[, frame_type := "*"]
  rule_rows(rows, sources, "absent by design")
}

# The pipeline's rows and review rows (D13.3, D13.4): each register checked against its verified
# row count, the frame rules and the absent links; without the folder, the rules' rows marked
# "not seeded" and one review row saying what wasn't seeded (D13.5 (1)).
pipeline_seed <- function(pipeline_dir, attributes, frame_codes) {
  sources <- if (!is.null(pipeline_dir)) pipeline_sources(pipeline_dir)
  rows <- list(frame_rule_rows(frame_codes, sources), absent_link_rows(sources))
  if (is.null(sources)) {
    review <- data.table::data.table(topic = "not_seeded", detail = paste(
      "pipeline scripts not given: BC's, ON's and QC's registers, the frame rules and ON's",
      "absent links"
    ))
    return(list(rows = rows, review = list(review), sources = NULL))
  }
  review <- list()
  for (i in which(!is.na(sources$register_rows))) {
    source <- sources[i]
    # Picked outside the brackets, since register_parts has a column named file.
    picked <- register_parts$file == source$file
    parts <- register_parts[picked]
    register <- read_register(source$path, parts, attributes)
    if (nrow(register) != source$register_rows) {
      stop(sprintf(
        paste(
          "%s's register has %d rows where %d were verified (D13.3 (1)); check the script,",
          "then update pipeline_files."
        ),
        source$file, nrow(register), source$register_rows
      ), call. = FALSE)
    }
    seed <- register_seed(register, source, attributes)
    rows <- c(rows, list(seed$rows))
    review <- c(review, list(seed$review))
  }
  list(rows = rows, review = review, sources = sources)
}

# ---- The AB mapping workbook (task 3) ----

# The AB mapping workbook (D13.1, D13.3 (3)): the sheet read, the status that seeds O, the status
# listed for review, and the register's row count verified on 2026-10-06 for this file.
mapping_file <- list(
  file = "20260928_magpv2_mapping_AB.xlsx", contributor = "AB", sheet = "Mapping",
  seed_status = "unavailable", review_status = "question", register_rows = 55L
)

# The AB script v7.7's four corrections to its register (its lines 1139-1190), as data
# (D13.3 (1), (3)): rows moved to another table, then rows dropped (a blank table is any table).
# None may touch a register row, or the row would cite the workbook alone (D13.6 (3)).
mapping_moves <- data.table::data.table(
  from = "magp_plot_meas", to = "magp_subplot_meas",
  attribute_name = c(
    "magp_subpmeas_id", "src_subpmeas_id", "magp_subplot_id", "magp_frame_id", "comments"
  )
)
mapping_drops <- data.table::data.table(
  table_name = c(
    NA, "magp_plot_meas", "magp_subplot_meas", "magp_tally_trees", rep("magp_plot_meas", 10L)
  ),
  attribute_name = c(
    "n_species", "comments", "comments", "comments", "src_sph_tree", "src_dbh_cutoff",
    "src_sph_tally", "src_wsv_volume", "src_baph_tree", "src_gmer_volume", "src_biomass",
    "src_lorey_ht", "src_age", "src_leadgenus"
  )
)

# The Mapping sheet's rows (D13.3 (3)): table, attribute, status and rule, with each row's sheet
# row.
read_mapping <- function(path) {
  sheet <- raw_to_table(read_xlsx_raw(path, mapping_file$sheet))
  wanted <- c("table_name", "attribute_name", "status", "rule")
  absent <- setdiff(wanted, names(sheet))
  if (length(absent) > 0L) {
    stop(
      basename(path), "'s ", mapping_file$sheet, " sheet lacks ", paste(absent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  sheet <- sheet[, wanted, with = FALSE]
  sheet[, sheet_row := seq_len(.N) + 1L]
  sheet[]
}

# The AB register: the Mapping rows of the seed and review statuses (D13.3 (1), (3)), or a stop
# naming each row one of the AB script's corrections would move or drop (D13.6 (3)). A row a
# move doesn't touch keeps its table, so the drops are checked on the sheet's tables.
mapping_register <- function(sheet) {
  # Picked outside the brackets, where no column of the sheet can shadow the names.
  keep <- sheet$status %chin% c(mapping_file$seed_status, mapping_file$review_status)
  register <- sheet[keep]
  moved <- paste(register$table_name, register$attribute_name) %chin%
    paste(mapping_moves$from, mapping_moves$attribute_name)
  named <- mapping_drops[!is.na(mapping_drops$table_name)]
  any_table <- mapping_drops$attribute_name[is.na(mapping_drops$table_name)]
  dropped <- register$attribute_name %chin% any_table |
    paste(register$table_name, register$attribute_name) %chin%
      paste(named$table_name, named$attribute_name)
  hit <- moved | dropped
  if (any(hit)) {
    stop(
      "The AB script's corrections would change rows of the ", mapping_file$sheet,
      " sheet, whose evidence would then cite the workbook alone (D13.6 (3)): ",
      paste(sprintf(
        "row %d %s.%s (%s)", register$sheet_row[hit], register$table_name[hit],
        register$attribute_name[hit], data.table::fifelse(moved[hit], "moved", "dropped")
      ), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  register
}

# The AB register's rows as seeded contributor rows and review rows (D13.3 (3)).
mapping_seed <- function(register, workbook, attributes) {
  topic <- placement(register, attributes)
  topic[is.na(topic) & register$status == mapping_file$review_status] <- "question"
  evidence <- sprintf(
    "AB mapping: %s (%s, %s, row %d)", register$status, workbook, mapping_file$sheet,
    register$sheet_row
  )
  rule <- blank_to_na(gsub("\\s+", " ", register$rule))
  note <- data.table::fifelse(
    is.na(rule), sprintf("AB mapping, %s", register$status),
    sprintf("AB mapping, %s: %s", register$status, trimws(rule))
  )
  seeded <- is.na(topic)
  list(
    rows = data.table::data.table(
      table_name = register$table_name[seeded], attribute_name = register$attribute_name[seeded],
      contributor = mapping_file$contributor, frame_type = "*", meas_type = "*",
      applicability = "O", evidence = evidence[seeded], note = note[seeded], source = "mapping"
    ),
    review = review_rows(
      topic[!seeded], mapping_file$contributor, register[!seeded], evidence[!seeded],
      note[!seeded], attributes
    )
  )
}

# The workbook's rows and review rows (D13.3 (1), (3)): only the file whose register count was
# verified is read; without it, one review row saying AB's rows weren't seeded.
workbook_seed <- function(workbook_path, attributes) {
  if (is.null(workbook_path)) {
    review <- data.table::data.table(
      topic = "not_seeded", detail = "AB mapping workbook not given: AB's rows"
    )
    return(list(rows = list(), review = list(review), source = NULL))
  }
  workbook <- basename(workbook_path)
  if (workbook != mapping_file$file) {
    stop(sprintf(
      paste(
        "No register row count is recorded for %s, only for %s (D13.3 (1)); check it, then",
        "update mapping_file."
      ),
      workbook, mapping_file$file
    ), call. = FALSE)
  }
  register <- mapping_register(read_mapping(workbook_path))
  if (nrow(register) != mapping_file$register_rows) {
    stop(sprintf(
      "%s gives %d register rows where %d were verified (D13.3 (1)).",
      workbook, nrow(register), mapping_file$register_rows
    ), call. = FALSE)
  }
  seed <- mapping_seed(register, workbook, attributes)
  list(
    rows = list(seed$rows), review = list(seed$review),
    source = data.table::data.table(
      file = workbook, sha256 = unname(tools::sha256sum(workbook_path))
    )
  )
}

# The template: the matrix, its review rows and the sources read (plan 5.6, D13.1 to D13.5).
build_template <- function(spec, pipeline_dir = NULL, workbook_path = NULL) {
  attributes <- spec$attributes
  manifest <- spec$manifest
  datasets <- contributor_datasets(spec$code_lists)
  frame_codes <- spec$codes$code[which(
    spec$codes$table_name == "magp_design_frames" & spec$codes$attribute_name == "frame_type"
  )]
  if (length(frame_codes) == 0L) {
    stop(
      "The spec gives no codes for magp_design_frames.frame_type, so the frame rules have no ",
      "frame types (D13.4 (2)).",
      call. = FALSE
    )
  }
  a2 <- a2_rows(spec$lineage_spec, manifest$file[manifest$input == "lineage_spec"])
  misplaced <- which(!is.na(placement(a2, attributes)))
  if (length(misplaced) > 0L) {
    pairs <- unique(paste0(a2$table_name[misplaced], ".", a2$attribute_name[misplaced]))
    stop(
      "Internal error: A2 rows fall on a key, a table without dataset rows or a pair the DD ",
      "lacks: ", paste(utils::head(pairs, 3L), collapse = ", "),
      if (length(pairs) > 3L) sprintf(" and %d more", length(pairs) - 3L), ".",
      call. = FALSE
    )
  }
  pipeline <- pipeline_seed(pipeline_dir, attributes, frame_codes)
  workbook <- workbook_seed(workbook_path, attributes)
  rows <- c(
    list(national_rows(attributes, manifest$file[manifest$input == "dictionary"]), a2),
    pipeline$rows, workbook$rows
  )
  merged <- merge_rows(data.table::rbindlist(rows, use.names = TRUE))
  matrix <- dataset_rows(merged$rows, datasets)
  position <- match(
    paste(matrix$table_name, matrix$attribute_name),
    paste(attributes$table_name, attributes$attribute_name)
  )
  # The order is made outside the brackets, where no column can shadow position.
  ordering <- order(
    position, matrix$magp_dataset_id, matrix$frame_type, matrix$meas_type,
    method = "radix"
  )
  matrix <- matrix[ordering]
  review <- c(pipeline$review, workbook$review, list(merged$review))
  list(
    matrix = matrix[, matrix_columns, with = FALSE],
    review = data.table::rbindlist(review, use.names = TRUE, fill = TRUE),
    sources = list(
      spec = manifest[manifest$input %chin% c("dictionary", "code_lists", "lineage_spec")],
      pipeline = pipeline$sources, workbook = workbook$source
    )
  )
}

# ---- The files and the command line (task 4) ----

# What the code does that the build doesn't seed, and where it disagrees with the plan, read on
# 2026-10-06 (D13.4 (4)); listed in the review guide under pipeline_rule (D13.6 (1)). The guide
# prints `what` as written, so an attribute name in it is in a code span (D13.11).
code_notes <- data.table::data.table(
  table_name = c(
    "magp_design_frames", "magp_subplots", "magp_design_frames", "magp_design_frames",
    "magp_design_frames", "magp_design_frames", "magp_design_frames", "magp_design_frames",
    "magp_tree_meas"
  ),
  attribute_name = c(
    "plot_shape", "ef_change", "tag_type", "min_dbh, max_dbh, min_ht",
    "length, width, max_ht, baf", "max_ht, max_dbh", "max_dbh", "max_dbh, min_ht, max_ht",
    "magp_frame_id, magp_subpmeas_id"
  ),
  what = c(
    "\"Z\" on O and V frames, \"X\" on the others (AB, BC and ON)",
    "\"Z\" where a subplot's first frame type is O",
    "\"Z\" on AB's R and O frames and on QC's frames other than M",
    "-9 on QC's R frames' `min_dbh`, M frames' `max_dbh`, and S and M frames' `min_ht`",
    "-9 on every QC frame",
    "-9 on every AB frame for `max_ht`, and on AB's frames other than R for `max_dbh`",
    "-9 on BC's M frames",
    "ON writes -1 on its O frames, where plan 5.6 gives N",
    "the pipeline delivers NA for ON's age-sample trees, where plan 6.7 expects \"Z\""
  ),
  where = c(
    "magpv2_functions_v7.2_candidate.R lines 445-448",
    "magpv2_blocks_5-9_v7.2_candidate.R lines 897-909",
    "AB v7.7 line 2329; QUE v7.3 line 3838",
    "QUE v7.3 lines 751-753",
    "QUE v7.3 line 3836",
    "AB v7.7 lines 2676, 2683",
    "BC v7.3 lines 3219, 5491",
    "ON v7.3 line 3264",
    "magpv2_blocks_5-9_v7.2_candidate.R lines 1475-1481"
  )
)

# The review guide's headings for the review rows, in the order they're listed (D13.5 (2)); the
# last lists code_notes (D13.6 (1)).
review_topics <- c(
  clash = "Clashes: A2 kept over a register (D13.2 (2))",
  key = "Not seeded: keys, R nationally (D13.3 (4))",
  design_table = "Not seeded: tables that take no dataset rows (D13.4 (1))",
  not_in_dd = "Not seeded: pairs the DD doesn't have",
  open = "Not seeded: \"open\" in a register (D13.3 (2))",
  question = "Not seeded: \"question\" in the AB workbook (D13.3 (3))",
  computed_later = "Not seeded: computed by MAGPlot, \"computed later\" in a register (D13.3 (2))",
  not_seeded = "Not seeded: sources not given (D13.5 (1))",
  pipeline_rule = "Not seeded: rules seen in the pipeline code beyond plan 5.6's list (D13.4 (4))"
)

# The guide's fixed text (D8.23, D13.5 (2)), one element per line of the file.
guide_text <- c(
  "## What a row says",
  "",
  paste(
    "A row gives one attribute's applicability for the records its four keys match:",
    "`jurisdiction`, `magp_dataset_id`, `frame_type` and `meas_type`, where `*` matches every",
    "record. `applicability` is R (required), O (optional) or N (not applicable). Every DD",
    "attribute has one national row, its four keys `*`. Where several rows match a record,",
    "the most specific wins, and `meas_type` rows that still disagree combine as R beats O beats",
    "N (plan 5.4). Two rows that neither is more specific than the other and that disagree are",
    "an error (plan 5.4), e.g. one keyed (BC, `*`) and one keyed (`*`, 110.05) on the same",
    "attribute with different values. Every row starts with `status` \"proposed\"."
  ),
  "",
  "## Where a value comes from",
  "",
  "`evidence` says where a value comes from, and `note` adds the source's own words:",
  "",
  "- `DD key_type PK` or `FK`: a primary or foreign key, R nationally.",
  paste(
    "- `A2 X` or `A2 -1`: the contributor doesn't collect it, O for each of its datasets.",
    "`A2 Z` or `A2 -9`: it doesn't apply to the contributor, N."
  ),
  paste(
    "- `pipeline: <file> <version> sha256:<hash> line <n> (reg_unavailable: <reason>)`: the",
    "script's list of what its contributor doesn't supply, O for each of the contributor's",
    "datasets."
  ),
  paste(
    "- `pipeline: ... (set_design_sentinels(): -9)`: the pipeline's frame rule, N on the frame",
    "type the row names."
  ),
  "- `pipeline: ... (absent by design)`: a link the pipeline leaves out on purpose, N.",
  paste(
    "- `AB mapping: unavailable (<workbook>, Mapping, row <n>)`: AB's mapping workbook, O for",
    "each of AB's datasets."
  ),
  paste(
    "- `not seeded: ...`: the source wasn't given to this run; the row holds the default O, and",
    "its note gives the value the rule would give."
  ),
  "- No evidence: the default O (D8.10), for you to confirm or change.",
  "",
  paste(
    "Where two sources give the same row, their evidence is joined with \"; \" and their notes",
    "with \" | \". Where they disagreed, A2's value was kept, and the row is listed under",
    "\"Clashes\" below."
  ),
  "",
  "## What to do",
  "",
  paste(
    "1. Check the seeded rows against what you know of each contributor, and change",
    "`applicability` where it's wrong. You can set `status` to \"confirmed\" on a row you've",
    "settled, to track your progress; sign-off doesn't require it."
  ),
  "2. Go through the national O rows: an attribute every record must carry becomes R.",
  paste(
    "3. Add a row where a dataset, a frame type or a component differs from the national value,",
    "its keys spelled as the lookup spells them."
  ),
  paste(
    "4. Decide each item listed under \"For your review\". The build left most of them out; a",
    "clash is a row it did write, with A2's value. If a source is listed under \"Not seeded:",
    "sources not given\", rebuild with its `--pipeline-dir` or `--workbook` before you start",
    paste0(
      "editing (move `", output_files[["matrix"]], "` and `", output_files[["guide"]],
      "` away first)."
    ),
    "Once you have edits, don't rebuild: set the frame-rule and link rows by hand from their",
    "notes; register and workbook rows can't be recovered that way and need a rebuild into a",
    "separate folder (`--output-dir`) to copy from."
  ),
  paste(
    "5. Edit the file in a text editor, or in Excel through Data > From Text/CSV with every",
    "column set to Text, and save it as CSV UTF-8 with the same ten columns in the same order.",
    "Opened with a double-click, Excel reads dataset 110.10 as the number 110.1 and saves it as",
    "110.1."
  ),
  "",
  paste(
    "At M9 the matrix checks run on the file (plan 5.7): every attribute is a DD attribute with",
    "one national row, every key and value is valid, no key repeats, primary keys are R, and",
    "the combinations the spec knows resolve to one value. Sign-off marks the matrix confirmed",
    "(plan 21 M9): every item under \"For your review\" is decided, and seeded rows are accepted",
    "on their evidence unless you change them. Once you sign it off, it enters `spec/` through",
    "`update-spec`."
  )
)

# The kind of a row's first evidence, for the guide's counts.
evidence_kind <- function(evidence) {
  data.table::fcase(
    is.na(evidence), "no evidence (the default O)",
    startsWith(evidence, "DD key_type"), "DD key_type",
    startsWith(evidence, "A2 "), "A2",
    startsWith(evidence, "AB mapping"), "AB mapping",
    startsWith(evidence, "not seeded"), "not seeded",
    grepl("^pipeline: [^;]*reg_unavailable", evidence), "pipeline: reg_unavailable",
    grepl("^pipeline: [^;]*set_design_sentinels", evidence), "pipeline: set_design_sentinels()",
    grepl("^pipeline: [^;]*absent by design", evidence), "pipeline: absent by design",
    default = "other"
  )
}

# Text as Markdown shows it: runs of whitespace folded to one space, so a line break can't start
# a list item or a heading, and the characters Markdown reads as markup escaped.
markdown_text <- function(x) {
  gsub("([\\\\`*_{}\\[\\]<>|#&])", "\\\\\\1", gsub("[[:space:]]+", " ", x), perl = TRUE)
}

# Text in a code span, or escaped where it holds a backtick, which would end the span.
code_span <- function(x) {
  data.table::fifelse(grepl("`", x, fixed = TRUE), markdown_text(x), paste0("`", x, "`"))
}

# The guide's lines for this run: its sources, its counts and the rows it didn't seed.
run_lines <- function(template) {
  matrix <- template$matrix
  review <- template$review
  sources <- template$sources
  stray <- setdiff(unique(review$topic), names(review_topics))
  if (length(stray) > 0L) {
    stop(
      "The review rows hold topics the guide doesn't list: ", paste(stray, collapse = ", "), ".",
      call. = FALSE
    )
  }
  # The evidence list's names, in its order, with evidence_kind()'s other last.
  kinds <- c(
    "DD key_type", "A2", "pipeline: reg_unavailable", "pipeline: set_design_sentinels()",
    "pipeline: absent by design", "AB mapping", "not seeded", "no evidence (the default O)",
    "other"
  )
  counts <- table(
    factor(evidence_kind(matrix$evidence), levels = kinds),
    factor(matrix$applicability, levels = c("R", "O", "N"))
  )
  counts <- counts[rowSums(counts) > 0L, , drop = FALSE]
  source_line <- function(file, sha256, version = "") {
    sprintf("- `%s`%s, SHA-256 `%s`", file, version, sha256)
  }
  national <- matrix$jurisdiction == "*" & matrix$magp_dataset_id == "*" &
    matrix$frame_type == "*" & matrix$meas_type == "*"
  # A row holds two sources where a "; " is followed by the start of a source's evidence; a
  # script's own reason may hold a "; " too.
  joined <- grepl("; (DD key_type|A2 |pipeline: |AB mapping: |not seeded: )", matrix$evidence)
  lines <- c(
    "## This run", "", "Sources:", "",
    source_line(sources$spec$file, sources$spec$sha256),
    if (is.null(sources$pipeline)) {
      "- pipeline scripts: not given"
    } else {
      source_line(
        sources$pipeline$file, sources$pipeline$sha256, paste0(" ", sources$pipeline$version)
      )
    },
    if (is.null(sources$workbook)) {
      "- AB mapping workbook: not given"
    } else {
      source_line(sources$workbook$file, sources$workbook$sha256)
    },
    "",
    sprintf(
      "Rows: %d, %d of them national; %d hold two sources.", nrow(matrix), sum(national),
      sum(joined)
    ),
    "",
    "Rows by first evidence and applicability (a row with two sources counts under its first):",
    "", "| Evidence | R | O | N |", "|---|---|---|---|",
    sprintf("| %s | %d | %d | %d |", rownames(counts), counts[, "R"], counts[, "O"], counts[, "N"]),
    "", "## For your review"
  )
  for (heading in names(review_topics)) {
    # Picked outside the brackets, since review has a column named topic.
    picked <- review$topic == heading
    rows <- if (heading == "pipeline_rule") code_notes else review[picked]
    count <- nrow(rows)
    noun <- if (count == 1L) "item" else "items"
    lines <- c(lines, "", sprintf("### %s, %d %s", review_topics[[heading]], count, noun), "")
    if (count == 0L) {
      lines <- c(lines, "None.")
      next
    }
    if (heading == "pipeline_rule") {
      scripts <- c(
        "AB v7.7 = `magpv2_blocks_1-4_AB_2026data_v7.7_candidate.R` (contributor AB)",
        "BC v7.3 = `magpv2_blocks_1-4_BC_2026data_v7.3_candidate.R` (contributor BC)",
        "ON v7.3 = `magpv2_blocks_1-4_ON_2026data_v7.3_candidate.R` (contributor ON)",
        "QUE v7.3 = `magpv2_blocks_1-4_QUE_2026data_v7.3_candidate.R` (contributor QC)"
      )
      lines <- c(lines, paste0("Scripts: ", paste(scripts, collapse = "; "), "."), "")
    }
    items <- if (heading == "not_seeded") {
      paste0("- ", markdown_text(rows$detail))
    } else if (heading == "pipeline_rule") {
      paste0(
        "- ", code_span(rows$table_name), " ", code_span(rows$attribute_name), ": ",
        rows$what, " (", code_span(rows$where), ")"
      )
    } else {
      paste0(
        "- `", rows$table_name, ".", rows$attribute_name, "`, ", rows$contributor,
        data.table::fifelse(is.na(rows$detail), "", paste0(": ", markdown_text(rows$detail))),
        ". Evidence: ", code_span(rows$evidence),
        data.table::fifelse(is.na(rows$note), "", paste0(". Note: ", markdown_text(rows$note)))
      )
    }
    lines <- c(lines, items)
  }
  lines
}

# The review guide (D8.23, D13.5 (2)): the fixed text, then this run's sources, counts and the
# rows the build didn't seed.
review_guide <- function(template, date = Sys.Date()) {
  c(
    "# Applicability matrix: review guide",
    "",
    paste(
      "Written by `data-raw/build_matrix_template.R` on", format(date), "beside",
      paste0("`", output_files[["matrix"]], "`,"), "the working copy you fill and confirm",
      "before M9 (plan 5.6, D7.26). Nothing reads the working copy before M9, so every QC run",
      "until then is a run without a matrix (D7.3)."
    ),
    "",
    guide_text,
    "",
    run_lines(template)
  )
}

# A stop naming the output files already there, so nothing is written over them (D13.5 (3)).
refuse_existing <- function(paths) {
  present <- paths[file.exists(paths)]
  if (length(present) > 0L) {
    stop(
      paste(present, collapse = " and "),
      if (length(present) == 1L) " already exists" else " already exist",
      ", so nothing was written (D13.5 (3)); move or delete ",
      if (length(present) == 1L) "it" else "them", " to build again.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# The working copy and the guide, UTF-8 with LF line ends, the output folder made if missing; the
# paths come back named as output_files is.
write_template <- function(template, guide, output_dir) {
  paths <- file.path(output_dir, output_files)
  names(paths) <- names(output_files)
  refuse_existing(paths)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) {
    stop("Couldn't create the output folder ", output_dir, ".", call. = FALSE)
  }
  data.table::fwrite(
    template$matrix, paths[["matrix"]],
    na = "", quote = TRUE, eol = "\n", encoding = "UTF-8"
  )
  writeBin(charToRaw(enc2utf8(paste0(paste(guide, collapse = "\n"), "\n"))), paths[["guide"]])
  invisible(paths)
}

# The command line: --pipeline-dir=, --workbook= and --output-dir=, each at most once; the
# output folder defaults to matrix/ in GPQ_PLANS_DIR, and with neither given the build stops
# rather than guess a path (CLAUDE.md "Before you start").
parse_args <- function(args, plans_dir = Sys.getenv("GPQ_PLANS_DIR")) {
  known <- c("--pipeline-dir", "--workbook", "--output-dir")
  given <- list()
  for (arg in args) {
    parts <- regmatches(arg, regexec("^(--[a-z-]+)=(.+)$", arg))[[1L]]
    ok <- length(parts) == 3L && parts[[2L]] %chin% known && is.null(given[[parts[[2L]]]])
    if (!ok) {
      stop(
        "Unknown or repeated argument ", encodeString(arg, quote = "\""), "; give ",
        paste0(known, "=<path>", collapse = ", "), ", each at most once.",
        call. = FALSE
      )
    }
    given[[parts[[2L]]]] <- parts[[3L]]
  }
  output_dir <- given[["--output-dir"]]
  if (is.null(output_dir)) {
    if (!nzchar(plans_dir)) {
      stop("GPQ_PLANS_DIR is unset and no --output-dir was given.", call. = FALSE)
    }
    output_dir <- file.path(plans_dir, "matrix")
  }
  list(
    pipeline_dir = given[["--pipeline-dir"]], workbook = given[["--workbook"]],
    output_dir = output_dir
  )
}

# Paths made absolute: one with no drive letter, slash, backslash or tilde gets getwd() first.
absolute_paths <- function(paths) {
  # normalizePath() leaves a missing relative path as it is off Windows, hence the join (D13.12).
  paths <- data.table::fifelse(
    grepl("^([A-Za-z]:|[/\\\\~])", paths), paths, file.path(getwd(), paths)
  )
  normalizePath(paths, winslash = "/", mustWork = FALSE)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  library(data.table)
  pkgload::load_all(".", quiet = TRUE)
  given <- parse_args(args)
  refuse_existing(file.path(given$output_dir, output_files))
  spec <- magp_spec()
  template <- build_template(spec, given$pipeline_dir, given$workbook)
  guide <- review_guide(template)
  # Absolute forms only, so a short relative folder can't match the guide's own words.
  given_paths <- absolute_paths(unlist(given, use.names = FALSE))
  check_template(template, spec$attributes, given_paths, guide)
  paths <- write_template(template, guide, given$output_dir)
  # The console says what wasn't seeded, in the guide's own words (D13.5 (1)).
  review <- template$review
  not_seeded <- review$detail[review$topic %chin% "not_seeded"]
  if (length(not_seeded) > 0L) {
    message(paste(not_seeded, collapse = "\n"))
  }
  cat(sprintf(
    "Wrote %d rows to %s, and the review guide beside it.\n", nrow(template$matrix),
    paths[["matrix"]]
  ))
}

if (sys.nframe() == 0L) {
  main()
}
