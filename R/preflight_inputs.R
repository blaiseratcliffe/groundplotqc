# Pre-flight's run inputs (plan 3.6, 4.2; D19.51, D23.33): the optional tables
# gpq_preflight(inputs = ) takes, each a data.frame or a CSV path read as the specification's
# inputs are (the earlier run, previous, also a run folder, read for its columns only), and the
# three checks of them: input_unparseable (a stop), override_scope_conflict and
# override_unknown_name (warnings). gpq_classify_issues() reads its overrides and evidence
# through the same reader (R/lineage_classify.R, R/lineage_evidence.R).

#' The run inputs pre-flight takes, with their own columns (3.6; D19.51, D23.33)
#'
#' `overrides` (10.3 step 1) may also hold group columns, each an attribute of the
#' specification; `lineage` is the raw-value table (10.2); `pipeline_fill` has the `filled`
#' evidence's columns (3.6); `previous`, an earlier run, a run folder or an issues table (9.3),
#' has the columns the comparison reads (gpq_compare_runs()), and may hold any other column of
#' the issues or attribute of the specification; `acknowledgements`, your register, has 11.8's
#' columns and may also hold group columns, each an attribute of the specification (D23.26). The
#' other inputs of 3.6's list join with their milestones.
#' @noRd
run_input_schema <- function() {
  list(
    overrides = c(
      "fingerprint", "rule_id", "table_name", "column_name", "issue_class", "who", "when", "why"
    ),
    lineage = c("table_name", "record_pk", "column_name", "raw_value", "raw_source"),
    pipeline_fill = c("table_name", "record_pk", "column_name", "written_value"),
    previous = c(
      "run_id", "run_scope", "rule_id", "rule_fingerprint", "table_name", "fingerprint",
      "fingerprint_pk", "first_seen_run"
    ),
    acknowledgements = c(
      "fingerprint", "fingerprint_pk", "rule_id", "table_name", "column_name", "provider_key",
      "acknowledged_value", "decision", "who", "when", "why", "run_scope"
    ),
    # The attribution map (10.4) is also keyed by one group column its caller names (MAGPlot:
    # jurisdiction, D23.27), an attribute of the specification, so it isn't among its own.
    attribution_map = c(
      "table_name", "attribute_name", "script_file", "script_block", "script_commit", "note"
    )
  )
}

#' A named list of run inputs, each read as text (D19.51)
#'
#' `x` is a list named by input, each name one of `taken` and given once; each element a
#' data.frame or the path of a CSV file, read by read_input_table(): every cell text, a blank
#' NA, and a file's reading problems kept as findings. `previous` may also be a run folder, and
#' is read for its form, a folder's files and run outcome and its issues' columns, never their
#' rows, which gpq_compare_runs() reads (D23.33): its `data` has no rows, and a folder that isn't
#' a completed run's is its finding. Anything else is a caller's error naming `arg`, or, for
#' `previous`, naming it. Returns the reads, named as `x` is; the caller's tables are never
#' changed.
#' @noRd
read_run_inputs <- function(x, arg, taken) {
  shaped <- is.list(x) && !is.data.frame(x) && (length(x) == 0L || (
    !is.null(names(x)) && !anyNA(names(x)) && all(nzchar(names(x))) && !anyDuplicated(names(x))
  ))
  if (!shaped) {
    # W200
    stop(
      sprintf("`%s` must be a list of data.frames or CSV paths, each named once.", arg),
      call. = FALSE
    )
  }
  given <- names(x)
  bad <- setdiff(given, taken)
  if (length(bad) > 0L) {
    n <- length(taken)
    listed <- paste(paste(taken[-n], collapse = ", "), "and", taken[[n]])
    # W201
    stop(sprintf(
      "`%s` names tables it doesn't take: %s. It takes %s.", arg,
      paste(mark_invalid_utf8(bad), collapse = ", "), listed
    ), call. = FALSE)
  }
  # An earlier run, as read_input_table() reads an input: its issues' header as `data`, with no
  # rows, and a folder's problem as its finding (D23.46).
  previous_read <- function(previous) {
    folder <- is.character(previous) && length(previous) == 1L && !is.na(previous) &&
      dir.exists(previous)
    if (!folder && !is.data.frame(previous) && !is_path_to(previous, "csv")) {
      # M8a1 W15
      stop(
        "`previous` must be a run folder, a data.frame or the path of an existing .csv file.",
        call. = FALSE
      )
    }
    # The header alone, its first line read as a CSV's: fread() given the whole file would read
    # its rows to place the header.
    header <- function(path) {
      first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
      columns <- if (length(first) == 0L) {
        character()
      } else {
        names(fread(
          text = first, sep = ",", header = TRUE, colClasses = "character",
          encoding = "UTF-8", showProgress = FALSE
        ))
      }
      mark_invalid_utf8(gsub("\"\"", "\"", columns, fixed = TRUE))
    }
    problem <- NULL
    if (folder) {
      inside <- c("metadata/run.csv", "tables/issues.csv", "tables/results.csv")
      lacking <- inside[!file.exists(file.path(previous, inside))]
      if (length(lacking) > 0L) {
        problem <- report_text(
          "preflight_detail_input_unparseable_folder",
          input = "previous", folder = basename(previous), file = lacking[[1L]]
        )
      } else {
        run <- read_run_csv(file.path(previous, "metadata", "run.csv"), metadata_schema()$run)
        if (!identical(run$outcome, "completed")) {
          problem <- report_text(
            "preflight_detail_input_unparseable_stopped",
            input = "previous", folder = basename(previous)
          )
        }
      }
      columns <- if (is.null(problem)) {
        header(file.path(previous, "tables", "issues.csv"))
      } else {
        run_input_schema()$previous
      }
      where <- list(kind = "csv", file = "issues.csv", row_map = 1L)
      # The run folder by its name, its run ID, not hashed: its issues file can be large.
      manifest <- data.table(
        input = "previous", file = basename(previous), file_date = NA_character_,
        sha256 = NA_character_
      )
    } else if (is.data.frame(previous)) {
      columns <- names(previous)
      where <- list(kind = "memory", file = "in memory")
      manifest <- manifest_row("previous")
    } else {
      columns <- header(previous)
      where <- list(kind = "csv", file = basename(previous), row_map = 1L)
      manifest <- manifest_row("previous", previous)
    }
    found <- if (!is.null(problem)) {
      data.table(
        rule_id = "input_unparseable", input = "previous", file = basename(previous),
        detail = problem, source_cell = NA_character_
      )
    }
    list(
      data = setDT(structure(rep(list(character()), length(columns)), names = columns)),
      where = where, manifest = manifest, invalid = fix_invalid_utf8(data.table()),
      findings = bind_component("read_findings", list(found)),
      blank_header = logical(length(columns))
    )
  }
  read <- lapply(given, function(input) {
    if (input == "previous") previous_read(x[[input]]) else read_input_table(x[[input]], input)
  })
  names(read) <- given
  read
}

#' Where rows of a run input are, as report text (D19.51)
#'
#' A CSV file's rows by their lines, as rule_set_rows() places a rule set's rows ("Line 3 of
#' overrides.csv", or "line 3" when `short`); a data.frame's as R counts its rows ("Row 2 of
#' overrides (as R counts rows)", or "row 2"). These start pre-flight's findings, so they are
#' rows of the text table, which a caller's text table can reword; input_place()
#' (R/lineage_classify.R) places a row inside a caller's error instead, English in the code
#' (D12.37), so the two stay apart (D19.67).
#' @noRd
input_positions <- function(read, input, rows, short = FALSE) {
  where <- read$where
  if (identical(where$kind, "csv")) {
    lines <- file_rows(where, rows + 1L)
    if (short) {
      return(report_text("rule_set_position_file_short", line = lines))
    }
    return(report_text("rule_set_position_file", line = lines, file = where$file))
  }
  if (short) {
    return(report_text("rule_set_position_memory_short", row = rows))
  }
  report_text("input_position_memory", row = rows, input = input)
}

#' The columns an override's scope is read from (10.3 step 1; D19.54)
#'
#' `fingerprint`, `rule_id`, `table_name` and `column_name`, those the overrides have, then
#' each column beyond the overrides' own that `groups` holds: the specification's attributes
#' in pre-flight, the issues' group columns in classification.
#' @noRd
override_scope <- function(data, groups) {
  own <- run_input_schema()$overrides
  c(
    intersect(c("fingerprint", "rule_id", "table_name", "column_name"), names(data)),
    intersect(setdiff(names(data), own), groups)
  )
}

#' One run input's input_unparseable findings (3.6; D19.51)
#'
#' In order: the reader's findings (a cell that isn't valid UTF-8, a malformed line; for
#' `previous`, a folder that isn't a completed run's), at their cells; one finding naming the
#' columns the input lacks and those it doesn't take, at its header, an overrides or
#' acknowledgements column that is an attribute of the specification taken as a group column,
#' an attribution map's as its key column, and an earlier run's column that is another column
#' of the issues or an attribute taken too (D23.33); then, for overrides holding all their own
#' columns, each row whose class isn't source or harmonization, and each row with no scope
#' cell; for acknowledgements holding all theirs, each row whose decision isn't verified or
#' rejected_proposal, and each whose run scope isn't package or merged (D23.26); and for an
#' attribution map holding all its own columns, each row with a blank cell among its table,
#' attribute and key, and each repeating an earlier row's (D23.39).
#' @noRd
input_problems <- function(spec, read, input) {
  data <- read$data
  where <- read$where
  own <- run_input_schema()[[input]]
  attributes <- unique(spec$attributes$attribute_name)
  missing <- setdiff(own, names(data))
  extra <- setdiff(names(data), own)
  # The overrides and the acknowledgements may hold group columns, and the attribution map is
  # keyed by the group column its caller names, each an attribute (D19.51, D23.26, D23.27).
  if (input %chin% c("overrides", "acknowledgements", "attribution_map")) {
    extra <- setdiff(extra, attributes)
  }
  # An earlier run's issues hold the issues' other columns and the run's group and key
  # columns, each an attribute of the specification (9.3; D23.33).
  if (input == "previous") {
    extra <- setdiff(extra, c(names(results_schema()$issues), attributes))
  }
  found <- list(findings_of(read$findings$file, read$findings$detail, read$findings$source_cell))
  if (length(missing) > 0L || length(extra) > 0L) {
    case <- if (length(extra) == 0L) "missing" else if (length(missing) == 0L) "extra" else "both"
    found <- c(found, list(findings_of(
      where$file,
      report_text(
        paste0("preflight_detail_input_unparseable_", case),
        input = input, missing = paste(missing, collapse = ", "),
        extra = paste(extra, collapse = ", ")
      ),
      cell_references(where, 1L, 1L)
    )))
  }
  if (input == "overrides" && length(missing) == 0L) {
    class_cell <- data$issue_class
    bad_class <- which(!class_cell %chin% c("source", "harmonization"))
    scope <- override_scope(data, attributes)
    no_scope <- which(!Reduce(`|`, lapply(scope, function(column) !is.na(data[[column]]))))
    found <- c(found, list(
      findings_of(
        where$file,
        report_text(
          "preflight_detail_input_unparseable_class",
          position = input_positions(read, input, bad_class), value = class_cell[bad_class]
        ),
        cell_references(where, bad_class + 1L, match("issue_class", names(data)))
      ),
      findings_of(
        where$file,
        report_text(
          "preflight_detail_input_unparseable_scope",
          position = input_positions(read, input, no_scope)
        ),
        cell_references(where, no_scope + 1L, 1L)
      )
    ))
  }
  if (input == "acknowledgements" && length(missing) == 0L) {
    coded <- list(decision = c("verified", "rejected_proposal"), run_scope = c("package", "merged"))
    found <- c(found, lapply(names(coded), function(column) {
      cell <- data[[column]]
      bad <- which(!cell %chin% coded[[column]])
      findings_of(
        where$file,
        report_text(
          paste0("preflight_detail_input_unparseable_", column),
          position = input_positions(read, input, bad), value = cell[bad]
        ),
        cell_references(where, bad + 1L, match(column, names(data)))
      )
    }))
  }
  if (input == "attribution_map" && length(missing) == 0L) {
    # Each map row names one table and attribute for one key value, "*" standing for all in a
    # key value's own row (D23.39), so a row with a blank among them, or repeating an earlier
    # row's, would leave an issue no script or two. The key is the table, the attribute and
    # the columns taken as the key above.
    keys <- c("table_name", "attribute_name", setdiff(names(data), c(own, extra)))
    listed <- paste(keys, collapse = ", ")
    given <- Reduce(`&`, lapply(keys, function(column) !is.na(data[[column]])))
    blank <- which(!given)
    keyed <- which(given)
    # Each keyed row's first row with the same key; the columns are renamed first, so no
    # caller's key column can meet a name of the grouping's own.
    cells <- data[keyed, keys, with = FALSE]
    key_names <- sprintf("key_%d", seq_along(keys))
    setnames(cells, key_names)
    grouped <- cells[, list(row = .I, first = .I[1L]), by = key_names]
    again <- grouped[grouped$row != grouped$first]
    again <- again[order(again$row)]
    twice <- keyed[again$row]
    found <- c(found, list(
      findings_of(
        where$file,
        report_text(
          "preflight_detail_input_unparseable_map_blank",
          position = input_positions(read, input, blank), columns = rep(listed, length(blank))
        ),
        cell_references(where, blank + 1L, 1L)
      ),
      findings_of(
        where$file,
        report_text(
          "preflight_detail_input_unparseable_map_repeat",
          position = input_positions(read, input, twice),
          first = input_positions(read, input, keyed[again$first], short = TRUE),
          columns = rep(listed, length(twice))
        ),
        cell_references(where, twice + 1L, 1L)
      )
    ))
  }
  rbindlist(found)
}

#' override_scope_conflict's findings: each override that repeats an earlier one's scope with
#' the other class, and the first such earlier row (10.3 step 1; D9.27, D19.51)
#'
#' Scopes are compared cell by cell as written, a blank equal to a blank, a rule ID before the
#' rule ID map reads it. Only the rows input_unparseable passes are read (a class of source or
#' harmonization and a scope cell), and none where the overrides lack their own columns.
#' @noRd
override_conflicts <- function(spec, read) {
  data <- read$data
  if (!all(run_input_schema()$overrides %chin% names(data))) {
    return(no_findings())
  }
  scope <- override_scope(data, unique(spec$attributes$attribute_name))
  class_cell <- data$issue_class
  given <- Reduce(`|`, lapply(scope, function(column) !is.na(data[[column]])))
  judged <- which(class_cell %chin% c("source", "harmonization") & given)
  if (length(judged) < 2L) {
    return(no_findings())
  }
  # Each judged row's scope as a group number, NA grouping with NA; the columns are renamed
  # first, so no caller's group column can meet a name of the grouping's own.
  cells <- data[judged, scope, with = FALSE]
  scope_names <- sprintf("scope_%d", seq_along(scope))
  setnames(cells, scope_names)
  grouped <- cells[, list(row = .I, group = .GRP), by = scope_names]
  group <- grouped$group[order(grouped$row)]
  classes <- class_cell[judged]
  # For each class, its first row in each scope; a row of the other class after it repeats it.
  earlier <- rep(NA_integer_, length(judged))
  for (kind in c("source", "harmonization")) {
    mine <- classes == kind
    leads <- !duplicated(group[mine])
    first <- judged[mine][leads][match(group, group[mine][leads])]
    hit <- !mine & !is.na(first) & first < judged
    earlier[hit] <- first[hit]
  }
  at <- which(!is.na(earlier))
  findings_of(
    read$where$file,
    report_text(
      "preflight_detail_override_scope_conflict",
      position = input_positions(read, "overrides", judged[at]),
      first = input_positions(read, "overrides", earlier[at], short = TRUE),
      class = classes[at], other_class = class_cell[earlier[at]]
    ),
    cell_references(read$where, judged[at] + 1L, match("issue_class", names(data)))
  )
}

#' override_unknown_name's findings: each rule, table and column an override names that the
#' registry or the dictionary lacks, one per name, at its cell (D19.51)
#'
#' A rule ID the rule ID map maps to a current rule is known, as the override applies to the
#' new ID. A column is looked up in the row's table where the row gives a known one, in any
#' table where it gives none, and not at all where its table is unknown, which is the finding.
#' A column "*" is known, as an issue on a whole table holds it, and so are the table and
#' column of an override of table_unexpected or column_unexpected, whose issues hold the
#' delivery's own names (D19.72). The findings in that order: rules, tables, columns of a
#' table, columns of any table. None where the overrides lack their own columns.
#' @noRd
override_unknown_names <- function(spec, read, registry) {
  data <- read$data
  if (!all(run_input_schema()$overrides %chin% names(data))) {
    return(no_findings())
  }
  a <- spec$attributes
  rule <- data$rule_id
  table <- data$table_name
  column <- data$column_name
  known_rules <- c(registry$rule_id, rule_id_map()$old_rule_id)
  delivered <- rule %chin% c("table_unexpected", "column_unexpected")
  known_table <- !is.na(table) & (table %chin% a$table_name | delivered)
  known_column <- column %chin% "*" | delivered
  # Each row's attribute in the dictionary, found on its table and column together by one join
  # (16.2; M6 m59, D20.86); NA where the row's table has no such column.
  attribute_row <- a[
    data,
    on = c(table_name = "table_name", attribute_name = "column_name"), mult = "first",
    which = TRUE
  ]
  cases <- list(
    rule = which(!is.na(rule) & !rule %chin% known_rules),
    table = which(!is.na(table) & !known_table),
    attribute = which(!is.na(column) & !known_column & known_table & is.na(attribute_row)),
    any = which(
      !is.na(column) & !known_column & is.na(table) & !column %chin% a$attribute_name
    )
  )
  cell <- c(rule = "rule_id", table = "table_name", attribute = "column_name", any = "column_name")
  rbindlist(lapply(names(cases), function(case) {
    at <- cases[[case]]
    findings_of(
      read$where$file,
      report_text(
        paste0("preflight_detail_override_unknown_name_", case),
        position = input_positions(read, "overrides", at), rule_id = rule[at],
        table_name = table[at], attribute_name = column[at]
      ),
      cell_references(read$where, at + 1L, match(cell[[case]], names(data)))
    )
  }))
}

#' The checks of the run inputs (4.2; D19.51), after the report text's in the registry
#'
#' Each takes the spec and pre-flight's context, whose `inputs` holds the reads
#' (preflight_context(), read_run_inputs()), and returns findings_of(...), or "no_input"
#' without its input.
#' @noRd
preflight_input_checks <- function() {
  list(
    input_unparseable = function(spec, context) {
      inputs <- context$inputs
      if (length(inputs) == 0L) {
        return("no_input")
      }
      rbindlist(lapply(names(inputs), function(input) {
        input_problems(spec, inputs[[input]], input)
      }))
    },
    override_scope_conflict = function(spec, context) {
      read <- context$inputs[["overrides"]]
      if (is.null(read)) {
        return("no_input")
      }
      override_conflicts(spec, read)
    },
    override_unknown_name = function(spec, context) {
      read <- context$inputs[["overrides"]]
      if (is.null(read)) {
        return("no_input")
      }
      override_unknown_names(spec, read, context$registry)
    }
  )
}
