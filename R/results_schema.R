# The results model (plan 9.1 to 9.4; D4.1, D7.3, D7.5, D8.16, D9.21, D9.24): a run's results
# summary, issues and corrections, their columns, classes and coded values, the
# constructors that complete and check them, and the gpq_results object that holds them
# with the run metadata (R/results_metadata.R). The caller's group columns, and in issues
# the problem unit's key column, carry the caller's names and hold text: "*" where a table
# can't reach the column (D7.5; D9.24 for the group columns, R6 for the key column). The
# engine refuses rows for a rule the registry doesn't declare (9.7).

#' The three tables' columns and classes, with the caller's columns in their places
#'
#' Results carry the group columns after `column_name`; issues carry the group columns,
#' then the problem unit's key column unless it is one of them, after
#' `provider_key_status` and before `reaches_other_unit` (9.2, 9.3; D20.18). Each caller column
#' is character. A caller column that
#' repeats, is blank, or has the name of a fixed column is a caller error, and so is a group
#' column whose name holds "; " (D15.39, D15.40). So is one named like a column a report adds,
#' `message` or `n_not_evaluated_own`, or starting with `key_`, the provider CSV's key parts
#' (D20.76).
#' @noRd
results_schema <- function(group_cols = character(), key_col = NULL) {
  chr <- "character"
  int <- "integer"
  lgl <- "logical"
  results <- c(
    run_id = chr, run_scope = chr, rule_id = chr, rule_fingerprint = chr, stage = chr,
    layer = int, severity = chr, table_name = chr, column_name = chr, n_checked = int,
    n_fail = int, n_not_evaluated = int, n_unchecked = int, reason_code = chr,
    n_distinct_fail_values = int, n_easy_fix = int, examples = chr, status = chr,
    not_run_reason = chr, message_id = chr
  )
  issues <- c(
    issue_id = int, run_id = chr, run_scope = chr, rule_id = chr, rule_fingerprint = chr,
    stage = chr, layer = int, severity = chr, reason_code = chr, table_name = chr,
    column_name = chr, record_pk = chr, row_number = int, pk_positional = lgl, level = chr,
    n_records = int, value = chr, expected = chr, entity_key = chr, visit_keys = chr,
    provider_key = chr, provider_key_status = chr, reaches_other_unit = lgl,
    root_issue_id = int, status = chr,
    issue_class = chr, class_basis = chr, script_file = chr, script_block = chr,
    attribution_status = chr, fingerprint = chr, fingerprint_pk = chr, first_seen_run = chr,
    change = chr, change_cause = chr
  )
  corrections <- c(
    correction_id = int, run_id = chr, issue_id = int, related_issue_ids = chr,
    table_name = chr, record_pk = chr, column_name = chr, entity_key = chr,
    current_value = chr, proposed_value = chr, strategy = chr, tie_break = chr, tie = lgl,
    reason_code = chr, support = chr, support_n = int, support_d = int, evidence = chr,
    fixed_value_passes = lgl, easy_fix = lgl, easy_fix_blocked_by = chr
  )
  # A report's routed results add n_not_evaluated_own (route_unit()) and the provider's CSV
  # message (provider_csv_table()), so a caller's column can't take either name (D20.76).
  report <- c("message", "n_not_evaluated_own")
  validate_caller_columns(
    group_cols, key_col, unique(c(names(results), names(issues), names(corrections), report))
  )
  caller <- function(columns) structure(rep(chr, length(columns)), names = columns)
  list(
    results = append(
      results, caller(group_cols),
      after = match("column_name", names(results))
    ),
    issues = append(
      issues, caller(unique(c(group_cols, key_col))),
      after = match("provider_key_status", names(issues))
    ),
    corrections = corrections
  )
}

#' The caller's group columns and key column checked against the fixed columns
#' @noRd
validate_caller_columns <- function(group_cols, key_col, fixed) {
  named <- function(x) is.character(x) && !anyNA(x) && all(nzchar(trimws(x)))
  if (!(named(group_cols) && !anyDuplicated(group_cols))) {
    stop("`group_cols` must be column names, each given once.", call. = FALSE)
  }
  if (!is.null(key_col) && !(named(key_col) && length(key_col) == 1L)) {
    stop("The problem unit's key column must be one column name.", call. = FALSE)
  }
  # run.csv's group_cols joins the group columns' names with "; " (R11, D9.29), so a group
  # column's name holding it could not be told from two names when the run is read back
  # (D15.39). The key column isn't recorded there, so its name is not checked (D15.40).
  joined <- grep("; ", group_cols, fixed = TRUE, value = TRUE)
  if (length(joined) > 0L) {
    stop(
      "The caller's columns ", paste0("\"", joined, "\"", collapse = ", "),
      " have \"; \" in their names, which the run's `group_cols` joins names with.",
      call. = FALSE
    )
  }
  clash <- intersect(c(group_cols, key_col), fixed)
  if (length(clash) > 0L) {
    stop(sprintf(
      "The caller's columns %s have the names of columns the results already have.",
      paste(clash, collapse = ", ")
    ), call. = FALSE)
  }
  # The provider issues CSV names each part of a provider key key_<label> (D20.31), so a
  # caller's column can't start with key_ (D20.76), which has its own error (D20.95).
  prefixed <- grep("^key_", unique(c(group_cols, key_col)), value = TRUE)
  if (length(prefixed) > 0L) {
    stop(sprintf(
      paste(
        "The caller's columns %s start with \"key_\", which the provider issues CSV uses for",
        "the parts of each provider key."
      ),
      paste(prefixed, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' A table completed to its schema: the columns given, then every other column as NA
#'
#' `x` is NULL (no rows) or a data.frame or list holding some of the schema's columns, of
#' their classes; a column the schema lacks, or a list of values with no names, is a
#' developer error. The result is a new data.table in the schema's order, sharing no column
#' with `x`.
#' @noRd
complete_table <- function(x, schema, name) {
  if (is.null(x)) {
    return(empty_table(schema))
  }
  if (length(x) > 0L && is.null(names(x))) {
    stop(sprintf("The %s table's columns must be named.", name), call. = FALSE)
  }
  extra <- setdiff(names(x), names(schema))
  if (length(extra) > 0L) {
    stop(sprintf(
      "The %s table has no columns %s.", name, paste(extra, collapse = ", ")
    ), call. = FALSE)
  }
  given <- intersect(names(schema), names(x))
  if (length(given) == 0L) {
    return(empty_table(schema))
  }
  # as.data.table() copies each column it is given and recycles a short one, so the table
  # shares none with `x`; each NA column is made and set in turn, so no more than one filler
  # is alive at a time.
  table <- as.data.table(as.list(x)[given])
  absent <- setdiff(names(schema), given)
  for (column in absent) {
    set(table, j = column, value = rep(vector(schema[[column]], 1L)[NA], nrow(table)))
  }
  if (length(absent) > 0L) {
    setcolorder(table, names(schema))
  }
  table
}

#' A results summary table (9.2), completed and checked
#' @noRd
new_results <- function(x = NULL, group_cols = character(), registry = rule_registry()) {
  schema <- results_schema(group_cols)$results
  table <- complete_table(x, schema, "results")
  validate_schema(table, "results", schema, registry)
  table
}

#' An issues table (9.3), completed and checked
#' @noRd
new_issues <- function(x = NULL, group_cols = character(), key_col = NULL,
                       registry = rule_registry()) {
  schema <- results_schema(group_cols, key_col)$issues
  table <- complete_table(x, schema, "issues")
  validate_schema(table, "issues", schema, registry)
  table
}

#' A corrections table (9.4), completed and checked
#' @noRd
new_corrections <- function(x = NULL) {
  schema <- results_schema()$corrections
  table <- complete_table(x, schema, "corrections")
  validate_schema(table, "corrections", schema)
  table
}

#' A table checked against its schema, changing nothing
#'
#' Every table: a data.table with exactly the schema's columns in order, of its classes.
#' Then, for the results, issues and corrections tables (9.2 to 9.4) and the run and
#' settings tables (9.5): the columns that must hold a value hold one, a text one never
#' empty; coded columns hold their values only; and each table's rules between columns. Results and
#' issues name only rules the registry declares that aren't pre-flight checks, with the
#' registry's stage and layer (9.7), a conformance row's reason code is "*" (R3), and a results
#' row has `n_unchecked` unless its rule is a unit rule (D20.15). A problem is a developer
#' error naming the table.
#' @noRd
validate_schema <- function(x, name, schema, registry = rule_registry()) {
  if (!is.data.table(x)) {
    stop(sprintf("The %s table must be a data.table.", name), call. = FALSE)
  }
  if (!identical(names(x), names(schema))) {
    stop(sprintf(
      "The %s table has the columns %s, in that order.", name,
      paste(names(schema), collapse = ", ")
    ), call. = FALSE)
  }
  classes <- vapply(x, function(column) class(column)[[1L]], character(1))
  wrong <- names(schema)[classes != schema]
  if (length(wrong) > 0L) {
    stop(sprintf(
      "The %s table has columns of the wrong class: %s.", name, paste(wrong, collapse = ", ")
    ), call. = FALSE)
  }
  fail <- function(problem) {
    stop(sprintf("The %s table %s.", name, problem), call. = FALSE)
  }
  caller <- setdiff(names(schema), names(results_schema()[[name]]))
  required <- switch(name,
    results = c(
      "run_id", "run_scope", "rule_id", "rule_fingerprint", "stage", "layer", "severity",
      "table_name", "column_name", caller, "n_checked", "n_fail", "n_not_evaluated",
      "reason_code", "n_distinct_fail_values", "n_easy_fix", "status", "message_id"
    ),
    issues = c(
      "issue_id", "run_id", "run_scope", "rule_id", "rule_fingerprint", "stage", "layer",
      "severity", "reason_code", "table_name", "column_name", "level", caller,
      "reaches_other_unit", "status"
    ),
    corrections = c(
      "correction_id", "run_id", "issue_id", "table_name", "column_name", "proposed_value",
      "strategy", "easy_fix"
    ),
    run = c(
      "run_id", "run_start", "run_end", "package_version", "r_version", "platform",
      "data_table_version", "outcome"
    ),
    settings = c("setting", "tier"),
    character()
  )
  for (column in required) {
    value <- x[[column]]
    if (anyNA(value) || (is.character(value) && any(!nzchar(value)))) {
      fail(sprintf("has blank cells in %s", column))
    }
  }
  values <- list(
    run_scope = c("package", "merged"), stage = c("conformance", "plausibility"),
    severity = c("error", "warning", "flag", "info"), level = c("record", "column"),
    not_run_reason = c(
      "no_matrix", "offline", "no_companion_data", "text_input", "no_sentinels",
      "no_dd_column", "no_thresholds", "no_table", "no_column", "no_input", "no_dd_values",
      "none_checked", "no_records"
    ),
    issue_class = c("source", "harmonization", "unclassified", "none"),
    provider_key_status = c("decoded", "partial", "raw", "fallback_pk", "no_lineage_spec"),
    attribution_status = c("current", "may_be_stale", "script_version_unknown", "unmapped"),
    change = c("new", "persisting", "resolved"), change_cause = c("data", "rules_or_spec"),
    outcome = c("completed", "stopped_preflight"),
    tier = c("argument", "rule set", "option", "built-in")
  )
  values$status <- switch(name,
    results = c("pass", "fail", "not_run"),
    issues = c("open", "not_evaluated", "acknowledged")
  )
  coded <- intersect(names(values), names(x))
  if (name %in% c("results", "issues", "corrections", "run", "settings")) {
    for (column in coded) {
      value <- x[[column]]
      bad <- unique(value[!is.na(value) & !value %chin% values[[column]]])
      if (length(bad) > 0L) {
        fail(sprintf("has values %s outside %s", paste(bad, collapse = ", "), column))
      }
    }
  }
  if (name %in% c("results", "issues")) {
    data_rules <- registry[registry$check_type != "preflight"]
    at <- match(x$rule_id, data_rules$rule_id)
    if (anyNA(at)) {
      fail(sprintf(
        "names rules the registry doesn't declare: %s",
        paste(unique(x$rule_id[is.na(at)]), collapse = ", ")
      ))
    }
    off <- x$stage != data_rules$stage[at] | x$layer != data_rules$layer[at]
    if (any(off)) {
      fail(sprintf(
        "gives rules a stage or layer the registry doesn't: %s",
        paste(unique(x$rule_id[off]), collapse = ", ")
      ))
    }
    if (any(x$stage == "conformance" & x$reason_code != "*")) {
      fail("gives a conformance row a reason code other than \"*\"")
    }
    # A rule that checks a table or a column as one unit counts no records, so its rows have no
    # n_unchecked, and every other rule's rows have one (D20.15).
    if (name == "results" && any(is.na(x$n_unchecked) != (data_rules$unit[at] %in% TRUE))) {
      fail(paste(
        "gives n_unchecked to the row of a rule that checks whole tables or columns, or leaves",
        "it NA on another rule's row"
      ))
    }
  }
  switch(name,
    results = check_results_rows(x, caller, fail),
    issues = check_issues_rows(x, fail),
    corrections = check_corrections_rows(x, fail),
    run = if (nrow(x) != 1L) fail("must have one row"),
    NULL
  )
  invisible(NULL)
}

#' The results table's rules between columns (9.2; D7.3)
#'
#' Counts never negative, `n_unchecked` NA on a unit rule's row (D20.15); no more failures than
#' records checked, and no more easy fixes or
#' distinct failing values than failures (m24, D16.8: a table or column rule counts tables
#' or columns, so the bounds hold for it too); a pass has no failures, a fail some; a
#' not_run row has a not-run reason, no records checked and no failures, and only it has a
#' reason; one row per rule, table, column, group and reason code.
#' @noRd
check_results_rows <- function(x, caller, fail) {
  counts <- c(
    "n_checked", "n_fail", "n_not_evaluated", "n_unchecked", "n_distinct_fail_values",
    "n_easy_fix"
  )
  if (any(vapply(counts, function(column) any(x[[column]] < 0L, na.rm = TRUE), logical(1)))) {
    fail("has negative counts")
  }
  if (any(x$n_fail > x$n_checked)) {
    fail("counts more failing records than records checked")
  }
  if (any(x$n_easy_fix > x$n_fail | x$n_distinct_fail_values > x$n_fail)) {
    fail("counts more easy fixes or distinct failing values than failing records")
  }
  not_run <- x$status == "not_run"
  if (any(not_run != !is.na(x$not_run_reason))) {
    fail("gives a not-run reason to a row that ran, or none to one that didn't")
  }
  if (any(not_run & (x$n_checked != 0L | x$n_fail != 0L))) {
    fail("counts checked or failing records on a not_run row")
  }
  if (any(x$status == "pass" & x$n_fail != 0L) || any(x$status == "fail" & x$n_fail == 0L)) {
    fail("has a pass with failures or a fail without one")
  }
  if (anyDuplicated(x, by = c("rule_id", "table_name", "column_name", caller, "reason_code"))) {
    fail("has more than one row for a rule, table, column, group and reason code")
  }
  invisible(NULL)
}

#' The issues table's rules between columns (9.3, 6.2, 6.3; D15.9)
#'
#' Issue IDs from 1, each once; a record-level issue has its row number and whether it is
#' positional, and only a positional one may lack its PK; a column-level issue has
#' `record_pk` "*", a count of records (0 for a table rule's missing table, D16.8) and no row
#' number; a not_evaluated issue, and only
#' it, names its root, an issue of the same table.
#' @noRd
check_issues_rows <- function(x, fail) {
  if (any(x$issue_id < 1L) || anyDuplicated(x$issue_id)) {
    fail("has an issue ID below 1 or used twice")
  }
  record <- x$level == "record"
  if (any(record & (is.na(x$row_number) | x$row_number < 1L | is.na(x$pk_positional)))) {
    fail("has record-level issues without a row number or a positional flag")
  }
  if (any(record & is.na(x$record_pk) & !x$pk_positional %in% TRUE)) {
    fail("has record-level issues without a PK that aren't marked positional")
  }
  column <- !record
  counted <- !is.na(x$n_records) & x$n_records >= 0L
  if (any(column & (!x$record_pk %chin% "*" | !counted | !is.na(x$row_number)))) {
    fail("has column-level issues without record_pk \"*\" and a count, or with a row number")
  }
  has_root <- !is.na(x$root_issue_id)
  if (any(has_root != (x$status == "not_evaluated"))) {
    fail("has a root on an issue that isn't not_evaluated, or a not_evaluated one without")
  }
  root_at <- match(x$root_issue_id[has_root], x$issue_id)
  if (anyNA(root_at) || any(x$table_name[root_at] != x$table_name[has_root])) {
    fail("names a root that isn't an issue of the same table")
  }
  invisible(NULL)
}

#' The corrections table's rules between columns (9.4)
#'
#' Correction IDs from 1, each once; support counts never negative, each checked even when
#' the other is missing; `support_n` no more than `support_d` where both are given.
#' @noRd
check_corrections_rows <- function(x, fail) {
  if (any(x$correction_id < 1L) || anyDuplicated(x$correction_id)) {
    fail("has a correction ID below 1 or used twice")
  }
  if (any(x$support_n < 0L | x$support_d < 0L, na.rm = TRUE)) {
    fail("has negative support counts")
  }
  both <- !is.na(x$support_n) & !is.na(x$support_d)
  if (any(both & x$support_n > x$support_d)) {
    fail("has support counts out of order")
  }
  invisible(NULL)
}

#' A gpq_results object: the three tables and the run metadata, checked together
#'
#' The group columns are the ones the run table records (D9.29); the problem unit's key
#' column is the issues column beyond them and the fixed ones, if any. Every row carries
#' the run's ID; every correction names an issue. Returns
#' `structure(list(results, issues, corrections, metadata), class = "gpq_results")`. The
#' object holds the tables it is given, not copies of them, so a later change by reference
#' to a table the caller still holds reaches the object.
#' @noRd
new_gpq_results <- function(results, issues, corrections, metadata,
                            registry = rule_registry()) {
  object <- structure(
    list(results = results, issues = issues, corrections = corrections, metadata = metadata),
    class = "gpq_results"
  )
  validate_gpq_results(object, registry)
  object
}

#' A gpq_results object checked, changing nothing
#' @noRd
validate_gpq_results <- function(x, registry = rule_registry()) {
  whole <- inherits(x, "gpq_results") &&
    identical(names(x), c("results", "issues", "corrections", "metadata"))
  if (!whole) {
    stop("A gpq_results object holds results, issues, corrections and metadata.", call. = FALSE)
  }
  validate_metadata(x$metadata)
  run <- x$metadata$run
  group_cols <- split_joined(run$group_cols)
  extra <- setdiff(names(x$issues), c(names(results_schema()$issues), group_cols))
  if (length(extra) > 1L) {
    stop(sprintf(
      "The issues table has columns no schema has: %s.", paste(extra, collapse = ", ")
    ), call. = FALSE)
  }
  key_col <- if (length(extra) == 1L) extra
  schema <- results_schema(group_cols, key_col)
  for (name in c("results", "issues", "corrections")) {
    validate_schema(x[[name]], name, schema[[name]], registry)
    if (any(x[[name]]$run_id != run$run_id)) {
      stop(sprintf("The %s table holds rows of another run.", name), call. = FALSE)
    }
  }
  if (!all(x$corrections$issue_id %in% x$issues$issue_id)) {
    stop("The corrections table names issues the issues table doesn't have.", call. = FALSE)
  }
  invisible(NULL)
}

#' One run's results with another's for the same run appended, issue and correction IDs
#' renumbered after the first's (plan 3.4: Layer 6 appends to results; R16, D15.26)
#'
#' Both are gpq_results of the same run, with the same group columns; the metadata is the
#' first's, not a copy of it, so a later change by reference to it reaches the bound object.
#' The second object's `issue_id`, `root_issue_id`, `correction_id` and corrections'
#' `issue_id` move by the first's largest ID. So do the issue IDs written in corrections'
#' `related_issue_ids` and `easy_fix_blocked_by` (D15.26): cells of parts joined with "; ",
#' where every all-digit part of at most nine digits (which an integer holds) is an issue ID
#' and any other part (`problem_plot`, a longer run of digits) and NA stay as written. The
#' tables are bound first and the second object's rows changed in the new tables, so neither
#' input is copied whole or changed.
#' @noRd
bind_results <- function(x, y, registry = rule_registry()) {
  validate_gpq_results(x, registry)
  validate_gpq_results(y, registry)
  one_run <- identical(x$metadata$run$run_id, y$metadata$run$run_id) &&
    identical(x$metadata$run$group_cols, y$metadata$run$group_cols) &&
    identical(names(x$issues), names(y$issues))
  if (!one_run) {
    stop("Only results of one run, with the same columns, are bound.", call. = FALSE)
  }
  issue_offset <- max(c(0L, x$issues$issue_id))
  correction_offset <- max(c(0L, x$corrections$correction_id))
  issues <- rbindlist(list(x$issues, y$issues))
  corrections <- rbindlist(list(x$corrections, y$corrections))
  issues_at <- nrow(x$issues) + seq_len(nrow(y$issues))
  corrections_at <- nrow(x$corrections) + seq_len(nrow(y$corrections))
  move_by <- function(table, at, column, offset) {
    set(table, i = at, j = column, value = table[[column]][at] + offset)
  }
  # The all-digit parts of "; "-joined cells moved by the issue offset, other parts and NA
  # as they were. At most nine digits keep the shifted value inside an integer (R63), so a
  # longer part stays as written. Each pass finds the first ID left to move in every cell that
  # holds one (one regexpr() over those cells), moves it and cuts the cell after it, so a cut
  # piece begins with "; " or is empty and the pattern's "^" can't match inside it. The loop
  # runs as many times as the most IDs in one cell, never once per cell (16.2).
  shift_ids <- function(cells) {
    given <- which(!is.na(cells))
    rest <- cells[given]
    done <- character(length(rest))
    active <- seq_along(rest)
    repeat {
      found <- regexpr("(?<=^|; )[0-9]{1,9}(?=; |$)", rest[active], perl = TRUE)
      hit <- which(found > 0L)
      if (length(hit) == 0L) {
        break
      }
      first <- found[hit]
      last <- first + attr(found, "match.length")[hit] - 1L
      active <- active[hit]
      cut <- rest[active]
      done[active] <- paste0(
        done[active], substr(cut, 1L, first - 1L),
        as.integer(substr(cut, first, last)) + issue_offset
      )
      rest[active] <- substr(cut, last + 1L, .Machine$integer.max)
    }
    cells[given] <- paste0(done, rest)
    cells
  }
  move_by(issues, issues_at, "issue_id", issue_offset)
  move_by(issues, issues_at, "root_issue_id", issue_offset)
  move_by(corrections, corrections_at, "correction_id", correction_offset)
  move_by(corrections, corrections_at, "issue_id", issue_offset)
  for (column in c("related_issue_ids", "easy_fix_blocked_by")) {
    set(
      corrections,
      i = corrections_at, j = column, value = shift_ids(corrections[[column]][corrections_at])
    )
  }
  new_gpq_results(
    rbindlist(list(x$results, y$results)), issues, corrections, x$metadata, registry
  )
}

#' A "; "-joined text cell split back into its parts; NA gives none
#' @noRd
split_joined <- function(x) {
  if (is.na(x)) character() else strsplit(x, "; ", fixed = TRUE)[[1L]]
}
