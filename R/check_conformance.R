# A conformance run (plan 3.2, 6.1 to 6.3, 9.1 to 9.7; D2.10, D4.1, D7.3, D7.5, D9.24,
# D16.3 to D16.17). gpq_check_conformance() runs pre-flight, matches the tables to the spec
# (R/check_input.R) and runs each asked layer's rule family (R/check_structure.R,
# R/check_types.R, R/check_sentinels.R). A family returns outcomes, by rule; this file turns
# them into the results summary and the issues: each rule's severity and whether it is on,
# from the rule set (D16.6, D16.7); a value that failed an earlier layer masked as
# not_evaluated (6.2); failures and masked records collapsed into column-level issues (6.3,
# D16.10); and the run's metadata with its self-test (9.5, 9.7).

#' Check tables against a specification: structure, types and missing-value codes
#'
#' @description
#' Runs the conformance stage on a set of related tables: pre-flight of the
#' specification and configuration, then the checks of the layers asked for, each
#' table and column read as the specification describes it. Returns the results
#' summary and one issue per failing record, or one per column where a rule fails
#' on every record of a column in a group.
#'
#' @details
#' Pre-flight ([gpq_preflight()]) runs before any check, on `spec`, `rules` and `settings`; it
#' stops the run where it would stop, and its warning reaches the caller. The checks never
#' change `tables`.
#'
#' A CSV file is read with [data.table::fread()], which finds its separator. A file it
#' can't read cleanly, such as one with a ragged line or a title line shorter than its
#' header, stops the run before any check, and so does a file that doesn't end with a line
#' break, which may be cut off. A title line as long as the header, or above a file of one
#' column, is read as the header, so the checks report the file's columns as missing. The
#' run's inputs metadata names each file with its SHA-256, its rows, its separator and how
#' many values were read as Windows-1252. A table read from a DBI connection is recorded as
#' "DBI connection, not hashed", with the database's name for it and the driver's class.
#'
#' Layers and the rules they can raise:
#' - Layer 0, structure: `table_missing`, `table_unexpected`, `column_missing`,
#'   `column_unexpected`, `column_name_mismatch`.
#' - Layer 1, data types: `class_mismatch`, `coercion_loss`, `date_unparseable`,
#'   `integer_stored_as_double`, `value_not_finite`.
#' - Layer 2, missing-value codes: `raw_na`, `blank_string`, `sentinel_wrong_type`,
#'   `sentinel_rogue_code`, `species_unknown_code`, `whitespace_padding`, `na_text_in_id`.
#'
#' Layers 3 to 5 have no rules yet. A column whose name differs from its dictionary
#' attribute's only in letter case or spaces is reported by `column_name_mismatch` and
#' checked as that attribute. Layer 0's rules and `class_mismatch` and
#' `integer_stored_as_double` count tables or columns, not records, and their rows have `"*"`
#' in every group column.
#' A value that failed Layer 1 is not evaluated by Layer 2: its
#' issue there has status `not_evaluated` and names the earlier issue as its root. When a
#' rule fails on every checked record of a column in a group, and at least
#' `column_level_min_n` records were checked, the run records one column-level issue; masked
#' records sharing one root collapse the same way.
#'
#' A record's group values are read from its own table, or from the nearest table that holds
#' the column, through the foreign keys; a record that can't reach one, whose keys on the way
#' don't match, or whose value there is blank or missing, has `"*"` there. A group value
#' marked latin1 is converted to UTF-8 and runs; one whose bytes aren't valid UTF-8 stops the
#' run, naming the column, the table it is read from and its first such row.
#'
#' Settings (each from the argument, the rule set, the option, or the built-in value, see
#' [groundplotqc_options]): `raw_na_policy`, `rogue_codes`, `unknown_codes`,
#' `column_level_min_n`, `example_cap`, `lang` and `severity`. A rule set's `rules` rows
#' give a rule's severity and turn it off (`enabled`) for a table and attribute: the most
#' specific row that gives a value wins (table and attribute, then table and `"*"`, then
#' `"*"` and attribute, then `"*"` and `"*"`). A rule turned off writes no rows there.
#'
#' @param tables The tables, one per table of the specification, named as it names them: a
#'   named list whose elements are data.frames or paths of CSV files, mixed as you like, or
#'   a named character vector of paths. Every column of a CSV file is read as text, so
#'   `class_mismatch` and `integer_stored_as_double` don't run on its tables. In a CSV file
#'   an empty cell is a missing value and everything else is read as written, so write
#'   files with data.table::fwrite() or write.csv(na = "", row.names = FALSE). A cell that
#'   isn't valid UTF-8 is read as Windows-1252, unless it holds a byte Windows-1252 doesn't
#'   define. A table holding text marked as bytes is refused. Or a DBI connection: the
#'   specification's tables are read from it under the names `table_map` gives, and its
#'   other tables are left alone.
#' @param table_map `NULL`, or, when `tables` is a DBI connection, a named character vector
#'   of the database's names for tables of the specification: names the specification's
#'   table names, values the database's, such as `c(trees = "survey.tree_list")` for the
#'   table tree_list in the schema survey. A table it doesn't name is read under its own
#'   name. A table of the specification the database doesn't have is reported as missing
#'   (`table_missing`).
#' @param spec A specification from [gpq_read_spec()] or [magp_spec()].
#' @param group_cols `NULL`, or the grouping columns, report unit first: results and issues
#'   carry them under these names.
#' @param problem_unit `NULL`, or `list(table, key_col)`: the table and key column of the
#'   unit problem plots are ranked by; issues carry that key under its name.
#' @param rules `NULL`, or a rule set such as [magp_rules()] returns.
#' @param layers The layers to run, whole numbers from 0 to 5.
#' @param settings A named list of settings.
#' @param run_id `NULL`, to make one from the run's start, or a run ID such as
#'   `"20261215T140312Z_3fa9c1"`: the UTC start time and six hex characters.
#' @return An object of class `gpq_results`, a list of three tables and the run metadata:
#'   - `results`: the summary, one row per rule, table, column and group: `run_id`,
#'     `run_scope`, `rule_id`, `rule_fingerprint`, `stage`, `layer`, `severity`, `table_name`,
#'     `column_name`, the group columns, `n_checked`, `n_fail`, `n_not_evaluated`,
#'     `reason_code`, `n_distinct_fail_values`, `n_easy_fix`, `examples`, `status` (`pass`,
#'     `fail` or `not_run`), `not_run_reason` and `message_id`.
#'   - `issues`: one row per failing or not-evaluated record, or per column-level issue:
#'     `issue_id`, `run_id`, `run_scope`, `rule_id`, `rule_fingerprint`, `stage`, `layer`,
#'     `severity`, `reason_code`, `table_name`, `column_name`, `record_pk`, `row_number`,
#'     `pk_positional`, `level` (`record` or `column`), `n_records`, `value`, `expected`,
#'     `entity_key`, `visit_keys`, `provider_key`, `provider_key_status`, the group columns,
#'     the problem unit's key column, `root_issue_id`, `status` (`open` or `not_evaluated`),
#'     `issue_class`, `class_basis`, `script_file`, `script_block`, `attribution_status`,
#'     `fingerprint`, `fingerprint_pk`, `first_seen_run`, `change` and `change_cause`. The
#'     columns a conformance run doesn't fill are NA.
#'   - `corrections`: an empty table.
#'   - `metadata`: the tables of `metadata/` in a run folder.
#'
#'   Write it with [gpq_write_results()].
#' @examples
#' # magp_example with one planted defect: a missing value in a column that must hold one.
#' trees <- data.table::copy(magp_example$magp_trees)
#' trees$tree_dist[1] <- NA
#' planted <- magp_example
#' planted$magp_trees <- trees
#' # MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
#' results <- gpq_check_conformance(
#'   planted, magp_spec(),
#'   group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
#'   problem_unit = list("magp_plots", "magp_plot_id"), rules = magp_rules(), layers = 0:2
#' )
#' table(results$results$status)
#' results$issues[results$issues$status == "open", c("rule_id", "table_name", "column_name")]
#' @export
gpq_check_conformance <- function(tables, spec, table_map = NULL, group_cols = NULL,
                                  problem_unit = NULL, rules = NULL, layers = 0:5,
                                  settings = list(), run_id = NULL) {
  run_start <- Sys.time()
  if (!inherits(spec, "gpq_spec")) {
    stop("`spec` must be a specification from gpq_read_spec().", call. = FALSE)
  }
  layers_ok <- is.numeric(layers) && is.null(dim(layers)) && length(layers) > 0L &&
    !anyNA(layers) && all(layers %in% 0:5)
  if (!layers_ok) {
    stop("`layers` must be whole numbers from 0 to 5.", call. = FALSE)
  }
  layers <- sort(unique(as.integer(layers)))
  if (is.null(group_cols)) {
    group_cols <- character()
  }
  key_col <- problem_unit_key(problem_unit, spec)
  results_schema(group_cols, key_col)
  unknown <- setdiff(group_cols, spec$attributes$attribute_name)
  if (length(unknown) > 0L) {
    stop(sprintf(
      "No table of the specification has the group columns %s.", paste(unknown, collapse = ", ")
    ), call. = FALSE)
  }
  run_id_ok <- is.null(run_id) || (
    is.character(run_id) && length(run_id) == 1L && !is.na(run_id) &&
      grepl(run_id_pattern, run_id)
  )
  if (!run_id_ok) {
    stop(
      "`run_id` must be NULL or a run ID such as \"20261215T140312Z_3fa9c1\": the UTC start ",
      "time and six hex characters.",
      call. = FALSE
    )
  }
  preflight <- gpq_preflight(spec, rules = rules, settings = settings)
  rules <- validate_rule_set(rules)
  context <- conformance_context(spec, rules, settings, layers)
  # The tables are read after the argument checks, the settings' values and pre-flight, which
  # need none of them, so a mistake in an argument or a pre-flight stop never loads a file or
  # queries a database first (D17.32).
  read <- read_tables(tables, spec, table_map)
  tables <- read$tables
  validate_tables(tables)
  input <- conformance_input(tables, spec, group_cols, key_col, read$text_tables)
  if (is.null(run_id)) {
    run_id <- new_run_id(run_start)
  }
  state <- new_run_state()
  # A layer with no registered rule runs nothing (Layers 3 to 5 until M5, M10a and M10b).
  for (layer in intersect(layers, context$run_rules$layer)) {
    found <- switch(as.character(layer),
      "0" = check_structure(input, context),
      "1" = check_types(input, context),
      "2" = check_sentinels(input, context)
    )
    record_layer(found, layer, input, context, state, run_id)
  }
  registry <- context$registry
  # Each rule's rows were kept as lists of columns: bound here, once (D16.18, R11).
  results <- new_results(rbindlist(state$results, use.names = TRUE), group_cols, registry)
  issues <- rbindlist(state$issues, use.names = TRUE)
  issues <- new_issues(if (nrow(issues) == 0L) NULL else issues, group_cols, key_col, registry)
  # Where the caller's text enters the results (the names of its tables and columns, the
  # failing values with what was expected and the examples, the PKs and the problem unit's
  # key), valid UTF-8 left unmarked, as fread() leaves it, is marked UTF-8 in any locale and
  # never converted, since the writer reads unmarked text in the session's encoding. Text
  # marked latin1, which the writer converts, and text that isn't valid UTF-8 are left as
  # given (D16.46; D16.47, I1); group_index() holds the group values as UTF-8, and
  # validate_tables() has refused text marked as bytes. Only the run's own text that isn't
  # plain ASCII is read, never a caller's column (16.2; utf8_marked()).
  for (column in c("table_name", "column_name", "examples")) {
    set(results, j = column, value = utf8_marked(results[[column]]))
  }
  given_text <- c(
    "table_name", "column_name", "record_pk", "value", "expected", setdiff(key_col, group_cols)
  )
  for (column in given_text) {
    set(issues, j = column, value = utf8_marked(issues[[column]]))
  }
  fingerprints <- issue_fingerprint(issues)
  set(issues, j = "fingerprint", value = fingerprints$fingerprint)
  set(issues, j = "fingerprint_pk", value = fingerprints$fingerprint_pk)
  run_rules <- context$run_rules
  expected <- run_rules[!run_rules$rule_id %chin% state$not_expected]
  report_units <- if (length(group_cols) > 0L) {
    units <- unlist(lapply(input$groups, function(grouping) grouping$values[[group_cols[[1L]]]]))
    # With no table of the spec given there is no unit at all: NULL, made text here.
    setdiff(as.character(units), "*")
  } else {
    character()
  }
  inputs <- data.table(
    input = utf8_marked(paste0("tables:", input$given$table_name, recycle0 = TRUE)),
    file = "in memory",
    sha256 = NA_character_,
    detail = paste(
      input$given$n_rows, fifelse(input$given$n_rows == 1L, "row", "rows"),
      recycle0 = TRUE
    )
  )
  # A table read from a file or a connection is recorded as read_tables() describes it.
  read_from <- match(read$inputs$table_name, input$given$table_name)
  for (column in c("file", "sha256", "detail")) {
    set(inputs, i = read_from, j = column, value = utf8_marked(read$inputs[[column]]))
  }
  run <- run_metadata(run_id, run_start, Sys.time(), group_cols, report_units)
  metadata <- new_metadata(
    run, spec, preflight,
    rules = rules, settings = settings, inputs = inputs,
    self_test = run_self_test(results, expected)
  )
  new_gpq_results(results, issues, new_corrections(), metadata, registry)
}

#' A run's state while its layers are recorded: the results and issue rows so far, the next
#' issue ID, the failures later layers' checks are masked by (record-level cells, and
#' column-level failures by group), and the rules the self-test doesn't expect
#' @noRd
new_run_state <- function() {
  state <- new.env(parent = emptyenv())
  state$results <- list()
  state$issues <- list()
  state$next_id <- 1L
  state$failed <- list()
  state$column_failed <- list()
  state$not_expected <- character()
  state
}

#' The problem unit's key column, checked: `list(table, key_col)`, both named in the spec
#' @noRd
problem_unit_key <- function(problem_unit, spec) {
  if (is.null(problem_unit)) {
    return(NULL)
  }
  shaped <- is.list(problem_unit) && !is.data.frame(problem_unit) &&
    length(problem_unit) == 2L && all(vapply(problem_unit, function(x) {
    is.character(x) && length(x) == 1L && !is.na(x)
  }, logical(1)))
  if (!shaped) {
    stop("`problem_unit` must be NULL or list(table, key_col), two names.", call. = FALSE)
  }
  table <- problem_unit[[1L]]
  key_col <- problem_unit[[2L]]
  attributes <- spec$attributes
  if (!any(attributes$table_name == table & attributes$attribute_name == key_col)) {
    stop(sprintf(
      "`problem_unit` names %s.%s, which the specification doesn't have.", table, key_col
    ), call. = FALSE)
  }
  key_col
}

#' What every check of the run reads besides the tables
#'
#' The registry and the run's rules (conformance data rules of the asked layers), the rule
#' set's rules rows, the severity setting's entries, the settings the checks read, the codes
#' of each coded column, and each run rule's fingerprint.
#' @noRd
conformance_context <- function(spec, rules, settings, layers) {
  registry <- rule_registry()
  run_rules <- registry[registry$check_type != "preflight" & registry$stage == "conformance"]
  # Worked out before the brackets, so `layers` is never taken for a column of the registry.
  asked <- run_rules$layer %in% layers
  run_rules <- run_rules[asked]
  setting <- function(name) resolve_setting(name, settings, rules)$value
  codes <- spec$codes
  list(
    spec = spec, registry = registry, run_rules = run_rules, rule_rows = rules$rules,
    severity = severity_entries(settings),
    raw_na_policy = setting("raw_na_policy"), rogue_codes = setting("rogue_codes"),
    unknown_codes = setting("unknown_codes"),
    column_level_min_n = as.integer(setting("column_level_min_n")),
    example_cap = as.integer(setting("example_cap")),
    codes = split(codes$code, paste(codes$table_name, codes$attribute_name, sep = "\r")),
    fingerprints = if (nrow(run_rules) > 0L) {
      rule_fingerprint(run_rules$rule_id, spec, rules, settings, registry)
    } else {
      character()
    }
  )
}

#' A rule's outcome on a table or a column, which it checks as one unit (D16.8)
#' @noRd
unit_outcome <- function(rule_id, table_name, column_name, failed, n_records,
                         value = NA_character_, expected = NA_character_) {
  list(
    kind = "unit", rule_id = rule_id, table_name = table_name, column_name = column_name,
    failed = failed, n_records = as.integer(n_records), value = as.character(value),
    expected = as.character(expected)
  )
}

#' A rule's outcome on a column's records: the rows that fail (row numbers, a vector without
#' dimensions), each failing value as text (one per failing row), and what was expected (one
#' text, or one per failing row)
#' @noRd
record_outcome <- function(rule_id, table_name, column_name, fail, value,
                           expected = NA_character_) {
  rows <- is.numeric(fail) && is.null(dim(fail)) && !anyNA(fail) && all(is.finite(fail)) &&
    all(fail >= 1L & fail <= .Machine$integer.max & fail == trunc(fail)) &&
    anyDuplicated(fail) == 0L
  if (!rows) {
    stop(
      "Rule ", rule_id, ": `fail` must be row numbers, each once: whole numbers from 1 ",
      "within the integer range.",
      call. = FALSE
    )
  }
  if (length(value) != length(fail)) {
    stop("Rule ", rule_id, ": `value` must hold one text for each failing row.", call. = FALSE)
  }
  list(
    kind = "record", rule_id = rule_id, table_name = table_name, column_name = column_name,
    fail = as.integer(fail), value = as.character(value), expected = as.character(expected)
  )
}

#' A rule's outcome where it couldn't run, with the reason (6.2; D7.3, D9.24)
#' @noRd
not_run_outcome <- function(rule_id, table_name, column_name, reason) {
  list(
    kind = "not_run", rule_id = rule_id, table_name = table_name, column_name = column_name,
    reason = reason
  )
}

#' A rule's outcomes over the attributes in `scope` (rows of the input's columns table)
#'
#' not_run where the table wasn't given (no_table) or Layer 0 found no column for the
#' attribute (no_column) (D9.24); otherwise what `evaluate(table, attribute, x, row)` gives,
#' `x` the column as given and `row` the attribute's row of `scope`.
#' @noRd
rule_outcomes <- function(rule_id, scope, input, evaluate) {
  lapply(seq_len(nrow(scope)), function(i) {
    table <- scope$table_name[[i]]
    attribute <- scope$attribute_name[[i]]
    if (!scope$table_present[[i]]) {
      return(not_run_outcome(rule_id, table, attribute, "no_table"))
    }
    column <- scope$column[[i]]
    if (is.na(column)) {
      return(not_run_outcome(rule_id, table, attribute, "no_column"))
    }
    evaluate(table, attribute, input$data[[table]][[column]], scope[i])
  })
}

#' Each outcome's severity and whether its rule is on there (D16.6, D16.7)
#'
#' Severity: the severity setting's argument entry, then the rule set's most specific rows
#' (rule_row_values()), then the option entry, then the registry's default. On: the most
#' specific row's `enabled`, TRUE without one.
#' @noRd
outcome_values <- function(rule_id, table_names, column_names, default, context) {
  rows <- rule_row_values(
    context$rule_rows, rep(rule_id, length(table_names)), table_names, column_names
  )
  entries <- context$severity
  entry <- function(tier) {
    entries$severity[entries$tier == tier & entries$rule_id == rule_id][1L]
  }
  argument <- entry("argument")
  option <- entry("option")
  severity <- if (!is.na(argument)) {
    rep(argument, length(table_names))
  } else {
    fcoalesce(rows$severity, if (is.na(option)) default else option)
  }
  list(severity = severity, enabled = fcoalesce(rows$enabled, TRUE))
}

#' One layer's outcomes recorded in the run's state, rule by rule in the registry's order
#'
#' A rule the family returns no outcomes for has nothing to check and isn't expected by the
#' self-test (D16.17); nor is a rule turned off at every outcome (D16.7). A rule the family
#' doesn't return at all stays expected, so the self-test lists it.
#'
#' A unit rule (the registry's `unit`, D17.15) writes each of its not_run outcomes as one row
#' with "*" groups counting the one table or column it couldn't check, whatever its other
#' outcomes; a record rule's not_run rows count records by group (D16.29, R12).
#' @noRd
record_layer <- function(found, layer, input, context, state, run_id) {
  # Worked out before the brackets: `layer` is also a column of the run's rules.
  in_layer <- context$run_rules$layer == layer
  rules <- context$run_rules[in_layer]
  unit_not_run <- function(outcome, rule, severity) {
    state$results[[length(state$results) + 1L]] <- results_rows(
      run_id, rule, severity, context, outcome$table_name, outcome$column_name,
      star_groups(input),
      n_checked = 0L, n_fail = 0L, n_not_evaluated = 1L, n_distinct = 0L,
      examples = NA_character_, status = "not_run", reason = outcome$reason
    )
  }
  for (k in seq_len(nrow(rules))) {
    rule <- rules[k]
    if (!rule$rule_id %chin% names(found)) {
      next
    }
    outcomes <- found[[rule$rule_id]]
    if (length(outcomes) == 0L) {
      state$not_expected <- c(state$not_expected, rule$rule_id)
      next
    }
    values <- outcome_values(
      rule$rule_id, vapply(outcomes, `[[`, "", "table_name"),
      vapply(outcomes, `[[`, "", "column_name"), rule$default_severity, context
    )
    on <- which(values$enabled)
    if (length(on) == 0L) {
      state$not_expected <- c(state$not_expected, rule$rule_id)
      next
    }
    kinds <- vapply(outcomes, function(outcome) as.character(outcome$kind)[1L], "")
    for (i in on) {
      outcome <- outcomes[[i]]
      switch(kinds[[i]],
        unit = record_unit(outcome, rule, values$severity[[i]], input, context, state, run_id),
        record = record_records(
          outcome, rule, values$severity[[i]], layer, input, context, state, run_id
        ),
        not_run = if (isTRUE(rule$unit)) {
          unit_not_run(outcome, rule, values$severity[[i]])
        } else {
          record_not_run(outcome, rule, values$severity[[i]], input, context, state, run_id)
        },
        stop(sprintf(
          "Rule %s returned an outcome of kind \"%s\", which the run can't record.",
          rule$rule_id, kinds[[i]]
        ), call. = FALSE)
      )
    }
  }
  invisible(state)
}

#' Results rows for one rule at a table and column, one per group given, as a list of
#' columns: the run binds every rule's rows into its results table once (D16.18)
#'
#' `groups` is a list of the group columns' values, one per results row (or NULL without
#' group columns); the counts and texts are one per row, or one for all.
#' @noRd
results_rows <- function(run_id, rule, severity, context, table_name, column_name, groups,
                         n_checked, n_fail, n_not_evaluated, n_distinct, examples, status,
                         reason = NA_character_) {
  c(
    list(
      run_id = run_id, run_scope = "package", rule_id = rule$rule_id,
      rule_fingerprint = context$fingerprints[[rule$rule_id]], stage = rule$stage,
      layer = rule$layer, severity = severity, table_name = table_name,
      column_name = column_name, n_checked = as.integer(n_checked),
      n_fail = as.integer(n_fail), n_not_evaluated = as.integer(n_not_evaluated),
      reason_code = "*", n_distinct_fail_values = as.integer(n_distinct), n_easy_fix = 0L,
      examples = as.character(examples), status = status, not_run_reason = reason,
      message_id = rule$message_id
    ),
    groups
  )
}

#' Issue rows for one rule at a table and column, numbered from the state's next ID, as a
#' list of columns the run binds into its issues table once (D16.18)
#'
#' `fields` holds the columns that vary (record_pk, row_number, pk_positional, level,
#' n_records, value, expected, root_issue_id, status), each one value per issue or one for
#' all; `groups` the group columns' values and `key` the key column's text, one per row (NULL
#' without them).
#' @noRd
issue_rows <- function(run_id, rule, severity, context, table_name, column_name, fields,
                       groups, key, input, state) {
  n <- max(lengths(fields))
  if (n == 0L) {
    return(invisible(integer()))
  }
  ids <- state$next_id + seq_len(n) - 1L
  state$next_id <- state$next_id + n
  rows <- c(
    list(
      issue_id = ids, run_id = run_id, run_scope = "package", rule_id = rule$rule_id,
      rule_fingerprint = context$fingerprints[[rule$rule_id]], stage = rule$stage,
      layer = rule$layer, severity = severity, reason_code = "*", table_name = table_name,
      column_name = column_name
    ),
    fields, groups
  )
  if (!is.null(input$key_col) && !input$key_col %chin% names(groups)) {
    rows[[input$key_col]] <- key
  }
  state$issues[[length(state$issues) + 1L]] <- rows
  invisible(ids)
}

#' Group values of "*" in every group column, `n` rows, as a list of columns; NULL without
#' group columns
#' @noRd
star_groups <- function(input, n = 1L) {
  if (length(input$group_cols) == 0L) {
    return(NULL)
  }
  stars <- rep(list(rep("*", n)), length(input$group_cols))
  names(stars) <- input$group_cols
  stars
}

#' The group columns' values at some of a table's groups (`at`, indexes into
#' `grouping$values`), as a list of columns read by position, never a data.table subset
#' (D16.18); NULL without group columns
#' @noRd
group_values <- function(grouping, at) {
  if (is.null(grouping$values)) {
    return(NULL)
  }
  lapply(grouping$values, function(column) column[at])
}

#' A unit outcome recorded: one results row with "*" groups, and one column-level issue when
#' it failed, masking nothing (D16.8, D16.9)
#' @noRd
record_unit <- function(outcome, rule, severity, input, context, state, run_id) {
  failed <- isTRUE(outcome$failed)
  state$results[[length(state$results) + 1L]] <- results_rows(
    run_id, rule, severity, context, outcome$table_name, outcome$column_name, star_groups(input),
    n_checked = 1L, n_fail = as.integer(failed), n_not_evaluated = 0L,
    n_distinct = as.integer(failed && !is.na(outcome$value)),
    examples = if (failed) outcome$value else NA_character_,
    status = if (failed) "fail" else "pass"
  )
  if (failed) {
    issue_rows(
      run_id, rule, severity, context, outcome$table_name, outcome$column_name,
      list(
        record_pk = "*", row_number = NA_integer_, pk_positional = NA, level = "column",
        n_records = outcome$n_records, value = outcome$value, expected = outcome$expected,
        root_issue_id = NA_integer_, status = "open"
      ),
      star_groups(input), "*", input, state
    )
  }
  invisible(state)
}

#' A record rule's not_run outcome recorded: one row per group of the table's records, each
#' counting them as not evaluated; "*" groups and no records where the table wasn't given,
#' has no rows or the outcome names no table (6.2; D7.3, D9.24). A unit rule's not_run
#' outcome is written by record_layer() (D16.29).
#' @noRd
record_not_run <- function(outcome, rule, severity, input, context, state, run_id) {
  table <- outcome$table_name
  grouping <- input$groups[[table]]
  if (is.null(grouping) || length(grouping$row_group) == 0L) {
    groups <- star_groups(input)
    counts <- 0L
  } else {
    in_group <- tabulate(grouping$row_group, grouping$n)
    present <- which(in_group > 0L)
    groups <- group_values(grouping, present)
    counts <- in_group[present]
  }
  state$results[[length(state$results) + 1L]] <- results_rows(
    run_id, rule, severity, context, table, outcome$column_name, groups,
    n_checked = 0L, n_fail = 0L, n_not_evaluated = counts, n_distinct = 0L,
    examples = NA_character_, status = "not_run", reason = outcome$reason
  )
  invisible(state)
}

#' The records a check of `layer` on a table's column mustn't evaluate, each with its root
#' (6.2; D2.10, R17, R18)
#'
#' Record-level failures of earlier layers come through failed_cells() and
#' mark_not_evaluated(); a column-level failure of an earlier layer masks every record of
#' its group. A record masked twice keeps the lowest issue ID, which is the lowest layer's,
#' as IDs are given layer by layer. Returns `row_number` and `root_issue_id`, or NULL where
#' nothing of Layers 1 to 3 failed in the column, so no table is built (D16.18).
#' @noRd
cascade_masks <- function(state, table, attribute, layer, row_group) {
  key <- paste(table, attribute, sep = "\r")
  pieces <- state$failed[[key]]
  columns <- state$column_failed[[key]]
  if (is.null(pieces) && is.null(columns)) {
    return(NULL)
  }
  masks <- list(data.table(row_number = integer(), root_issue_id = integer()))
  if (!is.null(pieces)) {
    masks[[2L]] <- mark_not_evaluated(
      failed_cells(rbindlist(pieces), layers = 1:3), table, attribute, layer
    )
  }
  if (!is.null(columns)) {
    whole <- rbindlist(columns)
    # Worked out before the brackets: `layer` is also a column of `whole`.
    earlier <- whole$layer < layer
    whole <- whole[earlier]
    if (nrow(whole) > 0L) {
      whole <- whole[order(whole$issue_id)]
      whole <- whole[!duplicated(whole$group)]
      rows <- which(row_group %in% whole$group)
      masks[[3L]] <- data.table(
        row_number = rows, root_issue_id = whole$issue_id[match(row_group[rows], whole$group)]
      )
    }
  }
  all_masks <- rbindlist(masks)
  all_masks <- all_masks[order(all_masks$row_number, all_masks$root_issue_id)]
  all_masks[!duplicated(all_masks$row_number)]
}

#' A record outcome recorded: results per group, issues for the failing and masked records
#'
#' Masked records (cascade_masks()) are taken out of the failures and counted as not
#' evaluated. In a group where every checked record fails and at least
#' `column_level_min_n` were checked, the failures become one column-level issue (6.3);
#' masked records sharing one root, as many as that, become one not_evaluated column-level
#' issue (D16.10). Failures of Layers 1 to 3 are kept for the cascade of later layers. A
#' table with no rows gives one row with "*" groups and nothing checked (D16.17).
#' @noRd
record_records <- function(outcome, rule, severity, layer, input, context, state, run_id) {
  table <- outcome$table_name
  attribute <- outcome$column_name
  grouping <- input$groups[[table]]
  row_group <- grouping$row_group
  if (length(row_group) == 0L) {
    state$results[[length(state$results) + 1L]] <- results_rows(
      run_id, rule, severity, context, table, attribute, star_groups(input),
      n_checked = 0L, n_fail = 0L, n_not_evaluated = 0L, n_distinct = 0L,
      examples = NA_character_, status = "pass"
    )
    return(invisible(state))
  }
  n_groups <- grouping$n
  masked <- cascade_masks(state, table, attribute, layer, row_group)
  masked_rows <- if (is.null(masked)) integer() else masked$row_number
  kept <- !outcome$fail %in% masked_rows
  fail <- outcome$fail[kept]
  value <- outcome$value[kept]
  # Valid UTF-8 left unmarked, as fread() leaves it, is marked UTF-8 before the values are
  # grouped and joined into examples, as gpq_check_conformance() marks the rest of the
  # caller's text, so neither reads it in the session's encoding; any other value is left as
  # given (D16.46; D16.47, I1). Only values that aren't plain ASCII are read.
  value <- utf8_marked(value)
  # A value marked as bytes keeps its bytes but loses the mark, which data.table's grouping
  # and the results' writer stop on; it is then held as unmarked text, as text that isn't
  # valid UTF-8 is (D16.36). validate_tables() refuses such text first (D16.38): this is a
  # second line.
  value <- bytes_unmarked(value)
  expected <- if (length(outcome$expected) == 1L) {
    rep(outcome$expected, length(fail))
  } else {
    outcome$expected[kept]
  }
  in_group <- tabulate(row_group, n_groups)
  masked_count <- tabulate(row_group[masked_rows], n_groups)
  fail_count <- tabulate(row_group[fail], n_groups)
  checked <- in_group - masked_count
  n_distinct <- integer(n_groups)
  examples <- rep(NA_character_, n_groups)
  # Only a check that failed somewhere has values to count and show, so a passing check
  # builds no table (D16.18).
  if (length(fail) > 0L) {
    failed_values <- unique(data.table(group = row_group[fail], value = value))
    n_distinct <- tabulate(failed_values$group, n_groups)
    shown <- failed_values[!is.na(failed_values$value)]
    shown <- shown[rowid(shown$group) <= context$example_cap]
    joined <- vapply(split(shown$value, shown$group), paste, "", collapse = "; ")
    examples[as.integer(names(joined))] <- joined
  }
  present <- which(in_group > 0L)
  state$results[[length(state$results) + 1L]] <- results_rows(
    run_id, rule, severity, context, table, attribute, group_values(grouping, present),
    n_checked = checked[present], n_fail = fail_count[present],
    n_not_evaluated = masked_count[present], n_distinct = n_distinct[present],
    examples = examples[present], status = fifelse(fail_count[present] > 0L, "fail", "pass")
  )
  cascades <- layer %in% 1:3
  key <- paste(table, attribute, sep = "\r")
  # Failures: column-level per group where 6.3 says so, record-level otherwise.
  whole <- which(collapse_column_level(fail_count, checked, context$column_level_min_n))
  if (length(whole) > 0L) {
    ids <- issue_rows(
      run_id, rule, severity, context, table, attribute,
      list(
        record_pk = "*", row_number = NA_integer_, pk_positional = NA, level = "column",
        n_records = fail_count[whole], value = NA_character_,
        expected = if (length(outcome$expected) == 1L) outcome$expected else NA_character_,
        root_issue_id = NA_integer_, status = "open"
      ),
      group_values(grouping, whole), rep("*", length(whole)), input, state
    )
    if (cascades) {
      state$column_failed[[key]] <- c(state$column_failed[[key]], list(data.table(
        group = whole, layer = layer, issue_id = ids
      )))
    }
  }
  single <- !row_group[fail] %in% whole
  rows <- fail[single]
  if (length(rows) > 0L) {
    pk <- record_pk_text(input, table, rows)
    ids <- issue_rows(
      run_id, rule, severity, context, table, attribute,
      list(
        record_pk = pk$record_pk, row_number = rows, pk_positional = pk$positional,
        level = "record", n_records = NA_integer_, value = value[single],
        expected = expected[single], root_issue_id = NA_integer_, status = "open"
      ),
      group_values(grouping, row_group[rows]), key_text(input, table, rows), input, state
    )
    if (cascades) {
      state$failed[[key]] <- c(state$failed[[key]], list(data.table(
        table_name = table, column_name = attribute, row_number = rows, layer = layer,
        issue_id = ids, level = "record", status = "open"
      )))
    }
  }
  if (length(masked_rows) > 0L) {
    record_masked(masked, outcome, rule, severity, input, context, state, run_id, row_group)
  }
  invisible(state)
}

#' A check's masked records recorded as not_evaluated issues, those sharing one root in a
#' group collapsed into one column-level issue when there are at least column_level_min_n
#' (D16.10)
#' @noRd
record_masked <- function(masked, outcome, rule, severity, input, context, state, run_id,
                          row_group) {
  table <- outcome$table_name
  attribute <- outcome$column_name
  grouping <- input$groups[[table]]
  shared <- data.table(group = row_group[masked$row_number], root = masked$root_issue_id)
  sizes <- shared[, list(n = .N), by = c("group", "root")]
  whole <- sizes[collapse_column_level(sizes$n, sizes$n, context$column_level_min_n)]
  if (nrow(whole) > 0L) {
    issue_rows(
      run_id, rule, severity, context, table, attribute,
      list(
        record_pk = "*", row_number = NA_integer_, pk_positional = NA, level = "column",
        n_records = whole$n, value = NA_character_, expected = NA_character_,
        root_issue_id = whole$root, status = "not_evaluated"
      ),
      group_values(grouping, whole$group), rep("*", nrow(whole)), input, state
    )
  }
  single <- is.na(whole[shared, on = c("group", "root"), which = TRUE, mult = "first"])
  rows <- masked$row_number[single]
  if (length(rows) > 0L) {
    column <- input$columns$column[
      input$columns$table_name == table & input$columns$attribute_name == attribute
    ]
    pk <- record_pk_text(input, table, rows)
    issue_rows(
      run_id, rule, severity, context, table, attribute,
      list(
        record_pk = pk$record_pk, row_number = rows, pk_positional = pk$positional,
        level = "record", n_records = NA_integer_,
        value = value_text(input$data[[table]][[column]][rows]), expected = NA_character_,
        root_issue_id = masked$root_issue_id[single], status = "not_evaluated"
      ),
      group_values(grouping, row_group[rows]), key_text(input, table, rows), input, state
    )
  }
  invisible(state)
}
