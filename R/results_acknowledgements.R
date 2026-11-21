# The acknowledgements register (plan 3.4, 11.8; D4.2, D4.8, D8.16, D23.13, D23.21, D23.26,
# D23.34): gpq_apply_acknowledgements() matches each entry of your register to a run's issues as
# the comparison with an earlier run does (match_issues(), R/results_compare.R), a record-level
# entry on the group columns the register holds too (D23.111, D23.128), gives an issue
# whose entry still holds the status acknowledged, and lists the entries whose issue's value has
# changed as lapsed. The register is read as pre-flight reads its run inputs
# (read_input_table()); the issues are read by column and set by reference, never copied whole
# (16.2).

#' Apply your acknowledgements to a run's issues
#'
#' @description
#' Gives each issue your register acknowledges, whose value is still the one you acknowledged,
#' the status `acknowledged`, and lists the register's entries whose issue's value has changed
#' since as lapsed. The issues' `status` is set by reference.
#'
#' @details
#' An entry matches an issue as [gpq_compare_runs()] matches an earlier run's: by its
#' `fingerprint`, or failing that its `fingerprint_pk` where no other record-level issue of the
#' run has it, and the same `run_scope`, so it still
#' matches after the contributor's provider keys are decoded differently. An entry for a
#' column-level issue, which names no record, also needs the issue's values in the register's
#' group columns. An entry for a record-level issue also needs the issue's values in the group
#' columns the register holds; an entry with "*" in one of them matches every issue's value
#' there. An entry whose `fingerprint` and `fingerprint_pk`
#' are both blank matches nothing, and neither does an entry or an issue of a table in
#' `unmatched_tables`. An entry naming a rule renamed or split since matches the new rules'
#' issues.
#'
#' An entry holds while its issue's value is the entry's `acknowledged_value`, a blank value
#' equal to a blank one: the issue's status becomes `acknowledged`. Otherwise the entry has
#' lapsed: it is listed in `lapsed` with the issue's value, and the issue keeps its status. Only
#' an open issue, or one already acknowledged, is acknowledged: an issue that wasn't evaluated
#' keeps its status, and its entry isn't listed as lapsed.
#'
#' @param issues The `issues` table of a run's results, from [gpq_check_conformance()]: a
#'   data.table, changed by reference.
#' @param acknowledgements Your register: a data.frame or the path of a CSV file, read as text,
#'   with the columns `fingerprint`, `fingerprint_pk`, `rule_id`, `table_name`, `column_name`,
#'   `provider_key`, `acknowledged_value`, `decision` (`verified` or `rejected_proposal`),
#'   `who`, `when`, `why` and `run_scope` (`package` or `merged`), and the issues' group
#'   columns, each filled from the issue the entry acknowledges. [gpq_preflight()] checks it
#'   when given as its input `acknowledgements`. Give the file's path, or read it with
#'   `colClasses = "character"`: an `acknowledged_value` such as `"4.20"` read as the number 4.2
#'   no longer matches, so its entry lapses.
#' @param unmatched_tables `NULL`, or the names of tables whose issues aren't matched across
#'   runs, as [gpq_compare_runs()] takes them.
#' @return A list: `issues`, with `status` set; and `lapsed`, a new data.table of the entries
#'   that match an issue whose value has changed, their columns as read with the issue's value
#'   as `current_value`, one row for each entry and value.
#' @seealso [gpq_compare_runs()], which matches issues the same way but for the register's group
#'   columns, and [gpq_preflight()] for the checks of the register.
#' @examples
#' # magp_example with two trees' distances missing, which Layer 2 finds. MAGPlot's bundled
#' # specification has known pre-flight warnings, so this warning is expected.
#' trees <- data.table::copy(magp_example$magp_trees)
#' trees$tree_dist[1:2] <- NA
#' planted <- magp_example
#' planted$magp_trees <- trees
#' results <- gpq_check_conformance(
#'   planted, magp_spec(),
#'   group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
#'   rules = magp_rules(), layers = 2
#' )
#' issues <- results$issues
#' # Your register: the first tree's missing distance checked with its provider; the second
#' # acknowledged when it held 4.2, which it no longer does.
#' acknowledgements <- data.frame(
#'   fingerprint = issues$fingerprint, fingerprint_pk = issues$fingerprint_pk,
#'   rule_id = issues$rule_id, table_name = issues$table_name,
#'   column_name = issues$column_name, provider_key = issues$provider_key,
#'   acknowledged_value = c(NA, "4.2"), decision = "verified", who = "A. Reviewer",
#'   when = "2026-11-17", why = "Not measured at this visit.", run_scope = issues$run_scope,
#'   magp_contributor_id = issues$magp_contributor_id, jurisdiction = issues$jurisdiction,
#'   magp_dataset_id = issues$magp_dataset_id
#' )
#' applied <- gpq_apply_acknowledgements(issues, acknowledgements)
#' applied$issues[, c("rule_id", "record_pk", "status")]
#' applied$lapsed[, c("rule_id", "acknowledged_value", "current_value")]
#' @export
gpq_apply_acknowledgements <- function(issues, acknowledgements, unmatched_tables = NULL) {
  needed <- c(
    "run_scope", "rule_id", "table_name", "level", "value", "status", "fingerprint",
    "fingerprint_pk"
  )
  if (!(is.data.table(issues) && all(needed %chin% names(issues)))) {
    # W176 (gpq_decode_lineage()'s text)
    stop(
      "`issues` must be the issues table of a run's results, a data.table, as ",
      "gpq_check_conformance() returns it.",
      call. = FALSE
    )
  }
  # The issues' group columns: the caller's columns, beside 9.3's own.
  groups <- setdiff(names(issues), names(results_schema()$issues))
  typed <- vapply(groups, function(column) !is.character(issues[[column]]), logical(1L))
  if (any(typed)) {
    # M8a1 D23.98
    stop(sprintf(
      "The group columns %s of `issues` must be text, as gpq_check_conformance() returns them.",
      paste(groups[typed], collapse = ", ")
    ), call. = FALSE)
  }
  typed_own <- c("value", "status")[
    !vapply(c("value", "status"), function(column) is.character(issues[[column]]), logical(1L))
  ]
  if (length(typed_own) > 0L) {
    # M8a1 D23.101
    stop(sprintf(
      "The columns %s of `issues` must be text, as gpq_check_conformance() returns them.",
      paste(typed_own, collapse = ", ")
    ), call. = FALSE)
  }
  tables_ok <- is.null(unmatched_tables) ||
    (is.character(unmatched_tables) && !anyNA(unmatched_tables))
  if (!tables_ok) {
    # M8a1 W14, gpq_compare_runs()'s words
    stop("`unmatched_tables` must be NULL or a character vector of table names.", call. = FALSE)
  }
  read <- read_input_table(acknowledgements, "acknowledgements")
  stop_if_unclean(read, "acknowledgements")
  data <- read$data
  own <- run_input_schema()$acknowledgements
  missing <- setdiff(own, names(data))
  extra <- setdiff(names(data), c(own, groups))
  if (length(missing) > 0L || length(extra) > 0L) {
    problems <- c(
      if (length(missing) > 0L) paste("missing:", paste(missing, collapse = ", ")),
      if (length(extra) > 0L) {
        paste("not taken:", paste(mark_invalid_utf8(extra), collapse = ", "))
      }
    )
    # M8a1 W24, W229's form
    stop(sprintf(
      paste(
        "`acknowledgements` must have the columns %s, and may have the issues' group columns",
        "(%s); %s."
      ),
      paste(own, collapse = ", "),
      if (length(groups) > 0L) paste(groups, collapse = ", ") else "the issues have none",
      paste(problems, collapse = "; ")
    ), call. = FALSE)
  }
  coded <- list(decision = c("verified", "rejected_proposal"), run_scope = c("package", "merged"))
  for (column in names(coded)) {
    bad <- which(!data[[column]] %chin% coded[[column]])
    if (length(bad) > 0L) {
      first <- bad[[1L]]
      cell <- data[[column]][[first]]
      # M8a1 W25, W230's form
      stop(sprintf(
        "The acknowledgement at %s gives the %s %s; an acknowledgement's %s is %s.",
        input_place(read, "acknowledgements", first), column,
        if (is.na(cell)) "(blank)" else paste0("\"", mark_invalid_utf8(cell), "\""), column,
        paste(coded[[column]], collapse = " or ")
      ), call. = FALSE)
    }
  }
  # A column-level entry matches on the group columns the register holds (D23.21, D23.26).
  held <- intersect(groups, names(data))
  pairs <- match_issues(issues, data, held, unmatched_tables)
  if (length(held) > 0L) {
    # A record-level entry matches only an issue with its values in the group columns the
    # register holds, "*" in one of them matching every issue's value there (D23.111, D23.128).
    # An issue whose fingerprint met only other groups' entries is matched again by its pk: form
    # alone, as an issue its fingerprint matches nothing is (D9.22): its fingerprint is left
    # out of a copy of those rows' matching columns, and so is a pk: form another record-level
    # issue holds too (D23.148). Each test worked out before the brackets.
    own_unit <- function(pairs) {
      own <- issues$level[pairs$row] %chin% "column"
      record <- which(!own)
      ok <- rep(TRUE, length(record))
      for (column in held) {
        given <- data[[column]][pairs$at[record]]
        ok <- ok & (given %chin% "*" | (given == issues[[column]][pairs$row[record]]) %in% TRUE)
      }
      own[record] <- ok
      own
    }
    own <- own_unit(pairs)
    left <- setdiff(pairs$row[!own], pairs$row[own])
    pairs <- pairs[own]
    if (length(left) > 0L) {
      columns <- c("table_name", "level", "run_scope", "fingerprint_pk")
      by_pk <- setDT(lapply(structure(columns, names = columns), function(column) {
        issues[[column]][left]
      }))
      set(by_pk, j = "fingerprint", value = NA_character_)
      # A pk: form another record-level issue holds too matches nothing, as in match_issues(),
      # which sees only the issues matched again here (D23.148).
      reached <- which(!issues$level %chin% "column" & !issues$table_name %chin% unmatched_tables)
      forms <- issues$fingerprint_pk[reached]
      shared <- which(by_pk$fingerprint_pk %chin% forms[duplicated(forms, incomparables = NA)])
      set(by_pk, i = shared, j = "fingerprint_pk", value = NA_character_)
      again <- match_issues(by_pk, data, character(), unmatched_tables)
      set(again, j = "row", value = left[again$row])
      own_again <- own_unit(again)
      pairs <- rbind(pairs, again[own_again])
    }
  }
  # Only an open issue, or one acknowledged already, takes the status (D23.47).
  takes <- issues$status[pairs$row] %chin% c("open", "acknowledged")
  pairs <- pairs[takes]
  # A blank value is compared as the register's reader reads a blank cell: as NA (D19.51).
  value <- issues$value[pairs$row]
  value[is_blank(value)] <- NA_character_
  given <- data$acknowledged_value[pairs$at]
  holds <- fifelse(is.na(value), is.na(given), !is.na(given) & value == given)
  set(issues, i = unique(pairs$row[holds]), j = "status", value = "acknowledged")
  changed <- pairs[!holds]
  at <- changed$at
  lapsed <- data[at]
  set(lapsed, j = "current_value", value = issues$value[changed$row])
  list(issues = issues, lapsed = unique(lapsed))
}
