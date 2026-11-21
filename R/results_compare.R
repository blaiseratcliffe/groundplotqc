# Comparing a run with an earlier one (plan 3.4, 9.6, 11.8; D4.2, D4.7, D8.16, D9.22, D15.9,
# D23.13, D23.21, D23.30 to D23.33, D23.41): gpq_compare_runs() marks each issue of a run new or
# persisting, with the run it was first seen in and, for a new issue, whether the data or the
# rules and specification changed, and lists the earlier run's issues it no longer has as
# resolved. match_issues() is the matching it shares with gpq_apply_acknowledgements()
# (R/results_acknowledgements.R); read_previous() reads the earlier run. The issues are read by
# column and set by reference, never copied whole, and each match is one key join (16.2).

#' The rows of `other` each issue matches: the fingerprint first, then its pk: form (9.6, 11.8;
#' D4.2, D8.16, D9.22, D15.9, D23.13, D23.21)
#'
#' `issues` is a run's issues table; `other` a previous run's issues or an acknowledgements
#' register, holding `fingerprint`, `fingerprint_pk`, `rule_id`, `table_name` and `run_scope`,
#' and the `group_cols` a column-level issue is matched on. An issue matches a row of `other`
#' with its own fingerprint and run scope; an issue that matches none that way matches a row
#' with its own pk: form and run scope, so a change in a contributor's decoding doesn't break the
#' match (D9.22), nor does a row giving only the pk: form (R51), where no other record-level
#' issue of `issues` holds that pk: form (D23.148). A column-level issue names no
#' record, so it matches on the group columns too, in both passes (D23.21), and on none where
#' `other` lacks one of them. An NA fingerprint or
#' pk: form matches nothing, on either side (D15.9), and an issue or row of a table in
#' `unmatched_tables` nothing at all (D12.75). A rule ID the rule ID map lists is read as each of
#' its new rule IDs, at the start of `other`'s fingerprints, a split rule's row once per new rule
#' (D4.2). A column of `other` that isn't text is read as text. Returns a data.table of `row` (an
#' issue) and `at` (a row of `other`), each pair once.
#' @noRd
match_issues <- function(issues, other, group_cols, unmatched_tables = NULL) {
  text <- function(x) if (is.character(x)) x else as_text(x)
  at <- seq_len(nrow(other))
  prints <- list(fingerprint = text(other$fingerprint), fingerprint_pk = text(other$fingerprint_pk))
  map <- rule_id_map()
  if (nrow(map) > 0L) {
    # A renamed rule's rows once, a split rule's once per new rule, each fingerprint starting
    # with its new rule ID (9.6, D4.2).
    asked <- data.table(rule_id = text(other$rule_id), at = at)
    mapped <- map[asked, on = c(old_rule_id = "rule_id"), allow.cartesian = TRUE]
    at <- mapped$at
    renamed <- which(!is.na(mapped$new_rule_id))
    old <- paste0(mapped$old_rule_id[renamed], "|")
    prints <- lapply(prints, function(x) {
      x <- x[at]
      starts <- which(startsWith(x[renamed], old))
      moved <- renamed[starts]
      x[moved] <- paste0(
        mapped$new_rule_id[moved], "|", substring(x[moved], nchar(old[starts]) + 1L)
      )
      x
    })
  }
  kept <- which(!text(other$table_name)[at] %chin% unmatched_tables)
  # The other side's keys under names of the join's own, so no caller's group column meets them.
  group_names <- sprintf("group_%d", seq_along(group_cols))
  grouped <- all(group_cols %chin% names(other))
  side <- setDT(c(
    list(
      fingerprint = prints$fingerprint[kept], fingerprint_pk = prints$fingerprint_pk[kept],
      run_scope = text(other$run_scope)[at[kept]]
    ),
    if (grouped) {
      structure(
        lapply(group_cols, function(column) text(other[[column]])[at[kept]]),
        names = group_names
      )
    },
    list(origin = at[kept])
  ))
  rows <- which(!issues$table_name %chin% unmatched_tables)
  column <- rows[issues$level[rows] %chin% "column"]
  record <- rows[!issues$level[rows] %chin% "column"]
  by_print <- key_matches(
    list(issues$fingerprint[record], issues$run_scope[record]), record, side,
    c("fingerprint", "run_scope")
  )
  # A record-level issue the fingerprint didn't match, by its pk: form (D9.22), where no other
  # record-level issue holds that form: issues sharing one, as records sharing a duplicated PK
  # give, match by their fingerprints alone (D23.148). The form carries its rule, table and
  # column, so it is counted as it is.
  forms <- text(issues$fingerprint_pk[record])
  shared <- forms %chin% forms[duplicated(forms, incomparables = NA)]
  unmatched <- record[!shared & !record %in% by_print$row]
  by_pk <- key_matches(
    list(issues$fingerprint_pk[unmatched], issues$run_scope[unmatched]), unmatched, side,
    c("fingerprint_pk", "run_scope")
  )
  by_group <- by_group_pk <- NULL
  if (grouped && length(column) > 0L) {
    groups <- function(at) lapply(group_cols, function(group) issues[[group]][at])
    by_group <- key_matches(
      c(list(issues$fingerprint[column], issues$run_scope[column]), groups(column)),
      column, side, c("fingerprint", "run_scope", group_names)
    )
    # A column-level issue the fingerprint didn't match, by its pk: form too, as a register
    # row may give only that (D9.22, R51).
    left <- column[!column %in% by_group$row]
    by_group_pk <- key_matches(
      c(list(issues$fingerprint_pk[left], issues$run_scope[left]), groups(left)),
      left, side, c("fingerprint_pk", "run_scope", group_names)
    )
  }
  pairs <- rbindlist(list(by_print, by_pk, by_group, by_group_pk))
  unique(data.table(row = pairs$row, at = side$origin[pairs$at]))
}

#' The earlier run gpq_compare_runs() compares with: its issues and, from a run folder, its
#' rules' fingerprints (3.6, 11.8; D23.33)
#'
#' `previous` is a run folder, of which only metadata/run.csv, tables/issues.csv and
#' tables/results.csv are read, so any folder a completed run of the package wrote will do, an
#' older one included (D23.33); or an issues table, a data.frame, read where it is and never
#' copied, a blank cell of a column the comparison reads taken as missing (D15.9, D19.51) and
#' only such a column read anew, or the path of a CSV file. A folder's files and an issues file
#' are read with their columns' classes, any other column as text, and stop on a file that
#' doesn't read cleanly (read_run_csv()). Caller's errors: any other `previous`, a folder of a
#' run that stopped at pre-flight, and issues lacking a column the comparison reads
#' (run_input_schema()). Returns `issues` and `rules`, each rule's fingerprint in the run's
#' results (`rule_id`, `rule_fingerprint`), NULL for an issues table.
#' @noRd
read_previous <- function(previous) {
  folder <- is.character(previous) && length(previous) == 1L && !is.na(previous) &&
    dir.exists(previous)
  rules <- NULL
  if (folder) {
    run <- read_run_csv(file.path(previous, "metadata", "run.csv"), metadata_schema()$run)
    if (!identical(run$outcome, "completed")) {
      # gpq_read_results()'s words
      stop(
        "The run in ", previous, " stopped at pre-flight, so it has no results to read.",
        call. = FALSE
      )
    }
    schema <- results_schema()
    issues <- read_run_csv(file.path(previous, "tables", "issues.csv"), schema$issues)
    results <- read_run_csv(file.path(previous, "tables", "results.csv"), schema$results)
    first <- !duplicated(results$rule_id)
    rules <- data.table(
      rule_id = results$rule_id[first], rule_fingerprint = results$rule_fingerprint[first]
    )
  } else if (is.data.frame(previous)) {
    issues <- previous
    # A blank cell, as utils::read.csv() and fread() read an empty one, is missing, as the run
    # inputs' blanks are (3.6; D15.9, D19.51). Only the columns the comparison reads are looked
    # at, and only a cell that is empty or starts with a space can be blank, so is_blank() reads
    # those alone; a column holding one is read anew, the others are used where they are.
    compared <- intersect(run_input_schema()$previous, names(previous))
    blanks <- lapply(compared, function(column) {
      values <- previous[[column]]
      if (!is.character(values)) {
        return(integer())
      }
      maybe <- which(
        !nzchar(values) | startsWith(values, " ") | startsWith(values, "\t") |
          startsWith(values, "\r") | startsWith(values, "\n")
      )
      maybe[is_blank(values[maybe])]
    })
    holding <- which(lengths(blanks) > 0L)
    if (length(holding) > 0L) {
      columns <- lapply(previous, identity)
      for (k in holding) {
        columns[[compared[[k]]]][blanks[[k]]] <- NA_character_
      }
      issues <- setDT(columns)
    }
  } else if (is_path_to(previous, "csv")) {
    issues <- read_run_csv(previous, results_schema()$issues)
  } else {
    # M8a1 W15
    stop(
      "`previous` must be a run folder, a data.frame or the path of an existing .csv file.",
      call. = FALSE
    )
  }
  lacking <- setdiff(run_input_schema()$previous, names(issues))
  if (length(lacking) > 0L) {
    # M8a1 W16
    stop(sprintf(
      "`previous` lacks the issues columns %s.", paste(lacking, collapse = ", ")
    ), call. = FALSE)
  }
  list(issues = issues, rules = rules)
}

#' Compare a run's issues with an earlier run's
#'
#' @description
#' Marks each issue of a run `new` or `persisting` against an earlier run, gives it the run it
#' was first seen in and, for a new issue, whether the data or the rules and specification
#' changed, and lists the earlier run's issues the run no longer has as `resolved`. The
#' issues' `first_seen_run`, `change` and `change_cause` are set by reference.
#'
#' @details
#' An issue persists when the earlier run has an issue with the same `fingerprint`, or failing
#' that the same `fingerprint_pk`, and the same `run_scope`, so it still matches after the
#' contributor's provider keys are decoded differently. A record-level issue's `fingerprint_pk`
#' is matched only where no other record-level issue of the run or of the earlier run has it,
#' so the issues of records sharing a primary key match by their `fingerprint` alone. A
#' column-level issue, which names no
#' record, also needs the same values in the group columns: the run's `group_cols`, or, for an
#' issues table, its columns beyond the issues' own. Every issue is matched, whatever its
#' status. An issue whose `fingerprint` and `fingerprint_pk` are both missing, as on a record
#' whose primary key is missing, never matches: it is new in every run, and never resolved. An
#' earlier issue of a rule renamed or split since matches the new rule's issues. The issues of
#' the tables in `unmatched_tables` aren't matched: they are new in every run, and the earlier
#' run's aren't listed as resolved.
#'
#' A persisting issue keeps the earlier issue's `first_seen_run`, or the earlier run's ID where
#' it has none; a new issue's is its own run's ID. A new or resolved issue's `change_cause` is
#' `rules_or_spec` when its rule's fingerprint differs between the two runs, or the rule ran in
#' only one of them, and `data` otherwise; a persisting issue's is `NA`. Which rules ran is read
#' from a run's results, so where `current` or `previous` is an issues table, a rule with no
#' issue in it counts as unchanged.
#'
#' `previous` should be an earlier run of the same contributors; it is compared as given.
#'
#' @param current A run's results, as [gpq_check_conformance()] returns them, or their issues
#'   table, a data.table; its issues are changed by reference.
#' @param previous The earlier run: its run folder, as [gpq_write_results()] returns it, of
#'   which only `metadata/run.csv`, `tables/issues.csv` and `tables/results.csv` are read; or
#'   its issues table, a data.frame or the path of a CSV file, with at least the columns
#'   `run_id`, `run_scope`, `rule_id`, `rule_fingerprint`, `table_name`, `fingerprint`,
#'   `fingerprint_pk` and `first_seen_run`. [gpq_preflight()] checks it when given as its input
#'   `previous`. Give the folder or the file's path, or read the file with
#'   `colClasses = "character"`: [utils::read.csv()] reads a group value such as `"100.10"` as
#'   the number 100.1, which no longer matches.
#' @param unmatched_tables `NULL`, or the names of tables whose issues aren't matched across
#'   runs, such as tables whose keys each delivery numbers anew.
#' @return A list: `issues`, the current issues, with `first_seen_run`, `change` and
#'   `change_cause` set; and `resolved`, the earlier run's issues that the current run doesn't
#'   have, a new data.table, with `change` `"resolved"`, `change_cause` set and
#'   `first_seen_run` filled as for a persisting issue.
#' @seealso [gpq_check_conformance()] and [gpq_write_results()] for the runs, and
#'   [gpq_apply_acknowledgements()], which matches issues the same way.
#' @examples
#' # Two runs of magp_example: the first with missing tree distances on two trees, the second
#' # with one of them filled in and a third tree's missing.
#' planted <- function(rows) {
#'   trees <- data.table::copy(magp_example$magp_trees)
#'   trees$tree_dist[rows] <- NA
#'   tables <- magp_example
#'   tables$magp_trees <- trees
#'   tables
#' }
#' # Layer 2, which finds missing values. MAGPlot's bundled specification has known pre-flight
#' # warnings, so these warnings are expected.
#' check <- function(rows) {
#'   gpq_check_conformance(planted(rows), magp_spec(), rules = magp_rules(), layers = 2)
#' }
#' run_dir <- gpq_write_results(check(1:2), tempfile("runs"))
#' changes <- gpq_compare_runs(check(2:3), run_dir)
#' changes$issues[, c("rule_id", "record_pk", "change", "change_cause")]
#' changes$resolved[, c("rule_id", "record_pk", "change", "change_cause")]
#' @export
gpq_compare_runs <- function(current, previous, unmatched_tables = NULL) {
  needed <- c(
    "run_id", "run_scope", "rule_id", "rule_fingerprint", "table_name", "level", "fingerprint",
    "fingerprint_pk"
  )
  results <- inherits(current, "gpq_results")
  if (results) {
    issues <- current$issues
    group_cols <- split_joined(current$metadata$run$group_cols)
    ran <- current$results
  } else {
    issues <- current
    # An issues table's group columns: the caller's columns, beside 9.3's own.
    group_cols <- setdiff(names(issues), names(results_schema()$issues))
    ran <- issues
  }
  if (!(is.data.table(issues) && all(needed %chin% names(issues)))) {
    # M8a1 W13
    stop(
      "`current` must be a gpq_results object, or the issues table of a run's results, a ",
      "data.table, as gpq_check_conformance() returns them.",
      call. = FALSE
    )
  }
  typed <- vapply(group_cols, function(column) {
    values <- issues[[column]]
    !is.null(values) && !is.character(values)
  }, logical(1L))
  if (any(typed)) {
    # M8a1 D23.96
    stop(sprintf(
      "`current`'s group columns %s must be text, as gpq_check_conformance() returns them.",
      paste(group_cols[typed], collapse = ", ")
    ), call. = FALSE)
  }
  tables_ok <- is.null(unmatched_tables) ||
    (is.character(unmatched_tables) && !anyNA(unmatched_tables))
  if (!tables_ok) {
    # M8a1 W14
    stop("`unmatched_tables` must be NULL or a character vector of table names.", call. = FALSE)
  }
  read <- read_previous(previous)
  before <- read$issues
  text <- function(x) if (is.character(x)) x else as_text(x)
  # Each run's rule fingerprints: from its results, or from an issues table's own issues.
  first_of <- function(rule_id, rule_fingerprint) {
    first <- !duplicated(rule_id)
    data.table(rule_id = rule_id[first], rule_fingerprint = rule_fingerprint[first])
  }
  now <- first_of(ran$rule_id, ran$rule_fingerprint)
  before_rule <- text(before$rule_id)
  before_print <- text(before$rule_fingerprint)
  then <- if (is.null(read$rules)) first_of(before_rule, before_print) else read$rules
  # A rule renamed or split since is known by its new rule IDs, the old rule's fingerprint
  # with each (D4.2), so it reads as changed.
  map <- rule_id_map()
  if (nrow(map) > 0L) {
    mapped <- map[then, on = c(old_rule_id = "rule_id"), allow.cartesian = TRUE]
    then <- data.table(
      rule_id = fcoalesce(mapped$new_rule_id, mapped$old_rule_id),
      rule_fingerprint = mapped$rule_fingerprint
    )
  }
  # A new or resolved issue's cause (D23.31): the rules or specification where its rule's
  # fingerprint differs in the other run, or the rule didn't run there; a rule a run's results
  # don't give is taken as unchanged where only that run's issues are known.
  cause_of <- function(rule_id, rule_fingerprint, other, absent) {
    known <- other$rule_fingerprint[match(rule_id, other$rule_id)]
    fifelse(is.na(known), absent, fifelse(known == rule_fingerprint, "data", "rules_or_spec"))
  }
  # A persisting issue was first seen when the earliest issue it matches was: that issue's
  # first_seen_run, or its run's ID, which sort as their UTC times do (9.1).
  seen <- fcoalesce(text(before$first_seen_run), text(before$run_id))
  # The earlier issues are matched by their distinct keys, the columns match_issues() reads,
  # each key with its earliest seen, so issues sharing a fingerprint in both runs, as records
  # sharing a primary key give, meet once per current issue and key, never once per pair (16.2).
  # Where no key repeats, the earlier issues are matched where they are, or their keys where a
  # pk: form is left out (below).
  key_cols <- c(
    "fingerprint", "fingerprint_pk", "run_scope", "rule_id", "table_name",
    intersect(group_cols, names(before))
  )
  keys <- setDT(lapply(structure(key_cols, names = key_cols), function(column) {
    text(before[[column]])
  }))
  key_of <- frankv(keys, ties.method = "dense", na.last = TRUE)
  # The pk: pass counts the earlier run's record-level issues too, as match_issues() counts the
  # current run's (D23.148, D23.149): a pk: form more than one of them holds is left out of the
  # keys matched, so a current issue meets it by fingerprint alone. Without a `level` column
  # every earlier issue is counted.
  counted <- !keys$table_name %chin% unmatched_tables
  if ("level" %chin% names(before)) {
    counted <- counted & !text(before$level) %chin% "column"
  }
  counted <- which(counted)
  forms <- keys$fingerprint_pk[counted]
  shared <- counted[forms %chin% forms[duplicated(forms, incomparables = NA)]]
  if (anyDuplicated(key_of) > 0L) {
    by_key <- data.table(group = key_of, seen = seen, at = seq_along(key_of))
    setorderv(by_key, c("group", "seen"))
    by_key <- by_key[!duplicated(by_key$group)]
    distinct <- setDT(lapply(keys, function(column) column[by_key$at]))
    key_seen <- by_key$seen
    shared <- which(by_key$at %in% shared)
  } else {
    # The earlier issues where they are, or their keys where a pk: form is to be left out.
    distinct <- if (length(shared) > 0L) keys else before
    key_seen <- seen
    key_of <- seq_along(seen)
  }
  if (length(shared) > 0L) {
    # A new column, never a write into one: `keys` holds the caller's own vectors.
    set(distinct, j = "fingerprint_pk", value = replace(
      distinct$fingerprint_pk, shared, NA_character_
    ))
  }
  rm(keys)
  pairs <- match_issues(issues, distinct, group_cols, unmatched_tables)
  rm(distinct)
  matched <- unique(pairs$at)
  set(pairs, j = "seen", value = key_seen[pairs$at])
  setorderv(pairs, c("row", "seen"))
  pairs <- pairs[!duplicated(pairs$row)]
  change <- rep("new", nrow(issues))
  change[pairs$row] <- "persisting"
  first_seen <- issues$run_id
  first_seen[pairs$row] <- pairs$seen
  cause <- rep(NA_character_, nrow(issues))
  new <- which(change == "new")
  cause[new] <- cause_of(
    issues$rule_id[new], issues$rule_fingerprint[new], then,
    if (is.null(read$rules)) "data" else "rules_or_spec"
  )
  set(issues, j = "first_seen_run", value = first_seen)
  set(issues, j = "change", value = change)
  set(issues, j = "change_cause", value = cause)
  # The earlier issues no current issue matches, but those of the tables not matched and those
  # with neither fingerprint, which never match (D12.75, D15.9).
  judged <- which(
    !text(before$table_name) %chin% unmatched_tables &
      !(is.na(before$fingerprint) & is.na(before$fingerprint_pk))
  )
  gone <- judged[!key_of[judged] %in% matched]
  resolved <- setDT(lapply(before, function(column) column[gone]))
  # A resolved issue's rule as the current run knows it: a renamed rule's new ID, the first of
  # a split rule's.
  rule_now <- fcoalesce(
    map$new_rule_id[match(before_rule[gone], map$old_rule_id)], before_rule[gone]
  )
  set(resolved, j = "change", value = rep("resolved", length(gone)))
  set(resolved, j = "change_cause", value = cause_of(
    rule_now, before_print[gone], now, if (results) "rules_or_spec" else "data"
  ))
  set(resolved, j = "first_seen_run", value = seen[gone])
  list(issues = issues, resolved = resolved)
}
