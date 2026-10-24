# Issue classes (plan 3.4, 9.3, 10.3; D4.3, D9.27, D9.33, D19.28, D19.34, D19.51 to D19.54):
# each issue's class from its rule's, then your overrides at 10.3 step 1's tiers, then the
# reason-code rule, and for an issue whose class is still depends, the evidence
# (R/lineage_evidence.R), then the rule's fallback class, else unclassified. The tables a
# caller gives are read as pre-flight reads its run inputs
# (read_input_table()); the issues are read by column and set by reference, never copied
# whole, and each step is a key join, never a loop over rows (16.2).

#' Pairs of `rows` and the rows of `table` whose values agree, by one key join (16.2)
#'
#' `values` is a list of vectors, each holding one value per element of `rows`; `given` names
#' the columns of `table` they meet, in order. A row of `table` with an NA among those columns
#' matches nothing, since data.table's join would match NA with NA. Each side is a new list
#' of the vectors (c() copies no vector), its columns named here, so neither the caller's list
#' nor a caller's column name meets the join's own. Returns a data.table of `row` (an element
#' of `rows`) and `at` (a row of `table`), one row per pair.
#' @noRd
key_matches <- function(values, rows, table, given) {
  keep <- which(Reduce(`&`, lapply(given, function(column) !is.na(table[[column]]))))
  keys <- sprintf("k%d", seq_along(given))
  left <- setDT(c(values, list(rows)))
  setnames(left, c(keys, "row"))
  right <- setDT(c(lapply(given, function(column) table[[column]][keep]), list(keep)))
  setnames(right, c(keys, "at"))
  found <- left[right, on = keys, nomatch = NULL, allow.cartesian = TRUE]
  found[, c("row", "at"), with = FALSE]
}

#' A caller's error where a table given to classification didn't read cleanly (D19.28 (7))
#'
#' `read` is read_input_table()'s: its first finding, a cell that isn't valid UTF-8 or a
#' malformed line, is named.
#' @noRd
stop_if_unclean <- function(read, input) {
  if (nrow(read$findings) > 0L) {
    # W228
    stop(sprintf(
      "`%s` doesn't read cleanly: %s", input, read$findings$detail[[1L]]
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' A row of a table given to classification, for a caller's error: "line 3 of overrides.csv"
#' for a CSV file, "row 2 of `overrides`" for a data.frame
#'
#' Mid-sentence and English in the code, as a caller's error is (D12.37). Pre-flight places a
#' run input's row in its findings with input_positions() (R/preflight_inputs.R) instead,
#' whose words are rows of the text table a caller's text table can reword (D19.67).
#' @noRd
input_place <- function(read, input, row) {
  where <- read$where
  if (identical(where$kind, "csv")) {
    return(sprintf("line %d of %s", file_rows(where, row + 1L), where$file))
  }
  sprintf("row %d of `%s`", row, input)
}

#' Each issue's class from its rule: the most specific rules row's class, a blank deferring,
#' then the registry's default class (10.3; D16.6, D19.28 (1))
#'
#' `rule_rows` is a checked rules component or NULL. A rule whose default class is "none"
#' gives "none" (D19.58).
#' @noRd
rule_classes <- function(issues, rule_rows, registry) {
  rows <- rule_row_values(rule_rows, issues$rule_id, issues$table_name, issues$column_name)
  fcoalesce(rows$class, registry$default_class[match(issues$rule_id, registry$rule_id)])
}

#' The overrides read and checked for classification, a rule ID the rule ID map maps read as
#' its new rule IDs (10.3 step 1; D19.28 (7), D19.51, D19.54)
#'
#' A data.frame or a CSV path, read as pre-flight reads it. Caller's errors: a file that
#' doesn't read cleanly; a column of its own missing, or one beyond them that isn't a group
#' column of the issues (`groups`); a class other than source or harmonization; a row with no
#' scope cell. A rule ID the map renames is read as the new ID, and one it splits as each new
#' ID, the row repeated. Returns the rows (`data`), the columns of their scope (`scope`), and
#' each row's `rank`: its tier (fingerprint; rule, column and group; rule and column; rule and
#' group; rule; column and group; column; group; table alone), a row naming `table_name` before
#' one without within its tier, 1 the strongest.
#' @noRd
read_overrides <- function(overrides, groups) {
  # A column of class integer64 is written as text through bit64's methods, which a table
  # read back with readRDS() doesn't load: load them where bit64 is installed, as a run does
  # (D18.86), so the cell is its number. Not installed, there is no stop (D19.63).
  wide <- is.data.frame(overrides) &&
    any(vapply(overrides, inherits, logical(1), what = "integer64"))
  if (wide) {
    requireNamespace("bit64", quietly = TRUE)
  }
  read <- read_input_table(overrides, "overrides")
  stop_if_unclean(read, "overrides")
  data <- read$data
  own <- run_input_schema()$overrides
  missing <- setdiff(own, names(data))
  extra <- setdiff(names(data), c(own, groups))
  if (length(missing) > 0L || length(extra) > 0L) {
    problems <- c(
      if (length(missing) > 0L) paste("missing:", paste(missing, collapse = ", ")),
      if (length(extra) > 0L) {
        paste("not taken:", paste(mark_invalid_utf8(extra), collapse = ", "))
      }
    )
    # W229
    stop(sprintf(
      "`overrides` must have the columns %s, and may have the issues' group columns (%s); %s.",
      paste(own, collapse = ", "),
      if (length(groups) > 0L) paste(groups, collapse = ", ") else "the issues have none",
      paste(problems, collapse = "; ")
    ), call. = FALSE)
  }
  bad_class <- which(!data$issue_class %chin% c("source", "harmonization"))
  if (length(bad_class) > 0L) {
    first <- bad_class[[1L]]
    class_cell <- data$issue_class[[first]]
    # W230
    stop(sprintf(
      "The override at %s gives the class %s; an override's class is source or harmonization.",
      input_place(read, "overrides", first),
      if (is.na(class_cell)) "(blank)" else paste0("\"", mark_invalid_utf8(class_cell), "\"")
    ), call. = FALSE)
  }
  scope <- override_scope(data, groups)
  has <- lapply(scope, function(column) !is.na(data[[column]]))
  names(has) <- scope
  no_scope <- which(!Reduce(`|`, has))
  if (length(no_scope) > 0L) {
    # W231
    stop(sprintf(
      paste(
        "The override at %s gives no scope: no fingerprint, rule_id, table_name, column_name",
        "or group column."
      ),
      input_place(read, "overrides", no_scope[[1L]])
    ), call. = FALSE)
  }
  group_columns <- setdiff(scope, c("fingerprint", "rule_id", "table_name", "column_name"))
  group <- if (length(group_columns) > 0L) {
    Reduce(`|`, has[group_columns])
  } else {
    rep(FALSE, nrow(data))
  }
  rule <- has$rule_id
  column <- has$column_name
  tier <- fcase(
    has$fingerprint, 1L,
    rule & column & group, 2L,
    rule & column, 3L,
    rule & group, 4L,
    rule, 5L,
    column & group, 6L,
    column, 7L,
    group, 8L,
    default = 9L
  )
  rank <- 2L * tier - as.integer(has$table_name)
  # A renamed rule's override applies to its new rule ID, a split rule's to each new ID (9.6,
  # D19.51).
  map <- rule_id_map()
  asked <- data.table(row = seq_len(nrow(data)), rule_id = data$rule_id)
  mapped <- map[asked, on = c(old_rule_id = "rule_id"), allow.cartesian = TRUE]
  rows <- mapped$row
  data <- data[rows]
  set(data, j = "rule_id", value = fcoalesce(mapped$new_rule_id, data$rule_id))
  list(data = data, scope = scope, rank = rank[rows])
}

#' The classes your overrides give, tier by tier, the strongest first (10.3 step 1; D8.17,
#' D9.27, D19.54)
#'
#' A row matches an issue when every scope cell it gives matches the issue's, `fingerprint`
#' the issue's `fingerprint` or its `fingerprint_pk`. An issue takes the classes of the
#' matching rows of the strongest rank holding any: one class gives it with basis "override",
#' both make it "unclassified" with basis "override conflict". One key join for each set of
#' scope cells given within a rank, over the issues no stronger rank has classed. Returns
#' `class` and `basis` with those issues set.
#' @noRd
override_classes <- function(issues, overrides, class, basis) {
  data <- overrides$data
  scope <- overrides$scope
  given <- lapply(scope, function(column) !is.na(data[[column]]))
  pattern <- do.call(paste0, lapply(given, function(x) fifelse(x, "1", "0")))
  open <- rep(TRUE, nrow(issues))
  for (strength in sort(unique(overrides$rank))) {
    candidates <- which(open)
    if (length(candidates) == 0L) {
      break
    }
    is_rank <- overrides$rank == strength
    found <- lapply(unique(pattern[is_rank]), function(one) {
      at <- which(is_rank & pattern == one)
      columns <- scope[substring(one, seq_along(scope), seq_along(scope)) == "1"]
      rows <- data[at]
      # A fingerprint meets the issue's fingerprint, or its pk: form (D9.22).
      sides <- if ("fingerprint" %chin% columns) c("fingerprint", "fingerprint_pk") else NA
      rbindlist(lapply(sides, function(side) {
        from <- if (is.na(side)) columns else replace(columns, columns == "fingerprint", side)
        values <- lapply(from, function(column) issues[[column]][candidates])
        pairs <- key_matches(values, candidates, rows, columns)
        data.table(row = pairs$row, issue_class = rows$issue_class[pairs$at])
      }))
    })
    pairs <- rbindlist(found)
    if (nrow(pairs) == 0L) {
      next
    }
    hit <- unique(pairs$row)
    sourced <- hit %in% pairs$row[pairs$issue_class == "source"]
    both <- sourced & hit %in% pairs$row[pairs$issue_class == "harmonization"]
    class[hit] <- fifelse(both, "unclassified", fifelse(sourced, "source", "harmonization"))
    basis[hit] <- fifelse(both, "override conflict", "override")
    open[hit] <- FALSE
  }
  list(class = class, basis = basis)
}

#' Class a run's issues
#'
#' @description
#' Gives each issue its class, `source` (the provider's to fix), `harmonization` (made by the
#' pipeline that converted the data) or `unclassified`, and says what decided it. The
#' issues' `issue_class` and `class_basis` are set by reference.
#'
#' @details
#' Each issue first takes its rule's class, with `class_basis` `"default"`: the class of the
#' most specific row of the rule set's `rules` that gives one, else the class the rule is
#' registered with. An information rule's class is `"none"`.
#'
#' Your overrides then class any issue, whatever its rule's class. An override gives a class
#' to the issues whose cells match every scope cell it gives: `fingerprint` (the issue's
#' `fingerprint` or `fingerprint_pk`), `rule_id`, `table_name`, `column_name` and any group
#' column; a blank cell matches every issue. Where several overrides match an issue, the most
#' specific wins, in this order: a fingerprint; a rule, column and group; a rule and column; a
#' rule and group; a rule; a column and group; a column; a group; a table alone. Within each,
#' an override naming the table comes before one that doesn't, and one group column or
#' several count alike. Two overrides of the winning kind that give different classes leave
#' the issue `unclassified`, with `class_basis` `"override conflict"`; otherwise `class_basis`
#' is `"override"`. An override naming a rule that was renamed or split applies to its new
#' rules.
#'
#' Then an issue whose `reason_code` is `harmonization_artifact` is `harmonization`, with
#' `class_basis` `"reason code"`, unless an override classed it or left it in conflict.
#'
#' Then the evidence settles an issue whose class is still `depends` as `harmonization`, the
#' first of these tables to settle it giving `class_basis`:
#' - `computed`: columns the pipeline computes, with the columns `table_name`,
#'   `column_name`, `derivation` (`id`, `class`, `formula`, `count`, `fill` or `reference`)
#'   and one group column of the issues, which gives the group value the row is for, `"*"`
#'   for every value. An issue on such a column in that group, of a record or of the whole
#'   column, is `"computed"`.
#' - `filled`: cells the pipeline's missing-value fill wrote (`table_name`, `record_pk`,
#'   `column_name`, `written_value`). An issue on such a cell whose value is the one written
#'   is `"filled"`.
#' - `lineage`, the provider's raw values of the cells the pipeline changed (`table_name`,
#'   `record_pk`, `column_name`, `raw_value`, `raw_source`), with `crosswalk`, each provider
#'   code and the code it is translated to (`provider_code`, `code`, `column_name` and one
#'   group column of the issues). Together they settle a record's `code_not_in_list` issue,
#'   whose raw value is its cell's `raw_value`, or its own value where `lineage` doesn't list
#'   the cell. Where `crosswalk` has rows for the issue's column and group value, a raw value
#'   listed there as a `provider_code` makes it `"lineage"`: the provider's code was valid. A
#'   raw value not listed there makes it `"crosswalk"` only where the pipeline changed the
#'   cell, that is where `lineage` lists the cell with a raw value other than its value;
#'   otherwise the issue stays unsettled, as does one whose column and group value
#'   `crosswalk` has no rows for.
#'
#' A cell listed twice in `filled` or `lineage` stops classification.
#'
#' An issue whose class is still `depends` takes the `fallback_class` setting's class for its
#' rule (`class_basis` `"fallback"`), else it is `unclassified` (`class_basis` `"default"`).
#'
#' @param issues The `issues` table of a run's results, from [gpq_check_conformance()]: a
#'   data.table, changed by reference.
#' @param rules `NULL`, or the rule set the run used, such as [magp_rules()] returns: its
#'   `rules` rows give rules their classes, and its settings rows can give `fallback_class`.
#' @param evidence A named list of evidence tables for issues whose class is `depends`, each
#'   a data.frame or the path of a CSV file: `computed`, `filled`, `lineage` and `crosswalk`
#'   (see Details).
#' @param overrides `NULL`, or your overrides: a data.frame or the path of a CSV file with
#'   the columns `fingerprint`, `rule_id`, `table_name`, `column_name`, `issue_class`
#'   (`source` or `harmonization`), `who`, `when` and `why`, and any of the issues' group
#'   columns, each row giving at least one of the scope cells. [gpq_preflight()] checks them
#'   when given as its input `overrides`.
#' @param settings A named list of settings, which come before the rule set's, the options'
#'   and the built-in values (see [groundplotqc_options]). It reads `fallback_class`, classes
#'   by rule ID, such as `c(some_rule = "source")`. The first of those that gives
#'   `fallback_class` gives all of it: an argument naming one rule replaces every entry of the
#'   rule set's row or the option, so the other rules have none.
#' @return `issues`, invisibly, with `issue_class` and `class_basis` set.
#' @seealso [gpq_check_conformance()] for the issues, [gpq_decode_lineage()] for their
#'   provider keys, and [gpq_preflight()] for the checks of the overrides.
#' @examples
#' example <- function(file) system.file("extdata", "examples", file, package = "groundplotqc")
#' fish <- gpq_read_spec(
#'   dictionary = example("fish_dictionary.csv"),
#'   code_lists = list(
#'     water_body = example("fish_water_body.csv"), gear = example("fish_gear.csv"),
#'     species = example("fish_species.csv")
#'   ),
#'   column_map = gpq_column_map(
#'     table = "table", attribute = "field", type = "kind", key_type = "key",
#'     reference = "parent", lookup = "codes", description = "notes",
#'     min_value = "low", max_value = "high", min_inclusive = "low_in",
#'     max_inclusive = "high_in", pattern = "format", max_length = "width",
#'     decimals = "places", units = "unit"
#'   ),
#'   type_map = gpq_type_map(example("fish_types.csv")),
#'   sentinels = gpq_sentinels(
#'     numeric = c(missing = -99, not_applicable = -88),
#'     character = c(missing = "?", not_applicable = "~"),
#'     date = c(missing = "?", not_applicable = "~")
#'   )
#' )
#' # A haul that fished 900 minutes, past the dictionary's 600, and a species code not in
#' # the species list.
#' tables <- list(
#'   stations = data.frame(
#'     station_id = "S1", station_name = "Long Lake", water_body = "LK", depth = 1.5,
#'     first_visit = "2020-06-01"
#'   ),
#'   hauls = data.frame(
#'     haul_id = c("H1", "H2"), station_id = "S1", gear = "GN", minutes = c(30L, 900L),
#'     haul_date = "2020-06-01"
#'   ),
#'   catches = data.frame(
#'     catch_id = c("C1", "C2"), haul_id = "H1", species = c("WAE", "XYZ"),
#'     fish_count = 3L, weight = 1.2
#'   )
#' )
#' results <- gpq_check_conformance(tables, fish, group_cols = "water_body", layers = 0:3)
#' # Your override: the long haul is the provider's to fix.
#' overrides <- data.frame(
#'   fingerprint = NA, rule_id = "value_above_max", table_name = "hauls",
#'   column_name = "minutes", issue_class = "source", who = "A. Reviewer",
#'   when = "2026-10-22", why = "The gear log gives 45 minutes."
#' )
#' gpq_classify_issues(results$issues, rules = NULL, overrides = overrides)
#' results$issues[, c("rule_id", "column_name", "value", "issue_class", "class_basis")]
#'
#' # A fallback class for the code issues nothing else settles.
#' gpq_classify_issues(
#'   results$issues,
#'   rules = NULL, overrides = overrides,
#'   settings = list(fallback_class = c(code_not_in_list = "source"))
#' )
#' results$issues[, c("rule_id", "column_name", "value", "issue_class", "class_basis")]
#' @export
gpq_classify_issues <- function(issues, rules, evidence = list(), overrides = NULL,
                                settings = list()) {
  needed <- c(
    "rule_id", "table_name", "column_name", "record_pk", "level", "value", "reason_code",
    "fingerprint", "fingerprint_pk", "issue_class", "class_basis"
  )
  registry <- rule_registry()
  shaped <- is.data.table(issues) && all(needed %in% names(issues)) &&
    all(issues$rule_id %chin% registry$rule_id)
  if (!shaped) {
    # W176 (gpq_decode_lineage()'s text)
    stop(
      "`issues` must be the issues table of a run's results, a data.table, as ",
      "gpq_check_conformance() returns it.",
      call. = FALSE
    )
  }
  rules <- validate_rule_set(rules)
  check_settings(settings, rules)
  fallback <- resolve_setting("fallback_class", settings, rules)$value
  # The issues' group columns: the caller's columns, beside 9.3's own.
  groups <- setdiff(names(issues), names(results_schema()$issues))
  # Every table is read and checked before any issue is classed (D19.28 (7)).
  evidence_in <- read_evidence(
    read_run_inputs(evidence, "evidence", names(evidence_schema())), groups
  )
  given <- if (!is.null(overrides)) read_overrides(overrides, groups)
  # Step 1: the rule's class (D19.28 (1)).
  class <- rule_classes(issues, rules$rules, registry)
  basis <- rep("default", nrow(issues))
  # Your overrides, at 10.3 step 1's tiers (D19.54).
  if (!is.null(given)) {
    classed <- override_classes(issues, given, class, basis)
    class <- classed$class
    basis <- classed$basis
  }
  # An issue no override classed or left in conflict (D19.64), with reason code
  # harmonization_artifact, is harmonization (D9.33, D19.28 (3)).
  artifact <- !basis %chin% c("override", "override conflict") &
    issues$reason_code %chin% "harmonization_artifact"
  class[artifact] <- "harmonization"
  basis[artifact] <- "reason code"
  # Steps 2 to 5: the evidence settles an issue still depends as harmonization (D19.58).
  open <- which(class == "depends")
  settled <- evidence_classes(issues, evidence_in, open)
  class[open[!is.na(settled)]] <- "harmonization"
  basis[open[!is.na(settled)]] <- settled[!is.na(settled)]
  # An issue still depends takes its rule's fallback class, else it is unclassified (D19.34).
  open <- which(class == "depends")
  falls_back <- unname(fallback[issues$rule_id[open]])
  class[open] <- fifelse(is.na(falls_back), "unclassified", falls_back)
  basis[open[!is.na(falls_back)]] <- "fallback"
  set(issues, j = "issue_class", value = class)
  set(issues, j = "class_basis", value = basis)
  invisible(issues)
}
