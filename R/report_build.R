# Building a run's provider reports (plan 11.1, 11.9, 13; D4.9, D16.16, D20.8, D20.29, D20.48):
# route_unit() gives one report unit its results, issues, corrections and lineage rows from the
# whole run before any HTML is made (D16.16, D18.60, D18.89, D20.10, D20.11, D20.14, D20.23,
# D20.24, D20.54, D20.61), and unit_file_name() each unit its file stem (D20.8), clear of the
# package's own files (package_file_stems, D20.70). The report reads the run's saved tables
# only, never the data (11.1). M7a task 11 adds gpq_build_reports() to this file.

#' One report unit's results, issues and corrections, routed from the whole run (D16.16,
#' D18.60, D18.89, D20.10, D20.11, D20.14, D20.23, D20.24, D20.29, D20.48, D20.54)
#'
#' `x` is a gpq_results object, never changed. `unit` is one report-unit value, or NULL for a
#' run with no group columns, which is one unit holding every row (D20.29). `units` is the
#' run's units, split_units() of the run table's `report_units` (D20.48); the run holds one
#' unit when it has at most one value. The report-unit column is the run's first group column
#' (D9.29).
#'
#' Results (D20.23, D20.24; R2): a row of the unit; a "*" row of a unit rule (the registry's
#' `unit`, D16.8), whose counts are the whole delivery's; a "*" row counting no records
#' (`n_checked`, `n_not_evaluated` and `n_unchecked` each 0 or NA), such as a not_run row for
#' a table not given; any other "*" row only in a run of one unit. A provider's report reads
#' only their counts (D20.23), so the routed rows' `examples` and breakdown columns (every
#' group column after the first) are NA: a unit rule's row would list other units' values,
#' and a group mixing units another unit's dataset (D20.77). Each row gains
#' `n_not_evaluated_own` (D20.62), after `n_unchecked`: the records of its `n_not_evaluated`
#' that no not-evaluated issue of its rule, table, column, group and reason code holds, a
#' collapsed issue holding `n_records` records and any other one (D20.54). It is worked out
#' from the run's whole issues table before routing, so an issue held back from the unit
#' still accounts for its record; a remainder below 0 is an internal error. A caller's
#' column can't take that name (results_schema(), D20.76).
#'
#' Issues (D16.8, D16.16, D18.60, D18.89, D20.10, D20.11, D20.14): in a run of one unit, every
#' issue of the unit or "*", marked or not; otherwise an issue `reaches_other_unit` doesn't
#' mark that is the unit's, or is a "*" issue of a unit rule. An NA mark counts as marked, so
#' an issue whose mark is unknown goes to no provider of several (fail closed, D18.89).
#' Corrections: those of the routed issues. Lineage (D20.17, D20.61, D20.65): `lineage` is NULL
#' or the lineage input as gpq_build_reports() reads it (`table_name`, `record_pk`,
#' `column_name`, `raw_value`, ...); only its rows whose table, record and column are a routed
#' issue's cell are kept, so a builder never sees the whole input. Returns
#' `list(results, issues, corrections, lineage)`, new data.tables, `lineage` NULL when none is
#' given.
#' @noRd
route_unit <- function(x, unit, units, lineage = NULL) {
  results <- x$results
  issues <- x$issues
  group_cols <- split_joined(x$metadata$run$group_cols)
  cells <- c("rule_id", "table_name", "column_name", group_cols, "reason_code")
  # The records the not-evaluated issues hold, summed per results cell over the whole run
  # (one grouped sum), then met with the results rows by one key join (key_matches(), whose
  # own column names keep a caller's group column clear of the join's syntax) (16.2).
  held <- which(issues$status == "not_evaluated")
  counted <- issues[held, cells, with = FALSE]
  set(counted, j = "n_records", value = fifelse(
    issues$level[held] == "column", issues$n_records[held], 1L
  ))
  counted <- counted[, lapply(.SD, sum), by = cells, .SDcols = "n_records"]
  pairs <- key_matches(
    lapply(cells, function(column) results[[column]]), seq_len(nrow(results)), counted, cells
  )
  accounted <- integer(nrow(results))
  accounted[pairs$row] <- counted$n_records[pairs$at]
  own <- results$n_not_evaluated - accounted
  below <- which(own < 0L)
  if (length(below) > 0L) {
    first <- below[[1L]]
    stop(sprintf(
      paste(
        "Internal error: the not-evaluated issues of rule %s on table %s, column %s hold %d",
        "records, more than the %d its results row counts not evaluated."
      ),
      results$rule_id[[first]], results$table_name[[first]], results$column_name[[first]],
      accounted[[first]], results$n_not_evaluated[[first]]
    ), call. = FALSE)
  }
  if (is.null(unit)) {
    take_results <- rep(TRUE, nrow(results))
    take_issues <- rep(TRUE, nrow(issues))
  } else {
    one <- length(units) <= 1L
    unit_col <- group_cols[[1L]]
    registry <- rule_registry()
    unit_rule <- function(rule_id) registry$unit[match(rule_id, registry$rule_id)] %in% TRUE
    none <- function(count) fcoalesce(count, 0L) == 0L
    value <- results[[unit_col]]
    counts_none <- none(results$n_checked) & none(results$n_not_evaluated) &
      none(results$n_unchecked)
    take_results <- value == unit |
      (value == "*" & (one | unit_rule(results$rule_id) | counts_none))
    value <- issues[[unit_col]]
    take_issues <- if (one) {
      value %chin% c(unit, "*")
    } else {
      issues$reaches_other_unit %in% FALSE &
        (value == unit | (value == "*" & unit_rule(issues$rule_id)))
    }
  }
  routed <- results[take_results]
  for (column in c("examples", group_cols[-1L])) {
    set(routed, j = column, value = NA_character_)
  }
  set(routed, j = "n_not_evaluated_own", value = own[take_results])
  setcolorder(routed, "n_not_evaluated_own", after = "n_unchecked")
  routed_issues <- issues[take_issues]
  take_corrections <- x$corrections$issue_id %in% routed_issues$issue_id
  # A raw value goes only to the report of a unit whose issue is on its cell (D20.17, D20.61).
  routed_lineage <- NULL
  if (!is.null(lineage)) {
    cell_columns <- c("table_name", "record_pk", "column_name")
    routed_lineage <- as.data.table(lineage)[
      unique(routed_issues[, cell_columns, with = FALSE]),
      on = cell_columns, nomatch = NULL
    ]
  }
  list(
    results = routed, issues = routed_issues, corrections = x$corrections[take_corrections],
    lineage = routed_lineage
  )
}

#' The stems of the files the package itself writes in a run's `reports/` and `tables/`
#' folders (D20.70, D20.73)
#'
#' Pre-flight's page (`reports/preflight.html`), M8a's national summary
#' (`reports/national_summary.html`), the one report of a run with no group columns
#' (`reports/report.html`, `tables/report_issues.csv`) and the run's own tables
#' (`tables/results.csv`, `tables/issues.csv`, `tables/corrections.csv`). unit_file_name()
#' encodes the first character of a unit that is one of them, in any case, so no provider's
#' file can take the name of one of them; a test holds every file a run and its build write
#' in those folders to a unit's file or one of these stems. A file added later adds its stem here.
#' @noRd
package_file_stems <- c(
  "preflight", "national_summary", "report", "results", "issues", "corrections"
)

#' Each report unit's file stem, for `reports/<stem>.html` and `tables/<stem>_issues.csv`
#' (D20.8, D20.37, D20.70)
#'
#' ASCII letters, digits, "-", "_" and "." are kept; every other byte of the unit's UTF-8 is
#' written as "%" and its two hex digits in upper case, "%" itself as %25, so a stem decodes
#' back to one value. A leading or trailing "." is written so too, and so is the first
#' character of a unit whose part before its first "." is a Windows reserved name (CON, PRN,
#' AUX, NUL, COM1 to COM9, LPT1 to LPT9, in any case: "CON" gives "%43ON"), or that is one of
#' the package's own file stems (package_file_stems), in any case: "Issues" gives "%49ssues".
#' Every stem is ASCII. The caller gives every unit at once, before any file is written: the
#' call stops when a stem is longer than 200 bytes, or when two stems are equal ignoring ASCII
#' case, as Windows and macOS read file names (D20.8). Returns the stems in `units`' order.
#' @noRd
unit_file_name <- function(units) {
  # Letter case is folded in ASCII only, so no other letter can read as a reserved name.
  folded <- chartr(paste(LETTERS, collapse = ""), paste(letters, collapse = ""), units)
  dot <- regexpr(".", folded, fixed = TRUE)
  base <- fifelse(dot > 0L, substr(folded, 1L, dot - 1L), folded)
  reserved <- c("con", "prn", "aux", "nul", paste0("com", 1:9), paste0("lpt", 1:9))
  # The package's own files (11.9), which no unit's file may replace (D20.70).
  first <- startsWith(units, ".") | base %chin% reserved | folded %chin% package_file_stems
  last <- endsWith(units, ".")
  # Units are few, one per provider, so each is encoded from its own bytes: 48 to 57 are the
  # digits, 65 to 90 and 97 to 122 the ASCII letters, 45 "-", 46 "." and 95 "_".
  stems <- vapply(seq_along(units), function(k) {
    bytes <- as.integer(charToRaw(enc2utf8(units[[k]])))
    n <- length(bytes)
    kept <- (bytes >= 48L & bytes <= 57L) | (bytes >= 65L & bytes <= 90L) |
      (bytes >= 97L & bytes <= 122L) | bytes %in% c(45L, 46L, 95L)
    if (n > 0L) {
      kept[[1L]] <- kept[[1L]] && !first[[k]]
      kept[[n]] <- kept[[n]] && !last[[k]]
    }
    chars <- character(n)
    chars[kept] <- intToUtf8(bytes[kept], multiple = TRUE)
    chars[!kept] <- sprintf("%%%02X", bytes[!kept])
    paste(chars, collapse = "")
  }, character(1))
  size <- nchar(stems, type = "bytes")
  long <- which(size > 200L)
  if (length(long) > 0L) {
    stop(sprintf(
      paste0(
        "Report unit %s gives a file name of %d bytes once encoded, over the limit of 200; ",
        "use a shorter unit value."
      ),
      units[[long[[1L]]]], size[[long[[1L]]]]
    ), call. = FALSE)
  }
  # Folded in ASCII as the reserved names are, whatever the session's locale: on Linux, in a
  # Turkish one, tolower() gives "I" a dotless i, so "I" and "i" wouldn't stop. The first pair
  # is named.
  lower <- chartr(paste(LETTERS, collapse = ""), paste(letters, collapse = ""), stems)
  again <- which(duplicated(lower))
  if (length(again) > 0L) {
    seen <- match(lower[[again[[1L]]]], lower)
    stop(sprintf(
      paste0(
        "Report units %s and %s give the same file name, %s, on a system that ignores ",
        "letter case; rename one of them."
      ),
      units[[seen]], units[[again[[1L]]]], paste0(stems[[seen]], ".html")
    ), call. = FALSE)
  }
  stems
}
