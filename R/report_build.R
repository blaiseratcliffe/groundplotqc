# Building a run's provider reports (plan 11.1, 11.9, 13; D4.9, D16.16, D20.8, D20.29, D20.48):
# route_unit() gives one report unit its results, issues, corrections and lineage rows from the
# whole run before any HTML is made (D16.16, D18.60, D18.89, D20.10, D20.11, D20.14, D20.23,
# D20.24, D20.54, D20.61), and unit_file_name() each unit its file stem (D20.8), clear of the
# package's own files (package_file_stems, D20.70); gpq_build_reports() builds each listed
# unit's report and issues CSV from them into a saved run's folder (D20.13). The report reads
# the run's saved tables only, never the data (11.1).

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
#' issue's cell are kept, each once and in the input's order, so a builder never sees the whole
#' input. Only a record-level issue whose key names one record (`pk_positional` FALSE) is on a
#' cell: a column-level issue, and one whose key is missing or repeats, takes none (D20.117).
#' Returns
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
  # own column names keep a caller's group column clear of the join's syntax) (16.2). The
  # cells are renamed first, so no caller's group column (one named cells, say) meets a name
  # of the grouping's own.
  held <- which(issues$status == "not_evaluated")
  counted <- issues[held, cells, with = FALSE]
  cell_names <- sprintf("cell_%d", seq_along(cells))
  setnames(counted, cells, cell_names)
  set(counted, j = "n_records", value = fifelse(
    issues$level[held] == "column", issues$n_records[held], 1L
  ))
  counted <- counted[, lapply(.SD, sum), by = cell_names, .SDcols = "n_records"]
  pairs <- key_matches(
    lapply(cells, function(column) results[[column]]), seq_len(nrow(results)), counted,
    cell_names
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
    # A "*" row's case is read on the "*" rows only (16.2).
    value <- results[[unit_col]]
    take_results <- value == unit
    star <- which(value == "*")
    counts_none <- none(results$n_checked[star]) & none(results$n_not_evaluated[star]) &
      none(results$n_unchecked[star])
    take_results[star] <- one | unit_rule(results$rule_id[star]) | counts_none
    value <- issues[[unit_col]]
    if (one) {
      take_issues <- value %chin% c(unit, "*")
    } else {
      take_issues <- value == unit
      star <- which(value == "*")
      take_issues[star] <- unit_rule(issues$rule_id[star])
      take_issues <- take_issues & issues$reaches_other_unit %in% FALSE
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
  # Only a record-level issue whose key names one record (`pk_positional` FALSE) is on a cell:
  # a column-level issue's "*", a missing key and a key that repeats could each pair it with
  # another unit's raw value (D20.117). The cells are met by one key join (key_matches(), which
  # matches no NA), and only the matched rows are copied, in the input's order (16.2).
  routed_lineage <- NULL
  if (!is.null(lineage)) {
    cell_columns <- c("table_name", "record_pk", "column_name")
    keyed <- which(routed_issues$level == "record" & routed_issues$pk_positional %in% FALSE)
    pairs <- key_matches(
      lapply(cell_columns, function(column) lineage[[column]]), seq_len(nrow(lineage)),
      unique(routed_issues[keyed, cell_columns, with = FALSE]), cell_columns
    )
    rows <- sort(pairs$row)
    routed_lineage <- setDT(lapply(lineage, function(column) column[rows]))
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

#' Build the provider reports and issues CSVs of a saved run
#'
#' @description
#' Builds one HTML report and one issues CSV for each report unit of a run, from its results
#' and never from its data, and writes them into the run's saved folder:
#' `reports/<unit>.html` and `tables/<unit>_issues.csv`. Each unit's files hold only what that
#' unit may see.
#'
#' @details
#' Every issue must have a class before the results are written: run [gpq_classify_issues()],
#' which needs no evidence, then [gpq_write_results()] to `output_dir`. A run that stopped at
#' pre-flight is refused, since it has no reports to build. A build first removes the reports
#' and issues CSVs an earlier build wrote in the run's folder, never `reports/preflight.html`
#' or the run's own tables. Each file is written under a temporary name and renamed when
#' complete, so a build that stops leaves no partly written file.
#'
#' One report and one CSV is built for each report unit `metadata/run.csv` lists, even a unit
#' with no issue. An issue goes to its own unit's report. An issue of a check that checks whole
#' tables or columns, with `"*"` as its unit and not marked `reaches_other_unit`, goes to every
#' unit, and so does a results row that counts no records, such as a check that couldn't run
#' because a table wasn't given. Any other issue or results row with `"*"` as its unit, a
#' whole-column issue of records included, goes to no unit's report, and so does an issue the
#' run marks `reaches_other_unit` (one holding a value read from another unit's records or from
#' records with no unit). In a run of one unit, all of these go to that unit's report. A run
#' with no group columns builds one report, `reports/report.html`, and one CSV,
#' `tables/report_issues.csv`, holding every issue.
#'
#' Each provider's file name is its unit value, with every character other than an ASCII
#' letter, a digit, "-", "_" or "." written as % and its UTF-8 bytes in uppercase hex ("%"
#' itself as %25). A leading or trailing "." is encoded too, and so is the first character of
#' a value whose part before its first "." is a Windows reserved name such as CON, NUL or COM1
#' ("CON" becomes "%43ON"). So is the first character of a value that, whatever its case, is
#' the name of one of the run's own files ("preflight", "national_summary", "report",
#' "results", "issues" or "corrections"), so a provider's file never replaces one of them. Any
#' value then makes a safe file name that decodes back.
#'
#' The CSV has one row per issue sent to the unit, open and not evaluated, codes as stored.
#' Its columns, in order: the group columns and the problem unit's key column; the table,
#' column, check, severity and class; the check's text in the run language; the key's parts as
#' `key_<label>` columns, named from the lineage spec's labels and empty where a key can't be
#' split back exactly (a raw, fallback or no-lineage-spec key); then `provider_key` and
#' `provider_key_status`; the record ID, value and expected value; `proposed_value` and
#' `easy_fix`, empty until proposals exist; status, change, fingerprint and record count. Rows
#' are sorted as the report's tables are. It is UTF-8 with a byte-order mark. In the
#' provider's CSV, a text cell that starts with =, +, @, a tab or a carriage return, or with -
#' where the cell isn't a number, is written with an apostrophe before it, so a spreadsheet
#' shows it as text instead of running it as a formula. `reaches_other_unit` isn't written.
#'
#' Tables show at most `row_cap` rows and `example_cap` examples per check, and text longer
#' than `cell_cap` characters is cut and ends with an ellipsis. The issues CSV has every issue,
#' with its text in full.
#'
#' @param results A `gpq_results` object, such as [gpq_check_conformance()] returns, its issues
#'   classed by [gpq_classify_issues()] and its run written to `output_dir` by
#'   [gpq_write_results()].
#' @param output_dir The folder that holds the run's folder, as given to [gpq_write_results()].
#' @param group_cols `NULL` for the run's own group columns, or the run's report-unit column
#'   followed by any of its other group columns, which choose the breakdowns the reports show.
#'   For a run with no group columns, `NULL` or empty. The issues CSVs always hold all the
#'   run's group columns.
#' @param lineage `NULL`, or the provider's raw values of the cells the pipeline changed: a
#'   data.frame or the path of a CSV file with the columns `table_name`, `record_pk`,
#'   `column_name`, `raw_value` and `raw_source`, as [gpq_classify_issues()] takes it in
#'   `evidence`. A listed cell's raw value shows beside its issue among the issues to act on,
#'   in its own unit's report only, and only where the record's key is given and, written as
#'   text, matches no other record's in its table.
#' @param lang,row_cap,example_cap,cell_cap The reports' language (`lang`) and caps: the most
#'   rows a table shows (`row_cap`), examples per check (`example_cap`) and characters of
#'   delivered text per cell (`cell_cap`). Each `NULL` takes the run's rule set, then the
#'   option, then the built-in value (see [groundplotqc_options]); a value given here wins.
#' @param text `NULL`, or a table of report text with the columns `text_id`, `lang` and
#'   `text`, whose rows stand in for the package's rows of the same `text_id` and `lang`.
#' @return The files written, invisibly: a character vector of their paths, each named by its
#'   report unit (by the run ID for a run with no group columns). A run with group columns
#'   whose `metadata/run.csv` lists no unit gets no file, and an empty vector.
#' @seealso [gpq_write_results()], which saves the run first; [gpq_classify_issues()], which
#'   gives the issues their classes; [gpq_decode_lineage()], which gives them provider keys.
#' @examples
#' # MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
#' results <- gpq_check_conformance(
#'   magp_example, magp_spec(),
#'   group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
#'   problem_unit = list("magp_plots", "magp_plot_id"), rules = magp_rules(), layers = 0:2
#' )
#' gpq_classify_issues(results$issues, magp_rules())
#' output <- tempfile("runs")
#' gpq_write_results(results, output)
#' files <- gpq_build_reports(results, output)
#' basename(files)
#' @export
gpq_build_reports <- function(results, output_dir, group_cols = NULL, lineage = NULL,
                              lang = NULL, text = NULL, row_cap = NULL, example_cap = NULL,
                              cell_cap = NULL) {
  if (!inherits(results, "gpq_results")) {
    stop("`results` must be a gpq_results object.", call. = FALSE)
  }
  validate_gpq_results(results)
  dir_ok <- is.character(output_dir) && length(output_dir) == 1L && !is.na(output_dir) &&
    !is_blank(output_dir)
  if (!dir_ok) {
    stop("`output_dir` must be one folder.", call. = FALSE)
  }
  run <- results$metadata$run
  run_id <- run$run_id
  run_dir <- file.path(output_dir, run_id)
  # The run's own saved run.csv (D20.13): its units are read back from it (D20.48).
  run_file <- file.path(run_dir, "metadata", "run.csv")
  saved <- if (file.exists(run_file)) read_run_csv(run_file, metadata_schema()$run)
  if (is.null(saved) || !identical(saved$run_id, run_id)) {
    # W205
    stop(sprintf(
      "`output_dir` holds no saved results of run %s; write them first with gpq_write_results().",
      run_id
    ), call. = FALSE)
  }
  if (!identical(saved$outcome, "completed")) {
    # W206
    stop(
      sprintf("Run %s stopped at pre-flight, so it has no results to report.", run_id),
      call. = FALSE
    )
  }
  unclassed <- sum(is.na(results$issues$issue_class))
  if (unclassed > 0L) {
    # W212
    stop(sprintf(
      paste(
        "Issues with no class: %d. Classify them first with gpq_classify_issues(issues, rules),",
        "which needs no evidence."
      ),
      unclassed
    ), call. = FALSE)
  }
  run_cols <- split_joined(run$group_cols)
  if (length(run_cols) == 0L) {
    if (length(group_cols) > 0L) {
      # W214
      stop(sprintf(
        "Run %s has no group columns, so `group_cols` must be NULL or empty.", run_id
      ), call. = FALSE)
    }
    group_cols <- character()
  } else if (is.null(group_cols)) {
    group_cols <- run_cols
  } else {
    # The report unit first, then any of the run's other group columns, each once (D20.26).
    ok <- is.character(group_cols) && length(group_cols) >= 1L && !anyNA(group_cols) &&
      identical(group_cols[[1L]], run_cols[[1L]]) && !anyDuplicated(group_cols) &&
      all(group_cols[-1L] %chin% run_cols[-1L])
    if (!ok && length(run_cols) > 1L) {
      # W208
      stop(sprintf(
        paste(
          "`group_cols` must start with the run's report-unit column, %s, and may add any of",
          "its other group columns (%s)."
        ),
        run_cols[[1L]], paste(run_cols[-1L], collapse = ", ")
      ), call. = FALSE)
    }
    if (!ok) {
      # W208b
      stop(sprintf(
        paste(
          "`group_cols` must be %s, the run's report-unit column, since the run has no other",
          "group columns."
        ),
        run_cols[[1L]]
      ), call. = FALSE)
    }
  }
  # Each setting from the argument, the run's own rule-set rows (its saved settings of tier
  # "rule set", 3.4, D9.29), the option or the built-in value; a bad value is resolve_setting()'s
  # caller error naming its tier. Each keeps the tier it came from, for section 1 (D20.120).
  given <- list(lang = lang, row_cap = row_cap, example_cap = example_cap, cell_cap = cell_cap)
  given <- given[!vapply(given, is.null, logical(1))]
  saved_settings <- results$metadata$settings
  run_rule_set <- list(settings = saved_settings[saved_settings$tier == "rule set"])
  shown <- c("lang", "row_cap", "example_cap", "cell_cap")
  resolved <- lapply(
    structure(shown, names = shown),
    function(name) resolve_setting(name, given, run_rule_set)
  )
  settings <- lapply(resolved, `[[`, "value")
  # W183's slot: the value the run itself used (D20.52).
  settings$column_level_min_n <- as.integer(
    saved_settings$value[saved_settings$setting == "column_level_min_n"]
  )
  lang <- settings$lang
  text <- validate_text_table(text)
  if (!is.null(lineage)) {
    # Read and checked as classification reads its lineage evidence (D20.17).
    lineage <- read_evidence(
      read_run_inputs(list(lineage = lineage), "lineage", "lineage"), run_cols
    )$lineage$data
  }
  unit_col <- if (length(group_cols) > 0L) group_cols[[1L]] else NULL
  if (is.null(unit_col)) {
    # A run with no group columns is a run of one unit, every row its own (D20.29, R3).
    units <- character()
    targets <- list(NULL)
    stems <- "report"
    labels <- run_id
  } else {
    units <- split_units(saved$report_units)
    found <- unique(c(results$results[[unit_col]], results$issues[[unit_col]]))
    stray <- setdiff(found, c(units, "*"))
    if (length(stray) > 0L) {
      # A caller's error: the results were changed after they were written (D20.114).
      stop(sprintf(
        paste(
          "The results hold report units that metadata/run.csv doesn't list: %s. They have",
          "changed since they were written; build from the written run, as gpq_read_results()",
          "reads it back."
        ),
        paste(mark_invalid_utf8(stray), collapse = ", ")
      ), call. = FALSE)
    }
    targets <- as.list(units)
    # Its stops come before any file is removed or written (D20.8).
    stems <- unit_file_name(units)
    labels <- units
  }
  reports_dir <- file.path(run_dir, "reports")
  tables_dir <- file.path(run_dir, "tables")
  # An earlier build's reports and CSVs, and any temporary file a stopped build left (D20.13);
  # never pre-flight's page or the run's own tables, whose names don't end in "_issues.csv".
  earlier <- c(
    setdiff(
      list.files(reports_dir, pattern = "[.]html$", full.names = TRUE),
      file.path(reports_dir, "preflight.html")
    ),
    list.files(tables_dir, pattern = "_issues[.]csv$", full.names = TRUE),
    list.files(
      c(reports_dir, tables_dir),
      pattern = "^[.]gpq_build_", full.names = TRUE, all.files = TRUE
    )
  )
  unlink(earlier)
  left <- earlier[file.exists(earlier)]
  if (length(left) > 0L) {
    stop(sprintf(
      "Can't remove %s, which an earlier build wrote; it may be open in another program.",
      left[[1L]]
    ), call. = FALSE)
  }
  files <- structure(character(), names = character())
  if (length(targets) == 0L) {
    # No unit listed, no provider to write for (R1).
    return(invisible(files))
  }
  for (folder in c(reports_dir, tables_dir)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(folder)) {
      stop(
        "Can't create the folder ", folder, " for the run's reports; a file of that name may be ",
        "in the way.",
        call. = FALSE
      )
    }
  }
  # Each file is written under a temporary name in its own folder, then renamed, so a build
  # that stops leaves no partly written file; one a stop leaves is removed on exit (D20.13).
  partial <- character()
  build <- environment()
  on.exit(unlink(partial), add = TRUE)
  write_whole <- function(path, write) {
    temp <- tempfile(".gpq_build_", tmpdir = dirname(path))
    assign("partial", c(partial, temp), envir = build)
    write(temp)
    if (!file.rename(temp, path)) {
      stop("Can't write ", path, "; it may be open in another program.", call. = FALSE)
    }
    assign("partial", setdiff(partial, temp), envir = build)
  }
  # What every report shows of the run: its metadata without the list of units (D20.40), the
  # problem unit's key column (the issues' one column outside the schema and the group columns;
  # run.csv doesn't record it, D15.40), and the footer's run ID and package version.
  metadata <- results$metadata
  metadata$run <- copy(metadata$run)
  set(metadata$run, j = "report_units", value = NA_character_)
  # Section 1 shows the settings this build used, each written as the run writes it, with the
  # tier it came from; only the report's copy changes, never the caller's results (D20.120).
  metadata$settings <- copy(metadata$settings)
  at <- match(shown, metadata$settings$setting)
  kept <- which(!is.na(at))
  set(
    metadata$settings,
    i = at[kept], j = c("value", "tier"),
    value = list(
      vapply(resolved[kept], function(one) setting_text(one$value), character(1)),
      vapply(resolved[kept], `[[`, character(1), "tier")
    )
  )
  key_col <- setdiff(names(results$issues), c(names(results_schema()$issues), run_cols))
  key_col <- if (length(key_col) == 1L) key_col else NULL
  meta <- c(run_id, run$package_version)
  names(meta) <- c(
    report_text("report_meta_run_id", lang, text = text),
    report_text("report_meta_package", lang, text = text)
  )
  # The masthead's run line (W7; task 6's `subtitle`, D20.66), its time as section 1 shows it.
  masthead <- report_text(
    "report_masthead_run", lang,
    run_id = run_id, run_end = sub("Z$", "", sub("T", " ", run$run_end, fixed = TRUE)),
    text = text
  )
  builders <- list(
    section_header, section_glance, section_actions, section_harmonization,
    section_under_review, section_column_level, section_not_evaluated, section_guide
  )
  for (k in seq_along(targets)) {
    unit <- targets[[k]]
    # The unit's lineage rows are filtered in route_unit(), so no builder sees the whole input
    # (D20.17, D20.61, D20.65).
    routed <- route_unit(results, unit, units, lineage)
    issues <- routed$issues
    tables <- list(
      results = routed$results, issues = issues, corrections = routed$corrections,
      metadata = metadata, unit = unit, group_cols = group_cols, key_col = key_col,
      lineage = routed$lineage
    )
    # The unit's CSV name, which the sections' cap notes and guide name, travels with the
    # settings (D20.68).
    unit_settings <- c(settings, list(csv_name = paste0(stems[[k]], "_issues.csv")))
    title <- if (is.null(unit)) {
      report_text("report_title_run", lang, run_id = run_id, text = text)
    } else {
      report_text("report_title", lang, unit = unit, text = text)
    }
    sections <- lapply(builders, function(builder) builder(tables, text, unit_settings))
    page <- html_page(
      title, sections, meta,
      subtitle = masthead, lang = lang, text = text
    )
    html <- file.path(reports_dir, paste0(stems[[k]], ".html"))
    csv <- file.path(tables_dir, paste0(stems[[k]], "_issues.csv"))
    write_whole(html, function(path) writeBin(charToRaw(enc2utf8(page)), path))
    # The run's group columns, all of them, as 11.6 lists them (T2, R7).
    provider <- provider_csv_table(issues, run_cols, key_col, lang, text)
    write_whole(csv, function(path) {
      fwrite(provider, path, bom = TRUE, encoding = "UTF-8", eol = "\n")
    })
    files <- c(files, structure(c(html, csv), names = rep(labels[[k]], 2L)))
    # Once the unit's files are written, its routed tables, page and CSV table are released,
    # so the next unit's are never held beside them (D20.92).
    rm(routed, issues, tables, sections, page, provider)
  }
  invisible(files)
}
