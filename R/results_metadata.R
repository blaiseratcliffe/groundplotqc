# Run metadata (plan 9.5, 11.9; D4.17, D7.11, D7.12, D8.16, D9.29, D15.2, D15.5, D15.8): the
# run's ID, the small tables written once each as metadata/<table>.csv in the run folder,
# their builders, and the self-test that lists declared rules a run didn't report (9.7).
# M4's gpq_check_conformance() makes a run's metadata with new_metadata(); a run stopped at
# pre-flight writes only its run and spec_manifest tables beside the pre-flight files. The run
# table's report units are one escaped text, written by join_units() and read back by
# split_units() (D20.7, D20.48).

# A run ID's form: the UTC start time, then six hex characters (9.1, D15.5).
run_id_pattern <- "^[0-9]{8}T[0-9]{6}Z_[0-9a-f]{6}$"

#' A new run ID for a run that started at `start`
#'
#' The UTC start time to the second, "_", and the first six hex characters of the SHA-256
#' of the start time to the microsecond, the R process ID and the session's temporary
#' folder (D15.5): unique per run, and the caller's random-number stream untouched.
#' @noRd
new_run_id <- function(start = Sys.time()) {
  seed <- paste(format(start, "%Y-%m-%d %H:%M:%OS6", tz = "UTC"), Sys.getpid(), tempdir())
  paste0(
    format(start, "%Y%m%dT%H%M%SZ", tz = "UTC"), "_",
    substr(sha256_bytes(charToRaw(enc2utf8(seed))), 1L, 6L)
  )
}

#' A time as UTC text, such as "2026-10-08T14:03:12Z"
#' @noRd
utc_text <- function(time) {
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' The run metadata's tables and their columns, in 9.5's order
#'
#' `run` is one row; `spec_manifest`, `clashes` and `preflight` have the spec's and
#' pre-flight's own columns; `inputs` names each input file with its SHA-256 or says how
#' the input came ("in memory"; a DBI connection, D9.23), `detail` holding what else 9.5
#' records of it; `rules` names the rule set (and from M13 the thresholds) with its
#' version and SHA-256 (D15.8); `settings` has each setting's value and tier (D15.2);
#' `self_test` the declared rules the run didn't report.
#' @noRd
metadata_schema <- function() {
  chr <- "character"
  list(
    run = c(
      run_id = chr, run_start = chr, run_end = chr, package_version = chr, git_commit = chr,
      r_version = chr, platform = chr, data_table_version = chr, sf_version = chr,
      report_units = chr, group_cols = chr, outcome = chr
    ),
    spec_manifest = spec_schema()$manifest,
    inputs = c(input = chr, file = chr, sha256 = chr, detail = chr),
    rules = c(input = chr, name = chr, version = chr, sha256 = chr),
    settings = c(setting = chr, value = chr, tier = chr),
    clashes = spec_schema()$clashes,
    preflight = preflight_columns(),
    self_test = c(rule_id = chr, stage = chr, layer = "integer")
  )
}

#' The run table: one row
#'
#' `run_start` and `run_end` are times, written as UTC text; `group_cols` the run's group
#' columns, report unit first (D9.29), "; "-joined, NA when there are none; `report_units` the
#' report-unit values found in the run's input (MAGPlot's contributors, D7.12), so a stopped
#' run records them too, as join_units() writes them: escaped, in the C order and joined
#' (D20.7). `outcome` is `completed` or `stopped_preflight` (D8.16). The package, R,
#' data.table and sf versions are read here, sf's only when it is installed, and the git
#' commit when the package runs from a git checkout.
#' @noRd
run_metadata <- function(run_id, run_start, run_end, group_cols = character(),
                         report_units = character(), outcome = "completed") {
  joined <- function(x) if (length(x) == 0L) NA_character_ else paste(x, collapse = "; ")
  sf_installed <- nzchar(system.file(package = "sf"))
  data.table(
    run_id = run_id, run_start = utc_text(run_start), run_end = utc_text(run_end),
    package_version = as.character(utils::packageVersion("groundplotqc")),
    git_commit = package_git_commit(),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    data_table_version = as.character(utils::packageVersion("data.table")),
    sf_version = if (sf_installed) as.character(utils::packageVersion("sf")) else NA_character_,
    report_units = join_units(report_units),
    group_cols = joined(group_cols),
    outcome = outcome
  )
}

#' The report units as the one text run.csv records (D20.7, D20.37)
#'
#' The units are taken once each, in the C order whatever the session's collation, so the
#' same input gives the same run.csv on every machine. Inside each unit a backslash is
#' written as two backslashes and a semicolon as a backslash and a semicolon; the units are
#' then joined with "; ", so a unit holding "; " can be split back (split_units()). A unit
#' holding neither is written as it is, so MAGPlot's IDs read as before. NA when there are
#' none. The caller leaves out "*", which is no unit (D20.6, gpq_check_conformance()).
#' @noRd
join_units <- function(units) {
  units <- sort(unique(units), method = "radix")
  if (length(units) == 0L) {
    return(NA_character_)
  }
  # Backslashes first, so the backslash a semicolon gains isn't doubled in turn.
  escaped <- gsub("\\", "\\\\", units, fixed = TRUE)
  escaped <- gsub(";", "\\;", escaped, fixed = TRUE)
  paste(escaped, collapse = "; ")
}

#' run.csv's report units split back into the units join_units() was given (D20.7, D20.48)
#'
#' `x` is one text as join_units() writes it, or NA. A "; " ends a unit only where its ";"
#' follows an even number of backslashes, none counting as even: each escape pair, a
#' backslash and the character after it, is matched and passed over, so a ";" in a pair never
#' splits. Each part is then unescaped in one left-to-right pass, each backslash and the
#' character after it becoming that character, never two replacements one after the other.
#' NA, which read_run_csv() reads from an empty field, gives no units; the empty text gives
#' one empty unit, as join_units("") writes it, though a blank value is never a unit (D9.24).
#' @noRd
split_units <- function(x) {
  if (is.na(x)) {
    return(character())
  }
  # Where each "; " outside every escape pair starts; (?s) lets "." take a line break too.
  at <- gregexpr("(?s)\\\\.(*SKIP)(*FAIL)|; ", x, perl = TRUE)[[1L]]
  at <- at[at > 0L]
  parts <- substring(x, c(1L, at + 2L), c(at - 1L, nchar(x)))
  gsub("(?s)\\\\(.)", "\\1", parts, perl = TRUE)
}

#' The git commit of the checkout the package runs from, or NA
#'
#' Only a package loaded from a source folder holding `.git` (a checkout or a worktree) has
#' one; git is asked for it, and NA comes back if git isn't there or says something else.
#' @noRd
package_git_commit <- function() {
  # Under pkgload's load_all(), system.file() looks in the source folder as well as its inst/
  # (the package's own folder being inst/); an installed package has no .git.
  git <- system.file(".git", package = "groundplotqc")
  if (!nzchar(git)) {
    return(NA_character_)
  }
  root <- dirname(git)
  commit <- tryCatch(
    suppressWarnings(system2(
      "git", c("-C", shQuote(root), "rev-parse", "HEAD"),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(e) character()
  )
  if (length(commit) == 1L && grepl("^[0-9a-f]{40}$", commit)) commit else NA_character_
}

#' The settings table: each declared setting's value and tier, then the severity entries
#'
#' One row per declared setting but severity, its value as text (setting_text(): "; "-joined,
#' "name=value" for a named value, NA for none) and the tier it came from (resolve_setting());
#' then one row per severity entry given as an argument
#' or an option, `setting` "severity:<rule_id>" (D15.2). The rule set's severities stay in
#' the rule set, which the rules table identifies. `rules` is a rule set, checked here.
#' @noRd
settings_metadata <- function(settings = list(), rules = NULL) {
  rules <- validate_rule_set(rules)
  check_settings(settings, rules)
  declared <- setdiff(names(setting_definitions()), "severity")
  rows <- lapply(declared, function(setting) {
    resolved <- resolve_setting(setting, settings, rules)
    data.table(
      setting = setting, value = setting_text(resolved$value), tier = resolved$tier
    )
  })
  entries <- severity_entries(settings)
  rbindlist(c(
    list(empty_table(metadata_schema()$settings)), rows,
    list(data.table(
      setting = paste0("severity:", entries$rule_id, recycle0 = TRUE), value = entries$severity,
      tier = entries$tier
    ))
  ))
}

#' The rules table: the rule set's name, version and SHA-256 (D15.8), or no row without one
#'
#' The SHA-256 is of the checked rule set's bytes (rule_set_bytes()), the same whether it
#' was read from files or given in memory. Name and version are NA without a meta row.
#' @noRd
rules_metadata <- function(rules = NULL) {
  rules <- validate_rule_set(rules)
  if (is.null(rules)) {
    return(empty_table(metadata_schema()$rules))
  }
  meta <- rules$meta
  data.table(
    input = "rules",
    name = if (is.null(meta)) NA_character_ else meta$rule_set_name,
    version = if (is.null(meta)) NA_character_ else meta$version,
    sha256 = sha256_bytes(rule_set_bytes(rules))
  )
}

#' The self-test (9.7; D8.11): the rules a run was to report that have no results row
#'
#' `expected` holds the registry rows of the rules the run was to run (`rule_id`, `stage`,
#' `layer`); its caller decides which, from the layers it ran and the rule set. A rule
#' with a not_run row was reported. Any row here marks the run's summary as incomplete.
#' @noRd
run_self_test <- function(results, expected) {
  missing <- !expected$rule_id %chin% results$rule_id
  data.table(
    rule_id = expected$rule_id[missing], stage = expected$stage[missing],
    layer = expected$layer[missing]
  )
}

#' A run's metadata: its tables in 9.5's order, each checked
#'
#' `run` is run_metadata()'s row; `spec` the spec the run read, its manifest and the clashes
#' the precedence rule resolved (D1.4b) copied; `preflight` gpq_preflight()'s table;
#' `rules` and `settings` as the run took them; `inputs` and `self_test` empty when NULL.
#' @noRd
new_metadata <- function(run, spec, preflight, rules = NULL, settings = list(),
                         inputs = NULL, self_test = NULL) {
  schema <- metadata_schema()
  resolved <- spec$clashes$rule_id == "spec_clash_resolved"
  metadata <- list(
    run = copy(run),
    spec_manifest = copy(spec$manifest),
    inputs = if (is.null(inputs)) empty_table(schema$inputs) else copy(inputs),
    rules = rules_metadata(rules),
    settings = settings_metadata(settings, rules),
    clashes = spec$clashes[resolved],
    preflight = copy(preflight),
    self_test = if (is.null(self_test)) empty_table(schema$self_test) else copy(self_test)
  )
  validate_metadata(metadata)
  metadata
}

#' A run's metadata checked: every table of 9.5, in order, each against its schema
#' @noRd
validate_metadata <- function(metadata) {
  schema <- metadata_schema()
  if (!is.list(metadata) || !identical(names(metadata), names(schema))) {
    stop(sprintf(
      "A run's metadata holds the tables %s, in that order.", paste(names(schema), collapse = ", ")
    ), call. = FALSE)
  }
  for (name in names(schema)) {
    validate_schema(metadata[[name]], name, schema[[name]])
  }
  if (!grepl(run_id_pattern, metadata$run$run_id)) {
    stop(sprintf(
      "The run ID %s isn't a UTC start time and six hex characters.", metadata$run$run_id
    ), call. = FALSE)
  }
  invisible(NULL)
}
