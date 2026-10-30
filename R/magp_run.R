# MAGPlot's run end to end (plan 3.2, 3.4, 9.5, 11.9; D7.5, D7.11, D7.34, D21.3, D21.5,
# D21.11 to D21.15, D21.18): magp_run_qc() loads one delivery's tables, pre-flights MAGPlot's
# specification, rule set and text, runs Layers 0 to 4 with MAGPlot's group columns and problem
# unit, decodes and classes the issues with MAGPlot's flags and evidence, records the delivery's
# file in the inputs metadata, writes the run folder and builds the reports. MAGPlot's names
# reach the engine through this file (plan 3.2 step 4); the engine never names them.

#' Run MAGPlot's QC on one delivery
#'
#' @description
#' Runs MAGPlot 2.0's quality checks on one delivery from start to finish: pre-flight, the
#' checks of Layers 0 to 4, each issue's provider key and class, the run's files in
#' `<output_dir>/<run_id>/`, and, by default, a report and an issues CSV for each contributor.
#'
#' @details
#' The run makes its run ID, then loads the tables, then runs pre-flight, so a run that
#' pre-flight stops still records the contributors its tables hold. This is the reverse of
#' [gpq_check_conformance()], which runs pre-flight before it reads any table. Pre-flight
#' checks MAGPlot's specification, rule set, report text and `settings`, and its warning
#' reaches you, naming the run's `reports/preflight.html`. When it stops, the run writes
#' `metadata/run.csv`, with the outcome `stopped_preflight`, and `metadata/spec_manifest.csv`,
#' as well as pre-flight's two files, `metadata/preflight.csv` and `reports/preflight.html`,
#' then stops with an error of class `gpq_preflight_error`.
#'
#' Otherwise the run checks Layers 0 to 4, with MAGPlot's group columns `magp_contributor_id`,
#' `jurisdiction` and `magp_dataset_id` and its problem unit, the plot (`magp_plot_id` of
#' `magp_plots`). It then gives each issue its provider key, from its contributor's lineage
#' spec and Ontario's age-tree flag, and its class, from the evidence: the columns MAGPlot's
#' pipeline computes and its translation tables' provider codes. It writes the results and the
#' run metadata, and with `report = TRUE` builds the reports.
#'
#' In `metadata/run.csv`, `run_start` is when the run started and `run_end` is set just before
#' the results are written: the time between them covers loading the tables, pre-flight, the
#' checks, the provider keys and the classes, not the writing or the reports.
#'
#' In `metadata/inputs.csv`, each table's row (`tables:<name>`) names the delivery's `.Rdata`
#' file and its SHA-256, with the table's row count as its detail. A delivery that holds
#' `magp_meta` adds a row with the input `magp_meta`, naming the same file and hash;
#' `magp_meta` itself is never checked. Tables given as a plain named list are recorded as "in
#' memory", with no hash.
#'
#' The run lets go of the tables once each issue has its provider key, before the classes and
#' the reports. Their memory is freed only when nothing else holds them, so give the `.Rdata`
#' file's path to use the least memory.
#'
#' `settings` goes to pre-flight, the checks and the classes; its `lang`, `row_cap`,
#' `example_cap` and `cell_cap` also go to the reports.
#'
#' A stop after pre-flight passes and before the results are written leaves pre-flight's two
#' files and no `metadata/run.csv`. A stop while the reports are built leaves the run's saved
#' results, whose reports [gpq_build_reports()] builds again.
#'
#' @param tables One delivery's tables: [magp_read_delivery()]'s result, the path of the
#'   delivery's `.Rdata` file, which the run passes to [magp_read_delivery()], or a named list
#'   of data.frames, one per table. More than one path, or a list holding CSV paths, is
#'   refused; check CSV files with [gpq_check_conformance()].
#' @param output_dir The folder that holds run folders, made if it doesn't exist. The run writes
#'   into `<output_dir>/<run_id>/`.
#' @param report `TRUE` to build each contributor's report and issues CSV; `FALSE` writes the
#'   results and run metadata, with no contributor reports or issues CSVs.
#' @param settings A named list of settings, which come before the rule set's, the options' and
#'   the built-in values (see [groundplotqc_options]).
#' @return The run's results, invisibly: a `gpq_results` object as [gpq_check_conformance()]
#'   describes it, each issue with its provider key and class, as written to the run folder. A
#'   run that pre-flight stops returns nothing.
#' @seealso [magp_read_delivery()], which loads a delivery; [gpq_build_reports()], which builds
#'   a saved run's reports again.
#' @examples
#' # magp_example's tables saved as a delivery saves them, each as its own object.
#' path <- tempfile(fileext = ".Rdata")
#' save(list = names(magp_example), envir = list2env(magp_example), file = path)
#' output <- tempfile("runs")
#' # MAGPlot's bundled specification has known pre-flight warnings, so this warning is expected.
#' results <- magp_run_qc(path, output)
#' run_dir <- file.path(output, results$metadata$run$run_id)
#' list.files(run_dir, recursive = TRUE)
#' @export
magp_run_qc <- function(tables, output_dir, report = TRUE, settings = list()) {
  # MAGPlot's names for the engine (3.2 step 4; D7.5).
  group_cols <- c("magp_contributor_id", "jurisdiction", "magp_dataset_id")
  problem_unit <- list("magp_plots", "magp_plot_id")
  # The arguments, checked before any table is loaded (D21.18 Q4). magp_meta may come in any
  # shape (D21.12), so only the other elements of a list are looked at for CSV paths.
  in_list <- is.list(tables) && !is.data.frame(tables)
  text_given <- if (in_list) vapply(tables, is.character, logical(1)) else logical()
  text_given[match("magp_meta", names(tables), 0L)] <- FALSE
  if ((is.character(tables) && length(tables) > 1L) || any(text_given)) {
    # M7b W70 (D21.11)
    stop(
      "`tables` holds more than one path, or a list with a path or other text in place of a ",
      "table. Give magp_read_delivery()'s result, the path of one .Rdata file or a named list ",
      "of data.frames, or check CSV files with gpq_check_conformance().",
      call. = FALSE
    )
  }
  path_given <- is.character(tables) && length(tables) == 1L
  # A list's tables are known by their names, so a list with an element unnamed is refused here,
  # before pre-flight writes its files.
  element_names <- names(tables)
  named <- in_list && !is.null(element_names) && !anyNA(element_names) &&
    all(nzchar(element_names))
  if (!path_given && !named) {
    # M7b W71
    stop(
      "`tables` must be magp_read_delivery()'s result, the path of one .Rdata file or a named ",
      "list of data.frames.",
      call. = FALSE
    )
  }
  dir_ok <- is.character(output_dir) && length(output_dir) == 1L && !is.na(output_dir) &&
    !is_blank(output_dir) && !(file.exists(output_dir) && !dir.exists(output_dir))
  if (!dir_ok) {
    # M7b W72
    stop("`output_dir` must be one folder, not an existing file.", call. = FALSE)
  }
  if (!isTRUE(report) && !isFALSE(report)) {
    # M7b W73
    stop("`report` must be TRUE or FALSE.", call. = FALSE)
  }
  rules <- magp_rules()
  # A setting the package doesn't have, or a value it can't take, stops here in the engine's
  # words, before a delivery is loaded (check_settings(), resolve_setting()).
  settings_metadata(settings, rules)
  spec <- magp_spec()
  # The run ID and its start, then the tables, so a run pre-flight stops records the
  # contributors its tables hold (9.5): the reverse of the engine's order (D17.32, D21.18 Q3).
  run_start <- Sys.time()
  run_id <- new_run_id(run_start)
  run_dir <- file.path(output_dir, run_id)
  if (path_given) {
    tables <- magp_read_delivery(tables)
  }
  # The delivery's file and hash, kept before magp_meta leaves the list, which drops them; a
  # plain list has neither, so it stays in memory (D7.34, D21.13).
  source_file <- attr(tables, "source_file", exact = TRUE)
  sha256 <- attr(tables, "sha256", exact = TRUE)
  has_meta <- "magp_meta" %in% names(tables)
  if (has_meta) {
    # magp_meta never reaches the engine (D21.12).
    tables <- tables[!names(tables) %in% "magp_meta"]
  }
  # An integer64 column reads as its numbers from here on, the contributor's among them
  # (D18.86).
  load_bit64(tables)
  units <- held_units(tables, spec, group_cols[[1L]])
  text <- magp_texts()
  preflight <- withCallingHandlers(
    tryCatch(
      gpq_preflight(
        spec, rules,
        inputs = list(), settings = settings, text = text, output_dir = run_dir
      ),
      gpq_preflight_error = function(cond) {
        # A stopped run's own two files beside pre-flight's, then its error (11.9; D7.11).
        run <- run_metadata(
          run_id, run_start, Sys.time(), group_cols, units,
          outcome = "stopped_preflight"
        )
        write_run_metadata(list(run = run, spec_manifest = spec$manifest), run_dir)
        stop(cond)
      }
    ),
    # Pre-flight's warning points to a returned table, which a run doesn't return: it is
    # signalled again naming the run folder's page, its class and table kept (D21.30).
    gpq_preflight_warning = function(cond) {
      # M7b W76
      warning(structure(
        class = c("gpq_preflight_warning", "warning", "condition"),
        list(
          message = sprintf(
            "Pre-flight found %d warning(s); see %s.", sum(cond$preflight$outcome == "warn"),
            file.path(run_dir, "reports", "preflight.html")
          ),
          call = NULL, preflight = cond$preflight
        )
      ))
      invokeRestart("muffleWarning")
    }
  )
  results <- withCallingHandlers(
    gpq_check_conformance(
      tables, spec,
      group_cols = group_cols, problem_unit = problem_unit, rules = rules, layers = 0:4,
      settings = settings, run_id = run_id
    ),
    # Its own pre-flight repeats the one above, whose warning has reached the caller.
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
  # The run's pre-flight table is the one its folder holds, so the write keeps that file
  # (R/results_files.R; D21.18 Q3).
  results$metadata$preflight <- preflight
  # Ontario's age trees on every run that has trees: decoding refuses a flag for a table not
  # given (W172), and a delivery may lack magp_trees, which Layer 0 reports (D21.3, D21.12,
  # D21.28).
  flags <- if ("magp_trees" %in% names(tables)) age_tree_flags(tables)
  gpq_decode_lineage(
    results$issues, tables, spec,
    key_col = group_cols[[1L]], flags = flags
  )
  # The run's own reference to the tables goes: R frees them when nothing else holds them, as
  # when the run loaded them from a path (D21.18 Q5).
  rm(tables, flags)
  gpq_classify_issues(
    results$issues, rules,
    evidence = list(
      computed = computed_evidence(spec = spec), crosswalk = crosswalk_evidence(spec = spec)
    ),
    settings = settings
  )
  # Each table's inputs row names the delivery's file and hash, its detail as the engine wrote
  # it, and a magp_meta row repeats them; a plain list's rows stay in memory (9.5; D21.13).
  inputs <- results$metadata$inputs
  if (!is.null(source_file)) {
    from_tables <- which(startsWith(inputs$input, "tables:"))
    set(inputs, i = from_tables, j = "file", value = utf8_marked(source_file))
    set(inputs, i = from_tables, j = "sha256", value = sha256)
  }
  if (has_meta) {
    delivered <- !is.null(source_file)
    results$metadata$inputs <- rbind(inputs, data.table(
      input = "magp_meta",
      file = if (delivered) utf8_marked(source_file) else "in memory",
      sha256 = if (delivered) sha256 else NA_character_,
      detail = NA_character_
    ))
  }
  # run_start the run's own, which its ID names; run_end set just before the write (D21.14).
  set(
    results$metadata$run,
    j = c("run_start", "run_end"), value = list(utc_text(run_start), utc_text(Sys.time()))
  )
  gpq_write_results(results, output_dir)
  if (report) {
    # The reports read no saved settings but the rule set's, so the caps given come as
    # arguments (D21.18 Q6).
    gpq_build_reports(
      results, output_dir,
      lang = settings[["lang"]], text = text, row_cap = settings[["row_cap"]],
      example_cap = settings[["example_cap"]], cell_cap = settings[["cell_cap"]]
    )
  }
  invisible(results)
}

#' The report units a run's tables hold, read before pre-flight (9.5; D21.18 Q3, D21.29)
#'
#' The distinct values of `unit_col` in the given tables of `spec` that hold it, each column
#' found as the run matches it (match_columns(): by name, else by letter case and spaces), less
#' what a run leaves out of its report units: NA, a blank, a missing-value code of the column's
#' type and "*" (D20.6, D21.16, D21.20). A table given as anything but a data.frame holds none.
#' Each value as text (value_text()), as UTF-8 (enc2utf8()), as group_index() holds a group
#' value. A run pre-flight stops records them; a completed run keeps the engine's report_units.
#' Reads each holder's one column, each distinct value once (16.2).
#' @noRd
held_units <- function(tables, spec, unit_col) {
  attributes <- spec$attributes
  holders <- unique(attributes$table_name[attributes$attribute_name %in% unit_col])
  holders <- holders[holders %chin% names(tables)]
  sentinels <- spec$sentinels
  units <- lapply(holders, function(table) {
    data <- tables[[table]]
    if (!is.data.frame(data)) {
      return(character())
    }
    own <- which(attributes$table_name == table)
    at <- own[match(unit_col, attributes$attribute_name[own])]
    column <- match_columns(names(data), attributes$attribute_name[own])$column[own == at]
    if (is.na(column)) {
      return(character())
    }
    text <- value_text(unique(data[[column]]))
    date_format <- spec$type_map$date_format[
      match(attributes$data_type[[at]], spec$type_map$data_type)
    ]
    family <- type_family(attributes$r_class[[at]], date_format)
    codes <- sentinels$value[sentinels$data_type == family]
    text[!is.na(text) & !is_blank(text) & !text %chin% c(codes, "*")]
  })
  enc2utf8(unique(as.character(unlist(units, use.names = FALSE))))
}
