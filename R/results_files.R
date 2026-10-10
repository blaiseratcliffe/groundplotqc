# A run's files (plan 9.1, 9.5, 11.9; D4.16, D7.11, D15.4): gpq_write_results() writes a
# gpq_results object to <output_dir>/<run_id>/, the three tables to tables/ and the
# metadata to metadata/, and gpq_read_results() reads them back with each column's class
# from its schema, so a round trip is identical. A run stopped at pre-flight writes only its
# run and spec_manifest tables (write_run_metadata()) beside the two pre-flight files. Both
# functions are internal until M4 exports them with examples (D15.4). Every file is the
# compiled writer's bytes: UTF-8, LF, every field quoted, a blank as an empty field. Each
# file is written once (D7.11, D15.17).

#' A run's results written to its folder, `<output_dir>/<run_id>/`
#'
#' `results` is a gpq_results object, checked first; `output_dir` one folder, made if it
#' doesn't exist. Writes `tables/results.csv`, `issues.csv`, `corrections.csv` and every
#' metadata table as `metadata/<table>.csv` (9.5, D7.11). A run folder that already holds
#' a run's results is never written over. The pre-flight files a run wrote there first
#' stay as they are: preflight.csv must hold the results' pre-flight table, byte for byte,
#' or the call stops before writing anything (D15.17). Returns the run folder's path,
#' invisibly.
#' @noRd
gpq_write_results <- function(results, output_dir) {
  if (!inherits(results, "gpq_results")) {
    stop("`results` must be a gpq_results object.", call. = FALSE)
  }
  validate_gpq_results(results)
  dir_ok <- is.character(output_dir) && length(output_dir) == 1L && !is.na(output_dir) &&
    !is_blank(output_dir) && !(file.exists(output_dir) && !dir.exists(output_dir))
  if (!dir_ok) {
    stop("`output_dir` must be one folder, not an existing file.", call. = FALSE)
  }
  run_dir <- file.path(output_dir, results$metadata$run$run_id)
  if (file.exists(file.path(run_dir, "tables", "results.csv"))) {
    stop(
      "The run folder ", run_dir, " already holds a run's results, which are never written ",
      "over.",
      call. = FALSE
    )
  }
  metadata <- results$metadata
  # Pre-flight's own preflight.csv is left as it is, once it is known to be this table.
  preflight_path <- file.path(run_dir, "metadata", "preflight.csv")
  if (file.exists(preflight_path)) {
    on_disk <- readBin(preflight_path, "raw", file.size(preflight_path))
    if (!identical(on_disk, compiled_bytes(metadata$preflight))) {
      stop(
        preflight_path, " holds a different pre-flight table from the results', and is never ",
        "written over.",
        call. = FALSE
      )
    }
    metadata$preflight <- NULL
  }
  tables_dir <- file.path(run_dir, "tables")
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(tables_dir)) {
    stop(
      "Can't create the folder ", tables_dir, " for the run's tables; a file of that name ",
      "may be in the way.",
      call. = FALSE
    )
  }
  for (name in c("results", "issues", "corrections")) {
    writeBin(compiled_bytes(results[[name]]), file.path(tables_dir, paste0(name, ".csv")))
  }
  write_run_metadata(metadata, run_dir)
  invisible(run_dir)
}

#' Metadata tables written as `metadata/<table>.csv` in a run folder
#'
#' `metadata` is a named list of metadata tables, all 9.5's or some of them, as a run
#' stopped at pre-flight writes its run and spec_manifest tables (D7.11); each is checked
#' against its schema before any is written. Returns the paths written, invisibly.
#' @noRd
write_run_metadata <- function(metadata, run_dir) {
  schema <- metadata_schema()
  unknown <- setdiff(names(metadata), names(schema))
  if (length(unknown) > 0L || is.null(names(metadata))) {
    stop(sprintf(
      "Run metadata has the tables %s only.", paste(names(schema), collapse = ", ")
    ), call. = FALSE)
  }
  for (name in names(metadata)) {
    validate_schema(metadata[[name]], name, schema[[name]])
  }
  folder <- file.path(run_dir, "metadata")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(folder)) {
    stop(
      "Can't create the folder ", folder, " for the run's metadata; a file of that name ",
      "may be in the way.",
      call. = FALSE
    )
  }
  paths <- file.path(folder, paste0(names(metadata), ".csv"))
  for (k in seq_along(paths)) {
    writeBin(compiled_bytes(metadata[[k]]), paths[[k]])
  }
  invisible(paths)
}

#' A run's results read back from its folder
#'
#' `run_dir` is one run folder, as gpq_write_results() returns it. Each file is read with
#' its columns' classes from the schema, the caller's columns as text, and the whole
#' checked as a gpq_results object, so the round trip is identical. A run that stopped at
#' pre-flight has no results to read; a missing file, or one that doesn't read cleanly,
#' stops naming it.
#' @noRd
gpq_read_results <- function(run_dir) {
  dir_ok <- is.character(run_dir) && length(run_dir) == 1L && !is.na(run_dir) &&
    dir.exists(run_dir)
  if (!dir_ok) {
    stop("`run_dir` must be one run folder that exists.", call. = FALSE)
  }
  schema <- metadata_schema()
  run <- read_run_csv(file.path(run_dir, "metadata", "run.csv"), schema$run)
  validate_schema(run, "run", schema$run)
  if (run$outcome != "completed") {
    stop(
      "The run in ", run_dir, " stopped at pre-flight, so it has no results to read.",
      call. = FALSE
    )
  }
  metadata <- lapply(names(schema), function(name) {
    if (name == "run") {
      return(run)
    }
    read_run_csv(file.path(run_dir, "metadata", paste0(name, ".csv")), schema[[name]])
  })
  names(metadata) <- names(schema)
  # The problem unit's key column, if any, is read as text like the group columns.
  tables <- results_schema(split_joined(run$group_cols))
  read <- lapply(names(tables), function(name) {
    read_run_csv(file.path(run_dir, "tables", paste0(name, ".csv")), tables[[name]])
  })
  names(read) <- names(tables)
  new_gpq_results(read$results, read$issues, read$corrections, metadata)
}

#' One of a run's CSV files read with its columns' classes
#'
#' A column `classes` doesn't name is read as text. The file is the package's own output,
#' so any warning while reading it stops the read naming the file (as R42 for the compiled
#' spec); warn is 1 meanwhile, so a session's warn = 2 doesn't hide the file's name.
#' @noRd
read_run_csv <- function(path, classes) {
  if (!file.exists(path)) {
    stop("The run folder has no ", path, ".", call. = FALSE)
  }
  session <- options(datatable.auto.index = FALSE, warn = 1L)
  on.exit(options(session), add = TRUE)
  warned <- FALSE
  read <- function(...) {
    withCallingHandlers(
      fread(
        file = path, sep = ",", header = TRUE, na.strings = "", encoding = "UTF-8",
        strip.white = FALSE, showProgress = FALSE, ...
      ),
      warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
  }
  header <- names(read(nrows = 0L, colClasses = "character"))
  wanted <- unname(classes[header])
  wanted[is.na(wanted)] <- "character"
  data <- read(colClasses = wanted)
  if (warned) {
    stop(path, " doesn't read cleanly; it may have been edited.", call. = FALSE)
  }
  undo_doubled_quotes(data, names(data)[vapply(data, is.character, logical(1))])
  data
}
