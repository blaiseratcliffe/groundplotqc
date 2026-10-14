# A run's files (plan 9.1, 9.5, 11.9; D4.16, D7.11, D15.4): gpq_write_results() writes a
# gpq_results object to <output_dir>/<run_id>/, the three tables to tables/ and the
# metadata to metadata/, and gpq_read_results() reads them back with each column's class
# from its schema, so a round trip is identical. A run stopped at pre-flight writes only its
# run and spec_manifest tables (write_run_metadata()) beside the two pre-flight files. Both
# functions' help pages give runnable examples on magp_example (D15.34). Every file is the
# compiled writer's bytes: UTF-8, LF, every text field quoted, NA as an empty field. Each
# file is written once: every check runs before any file is written, and a run folder that
# holds any file but that run's own pre-flight files stops the write (D7.11, D15.17, D15.27,
# D16.5). Both functions are exported from M4a, with gpq_check_conformance() to make the
# object their examples write (D15.4).

#' Write a run's results to its folder
#'
#' @description
#' Writes a `gpq_results` object to `<output_dir>/<run_id>/`: the results summary,
#' issues and corrections to `tables/`, and the run metadata to `metadata/`, one
#' CSV file each.
#'
#' @details
#' Every file is UTF-8 with LF line endings, every text field quoted and a missing
#' value an empty field, so [gpq_read_results()] reads back the same object. A
#' run's files are written once. Before writing anything the call stops when the
#' run folder already holds a file other than that run's own pre-flight files:
#' `reports/preflight.html`, and `metadata/preflight.csv`, which must hold the
#' results' pre-flight table byte for byte and is then left as it is.
#'
#' @param results A `gpq_results` object, such as [gpq_check_conformance()]
#'   returns.
#' @param output_dir The folder that holds run folders, made if it doesn't exist.
#' @return The run folder's path, invisibly.
#' @examples
#' results <- gpq_check_conformance(
#'   magp_example, magp_spec(),
#'   rules = magp_rules(), layers = 0:2
#' )
#' run_dir <- gpq_write_results(results, tempfile("runs"))
#' list.files(run_dir, recursive = TRUE)
#' @export
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
  # Every file the call would write, the run's pre-flight CSV left out when it is kept
  # (D15.27) that is there, a folder in its place too, then any other file in the run folder
  # but the run's own pre-flight files: a used run ID (D16.5). Each one is named by its path
  # inside the run folder, the files the call would write first, the last two joined by " and "
  # (R69).
  table_names <- c("results", "issues", "corrections")
  inside <- c(
    file.path("tables", paste0(table_names, ".csv")),
    file.path("metadata", paste0(names(metadata), ".csv"))
  )
  present <- if (dir.exists(run_dir)) {
    list.files(run_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  } else {
    character()
  }
  own_preflight <- c("metadata/preflight.csv", "reports/preflight.html")
  existing <- c(
    inside[file.exists(file.path(run_dir, inside))],
    sort(setdiff(present, c(inside, own_preflight)), method = "radix")
  )
  if (length(existing) > 0L) {
    last <- length(existing)
    stop(
      run_dir, " already holds ",
      paste0(paste(existing[-last], collapse = ", "), if (last > 1L) " and ", existing[[last]]),
      ", and a run's files are never written over.",
      call. = FALSE
    )
  }
  # Both folders are made, or the call stops naming the one it couldn't, before any file.
  for (folder in file.path(run_dir, c("tables", "metadata"))) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(folder)) {
      stop(
        "Can't create the folder ", folder, " for the run's ", basename(folder), "; a file of ",
        "that name may be in the way.",
        call. = FALSE
      )
    }
  }
  for (name in table_names) {
    writeBin(compiled_bytes(results[[name]]), file.path(run_dir, "tables", paste0(name, ".csv")))
  }
  write_run_metadata(metadata, run_dir)
  invisible(run_dir)
}

#' Metadata tables written as `metadata/<table>.csv` in a run folder
#'
#' `metadata` is a named list of metadata tables, all 9.5's or some of them, as a run
#' stopped at pre-flight writes its run and spec_manifest tables (D7.11); each is checked
#' against its schema before any is written, and a table's file that already exists stops
#' the call before any is written (D15.27). Returns the paths written, invisibly.
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
  inside <- file.path("metadata", paste0(names(metadata), ".csv"))
  paths <- file.path(run_dir, inside)
  existing <- inside[file.exists(paths)]
  if (length(existing) > 0L) {
    last <- length(existing)
    stop(
      run_dir, " already holds ",
      paste0(paste(existing[-last], collapse = ", "), if (last > 1L) " and ", existing[[last]]),
      ", and a run's files are never written over.",
      call. = FALSE
    )
  }
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(folder)) {
    stop(
      "Can't create the folder ", folder, " for the run's metadata; a file of that name ",
      "may be in the way.",
      call. = FALSE
    )
  }
  for (k in seq_along(paths)) {
    writeBin(compiled_bytes(metadata[[k]]), paths[[k]])
  }
  invisible(paths)
}

#' Read a run's results back from its folder
#'
#' @description
#' Reads the files [gpq_write_results()] wrote back into a `gpq_results` object,
#' each column with its class.
#'
#' @details
#' The object read is identical to the one written. A run that stopped at
#' pre-flight has no results to read, and the call stops; so does a missing file,
#' or one that doesn't read cleanly, naming the file.
#'
#' @param run_dir A run folder, as [gpq_write_results()] returns it.
#' @return An object of class `gpq_results`.
#' @examples
#' results <- gpq_check_conformance(
#'   magp_example, magp_spec(),
#'   rules = magp_rules(), layers = 0:2
#' )
#' run_dir <- gpq_write_results(results, tempfile("runs"))
#' identical(gpq_read_results(run_dir), results)
#' @export
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
  # fread() keeps a quote in a name doubled, as it does in a cell: the names are undone before
  # the class lookup and set after the read, so a caller's column named with a quote reads
  # back as written.
  header <- gsub("\"\"", "\"", names(read(nrows = 0L, colClasses = "character")), fixed = TRUE)
  wanted <- unname(classes[header])
  wanted[is.na(wanted)] <- "character"
  data <- read(colClasses = wanted)
  if (warned) {
    stop(path, " doesn't read cleanly; it may have been edited.", call. = FALSE)
  }
  setnames(data, header)
  undo_doubled_quotes(data, names(data)[vapply(data, is.character, logical(1))])
  data
}
