# MAGPlot 2.0's delivery loader (plan 3.2, 3.4; D7.34, D8.3, D21.11, D21.12): a delivered
# .Rdata file loaded into a private environment, its tables returned as data.tables with the
# file's name and SHA-256, which the run metadata records (9.5, D4.17).

#' Read a MAGPlot 2.0 delivery
#'
#' @description
#' Loads a delivered `.Rdata` file and returns its tables as data.tables, with the file's
#' name and SHA-256 for the run's metadata.
#'
#' @details
#' A delivery's file holds each of MAGPlot 2.0's 17 tables as an object of its own. The tables
#' it holds come first, in the data dictionary's order; a table it lacks is left out, and the
#' checks report it (`table_missing`). Any other data.frame in the file follows, in the file's
#' order, for the checks to report (`table_unexpected`). `magp_meta`, if the file has one,
#' comes last, unchanged. The file's `dd` and `lkp` are dropped, since the specification comes
#' from [magp_spec()], and so is any other object that isn't a data.frame.
#'
#' The file is loaded into an environment of its own, so no object in your session is
#' replaced.
#'
#' The call stops if `path` isn't one `.Rdata` file that R can load, if the file holds none
#' of the 17 tables, or if an object named as one of them isn't a data.frame.
#'
#' @param path The path of one `.Rdata` file.
#' @return A named list: the tables, as data.tables, in the data dictionary's order; any other
#'   data.frames, as data.tables, in the file's order; and `magp_meta`, unchanged, if the file
#'   has one. Its attributes are `source_file`, the file's name without its folder, and
#'   `sha256`, the file's SHA-256 in lower-case hexadecimal.
#' @examples
#' # magp_example's tables saved as a delivery saves them, each as its own object.
#' path <- tempfile(fileext = ".Rdata")
#' save(list = names(magp_example), envir = list2env(magp_example), file = path)
#' tables <- magp_read_delivery(path)
#' names(tables)
#' attr(tables, "source_file")
#' attr(tables, "sha256")
#' unlink(path)
#' @export
magp_read_delivery <- function(path) {
  path_ok <- is.character(path) && length(path) == 1L && !is.na(path) &&
    utils::file_test("-f", path)
  # A private environment, so the file's objects reach neither the caller's session nor the
  # package's.
  env <- new.env(parent = emptyenv())
  # load() warns before it stops on a file it can't read, so its warnings are muffled and the
  # caller's error says what went wrong, ending with load()'s own error where load() stopped,
  # such as memory running out (D22.18). It returns the names it loaded, in the file's order:
  # character() for a file saved with no objects.
  loaded <- if (path_ok) {
    tryCatch(suppressWarnings(load(path, envir = env)), error = function(e) e)
  }
  if (!is.character(loaded)) {
    # M7b W48
    problem <- "The file given must be one .Rdata file that R can load."
    if (inherits(loaded, "error")) {
      # M8 W10 (D22.18, D22.42)
      problem <- sprintf("%s load() stopped with this error: %s", problem, conditionMessage(loaded))
    }
    stop(problem, call. = FALSE)
  }
  file <- basename(path)
  table_names <- unique(magp_spec()$attributes$table_name)
  # The tables the file holds, in the dictionary's order.
  found <- table_names[table_names %chin% loaded]
  if (length(found) == 0L) {
    # M7b W49
    stop(sprintf(
      paste(
        "%s holds none of MAGPlot 2.0's %d tables as an object of its own; a delivery saves",
        "each table as its own object."
      ),
      file, length(table_names)
    ), call. = FALSE)
  }
  is_frame <- function(objects) {
    vapply(objects, function(name) is.data.frame(env[[name]]), logical(1), USE.NAMES = FALSE)
  }
  not_frames <- found[!is_frame(found)]
  if (length(not_frames) > 0L) {
    # M7b W50
    stop(sprintf(
      paste(
        "%s holds, under MAGPlot 2.0's table names, something other than a data.frame: %s.",
        "Each table must be a data.frame."
      ),
      file, paste(not_frames, collapse = ", ")
    ), call. = FALSE)
  }
  # The file's other data.frames, in its order, for table_unexpected; dd, lkp and magp_meta
  # aren't among them, and any object that isn't a data.frame is dropped.
  others <- setdiff(loaded, c(table_names, "dd", "lkp", "magp_meta"))
  extras <- others[is_frame(others)]
  # Each becomes a data.table in place: setDT() sets its class and shares its columns, so no
  # column is copied.
  tables <- mget(c(found, extras), envir = env)
  for (name in names(tables)) {
    frame <- tables[[name]]
    setDT(frame)
    tables[[name]] <- frame
  }
  # magp_meta as the file holds it, whatever its shape, NULL included (D21.12).
  if ("magp_meta" %chin% loaded) {
    tables["magp_meta"] <- list(env[["magp_meta"]])
  }
  attr(tables, "source_file") <- file
  attr(tables, "sha256") <- unname(tools::sha256sum(path))
  tables
}
