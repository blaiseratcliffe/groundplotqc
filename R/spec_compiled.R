# A compiled specification (plan 3.6, 4.4; D12.14, D12.15, D12.20, D12.36, D12.59, D12.69):
# one CSV per component, manifest.csv being the manifest component. read_compiled_spec()
# reads them back with each component's column classes, so the reloaded object is
# identical() to the one written. The files are the package's own output, so a typed one
# is read with fread() directly and any warning stops the read (R42, D12.69). MAGPlot's
# build writes its set into inst/extdata/magp/, which magp_spec() reads (R/magp_spec.R,
# M2's closing task, D12.20). compiled_bytes() makes a component's file bytes in memory,
# which the writer writes and the fingerprints hash (D15.7).

#' A spec written as one UTF-8 CSV per component
#'
#' Lines end in LF on every OS, and a double column is written as text to 17 significant
#' digits, since fwrite() keeps 15 and the reload must be identical() (R20). A spec that
#' doesn't validate is refused before anything is written.
#' @noRd
write_compiled_spec <- function(spec, dir) {
  # Reads only; a spec that doesn't validate stops here, before anything is written.
  validate_gpq_spec(spec)
  component_names <- names(spec_schema())
  # The folder is made, or the call stops naming it, before any file is written.
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir)) {
    stop(
      "Can't create the folder ", dir, " for the compiled specification; a file of that name ",
      "may be in the way.",
      call. = FALSE
    )
  }
  paths <- file.path(dir, paste0(component_names, ".csv"))
  for (k in seq_along(paths)) {
    writeBin(compiled_bytes(spec[[component_names[[k]]]]), paths[[k]])
  }
  invisible(paths)
}

#' A table as a compiled specification's CSV file, in bytes, made in memory (D15.7)
#'
#' UTF-8 with LF endings, so the bytes are the same on every OS (D12.69); every text field
#' and the header quoted; a blank an empty field; a double column as text to 17 significant
#' digits (R20), on a copy, so the caller's table keeps its numbers. A table with no columns
#' gives no bytes. fwrite() writes to a file or to the console, so the text is taken from
#' the console, sunk into a raw connection: its time grows with the rows, where
#' capture.output()'s text connection slows with every line. fwrite()'s verbose and progress
#' output are off, so nothing but the table reaches the bytes. The writers write these bytes
#' and the fingerprints hash them, so a table built in memory and its file give the same
#' hash.
#' @noRd
compiled_bytes <- function(table) {
  if (ncol(table) == 0L) {
    return(raw())
  }
  doubles <- names(table)[vapply(table, is.double, TRUE)]
  if (length(doubles) > 0L) {
    table <- copy(table)
    for (column in doubles) {
      x <- table[[column]]
      set(table, j = column, value = fifelse(is.na(x), NA_character_, sprintf("%.17g", x)))
    }
  }
  bytes <- rawConnection(raw(), "w")
  on.exit(close(bytes), add = TRUE)
  sink(bytes)
  # The sink ends before the connection closes, on an error too.
  on.exit(sink(), add = TRUE, after = FALSE)
  fwrite(
    table,
    file = "", na = "", quote = TRUE, eol = "\n", encoding = "UTF-8", verbose = FALSE,
    showProgress = FALSE
  )
  rawConnectionValue(bytes)
}

#' A compiled spec read back with each component's column classes, identical() to it
#'
#' The files are the package's own output, so one that doesn't read cleanly is an
#' internal error, never a finding (D12.54).
#' @noRd
read_compiled_spec <- function(dir) {
  # No index attributes while reading, so the result is identical() to the spec written
  # (D12.27). warn is 1, so a session's warn = 2 doesn't turn fread()'s warning into an
  # error that doesn't name the file (D12.59). The caller's settings are restored on exit.
  session <- options(datatable.auto.index = FALSE, warn = 1L)
  on.exit(options(session), add = TRUE)
  schema <- spec_schema()
  components <- lapply(names(schema), function(name) {
    path <- file.path(dir, paste0(name, ".csv"))
    if (!file.exists(path)) {
      stop(sprintf("The compiled specification in %s has no %s.csv.", dir, name), call. = FALSE)
    }
    if (file.size(path) == 0) {
      return(data.table())
    }
    classes <- schema[[name]]
    # A warning is noted and muffled, never caught, so fread() finishes its read.
    warned <- FALSE
    note <- function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
    if (length(classes) == 0L) {
      read <- withCallingHandlers(read_csv_text(path), warning = note)
      data <- read$data
      warned <- warned || nrow(read$malformed) > 0L
    } else {
      data <- withCallingHandlers(
        fread(
          file = path, sep = ",", header = TRUE, colClasses = unname(classes),
          na.strings = "", encoding = "UTF-8", strip.white = FALSE, showProgress = FALSE
        ),
        warning = note
      )
      undo_doubled_quotes(data, intersect(names(classes)[classes == "character"], names(data)))
    }
    if (warned) {
      stop(sprintf(
        "The compiled specification's %s.csv in %s doesn't read cleanly; write it again.",
        name, dir
      ), call. = FALSE)
    }
    data
  })
  names(components) <- names(schema)
  new_gpq_spec(components)
}
