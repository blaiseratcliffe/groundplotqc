# D12.73 (5a), D12.74 (4): applies the cell edits that csv_cell_edits (in edits_20261005.R) names
# to the converted CSV copies: CLR's treat_vs_dist, T to TD, in the treatment/disturbance copy,
# and the ten cells of the datasets copy's 110.05 and 110.06 that exchange their descriptors. Each
# file is read from <input_dir>; every edit of it is made on the raw field of its line, the
# quotes and every other byte of the file staying as they were; the result is checked by reading
# both files back (the edited lines change only in their edited fields, every other line is
# byte-identical, and the parsed tables differ in exactly the edited cells, each from -> to);
# every file is checked in a temporary file before any is written, once, to <output_dir>, so a
# stop leaves nothing there, and a failing check names the file, key and column. A blank cell is
# NA in an entry, the empty field in the file. It never overwrites a file.
#
# Usage: Rscript edit_csv_cell.R <input_dir> <output_dir>
#   input_dir   the folder holding the converted copies, as patch_workbooks.R wrote them
#   output_dir  a folder with no copy of those names in it (made if missing); it gets the edited
#               copies, to put in place of the input ones before verify_spec_edits.R runs
# Needs data.table.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  cat("Usage: Rscript edit_csv_cell.R <input_dir> <output_dir>\n")
  quit(save = "no", status = 1L)
}
input_dir <- args[[1L]]
output_dir <- args[[2L]]
script_dir <- dirname(normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]])
))
source(file.path(script_dir, "edits_20261005.R"))

# Splits one CSV record into its raw fields, a comma inside double quotes kept in its field.
split_record <- function(s) {
  chars <- strsplit(s, "")[[1]]
  fields <- character()
  cur <- character()
  in_quotes <- FALSE
  for (ch in chars) {
    if (ch == '"') {
      in_quotes <- !in_quotes
    }
    if (ch == "," && !in_quotes) {
      fields <- c(fields, paste(cur, collapse = ""))
      cur <- character()
    } else {
      cur <- c(cur, ch)
    }
  }
  c(fields, paste(cur, collapse = ""))
}
read_table <- function(path) {
  data.table::fread(
    file = path,
    sep = ",", header = TRUE, colClasses = "character", na.strings = "", strip.white = FALSE
  )
}
same_cells <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}
# A blank cell (NA) is the empty field in the file.
raw_text <- function(x) if (is.na(x)) "" else x
shown <- function(x) if (is.na(x)) "(blank)" else x

# Every file is checked and staged in a temporary file first; the output folder is made, and
# the files copied into it, only once all have passed, so a stop leaves nothing there. A failed
# check stops with a message that names its file and, for an entry, its key and column.
staged <- list()
for (name in unique(vapply(csv_cell_edits, `[[`, "", "file"))) {
  ce <- Filter(function(e) e$file == name, csv_cell_edits)
  path <- file.path(input_dir, name)
  target <- file.path(output_dir, name)
  if (!file.exists(path)) {
    stop("Missing input file: ", path, call. = FALSE)
  }
  if (file.exists(target)) {
    stop("The output file exists already: ", target, call. = FALSE)
  }
  tryCatch(
    {
      bytes <- readBin(path, "raw", file.info(path)$size)
      x <- rawToChar(bytes)
      Encoding(x) <- "UTF-8"
      original <- strsplit(x, "\r\n", fixed = TRUE)[[1]]
      # The file is whole CRLF-ended lines, so putting its lines back gives its bytes.
      stopifnot(
        "The file isn't whole CRLF-ended lines" =
          identical(charToRaw(paste0(paste(original, collapse = "\r\n"), "\r\n")), bytes)
      )
      tab <- read_table(path)
      entry_cells <- vapply(ce, function(e) paste(e$key_col, e$key, e$col), "")
      if (anyDuplicated(entry_cells)) {
        stop(
          "A cell is named by two entries: ",
          paste(unique(entry_cells[duplicated(entry_cells)]), collapse = "; "),
          call. = FALSE
        )
      }
      lines <- original
      rows <- integer()
      cols <- integer()
      for (e in ce) {
        tryCatch(
          {
            i <- which(tab[[e$key_col]] == e$key)
            j <- match(e$col, names(tab))
            stopifnot(
              "The key matches no row, or several" = length(i) == 1L,
              "The table has no such column" = !is.na(j),
              "The row isn't on the entry's line" = i + 1L == e$line,
              "The cell doesn't hold the entry's from" = same_cells(tab[[e$col]][i], e$from),
              "The entry's from and to are the same" = !identical(e$from, e$to),
              "The entry's to needs CSV quoting" = !grepl('[,"\r\n]', e$to)
            )
            fields <- split_record(lines[e$line])
            stopifnot(
              "The line has another number of fields than the header" =
                length(fields) == ncol(tab),
              "The line's field isn't the entry's from" = identical(fields[j], raw_text(e$from))
            )
            fields[j] <- raw_text(e$to)
            lines[e$line] <- paste(fields, collapse = ",")
            rows <- c(rows, i)
            cols <- c(cols, j)
          },
          error = function(err) {
            stop(
              conditionMessage(err), " (", e$key_col, " = ", e$key, ", column ", e$col, ")",
              call. = FALSE
            )
          }
        )
      }
      # Every other line is as it was; an edited line differs in its edited fields only.
      edited <- unique(vapply(ce, `[[`, 0L, "line"))
      stopifnot(
        "A line other than an edited one changed" =
          length(lines) == length(original) && identical(lines[-edited], original[-edited])
      )
      for (l in edited) {
        before <- split_record(original[l])
        after <- split_record(lines[l])
        same_fields <- length(before) == length(after) &&
          setequal(which(before != after), cols[rows + 1L == l])
        stopifnot("An edited line changed in more than its edited fields" = same_fields)
      }
      out <- charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n")))
      staged_path <- tempfile(fileext = ".csv")
      writeBin(out, staged_path)
      new_tab <- read_table(staged_path)
      stopifnot(
        "The edited table's shape or header changed" =
          identical(dim(tab), dim(new_tab)) && identical(names(tab), names(new_tab))
      )
      differ <- which(!same_cells(as.matrix(tab), as.matrix(new_tab)), arr.ind = TRUE)
      edited_only <- nrow(differ) == length(rows) &&
        setequal(paste(differ[, 1], differ[, 2]), paste(rows, cols))
      stopifnot("The cells that differ aren't the edited ones" = edited_only)
      for (e in ce) {
        i <- which(tab[[e$key_col]] == e$key)
        if (!identical(new_tab[[e$col]][i], e$to)) {
          stop(
            "The edited cell doesn't hold the entry's to (", e$key_col, " = ", e$key,
            ", column ", e$col, ")",
            call. = FALSE
          )
        }
      }
      staged[[name]] <- list(path = staged_path, ce = ce, edited = edited, n_bytes = length(out))
    },
    error = function(err) {
      stop(conditionMessage(err), " in file ", name, ".", call. = FALSE)
    }
  )
}
if (!dir.exists(output_dir) && !dir.create(output_dir)) {
  stop("Can't create the output folder: ", output_dir, call. = FALSE)
}
for (name in names(staged)) {
  one <- staged[[name]]
  target <- file.path(output_dir, name)
  if (!file.copy(one$path, target)) {
    stop("The file couldn't be copied: ", target, call. = FALSE)
  }
  cat(sprintf(
    "Wrote %s: %d cell(s) on %d line(s) changed, no other cell differs; %d bytes\n",
    target, length(one$ce), length(one$edited), one$n_bytes
  ))
  for (e in one$ce) {
    cat(sprintf(
      "  line %d, %s = %s, %s: %s -> %s\n", e$line, e$key_col, e$key, e$col, shown(e$from),
      shown(e$to)
    ))
  }
}
unlink(vapply(staged, `[[`, "", "path"))
