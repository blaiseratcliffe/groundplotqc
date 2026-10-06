# D12.73 (5a): changes the one cell of a converted CSV copy that csv_cell_edits (in
# edits_20261005.R) names: CLR's treat_vs_dist, T to TD, in the treatment/disturbance copy.
# The copy is read from <input_dir>, the one field is changed with the quotes kept (every other
# byte of the line stays as it was), the result is checked by reading both files back (one
# cell differs, the edited one), and it is written to <output_dir>. It never overwrites a file.
#
# Usage: Rscript edit_csv_cell.R <input_dir> <output_dir>
#   input_dir   the folder holding the converted copy, as patch_workbooks.R wrote it
#   output_dir  a folder with no copy of that name in it (made if missing); it gets the edited
#               copy, to put in place of the input one before verify_spec_edits.R runs
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
    path,
    sep = ",", header = TRUE, colClasses = "character", na.strings = "", strip.white = FALSE
  )
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
for (e in csv_cell_edits) {
  path <- file.path(input_dir, e$file)
  target <- file.path(output_dir, e$file)
  if (!file.exists(path)) {
    stop("Missing input file: ", path, call. = FALSE)
  }
  if (file.exists(target)) {
    stop("The output file exists already: ", target, call. = FALSE)
  }
  x <- rawToChar(readBin(path, "raw", file.info(path)$size))
  Encoding(x) <- "UTF-8"
  lines <- strsplit(x, "\r\n", fixed = TRUE)[[1]]
  tab <- read_table(path)
  i <- which(tab[[e$key_col]] == e$key)
  j <- match(e$col, names(tab))
  stopifnot(length(i) == 1L, i + 1L == e$line, identical(tab[[e$col]][i], e$from))
  fields <- split_record(lines[e$line])
  stopifnot(length(fields) == ncol(tab), identical(fields[j], e$from))
  fields[j] <- e$to
  new_line <- paste(fields, collapse = ",")
  stopifnot(identical(split_record(new_line)[-j], split_record(lines[e$line])[-j]))
  lines[e$line] <- new_line
  out <- charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n")))
  # Checked in a temporary file first, so a failed check leaves nothing in the output folder.
  staged <- tempfile(fileext = ".csv")
  writeBin(out, staged)
  new_tab <- read_table(staged)
  a <- as.matrix(tab)
  b <- as.matrix(new_tab)
  differ <- which(!((is.na(a) & is.na(b)) | a == b), arr.ind = TRUE)
  stopifnot(
    nrow(differ) == 1L, differ[1, 1] == i, differ[1, 2] == j,
    identical(new_tab[[e$col]][i], e$to)
  )
  stopifnot(file.copy(staged, target))
  cat(sprintf(
    "Wrote %s: line %d field %d (%s) %s -> %s; one cell differs; %d bytes\n",
    target, e$line, j, e$col, e$from, e$to, length(out)
  ))
}
