# D12.73 (5a), D12.74 (4): applies the cell edits that csv_cell_edits (in edits_20261005.R) names
# to the converted CSV copies: CLR's treat_vs_dist, T to TD, in the treatment/disturbance copy,
# and the ten cells of the datasets copy's 110.05 and 110.06 that exchange their descriptors. Each
# file is read from <input_dir>; every edit of it is made on the raw field of its line, the
# quotes and every other byte of the file staying as they were; the result is checked by reading
# both files back (the edited lines change only in their edited fields, every other line is
# byte-identical, and the parsed tables differ in exactly the edited cells, each from -> to);
# and it is written once to <output_dir>. A blank cell is NA in an entry, the empty field in the
# file. It never overwrites a file.
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
    path,
    sep = ",", header = TRUE, colClasses = "character", na.strings = "", strip.white = FALSE
  )
}
same_cells <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}
# A blank cell (NA) is the empty field in the file.
raw_text <- function(x) if (is.na(x)) "" else x
shown <- function(x) if (is.na(x)) "(blank)" else x

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
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
  bytes <- readBin(path, "raw", file.info(path)$size)
  x <- rawToChar(bytes)
  Encoding(x) <- "UTF-8"
  original <- strsplit(x, "\r\n", fixed = TRUE)[[1]]
  # The file is whole CRLF-ended lines, so putting its lines back gives its bytes.
  stopifnot(identical(charToRaw(paste0(paste(original, collapse = "\r\n"), "\r\n")), bytes))
  tab <- read_table(path)
  stopifnot(!anyDuplicated(vapply(ce, function(e) paste(e$key_col, e$key, e$col), "")))
  lines <- original
  rows <- integer()
  cols <- integer()
  for (e in ce) {
    i <- which(tab[[e$key_col]] == e$key)
    j <- match(e$col, names(tab))
    stopifnot(
      length(i) == 1L, !is.na(j), i + 1L == e$line, same_cells(tab[[e$col]][i], e$from),
      !identical(e$from, e$to), !grepl('[,"\r\n]', e$to)
    )
    fields <- split_record(lines[e$line])
    stopifnot(length(fields) == ncol(tab), identical(fields[j], raw_text(e$from)))
    fields[j] <- raw_text(e$to)
    lines[e$line] <- paste(fields, collapse = ",")
    rows <- c(rows, i)
    cols <- c(cols, j)
  }
  # Every other line is as it was; an edited line differs in its edited fields only.
  edited <- unique(vapply(ce, `[[`, 0L, "line"))
  stopifnot(length(lines) == length(original), identical(lines[-edited], original[-edited]))
  for (l in edited) {
    before <- split_record(original[l])
    after <- split_record(lines[l])
    stopifnot(
      length(before) == length(after),
      setequal(which(before != after), cols[rows + 1L == l])
    )
  }
  out <- charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n")))
  # Checked in a temporary file first, so a failed check leaves nothing in the output folder.
  staged <- tempfile(fileext = ".csv")
  writeBin(out, staged)
  new_tab <- read_table(staged)
  stopifnot(identical(dim(tab), dim(new_tab)), identical(names(tab), names(new_tab)))
  differ <- which(!same_cells(as.matrix(tab), as.matrix(new_tab)), arr.ind = TRUE)
  stopifnot(
    nrow(differ) == length(rows), setequal(paste(differ[, 1], differ[, 2]), paste(rows, cols))
  )
  for (e in ce) {
    i <- which(tab[[e$key_col]] == e$key)
    stopifnot(identical(new_tab[[e$col]][i], e$to))
  }
  stopifnot(file.copy(staged, target))
  cat(sprintf(
    "Wrote %s: %d cell(s) on %d line(s) changed, no other cell differs; %d bytes\n",
    target, length(ce), length(edited), length(out)
  ))
  for (e in ce) {
    cat(sprintf(
      "  line %d, %s = %s, %s: %s -> %s\n", e$line, e$key_col, e$key, e$col, shown(e$from),
      shown(e$to)
    ))
  }
}
