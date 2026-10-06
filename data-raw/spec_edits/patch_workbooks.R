# Makes the 20261005 specification files from the 20260925 ones (Y8; D12.72 (1), (18), (13),
# (21)). The three workbooks are built by patching copies of the 20260925 packages: only the
# XML of the sheets the edits touch changes (and, for A2, the filter's defined name in
# xl/workbook.xml); every other part is copied byte for byte. New text is written as inline
# strings. The CSV copies follow: the datasets table, then each translation table converted
# to UTF-8, the species one gaining its `comments` column. The edit list is
# edits_20261005.R. The treatment/disturbance copy's one cell edit is edit_csv_cell.R's.
#
# Usage: Rscript patch_workbooks.R <baseline_dir> <translation_dir> <output_dir>
#   baseline_dir     the 20260925 DD, Lookup_Tables and A2 workbooks and datasets CSV
#   translation_dir  the four translation tables under their master names (only read)
#   output_dir       a folder that doesn't exist yet; it gets the eight 20261005 files
# Needs readxl, data.table and zip (this script only; never the package). Check the result
# with verify_spec_edits.R.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  cat("Usage: Rscript patch_workbooks.R <baseline_dir> <translation_dir> <output_dir>\n")
  quit(save = "no", status = 1L)
}
baseline_dir <- args[[1L]]
translation_dir <- args[[2L]]
output_dir <- args[[3L]]
script_dir <- dirname(normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]])
))
source(file.path(script_dir, "edits_20261005.R"))

inputs <- c(
  file.path(baseline_dir, unname(workbooks)),
  vapply(names(csv_sources), csv_source_path, "", baseline_dir, translation_dir)
)
if (!all(file.exists(inputs))) {
  stop("Missing input files: ", paste(inputs[!file.exists(inputs)], collapse = ", "), call. = FALSE)
}
if (file.exists(output_dir)) {
  stop("The output folder exists already: ", output_dir, call. = FALSE)
}
dir.create(output_dir)

col_index <- function(letters) {
  j <- 0L
  for (ch in strsplit(letters, "")[[1]]) {
    j <- j * 26L + match(ch, LETTERS)
  }
  j
}
col_letters <- function(j) {
  s <- ""
  while (j > 0) {
    r <- (j - 1) %% 26
    s <- paste0(LETTERS[r + 1], s)
    j <- (j - 1) %/% 26
  }
  s
}
parse_ref <- function(ref) {
  list(col = col_index(sub("[0-9]+$", "", ref)), row = as.integer(sub("^[A-Z]+", "", ref)))
}
read_text <- function(path, sheet) {
  x <- suppressMessages(readxl::read_excel(path,
    sheet = sheet, col_names = FALSE,
    col_types = "text", trim_ws = FALSE, .name_repair = "minimal",
    range = readxl::cell_limits(c(1, 1), c(NA, NA))
  ))
  m <- as.matrix(x)
  dimnames(m) <- NULL
  m
}
get_cell <- function(m, row, col) {
  if (row <= nrow(m) && col <= ncol(m)) m[row, col] else NA_character_
}
check_guards <- function(m, guard, what) {
  for (ref in names(guard)) {
    p <- parse_ref(ref)
    got <- get_cell(m, p$row, p$col)
    want <- guard[[ref]]
    ok <- if (is.na(want)) is.na(got) else !is.na(got) && identical(got, want)
    if (!ok) {
      stop(sprintf("Guard failed for %s: %s holds [%s], expected [%s]", what, ref, got, want),
        call. = FALSE
      )
    }
  }
}
apply_text <- function(current, e) {
  if (e$op == "set") {
    return(if (e$type == "blank") NA_character_ else as.character(e$value))
  }
  if (is.na(current)) {
    stop("Can't ", e$op, " an empty cell: ", e$sheet, "!", e$ref, call. = FALSE)
  }
  if (e$op == "append") {
    if (endsWith(current, e$value)) {
      stop("Already appended: ", e$sheet, "!", e$ref, call. = FALSE)
    }
    return(paste0(current, e$value))
  }
  hits <- sum(gregexpr(e$from, current, fixed = TRUE)[[1]] > 0L)
  if (hits != 1L) {
    stop(sprintf("'%s' occurs %d times in %s!%s", e$from, hits, e$sheet, e$ref), call. = FALSE)
  }
  sub(e$from, e$value, current, fixed = TRUE)
}
read_raw_text <- function(path) {
  x <- rawToChar(readBin(path, "raw", file.info(path)$size))
  Encoding(x) <- "UTF-8"
  x
}
write_raw_text <- function(x, path) writeBin(charToRaw(enc2utf8(x)), path)
xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# ---- sheet XML: rows and cells ----
split_sheet <- function(xml) {
  start <- regexpr("<sheetData>", xml, fixed = TRUE)
  end <- regexpr("</sheetData>", xml, fixed = TRUE)
  if (start < 0 || end < 0) {
    stop("No <sheetData>...</sheetData> in the sheet", call. = FALSE)
  }
  body <- substr(xml, start + nchar("<sheetData>"), end - 1L)
  pattern <- "(?s)<row [^>]*/>|<row [^>]*>.*?</row>"
  rows <- regmatches(body, gregexpr(pattern, body, perl = TRUE))[[1]]
  if (!identical(paste(rows, collapse = ""), body)) {
    stop("Unexpected content between rows", call. = FALSE)
  }
  numbers <- as.integer(sub('^<row [^>]*?\\br="([0-9]+)".*$', "\\1", rows, perl = TRUE))
  list(
    head = substr(xml, 1L, start + nchar("<sheetData>") - 1L),
    tail = substr(xml, end, nchar(xml)),
    rows = setNames(as.list(rows), numbers)
  )
}
row_parts <- function(row) {
  if (grepl("/>$", row) && !grepl("</row>$", row)) {
    return(list(open = sub("/>$", ">", row), cells = character()))
  }
  open <- regmatches(row, regexpr("^<row [^>]*>", row))
  inner <- substr(row, nchar(open) + 1L, nchar(row) - nchar("</row>"))
  pattern <- "(?s)<c [^>]*?/>|<c [^>]*>.*?</c>"
  cells <- regmatches(inner, gregexpr(pattern, inner, perl = TRUE))[[1]]
  if (!identical(paste(cells, collapse = ""), inner)) {
    stop("Unexpected content in a row: ", open, call. = FALSE)
  }
  if (!all(startsWith(cells, '<c r="'))) {
    stop("A cell whose r attribute isn't first: ", open, call. = FALSE)
  }
  list(open = open, cells = cells)
}
cell_col <- function(cell) {
  col_index(sub('^<c [^>]*?\\br="([A-Z]+)[0-9]+".*$', "\\1", cell, perl = TRUE))
}
cell_style <- function(cell) {
  if (grepl('\\bs="[0-9]+"', cell)) {
    sub('^<c [^>]*?\\bs="([0-9]+)".*$', "\\1", cell, perl = TRUE)
  } else {
    NA_character_
  }
}
renumber <- function(parts, new_row) {
  parts$open <- sub('\\br="[0-9]+"', sprintf('r="%d"', new_row), parts$open)
  parts$cells <- sub('^<c r="([A-Z]+)[0-9]+"', sprintf('<c r="\\1%d"', new_row), parts$cells)
  parts
}
build_cell <- function(ref, style, type, value) {
  s_attr <- if (is.na(style)) "" else sprintf(' s="%s"', style)
  if (is.na(value)) {
    return(if (is.na(style)) NA_character_ else sprintf('<c r="%s"%s/>', ref, s_attr))
  }
  switch(type,
    num = sprintf(
      '<c r="%s"%s><v>%s</v></c>', ref, s_attr,
      format(as.numeric(value), scientific = FALSE, digits = 15, trim = TRUE)
    ),
    bool = sprintf(
      '<c r="%s"%s t="b"><v>%d</v></c>', ref, s_attr, as.integer(as.logical(value))
    ),
    sprintf(
      '<c r="%s"%s t="inlineStr"><is><t xml:space="preserve">%s</t></is></c>',
      ref, s_attr, xml_escape(value)
    )
  )
}
put_cell <- function(parts, col, row, type, value, fallback_style = NA_character_) {
  cols <- vapply(parts$cells, cell_col, 0L)
  k <- which(cols == col)
  style <- if (length(k)) cell_style(parts$cells[k]) else fallback_style
  new <- build_cell(paste0(col_letters(col), row), style, type, value)
  keep <- if (length(k)) parts$cells[-k] else parts$cells
  keep_cols <- if (length(k)) cols[-k] else cols
  if (!is.na(new)) {
    keep <- c(keep, new)
    keep_cols <- c(keep_cols, col)
  }
  parts$cells <- keep[order(keep_cols)]
  parts
}
row_xml <- function(parts) {
  cols <- vapply(parts$cells, cell_col, 0L)
  open <- parts$open
  if (length(cols) && grepl('\\bspans="', open)) {
    open <- sub(
      '\\bspans="[0-9]+:[0-9]+"', sprintf('spans="%d:%d"', min(cols), max(cols)), open
    )
  }
  if (length(parts$cells) == 0L) {
    return(sub(">$", "/>", open))
  }
  paste0(open, paste(parts$cells, collapse = ""), "</row>")
}

# ---- one workbook ----
sheet_paths <- function(d) {
  wbx <- read_raw_text(file.path(d, "xl", "workbook.xml"))
  rels <- read_raw_text(file.path(d, "xl", "_rels", "workbook.xml.rels"))
  sheets <- regmatches(wbx, gregexpr("<sheet [^>]*>", wbx))[[1]]
  out <- character()
  for (sh in sheets) {
    name <- sub('.*\\bname="([^"]*)".*', "\\1", sh, perl = TRUE)
    id <- sub('.*\\br:id="([^"]*)".*', "\\1", sh, perl = TRUE)
    rel <- regmatches(rels, regexpr(paste0('<Relationship [^>]*\\bId="', id, '"[^>]*>'), rels))
    target <- sub('.*\\bTarget="([^"]*)".*', "\\1", rel, perl = TRUE)
    out[[name]] <- if (startsWith(target, "/")) sub("^/", "", target) else file.path("xl", target)
  }
  out
}

for (kind in names(workbooks)) {
  src <- file.path(baseline_dir, workbooks[[kind]])
  target <- file.path(output_dir, new_name(workbooks[[kind]]))
  d <- tempfile("pkg")
  entries <- utils::unzip(src, list = TRUE)$Name
  utils::unzip(src, exdir = d)
  paths <- sheet_paths(d)
  file_edits <- Filter(function(e) e$file == kind, edits)
  for (sheet in unique(vapply(file_edits, `[[`, "", "sheet"))) {
    sheet_edits <- Filter(function(e) e$sheet == sheet, file_edits)
    m <- read_text(src, sheet)
    for (e in sheet_edits) {
      check_guards(m, e$guard, paste(kind, sheet, e$op, e$ref))
    }
    cell_edits <- Filter(function(e) e$op %in% c("set", "append", "sub"), sheet_edits)
    row_ops <- Filter(function(e) e$op %in% c("delete", "insert", "move"), sheet_edits)
    xml_path <- file.path(d, paths[[sheet]])
    sh <- split_sheet(read_raw_text(xml_path))
    xml_rows <- as.integer(names(sh$rows))
    n_rows <- max(nrow(m), xml_rows)
    width <- max(ncol(m), vapply(cell_edits, function(e) parse_ref(e$ref)$col, 0L), 0L)
    base <- matrix(NA_character_, n_rows, width)
    if (length(m)) {
      base[seq_len(nrow(m)), seq_len(ncol(m))] <- m
    }
    cur <- base
    cell_type <- list()
    for (e in cell_edits) {
      p <- parse_ref(e$ref)
      cur[p$row, p$col] <- apply_text(cur[p$row, p$col], e)
      cell_type[[e$ref]] <- if (e$op == "set" && e$type %in% c("num", "bool")) e$type else "text"
    }
    # final row order: original rows, deletions dropped, moves and inserts after their anchor
    deleted <- unlist(lapply(Filter(function(e) e$op == "delete", row_ops), `[[`, "row"))
    final_rows <- as.list(setdiff(seq_len(n_rows), deleted))
    position_after <- function(anchor) {
      if (anchor == 0L) {
        return(0L)
      }
      k <- which(vapply(final_rows, function(o) is.numeric(o) && o == anchor, TRUE))
      if (length(k) != 1L) {
        stop("Anchor row ", anchor, " not found once in ", sheet, call. = FALSE)
      }
      k
    }
    for (e in Filter(function(e) e$op == "move", row_ops)) {
      final_rows <- final_rows[!vapply(final_rows, function(o) is.numeric(o) && o == e$row, TRUE)]
      final_rows <- append(final_rows, list(e$row), after = position_after(e$after))
    }
    for (e in Filter(function(e) e$op == "insert", row_ops)) {
      clone <- if (!is.na(e$clone)) {
        e$clone
      } else if ((e$after + 1L) %in% xml_rows) {
        e$after + 1L
      } else {
        e$after
      }
      final_rows <- append(final_rows, list(list(values = e$values, clone = clone)),
        after = position_after(e$after)
      )
    }
    new_rows <- character()
    last_row <- 0L
    max_col <- 0L
    data_last <- 0L
    for (i in seq_along(final_rows)) {
      o <- final_rows[[i]]
      has_data <- if (is.numeric(o)) any(!is.na(cur[o, ])) else TRUE
      if (has_data) {
        data_last <- i
      }
      if (is.numeric(o)) {
        if (!(o %in% xml_rows)) {
          next
        }
        parts <- renumber(row_parts(sh$rows[[as.character(o)]]), i)
        for (j in seq_len(width)) {
          if (identical(cur[o, j], base[o, j])) {
            next
          }
          ref <- paste0(col_letters(j), o)
          type <- if (is.null(cell_type[[ref]])) "text" else cell_type[[ref]]
          left <- vapply(parts$cells, cell_col, 0L)
          fallback <- if (any(left < j)) {
            cell_style(parts$cells[max(which(left < j))])
          } else {
            NA_character_
          }
          parts <- put_cell(parts, j, i, type, cur[o, j], fallback)
        }
      } else {
        parts <- renumber(row_parts(sh$rows[[as.character(o$clone)]]), i)
        for (k in seq_along(parts$cells)) {
          parts$cells[k] <- build_cell(
            sub('^<c r="([A-Z]+[0-9]+)".*$', "\\1", parts$cells[k]),
            cell_style(parts$cells[k]), "text", NA_character_
          )
        }
        parts$cells <- parts$cells[!is.na(parts$cells)]
        for (l in names(o$values)) {
          parts <- put_cell(parts, col_index(l), i, "text", o$values[[l]])
        }
      }
      new_rows <- c(new_rows, row_xml(parts))
      if (length(parts$cells)) {
        last_row <- i
        max_col <- max(max_col, vapply(parts$cells, cell_col, 0L))
      }
    }
    sheet_head <- sh$head
    dim_ref <- sprintf("A1:%s%d", col_letters(max_col), last_row)
    sheet_head <- sub(
      '<dimension ref="[^"]*"/>', sprintf('<dimension ref="%s"/>', dim_ref), sheet_head
    )
    sheet_tail <- sh$tail
    if (length(row_ops) && grepl("<autoFilter ", sheet_tail)) {
      old_ref <- sub('.*<autoFilter ref="([^"]*)".*', "\\1", sheet_tail)
      new_ref <- sub("[0-9]+$", as.character(data_last), old_ref)
      sheet_tail <- sub(
        sprintf('<autoFilter ref="%s"', old_ref), sprintf('<autoFilter ref="%s"', new_ref),
        sheet_tail,
        fixed = TRUE
      )
      wb_path <- file.path(d, "xl", "workbook.xml")
      wbx <- read_raw_text(wb_path)
      p <- parse_ref(sub(".*:", "", old_ref))
      old_name <- sprintf("$%s$%d</definedName>", col_letters(p$col), p$row)
      if (sum(gregexpr(old_name, wbx, fixed = TRUE)[[1]] > 0L) != 1L) {
        stop("Filter name not found once", call. = FALSE)
      }
      wbx <- sub(old_name, sprintf("$%s$%d</definedName>", col_letters(p$col), data_last), wbx,
        fixed = TRUE
      )
      write_raw_text(wbx, wb_path)
      cat(sprintf("  %s!%s: filter %s -> %s\n", kind, sheet, old_ref, new_ref))
    }
    write_raw_text(paste0(sheet_head, paste(new_rows, collapse = ""), sheet_tail), xml_path)
    cat(sprintf("  %s!%s: %s patched, dimension %s\n", kind, sheet, paths[[sheet]], dim_ref))
  }
  zip::zip(target, files = entries, root = d, mode = "mirror")
  cat("Wrote", target, file.size(target), "bytes\n")
}

# ---- CSV copies ----
for (name in names(csv_sources)) {
  src <- csv_source_path(name, baseline_dir, translation_dir)
  target <- file.path(output_dir, name)
  x <- readBin(src, "raw", file.info(src)$size)
  y <- if (validUTF8(rawToChar(x))) {
    x
  } else {
    iconv(list(x), from = "CP1252", to = "UTF-8", toRaw = TRUE)[[1]]
  }
  if (is.null(y)) {
    stop("Conversion failed: ", src, call. = FALSE)
  }
  if (name == "20261005_magpv2_species.csv") {
    text <- rawToChar(y)
    Encoding(text) <- "UTF-8"
    if (!endsWith(text, "\r\n")) {
      stop("The species table doesn't end with CRLF", call. = FALSE)
    }
    lines <- strsplit(text, "\r\n", fixed = TRUE)[[1]]
    if (any(grepl("[\r\n]", lines))) {
      stop("A bare CR or LF in the species table", call. = FALSE)
    }
    sp <- data.table::fread(
      text = text, sep = ",", header = TRUE, colClasses = "character",
      na.strings = "", strip.white = FALSE
    )
    if (length(lines) != nrow(sp) + 1L) {
      stop("Species lines and rows differ", call. = FALSE)
    }
    found <- which(sp$NFI %in% names(fold_notes)) + 1L
    if (!identical(found, fold_lines)) {
      stop("Fold lines differ: ", paste(found, collapse = " "), call. = FALSE)
    }
    # The header gets the new column's name, a fold row its note, every other row nothing.
    notes <- rep("", length(lines))
    notes[1L] <- "comments"
    notes[fold_lines] <- unname(fold_notes[sp$NFI[fold_lines - 1L]])
    lines <- paste0(lines, ",", notes)
    y <- charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n")))
  }
  writeBin(y, target)
  cat(
    "Wrote", target, length(y), "bytes from", length(x),
    if (identical(x, y)) "(copied as is)", "\n"
  )
}
