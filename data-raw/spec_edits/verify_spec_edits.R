# Checks the 20261005 specification files against the 20260925 ones and the edit list
# (edits_20261005.R), independently of patch_workbooks.R (D12.72, the added validation):
#   1. every sheet, every cell, typed (text, number, logical, date): each new workbook must
#      equal the old one with the edits applied, computed here by its own method (row order by
#      sort keys); any other difference, or a missing one, fails;
#   2. sheet names and order identical; only the edited sheets' XML (and, where rows move under
#      a filter, xl/workbook.xml) may differ between the packages; every relationship and
#      content type resolves; the changed XML is well-formed; the workbook parts patching may
#      lose (validations, conditional formats, merges, comments, defined names, panes, widths)
#      are counted and compared;
#   3. each CSV: valid UTF-8, no BOM, CRLF kept, the same text as its source decoded (the
#      species copy: each line plus its `comments` field; a copy with cell edits, the
#      treatment/disturbance and datasets copies: each line but the edited cells' lines, and
#      those cells from -> to), and fread() reading it without a warning.
#
# Usage: Rscript verify_spec_edits.R <baseline_dir> <translation_dir> <check_dir>
#   baseline_dir     the 20260925 DD, Lookup_Tables and A2 workbooks and datasets CSV
#   translation_dir  the four translation tables under their master names (only read)
#   check_dir        the folder holding the eight 20261005 files to check (the tree's spec/,
#                    or patch_workbooks.R's output with edit_csv_cell.R's copy put in place)
# Prints PASS or FAIL and exits with status 1 on any failure. Writes nothing but temporary
# files. Needs readxl and data.table; xml2 (this script only; never the package) for the
# well-formedness check, which is skipped with a note where it is missing.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  cat("Usage: Rscript verify_spec_edits.R <baseline_dir> <translation_dir> <check_dir>\n")
  quit(save = "no", status = 1L)
}
baseline_dir <- args[[1L]]
translation_dir <- args[[2L]]
folder <- args[[3L]]
script_dir <- dirname(normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]])
))
source(file.path(script_dir, "edits_20261005.R"))

problems <- new.env()
problems$found <- character()
fail <- function(...) {
  msg <- paste0(...)
  problems$found <- c(problems$found, msg)
  cat("FAIL:", msg, "\n")
}

letters_of <- function(j) {
  s <- ""
  while (j > 0) {
    r <- (j - 1) %% 26
    s <- paste0(LETTERS[r + 1], s)
    j <- (j - 1) %/% 26
  }
  s
}
cell_pos <- function(ref) {
  cell_letters <- strsplit(sub("[0-9]+$", "", ref), "")[[1]]
  j <- 0L
  for (ch in cell_letters) {
    j <- j * 26L + match(ch, LETTERS)
  }
  c(row = as.integer(sub("^[A-Z]+", "", ref)), col = j)
}
typed <- function(e) {
  if (length(e) == 0L || (length(e) == 1L && is.na(e))) {
    return(NA_character_)
  }
  if (inherits(e, "POSIXct")) {
    return(paste0("dat:", format(e, "%Y-%m-%d %H:%M:%S")))
  }
  if (is.logical(e)) {
    return(paste0("lgl:", e))
  }
  if (is.numeric(e)) {
    return(paste0("num:", format(e, digits = 15)))
  }
  paste0("chr:", e)
}
read_typed <- function(path, sheet) {
  x <- suppressMessages(readxl::read_excel(path,
    sheet = sheet, col_names = FALSE,
    col_types = "list", trim_ws = FALSE, .name_repair = "minimal",
    range = readxl::cell_limits(c(1, 1), c(NA, NA))
  ))
  if (ncol(x) == 0L) {
    return(matrix(NA_character_, 0L, 0L))
  }
  m <- matrix(NA_character_, nrow(x), ncol(x))
  for (j in seq_len(ncol(x))) {
    m[, j] <- vapply(x[[j]], typed, "")
  }
  m
}
pad <- function(m, nr, nc) {
  out <- matrix(NA_character_, nr, nc)
  if (length(m)) {
    out[seq_len(nrow(m)), seq_len(ncol(m))] <- m
  }
  out
}
expected_value <- function(old, e) {
  if (e$op == "set") {
    return(switch(e$type,
      text = paste0("chr:", e$value),
      num = paste0("num:", format(as.numeric(e$value), digits = 15)),
      bool = paste0("lgl:", as.logical(e$value)),
      blank = NA_character_
    ))
  }
  if (is.na(old) || !startsWith(old, "chr:")) {
    return(paste0("ERROR: ", e$op, " on non-text ", old))
  }
  text <- substring(old, 5L)
  if (e$op == "append") {
    return(paste0("chr:", text, e$value))
  }
  if (sum(gregexpr(e$from, text, fixed = TRUE)[[1]] > 0L) != 1L) {
    return("ERROR: sub not found once")
  }
  paste0("chr:", sub(e$from, e$value, text, fixed = TRUE))
}
same_cells <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}
slurp_raw <- function(f) readBin(f, "raw", file.info(f)$size)
tag_counts <- function(dir) {
  tags <- c(
    "<dataValidation ", "<conditionalFormatting", "<mergeCell ", "<hyperlink ",
    "<legacyDrawing", "<drawing ", "<tablePart ", "<autoFilter", "<pane ", "<col ",
    "<sheetProtection", "<definedName "
  )
  files <- list.files(dir, recursive = TRUE, pattern = "\\.xml$", full.names = TRUE)
  files <- files[grepl("worksheets/sheet|workbook\\.xml$", files)]
  text <- vapply(files, function(f) {
    paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "")
  }, "")
  vapply(
    tags, function(tag) sum(lengths(regmatches(text, gregexpr(tag, text, fixed = TRUE)))), 0L
  )
}

cat("Folder:", folder, "\n")
report <- character()
for (kind in names(workbooks)) {
  old_path <- file.path(baseline_dir, workbooks[[kind]])
  new_path <- file.path(folder, new_name(workbooks[[kind]]))
  cat("\n=== ", basename(new_path), "\n", sep = "")
  if (!file.exists(new_path)) {
    fail(basename(new_path), " missing")
    next
  }
  old_sheets <- readxl::excel_sheets(old_path)
  new_sheets <- readxl::excel_sheets(new_path)
  if (!identical(old_sheets, new_sheets)) {
    fail(kind, ": sheet names or order differ")
  }
  file_edits <- Filter(function(e) e$file == kind, edits)
  for (sheet in old_sheets) {
    old <- read_typed(old_path, sheet)
    new <- read_typed(new_path, sheet)
    sheet_edits <- Filter(function(e) e$sheet == sheet, file_edits)
    # guards, on the old file
    for (e in sheet_edits) {
      for (ref in names(e$guard)) {
        p <- cell_pos(ref)
        got <- if (p[["row"]] <= nrow(old) && p[["col"]] <= ncol(old)) {
          old[p[["row"]], p[["col"]]]
        } else {
          NA
        }
        want <- e$guard[[ref]]
        ok <- if (is.na(want)) is.na(got) else !is.na(got) && sub("^[a-z]{3}:", "", got) == want
        if (!ok) {
          fail(kind, "!", sheet, " guard ", ref, ": [", got, "] not [", want, "]")
        }
      }
    }
    # expected: cell edits in old coordinates, then rows ordered by sort keys
    edit_cols <- unlist(lapply(sheet_edits, function(e) {
      c(
        if (!is.na(e$ref)) cell_pos(e$ref)[["col"]],
        if (!is.null(e$values)) {
          vapply(names(e$values), function(l) cell_pos(paste0(l, 1))[["col"]], 0L)
        }
      )
    }))
    width <- max(ncol(old), edit_cols, 0L)
    expected <- pad(old, nrow(old), width)
    for (e in Filter(function(e) e$op %in% c("set", "append", "sub"), sheet_edits)) {
      p <- cell_pos(e$ref)
      if (p[["row"]] > nrow(expected)) {
        expected <- pad(expected, p[["row"]], width)
      }
      expected[p[["row"]], p[["col"]]] <- expected_value(expected[p[["row"]], p[["col"]]], e)
    }
    keys <- as.numeric(seq_len(nrow(expected)))
    for (e in Filter(function(e) e$op == "move", sheet_edits)) {
      keys[e$row] <- e$after + 0.6
    }
    for (e in Filter(function(e) e$op == "delete", sheet_edits)) {
      keys[e$row] <- NA
    }
    for (e in Filter(function(e) e$op == "insert", sheet_edits)) {
      row <- rep(NA_character_, width)
      for (l in names(e$values)) {
        row[cell_pos(paste0(l, 1))[["col"]]] <- paste0("chr:", e$values[[l]])
      }
      expected <- rbind(expected, row)
      keys <- c(keys, e$after + 0.5)
    }
    keep <- !is.na(keys)
    expected <- expected[keep, , drop = FALSE][order(keys[keep]), , drop = FALSE]
    # trailing all-blank rows and columns don't exist in a sheet
    while (nrow(expected) > 0L && all(is.na(expected[nrow(expected), ]))) {
      expected <- expected[-nrow(expected), , drop = FALSE]
    }
    nr <- max(nrow(expected), nrow(new), nrow(old))
    nc <- max(ncol(expected), ncol(new), ncol(old))
    expected <- pad(expected, nr, nc)
    actual <- pad(new, nr, nc)
    old_padded <- pad(old, nr, nc)
    bad <- which(!same_cells(expected, actual), arr.ind = TRUE)
    for (k in seq_len(nrow(bad))) {
      i <- bad[k, 1]
      j <- bad[k, 2]
      fail(
        kind, "!", sheet, " ", letters_of(j), i, ": expected [", expected[i, j], "] got [",
        actual[i, j], "]"
      )
    }
    changed <- which(!same_cells(old_padded, actual), arr.ind = TRUE)
    if (nrow(changed) > 0L) {
      cat(sprintf(
        "  %s: %d cells differ from 20260925 (all as expected: %s)\n", sheet, nrow(changed),
        nrow(bad) == 0L
      ))
      for (k in seq_len(nrow(changed))) {
        i <- changed[k, 1]
        j <- changed[k, 2]
        report <- c(report, sprintf(
          "%s\t%s\t%s%d\t%s\t%s", kind, sheet, letters_of(j), i, old_padded[i, j], actual[i, j]
        ))
      }
    }
  }
  # workbook parts: same entries in the same order; only the edited sheets' XML (and, where
  # rows move under a filter, xl/workbook.xml) may differ; every reference resolves; changed
  # XML parses
  td_old <- tempfile("old")
  td_new <- tempfile("new")
  utils::unzip(old_path, exdir = td_old)
  utils::unzip(new_path, exdir = td_new)
  parts_old <- utils::unzip(old_path, list = TRUE)$Name
  parts_new <- utils::unzip(new_path, list = TRUE)$Name
  if (!identical(parts_old, parts_new)) {
    fail(kind, ": package entries or their order differ")
  }
  changed_parts <- parts_old[!vapply(parts_old, function(p) {
    file.exists(file.path(td_new, p)) &&
      identical(slurp_raw(file.path(td_old, p)), slurp_raw(file.path(td_new, p)))
  }, TRUE)]
  wbx <- rawToChar(slurp_raw(file.path(td_old, "xl", "workbook.xml")))
  rels <- rawToChar(slurp_raw(file.path(td_old, "xl", "_rels", "workbook.xml.rels")))
  edited_sheets <- unique(vapply(file_edits, `[[`, "", "sheet"))
  allowed <- vapply(edited_sheets, function(sh) {
    tag <- regmatches(wbx, regexpr(sprintf('<sheet [^>]*name="%s"[^>]*>', sh), wbx))
    id <- sub('.*r:id="([^"]*)".*', "\\1", tag)
    rel <- regmatches(rels, regexpr(paste0('<Relationship [^>]*Id="', id, '"[^>]*>'), rels))
    file.path("xl", sub('.*Target="([^"]*)".*', "\\1", rel))
  }, "")
  rows_move <- any(vapply(file_edits, function(e) e$op %in% c("insert", "delete", "move"), TRUE))
  if (rows_move && grepl("_FilterDatabase", wbx)) {
    allowed <- c(allowed, "xl/workbook.xml")
  }
  extra <- setdiff(changed_parts, allowed)
  if (length(extra)) {
    fail(kind, ": parts changed that shouldn't: ", paste(extra, collapse = ", "))
  }
  cat(
    "  parts:", length(parts_old), "entries, same order:", identical(parts_old, parts_new),
    "; changed:", paste(changed_parts, collapse = ", "), "\n"
  )
  content_types <- rawToChar(slurp_raw(file.path(td_new, "[Content_Types].xml")))
  overrides <- regmatches(
    content_types, gregexpr('(?<=PartName=")[^"]+', content_types, perl = TRUE)
  )[[1]]
  missing_parts <- setdiff(sub("^/", "", overrides), parts_new)
  if (length(missing_parts)) {
    fail(kind, ": content types name missing parts: ", paste(missing_parts, collapse = ", "))
  }
  for (r in parts_new[grepl("_rels/[^/]*\\.rels$", parts_new)]) {
    x <- rawToChar(slurp_raw(file.path(td_new, r)))
    rel_tags <- regmatches(x, gregexpr("<Relationship [^>]*>", x))[[1]]
    for (tag in rel_tags[!grepl('TargetMode="External"', rel_tags)]) {
      target <- sub('.*Target="([^"]*)".*', "\\1", tag)
      owner_dir <- dirname(dirname(r))
      full <- if (startsWith(target, "/")) {
        sub("^/", "", target)
      } else {
        from_root <- file.path(if (owner_dir == ".") "" else owner_dir, target)
        pieces <- strsplit(from_root, "/", fixed = TRUE)[[1]]
        out <- character()
        for (p in pieces[nzchar(pieces)]) {
          if (p == "..") {
            out <- head(out, -1L)
          } else if (p != ".") {
            out <- c(out, p)
          }
        }
        paste(out, collapse = "/")
      }
      if (!(full %in% parts_new)) {
        fail(kind, ": ", r, " points to a missing part: ", target)
      }
    }
  }
  if (requireNamespace("xml2", quietly = TRUE)) {
    for (p in changed_parts) {
      well_formed <- tryCatch(
        {
          xml2::read_xml(file.path(td_new, p))
          TRUE
        },
        error = function(e) FALSE
      )
      if (!well_formed) {
        fail(kind, ": ", p, " isn't well-formed XML")
      }
    }
  } else {
    cat("  (xml2 not installed: well-formedness checked by readxl's read only)\n")
  }
  before <- tag_counts(td_old)
  after <- tag_counts(td_new)
  differing <- names(before)[before != after]
  cat(
    "  parts:", length(parts_old), "->", length(parts_new), "; tag counts old/new:",
    paste0(sub("^<", "", trimws(names(before))), "=", before, "/", after, collapse = " "), "\n"
  )
  if (length(differing)) {
    cat("  NOTE: tag counts differ for", paste(differing, collapse = ", "), "\n")
  }
  if (sum(grepl("comments", parts_old)) != sum(grepl("comments", parts_new))) {
    fail(kind, ": cell comments differ")
  }
}

cat("\n=== CSV copies\n")
read_checked <- function(...) {
  data.table::fread(
    ...,
    sep = ",", header = TRUE, colClasses = "character", na.strings = "",
    strip.white = FALSE
  )
}
crlf_count <- function(text) lengths(regmatches(text, gregexpr("\r\n", text, fixed = TRUE)))
for (name in names(csv_sources)) {
  src <- csv_source_path(name, baseline_dir, translation_dir)
  path <- file.path(folder, name)
  if (!file.exists(path)) {
    fail(name, " missing")
    next
  }
  x <- slurp_raw(src)
  y <- slurp_raw(path)
  src_text <- if (validUTF8(rawToChar(x))) {
    rawToChar(x)
  } else {
    iconv(rawToChar(x), from = "CP1252", to = "UTF-8")
  }
  Encoding(src_text) <- "UTF-8"
  new_text <- rawToChar(y)
  Encoding(new_text) <- "UTF-8"
  if (!validUTF8(new_text)) {
    fail(name, ": not valid UTF-8")
  }
  if (length(y) >= 3L && identical(y[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    fail(name, ": has a BOM")
  }
  if (crlf_count(src_text) != crlf_count(new_text)) {
    fail(name, ": CRLF count differs")
  }
  if (name == "20261005_magpv2_species.csv") {
    # The expected lines below add each note as a plain field, so a note that needs CSV
    # quoting is a failure of the list, as patch_workbooks.R stops on it.
    needs_quotes <- grepl('[,"\r\n]', fold_notes)
    if (any(needs_quotes)) {
      fail(
        name, ": a fold note holds a comma, a quote or a line break: ",
        paste(names(fold_notes)[needs_quotes], collapse = ", ")
      )
    }
    a <- strsplit(src_text, "\r\n", fixed = TRUE)[[1]]
    b <- strsplit(new_text, "\r\n", fixed = TRUE)[[1]]
    notes <- rep("", length(a))
    notes[1L] <- "comments"
    sp <- read_checked(text = src_text)
    for (code in names(fold_notes)) {
      notes[which(sp$NFI == code) + 1L] <- fold_notes[[code]]
    }
    if (!identical(which(nzchar(notes))[-1L], fold_lines)) {
      fail(name, ": fold lines differ")
    }
    if (!identical(b, paste0(a, ",", notes))) {
      fail(name, ": lines aren't the source plus the comments field")
    }
  } else if (name %in% vapply(csv_cell_edits, `[[`, "", "file")) {
    # Every line as its source except the lines of the edited cells (several edits may share a
    # file and a line, and a blank cell is NA); the parsed tables differ in exactly the edited
    # cells, from -> to.
    ce <- Filter(function(e) e$file == name, csv_cell_edits)
    a <- strsplit(src_text, "\r\n", fixed = TRUE)[[1]]
    b <- strsplit(new_text, "\r\n", fixed = TRUE)[[1]]
    lines <- vapply(ce, `[[`, 0L, "line")
    if (length(a) != length(b) || !identical(a[-lines], b[-lines])) {
      fail(name, ": lines other than the edited ones differ")
    }
    ta <- read_checked(text = src_text)
    tb <- read_checked(text = new_text)
    if (!identical(dim(ta), dim(tb)) || !identical(names(ta), names(tb))) {
      fail(name, ": shape or header changed")
    }
    differ <- which(!same_cells(as.matrix(ta), as.matrix(tb)), arr.ind = TRUE)
    want <- t(vapply(
      ce, function(e) c(which(ta[[e$key_col]] == e$key), match(e$col, names(ta))), c(0, 0)
    ))
    differ <- differ[order(differ[, 1], differ[, 2]), , drop = FALSE]
    want <- want[order(want[, 1], want[, 2]), , drop = FALSE]
    if (nrow(differ) != nrow(want) || any(differ != want)) {
      fail(name, ": the cells that differ aren't the edited ones")
    }
    for (e in ce) {
      i <- which(ta[[e$key_col]] == e$key)
      right_cell <- length(i) == 1L && i + 1L == e$line &&
        identical(ta[[e$col]][i], e$from) && identical(tb[[e$col]][i], e$to)
      if (!right_cell) {
        # A blank cell (NA) shows as "(blank)", not "NA".
        from_text <- if (is.na(e$from)) "(blank)" else e$from
        to_text <- if (is.na(e$to)) "(blank)" else e$to
        fail(
          name, ": ", e$key, "'s ", e$col, " isn't ", from_text, " -> ", to_text, " on line ",
          e$line
        )
      }
    }
  } else if (!identical(src_text, new_text)) {
    fail(name, ": text differs from its source")
  }
  warnings_seen <- NULL
  d <- withCallingHandlers(
    read_checked(file = path),
    warning = function(cond) {
      warnings_seen <<- c(warnings_seen, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  if (length(warnings_seen)) {
    fail(name, ": fread warns: ", paste(warnings_seen, collapse = " | "))
  }
  cat(sprintf(
    "  %s: %d bytes (source %d), %d rows x %d columns, %s\n", name, length(y), length(x),
    nrow(d), ncol(d),
    if (identical(x, y)) "byte-identical to its source" else "converted or extended"
  ))
}

cat("\n=== Changed cells (file, sheet, cell, old, new):", length(report), "\n")
cat(report, sep = "\n")
failures <- problems$found
result <- if (length(failures)) paste("FAIL,", length(failures), "problems") else "PASS"
cat("\n=== RESULT:", result, "\n")
if (length(failures)) {
  quit(save = "no", status = 1L)
}
