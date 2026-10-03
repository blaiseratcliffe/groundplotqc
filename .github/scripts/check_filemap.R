# FILEMAP check (plan 20.6; D5.15, D7.10, D11.8). Compares the files git tracks
# with the paths in FILEMAP.md's tables and fails on either side's extras.
# Run from the repo root: Rscript .github/scripts/check_filemap.R

tracked_files <- function(dir = ".") {
  out <- system2(
    "git", c("-C", shQuote(dir), "-c", "core.quotepath=off", "ls-files"),
    stdout = TRUE
  )
  if (!is.null(attr(out, "status"))) {
    stop("git ls-files failed in ", dir, call. = FALSE)
  }
  Encoding(out) <- "UTF-8"
  out
}

outside_fences <- function(lines) {
  fence <- grepl("^\\s*(```|~~~)", lines)
  inside <- cumsum(fence) %% 2L == 1L
  lines[!fence & !inside]
}

filemap_section <- function(lines, heading) {
  start <- match(heading, trimws(lines))
  if (is.na(start)) {
    return(character())
  }
  rest <- lines[-seq_len(start)]
  end <- match(TRUE, grepl("^#{1,2} ", rest))
  if (is.na(end)) rest else rest[seq_len(end - 1L)]
}

filemap_paths <- function(lines) {
  rows <- grep("^\\s*\\|", outside_fences(lines), value = TRUE)
  first <- vapply(
    strsplit(rows, "|", fixed = TRUE),
    function(cells) trimws(cells[[2L]]),
    character(1)
  )
  first <- gsub("`", "", first, fixed = TRUE)
  first[first != "Path" & !grepl("^:?-+:?$", first)]
}

covered_folders <- function(lines) {
  section <- outside_fences(filemap_section(lines, "## Folders covered by one row"))
  found <- regmatches(section, regexec("^\\s*[-*]\\s+`([^`]+/)`", section))
  vapply(Filter(function(m) length(m) == 2L, found), function(m) m[[2L]], character(1))
}

filemap_problems <- function(tracked, paths, covered) {
  folder_rows <- paths[endsWith(paths, "/")]
  file_rows <- setdiff(paths, folder_rows)
  folders <- intersect(folder_rows, covered)
  in_folder <- vapply(tracked, function(f) any(startsWith(f, folders)), logical(1))
  empty <- folders[!vapply(folders, function(d) any(startsWith(tracked, d)), logical(1))]
  c(
    sprintf("tracked but not in FILEMAP.md: %s", setdiff(tracked[!in_folder], file_rows)),
    sprintf("in FILEMAP.md but not tracked: %s", setdiff(file_rows, tracked)),
    sprintf(
      "folder row not listed under 'Folders covered by one row': %s",
      setdiff(folder_rows, covered)
    ),
    sprintf("folder row with no tracked file under it: %s", empty)
  )
}

check_filemap_lines <- function(tracked, lines) {
  filemap_problems(tracked, filemap_paths(lines), covered_folders(lines))
}

main <- function() {
  tracked <- tracked_files()
  lines <- readLines("FILEMAP.md", encoding = "UTF-8", warn = FALSE)
  problems <- check_filemap_lines(tracked, lines)
  if (length(problems) > 0L) {
    cat(problems, sep = "\n")
    quit(save = "no", status = 1L)
  }
  cat(sprintf("FILEMAP.md covers all %d tracked files.\n", length(tracked)))
}

if (sys.nframe() == 0L) {
  main()
}
