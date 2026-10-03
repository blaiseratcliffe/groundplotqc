# Tests for the scripts in .github/scripts/ (D11.8). They run from the source
# tree and skip in a built package, which leaves .github out (plan 19.3).

load_dev_script <- function(name) {
  path <- testthat::test_path("..", "..", ".github", "scripts", name)
  if (!file.exists(path)) {
    testthat::skip(paste(name, "is not in this tree"))
  }
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

# A file name with an accented letter, built from its code point so this file
# stays ASCII.
accented_path <- paste0("R/donn", intToUtf8(233L), "es.R")

filemap_fixture <- c(
  "# FILEMAP",
  "",
  "## Folders covered by one row",
  "",
  "- `spec/`: its per-file list is elsewhere.",
  "",
  "## Root",
  "",
  "| Path | Purpose | Key functions (exported; internal) | Depends on |",
  "|---|---|---|---|",
  "| DESCRIPTION | Package metadata | none | none |",
  "| `R/utils_strings.R` | String helpers | none | none |",
  paste0("| ", accented_path, " | A file with an accented name | none | none |"),
  "",
  "## spec/",
  "",
  "| Path | Purpose | Key functions (exported; internal) | Depends on |",
  "|---|---|---|---|",
  "| spec/ | Specification files | none | none |",
  "",
  "An example table, not a FILEMAP row:",
  "",
  "```markdown",
  "| R/inside_a_fence.R | not a row | none | none |",
  "```"
)

filemap_tracked <- c(
  "DESCRIPTION", "R/utils_strings.R", accented_path, "spec/a.xlsx", "spec/b.csv"
)

test_that("filemap_paths reads table rows outside code fences only", {
  fm <- load_dev_script("check_filemap.R")
  expect_equal(
    fm$filemap_paths(filemap_fixture),
    c("DESCRIPTION", "R/utils_strings.R", accented_path, "spec/")
  )
})

test_that("covered_folders reads the folders covered by one row", {
  fm <- load_dev_script("check_filemap.R")
  expect_equal(fm$covered_folders(filemap_fixture), "spec/")
})

test_that("a FILEMAP that matches the tracked files has no problems", {
  fm <- load_dev_script("check_filemap.R")
  expect_equal(fm$check_filemap_lines(filemap_tracked, filemap_fixture), character())
})

test_that("a tracked file without a row is reported", {
  fm <- load_dev_script("check_filemap.R")
  expect_equal(
    fm$check_filemap_lines(c(filemap_tracked, "NEWS.md"), filemap_fixture),
    "tracked but not in FILEMAP.md: NEWS.md"
  )
})

test_that("a row without a tracked file is reported", {
  fm <- load_dev_script("check_filemap.R")
  expect_equal(
    fm$check_filemap_lines(setdiff(filemap_tracked, "DESCRIPTION"), filemap_fixture),
    "in FILEMAP.md but not tracked: DESCRIPTION"
  )
})

test_that("a folder row counts only when listed as covered by one row", {
  fm <- load_dev_script("check_filemap.R")
  unlisted <- filemap_fixture[filemap_fixture != "- `spec/`: its per-file list is elsewhere."]
  expect_equal(
    fm$check_filemap_lines(filemap_tracked, unlisted),
    c(
      "tracked but not in FILEMAP.md: spec/a.xlsx",
      "tracked but not in FILEMAP.md: spec/b.csv",
      "folder row not listed under 'Folders covered by one row': spec/"
    )
  )
})

test_that("a folder row with no tracked file under it is reported", {
  fm <- load_dev_script("check_filemap.R")
  expect_equal(
    fm$check_filemap_lines(
      filemap_tracked[!startsWith(filemap_tracked, "spec/")], filemap_fixture
    ),
    "folder row with no tracked file under it: spec/"
  )
})

test_that("tracked_files lists a non-ASCII path unquoted", {
  fm <- load_dev_script("check_filemap.R")
  testthat::skip_if(!nzchar(Sys.which("git")), "git is not on the PATH")
  repo <- withr::local_tempdir()
  name <- basename(accented_path)
  system2("git", c("-C", shQuote(repo), "init", "--quiet"))
  writeLines("x <- 1", file.path(repo, name))
  system2("git", c("-C", shQuote(repo), "add", shQuote(name)))
  expect_equal(fm$tracked_files(repo), name)
})
