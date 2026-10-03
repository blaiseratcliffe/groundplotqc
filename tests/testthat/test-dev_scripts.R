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

coverage_lines <- function(files, hits) {
  data.frame(
    filename = rep(files, lengths(hits)),
    line = sequence(lengths(hits)),
    value = unlist(hits)
  )
}

test_that("engine files are the R files of the engine families", {
  cov <- load_dev_script("check_coverage.R")
  engine <- c(
    "R/spec_read.R", "R/preflight.R", "R/preflight_report.R", "R/rules_registry.R",
    "R/results_schema.R", "R/check_keys.R", "R/fix_apply.R", "R/lineage_decode.R",
    "R/report_html.R", "R/utils_dt.R"
  )
  other <- c(
    "R/tool_coords.R", "R/magp_run.R", "R/groundplotqc-package.R", "R/globals.R",
    "R/options.R", "R/data.R", "R/specifics.R"
  )
  expect_equal(cov$is_engine_file(engine), rep(TRUE, length(engine)))
  expect_equal(cov$is_engine_file(other), rep(FALSE, length(other)))
})

test_that("the gate uses the engine files' lines combined", {
  cov <- load_dev_script("check_coverage.R")
  lines <- coverage_lines(
    c("R/spec_read.R", "R/check_keys.R", "R/magp_run.R"),
    list(c(rep(1, 9), 0), rep(3, 10), rep(0, 10))
  )
  per_file <- cov$engine_line_coverage(lines)
  expect_equal(per_file$filename, c("R/check_keys.R", "R/spec_read.R"))
  expect_equal(per_file$covered, c(10L, 9L))
  expect_equal(per_file$total, c(10L, 10L))
  verdict <- cov$coverage_verdict(per_file)
  expect_true(verdict$pass)
  expect_equal(verdict$percent, 95)
})

test_that("the gate fails below 90% and passes at exactly 90%", {
  cov <- load_dev_script("check_coverage.R")
  below <- coverage_lines(
    c("R/spec_read.R", "R/check_keys.R"),
    list(c(rep(1, 8), 0, 0), c(rep(1, 9), 0))
  )
  at <- coverage_lines("R/spec_read.R", list(c(rep(1, 9), 0)))
  expect_false(cov$coverage_verdict(cov$engine_line_coverage(below))$pass)
  expect_true(cov$coverage_verdict(cov$engine_line_coverage(at))$pass)
})

test_that("with no engine files the gate passes and says so", {
  cov <- load_dev_script("check_coverage.R")
  empty <- data.frame(filename = character(), line = integer(), value = numeric())
  layer_only <- coverage_lines("R/magp_run.R", list(c(0, 0)))
  for (lines in list(empty, layer_only)) {
    verdict <- cov$coverage_verdict(cov$engine_line_coverage(lines))
    expect_true(verdict$pass)
    expect_equal(verdict$message, "no engine files yet")
  }
})
