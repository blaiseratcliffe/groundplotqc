# Package-wide naming and documentation checks (plan 17.8; D5.8, D11.5).
# At M1 the package exports nothing, so the checks on exports pass on an empty
# set; the helpers they use are tested on synthetic help pages below. The
# rule-ID checks join with the registry at M3, the reason-code checks at M12.

export_pattern <- "^(gpq|magp)_[a-z0-9_]+$"

source_root <- function() {
  testthat::test_path("..", "..")
}

rd_tag <- function(x) {
  tag <- attr(x, "Rd_tag")
  if (is.null(tag)) "" else tag
}

rd_field <- function(rd, tag) {
  fields <- Filter(function(x) identical(rd_tag(x), tag), rd)
  vapply(fields, function(x) paste(as.character(x), collapse = ""), character(1))
}

rd_has_examples <- function(rd) {
  any(nzchar(trimws(rd_field(rd, "\\examples"))))
}

package_rd_db <- function() {
  path <- find.package("groundplotqc")
  if (dir.exists(file.path(path, "man"))) {
    tools::Rd_db(dir = path)
  } else {
    tools::Rd_db("groundplotqc")
  }
}

package_exports <- function() {
  path <- find.package("groundplotqc")
  parseNamespaceFile(basename(path), dirname(path), mustExist = TRUE)$exports
}

exports_without_examples <- function(exports, rd_db) {
  with_examples <- Filter(rd_has_examples, rd_db)
  documented <- unlist(lapply(with_examples, rd_field, tag = "\\alias"), use.names = FALSE)
  setdiff(exports, documented)
}

parse_rd_lines <- function(lines) {
  path <- withr::local_tempfile(fileext = ".Rd")
  writeLines(lines, path)
  tools::parse_Rd(path)
}

test_that("every export is gpq_ or magp_ followed by lower snake case", {
  exports <- package_exports()
  expect_equal(exports[!grepl(export_pattern, exports)], character())
})

test_that("the export pattern accepts gpq_ and magp_ lower snake names only", {
  expect_equal(
    grepl(export_pattern, c("gpq_ok", "magp_ok", "foo", "gpq_Bad")),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("every export has a help page with an example", {
  expect_equal(exports_without_examples(package_exports(), package_rd_db()), character())
})

test_that("the help-page check flags exports without a page or an example", {
  no_example <- parse_rd_lines(c(
    "\\name{gpq_demo}", "\\alias{gpq_demo}", "\\title{Demo}", "\\description{Demo.}"
  ))
  with_example <- parse_rd_lines(c(
    "\\name{gpq_other}", "\\alias{gpq_other}", "\\alias{magp_other}",
    "\\title{Other}", "\\description{Other.}", "\\examples{gpq_other()}"
  ))
  rd_db <- list(gpq_demo.Rd = no_example, gpq_other.Rd = with_example)
  expect_equal(
    exports_without_examples(c("gpq_demo", "gpq_other", "magp_other", "gpq_missing"), rd_db),
    c("gpq_demo", "gpq_missing")
  )
})

lint_with_package_config <- function(code, env = parent.frame()) {
  lintr_file <- file.path(source_root(), ".lintr")
  if (!file.exists(lintr_file)) {
    testthat::skip(".lintr is not in this tree")
  }
  testthat::skip_if_not_installed("lintr")
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(lintr_file, file.path(dir, ".lintr"))
  dir.create(file.path(dir, "R"))
  writeLines(code, file.path(dir, "R", "cases.R"))
  lints <- Filter(
    function(lint) identical(lint$linter, "export_prefix_linter"),
    lintr::lint_dir(dir)
  )
  sort(vapply(lints, function(lint) as.integer(lint$line_number), integer(1)))
}

test_that("the export-prefix linter flags unprefixed exports only", {
  cases <- c(
    "#' Prefixed export",
    "#' @export",
    "gpq_good_one <- function() 1",
    "",
    "#' Unprefixed export",
    "#' @param x A value.",
    "#' @export",
    "bad_export <- function(x) x",
    "",
    "#' S3 method, skipped (D11.7)",
    "#' @export",
    "print.gpq_results <- function(x, ...) invisible(x)",
    "",
    "#' Internal helper",
    "#' @noRd",
    "helper_thing <- function() 2",
    "",
    "#' Export named in the tag",
    "#' @export magp_named",
    "NULL",
    "",
    "#' Unprefixed name in the tag",
    "#' @export other_named",
    "NULL",
    "",
    "#' S3 tag, never checked",
    "#' @exportS3Method base::format",
    "format.gpq_spec <- function(x, ...) \"spec\"",
    "",
    "#' Export tag on the last line",
    "#' @export"
  )
  expect_equal(lint_with_package_config(cases), c(8L, 24L))
})

index_entries <- function(reference) {
  has_contents <- vapply(reference, function(section) length(section$contents) > 0L, logical(1))
  positions <- which(has_contents)
  contents <- lapply(reference[positions], function(section) as.character(unlist(section$contents)))
  data.frame(
    group = rep(positions, lengths(contents)),
    entry = as.character(unlist(contents, use.names = FALSE))
  )
}

is_plain_topic_name <- function(x) {
  grepl("^[A-Za-z.][A-Za-z0-9._-]*$", x)
}

exports_not_in_one_group <- function(exports, entries, rd_db) {
  page_names <- lapply(rd_db, function(rd) c(rd_field(rd, "\\name"), rd_field(rd, "\\alias")))
  n_groups <- vapply(exports, function(x) {
    pages <- Filter(function(names) x %in% names, page_names)
    selectors <- unique(c(x, unlist(pages, use.names = FALSE)))
    length(unique(entries$group[entries$entry %in% selectors]))
  }, integer(1))
  exports[n_groups != 1L]
}

test_that("index_entries keeps each section's position", {
  reference <- list(
    list(title = "A", contents = list("gpq_a", "gpq_b")),
    list(title = "Heading only"),
    list(subtitle = "B", contents = list("gpq_c"))
  )
  entries <- index_entries(reference)
  expect_equal(entries$entry, c("gpq_a", "gpq_b", "gpq_c"))
  expect_equal(entries$group, c(1L, 1L, 3L))
})

test_that("the index check finds exports in no group or in two", {
  page <- parse_rd_lines(c(
    "\\name{gpq_fit}", "\\alias{gpq_fit}", "\\alias{gpq_fit_ratio}",
    "\\title{Fit}", "\\description{Fit.}", "\\examples{gpq_fit()}"
  ))
  entries <- data.frame(
    group = c(1L, 2L, 2L, 3L),
    entry = c("gpq_once", "gpq_twice", "gpq_fit", "gpq_twice")
  )
  expect_equal(
    exports_not_in_one_group(
      c("gpq_once", "gpq_twice", "gpq_fit_ratio", "gpq_nowhere"), entries, list(gpq_fit.Rd = page)
    ),
    c("gpq_twice", "gpq_nowhere")
  )
})

test_that("_pkgdown.yml lists every export in exactly one reference group", {
  testthat::skip_if_not_installed("pkgdown")
  root <- source_root()
  if (!file.exists(file.path(root, "_pkgdown.yml"))) {
    testthat::skip("_pkgdown.yml is not in this tree")
  }
  entries <- index_entries(pkgdown::as_pkgdown(root)$meta$reference)
  expect_equal(
    entries$entry[!is_plain_topic_name(entries$entry)],
    character(),
    label = "Selectors in _pkgdown.yml (write topic names out in full, D11.9)"
  )
  expect_equal(exports_not_in_one_group(package_exports(), entries, package_rd_db()), character())
})
