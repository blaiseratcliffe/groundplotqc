# Tests for the pre-flight files (plan 4.2, 11.5, 18.4; D2.11, D7.11, D12.17, D12.24).

hostile_spec <- function() {
  gpq_read_spec(
    dictionary = data.frame(
      table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"
    ),
    code_lists = list(`<script>x</script>` = data.frame(a = "R&D \"quoted\""))
  )
}

test_that("a stop still writes both files, the CSV as the table", {
  dir <- withr::local_tempdir()
  expect_error(gpq_preflight(hostile_spec(), output_dir = dir), class = "gpq_preflight_error")
  csv <- file.path(dir, "metadata", "preflight.csv")
  expect_true(file.exists(file.path(dir, "reports", "preflight.html")))
  back <- read_csv_text(csv)$data
  expect_named(back, names(preflight_columns()))
  expect_equal(back$detail[back$rule_id == "code_list_unreferenced"], paste(
    "Sheet <script>x</script> isn't used by any attribute and isn't declared as not a",
    "code list."
  ))
})

test_that("the page shows outcomes and reasons as text, the CSV keeps their codes (D12.56)", {
  dir <- withr::local_tempdir()
  expect_error(gpq_preflight(hostile_spec(), output_dir = dir), class = "gpq_preflight_error")
  page <- paste(
    readLines(file.path(dir, "reports", "preflight.html"), encoding = "UTF-8"),
    collapse = "\n"
  )
  expect_match(page, "<td>Stop</td>", fixed = TRUE)
  expect_match(
    page, "<td>Not run</td><td></td><td>Its input wasn&#39;t given.</td>",
    fixed = TRUE
  )
  expect_false(grepl("<td>not_run</td>", page, fixed = TRUE))
  expect_false(grepl("<td>no_input</td>", page, fixed = TRUE))
  back <- read_csv_text(file.path(dir, "metadata", "preflight.csv"))$data
  expect_true(all(back$outcome %in% c("pass", "warn", "stop", "not_run")))
  expect_true("no_input" %in% back$not_run_reason)
  forest <- fx_forest_spec()
  expect_match(
    preflight_html(preflight_checks(forest), forest),
    "The dictionary has no column flagging attributes for the lineage spec.",
    fixed = TRUE
  )
})

test_that("the page escapes hostile text, is self-contained and lists every check", {
  spec <- hostile_spec()
  page <- preflight_html(preflight_checks(spec), spec)
  expect_false(grepl("<script>x", page, fixed = TRUE))
  expect_match(page, "&lt;script&gt;x&lt;/script&gt;", fixed = TRUE)
  expect_false(grepl("(src|href)=\"https?:", page))
  # An inline handler is looked for inside tags only: escaped text may hold " onx=".
  expect_false(grepl("<[^>]*\\son[a-z]+=", page))
  for (rule in preflight_rules()$rule_id) expect_match(page, rule, fixed = TRUE)
})

test_that("an invalid byte reaches the page escaped, without an error", {
  spec <- fx_planted_spec()
  page <- preflight_html(preflight_checks(spec), spec)
  expect_match(page, "ok&lt;97&gt;", fixed = TRUE)
})

test_that("an invalid byte in a column name is located and escaped on the page (D12.28)", {
  dictionary <- data.frame(
    table_name = "t", attribute_name = "id", key_type = "PK", data_type = "character", note = "x"
  )
  bad_name <- rawToChar(as.raw(c(0x6E, 0x61, 0x97, 0x6D, 0x65)))
  Encoding(bad_name) <- "UTF-8"
  names(dictionary)[[5L]] <- bad_name
  spec <- gpq_read_spec(dictionary)
  results <- preflight_checks(spec)
  expect_equal(
    results$detail[results$rule_id == "spec_encoding_invalid"],
    paste(
      "The name of column 5 of dictionary isn't valid UTF-8; it is kept as \"na<97>me\",",
      "each bad byte shown as its hex code in angle brackets."
    )
  )
  page <- preflight_html(results, spec)
  expect_match(page, "na&lt;97&gt;me", fixed = TRUE)
  expect_false(grepl("na<97>me", page, fixed = TRUE))
})

test_that("both files are written as UTF-8 whatever the session's locale", {
  spec <- gpq_read_spec(
    data.frame(table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"),
    code_lists = setNames(list(data.frame(a = "x")), "café")
  )
  dir <- withr::local_tempdir()
  withr::with_locale(
    c(LC_CTYPE = "C"),
    tryCatch(gpq_preflight(spec, output_dir = dir), gpq_preflight_error = function(e) NULL)
  )
  for (path in file.path(dir, c("reports/preflight.html", "metadata/preflight.csv"))) {
    text <- rawToChar(readBin(path, "raw", file.size(path)))
    Encoding(text) <- "UTF-8"
    expect_true(grepl("café", text, fixed = TRUE), info = path)
    expect_false(grepl("<U+", text, fixed = TRUE), info = path)
  }
})

test_that("the footer names files without a hash alone, and is empty with none (R19)", {
  spec <- gpq_read_spec(
    data.frame(table_name = "t", attribute_name = "a", key_type = "PK", data_type = "character"),
    crosswalks = list(cond = file.path(tempdir(), "no such.csv"))
  )
  page <- preflight_html(preflight_checks(spec), spec)
  expect_match(page, "<dd>no such.csv</dd>", fixed = TRUE)
  memory <- hostile_spec()
  empty <- preflight_html(preflight_checks(memory), memory)
  expect_match(empty, "<dt>Specification files</dt><dd></dd>", fixed = TRUE)
})
