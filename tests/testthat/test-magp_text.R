# Tests for MAGPlot's report text (plan 11.7; D4.19, D14.8).

test_that("MAGPlot's text table reads clean, with the text columns", {
  texts <- magp_texts()
  expect_named(texts, c("text_id", "lang", "text"))
  expect_true(all(vapply(texts, is.character, TRUE)))
})

test_that("every MAGPlot text overrides an engine text: its id is in the engine's file", {
  expect_equal(setdiff(magp_texts()$text_id, report_texts()$text_id), character())
})

test_that("MAGPlot's texts follow the engine file's rules: ASCII, braces only for slots", {
  texts <- magp_texts()
  expect_true(all(!grepl("[^ -~]", texts$text)))
  expect_false(any(grepl("[{}]", gsub("\\{[a-z0-9_]+\\}", "", texts$text))))
})

# The shipped file has no rows, so the three tests above assert nothing about a row until
# one arrives; the tests below read planted files through magp_texts() instead.

test_that("magp_texts hands on a clean file's rows, checked", {
  file <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c('"text_id","lang","text"', '"preflight_title","fr","Rapport de pre-vol {x}"'), file
  )
  real_read <- read_csv_text
  testthat::local_mocked_bindings(
    read_csv_text = function(path) real_read(if (grepl("report_text_magp", path)) file else path)
  )
  texts <- magp_texts()
  expect_equal(nrow(texts), 1L)
  expect_equal(
    as.list(texts),
    list(text_id = "preflight_title", lang = "fr", text = "Rapport de pre-vol {x}")
  )
  # The file's rows go through the table check, as a caller's do.
  writeLines(c('"text_id","lang","text"', '"preflight_title","FR","Rapport"'), file)
  expect_error(magp_texts(), "aren't a language code")
})

test_that("the checks above bite on a planted row: an id the engine lacks, a non-ASCII text", {
  file <- withr::local_tempfile(fileext = ".csv")
  accent <- rawToChar(as.raw(c(0x52, 0xC3, 0xA9)))
  Encoding(accent) <- "UTF-8"
  writeLines(
    c('"text_id","lang","text"', paste0('"no_such_id","fr","', accent, ' {"')),
    file,
    useBytes = TRUE
  )
  real_read <- read_csv_text
  testthat::local_mocked_bindings(
    read_csv_text = function(path) real_read(if (grepl("report_text_magp", path)) file else path)
  )
  texts <- magp_texts()
  expect_equal(setdiff(texts$text_id, report_texts()$text_id), "no_such_id")
  expect_true(any(grepl("[^ -~]", texts$text)))
  expect_true(any(grepl("[{}]", gsub("\\{[a-z0-9_]+\\}", "", texts$text))))
})

test_that("magp_texts stops on a file missing from the package", {
  testthat::local_mocked_bindings(system.file = function(...) "")
  expect_error(magp_texts(), "report_text_magp.csv is missing from the package.")
})

test_that("magp_texts stops on a file that doesn't read cleanly: a ragged row, an invalid byte", {
  file <- withr::local_tempfile(fileext = ".csv")
  real_read <- read_csv_text
  testthat::local_mocked_bindings(
    read_csv_text = function(path) real_read(if (grepl("report_text_magp", path)) file else path)
  )
  writeLines(c('"text_id","lang","text"', '"preflight_title","fr","T","extra"'), file)
  expect_error(magp_texts(), "report_text_magp.csv doesn't read cleanly.")
  writeBin(
    c(
      charToRaw('"text_id","lang","text"\n"preflight_title","fr","ok'),
      as.raw(0x97), charToRaw('"\n')
    ),
    file
  )
  expect_error(magp_texts(), "report_text_magp.csv doesn't read cleanly.")
})
