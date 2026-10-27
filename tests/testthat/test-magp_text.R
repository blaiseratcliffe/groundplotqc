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
  # No row uses a slot the engine's row of its text lacks, or pre-flight would stop (D14.21).
  found <- preflight_check_functions()[["text_slot_unknown"]](
    fx_fish_spec(), preflight_context(text = texts)
  )
  expect_equal(nrow(found), 0L)
})

# task 12's fix round (the report review's M2)
test_that("MAGPlot's src_id_duplicate row overrides the engine's when given as text (D14.8)", {
  texts <- magp_texts()
  row <- texts$text[texts$text_id == "src_id_duplicate" & texts$lang == "en"]
  expect_length(row, 1L)
  # The words aren't pinned: only that the table's row is the one used, and differs.
  overridden <- report_text("src_id_duplicate", text = texts)
  expect_equal(overridden, row)
  expect_false(identical(overridden, report_text("src_id_duplicate")))
})

# task 14's fix round 1 (the report review's F2)
test_that("MAGPlot's design candidate rows override the engine's when given as text (D14.8)", {
  texts <- magp_texts()
  for (id in c("design_geometry_inconsistent", "design_duplicate_signature")) {
    row <- texts$text[texts$text_id == id & texts$lang == "en"]
    expect_length(row, 1L)
    # The words aren't pinned: only that the table's row is the one used, and differs.
    overridden <- report_text(id, text = texts)
    expect_equal(overridden, row, info = id)
    expect_false(identical(overridden, report_text(id)), info = id)
  }
})

# The four tests above check the shipped file's rows (D18.54); the tests below read planted
# files through magp_texts().

test_that("magp_texts hands on a clean file's rows, checked", {
  file <- withr::local_tempfile(fileext = ".csv")
  # The engine's row of preflight_stop_line has the slots rule_id and detail.
  writeLines(
    c('"text_id","lang","text"', '"preflight_stop_line","fr","{rule_id} - {detail}"'), file
  )
  real_read <- read_csv_text
  testthat::local_mocked_bindings(
    read_csv_text = function(path) real_read(if (grepl("report_text_magp", path)) file else path)
  )
  texts <- magp_texts()
  expect_equal(nrow(texts), 1L)
  expect_equal(
    as.list(texts),
    list(text_id = "preflight_stop_line", lang = "fr", text = "{rule_id} - {detail}")
  )
  # The three checks above hold for a good row: its id is the engine's, it is ASCII and its
  # braces mark a slot, one the engine's row of that text has.
  expect_equal(setdiff(texts$text_id, report_texts()$text_id), character())
  expect_true(all(!grepl("[^ -~]", texts$text)))
  expect_false(any(grepl("[{}]", gsub("\\{[a-z0-9_]+\\}", "", texts$text))))
  found <- preflight_check_functions()[["text_slot_unknown"]](
    fx_fish_spec(), preflight_context(text = texts)
  )
  expect_equal(nrow(found), 0L)
  # The file's rows go through the table check, as a caller's do.
  writeLines(c('"text_id","lang","text"', '"preflight_title","FR","Rapport"'), file)
  expect_error(magp_texts(), "aren't a language code")
})

test_that("a stop on the file's rows names the file, the condition's class kept (D14.38)", {
  file <- withr::local_tempfile(fileext = ".csv")
  writeLines(c('"text_id","lang","text"', '"preflight_title","FR","Rapport"'), file)
  real_read <- read_csv_text
  testthat::local_mocked_bindings(
    read_csv_text = function(path) real_read(if (grepl("report_text_magp", path)) file else path)
  )
  # The table check's own stop for the same rows, with rows counted as data rows.
  direct <- tryCatch(
    validate_text_table(data.frame(text_id = "preflight_title", lang = "FR", text = "Rapport")),
    error = identity
  )
  from_file <- tryCatch(magp_texts(), error = identity)
  expect_s3_class(from_file, "error")
  expect_identical(class(from_file), class(direct))
  expect_identical(
    conditionMessage(from_file), paste0("report_text_magp.csv: ", conditionMessage(direct))
  )
  expect_match(conditionMessage(from_file), "^report_text_magp.csv: `text` has lang values")
  expect_match(conditionMessage(from_file), "in row 1.$")
  # A condition of another class keeps it: only the message changes.
  testthat::local_mocked_bindings(
    validate_text_table = function(text) {
      stop(structure(
        class = c("gpq_demo_error", "error", "condition"),
        list(message = "demo message", call = NULL, extra = 7L)
      ))
    }
  )
  classed <- tryCatch(magp_texts(), error = identity)
  expect_identical(class(classed), c("gpq_demo_error", "error", "condition"))
  expect_identical(conditionMessage(classed), "report_text_magp.csv: demo message")
  expect_identical(classed$extra, 7L)
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
  # Installed, as R CMD check runs it, the test mocks base system.file(), which no CRAN check
  # may change (R145).
  testthat::skip_on_cran()
  # system.file() stands in for a package without the file, for this test only. Under
  # load_all() the package calls pkgload's own system.file(), found among its imports; an
  # installed package, as R CMD check runs it, calls base's.
  imports <- parent.env(environment(magp_texts))
  if (exists("system.file", envir = imports, inherits = FALSE)) {
    testthat::local_mocked_bindings(system.file = function(...) "")
  } else {
    testthat::local_mocked_bindings(system.file = function(...) "", .package = "base")
  }
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
