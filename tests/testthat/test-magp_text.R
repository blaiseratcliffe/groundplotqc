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
