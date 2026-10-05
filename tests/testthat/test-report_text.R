# Tests for the report text lookup (plan 11.7; D4.19, D12.21).

test_that("every row has an id, an English text and a unique id per language", {
  texts <- report_texts()
  expect_named(texts, c("text_id", "lang", "text"))
  expect_false(anyNA(texts))
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", texts$text_id)))
  expect_equal(anyDuplicated(texts[, c("text_id", "lang")]), 0L)
  expect_true(all(texts$text_id %in% texts$text_id[texts$lang == "en"]))
})

test_that("report_text fills placeholders, vectorised", {
  expect_equal(
    report_text("preflight_detail_code_list_blank_row", row = c(13L, 14L), sheet = "visit_type"),
    c("Row 13 of sheet visit_type is blank.", "Row 14 of sheet visit_type is blank.")
  )
  expect_equal(report_text("preflight_title"), "Pre-flight report")
})

test_that("report_text stops on a missing row or an unfilled placeholder", {
  expect_error(report_text("no_such_text"), "no_such_text")
  expect_error(report_text("preflight_detail_code_list_blank_row", row = 1L), "sheet")
})

test_that("fill_placeholders keeps plain text and gives nothing for no values", {
  expect_equal(fill_placeholders("Plain {not one", list()), "Plain {not one")
  expect_equal(fill_placeholders("Row {row}.", list(row = integer())), character())
})
