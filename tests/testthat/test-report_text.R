# Tests for the report text lookup (plan 11.7; D4.19, D12.21, D12.45, D12.54, D12.55, D12.58).

test_that("every row has an id, an English text and a unique id per language", {
  texts <- report_texts()
  expect_named(texts, c("text_id", "lang", "text"))
  expect_false(anyNA(texts))
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", texts$text_id)))
  expect_equal(anyDuplicated(texts[, c("text_id", "lang")]), 0L)
  expect_true(all(texts$text_id %in% texts$text_id[texts$lang == "en"]))
})

test_that("every text is ASCII and braces only mark placeholders (D12.45)", {
  texts <- report_texts()
  expect_true(all(!grepl("[^ -~]", texts$text)))
  outside <- gsub("\\{[a-z0-9_]+\\}", "", texts$text)
  expect_false(any(grepl("[{}]", outside)))
})

test_that("no placeholder can be taken for text_id or lang, and every slot list name is used", {
  texts <- report_texts()
  found <- unlist(regmatches(texts$text, gregexpr("\\{[a-z0-9_]+\\}", texts$text)))
  slot_names <- unique(gsub("[{}]", "", found))
  # R matches a value named `te` or `la` to text_id or lang instead of to `...`.
  expect_false(any(startsWith("text_id", slot_names) | startsWith("lang", slot_names)))
  expect_true(all(c(quoted_slots, blank_slots) %in% slot_names))
})

test_that("report_text fills placeholders, vectorised", {
  expect_equal(
    report_text("preflight_detail_code_list_blank_row", row = c(13L, 14L), sheet = "visit_type"),
    c("Row 13 of sheet visit_type is blank.", "Row 14 of sheet visit_type is blank.")
  )
  expect_equal(report_text("preflight_title"), "Pre-flight report")
})

test_that("report_text stops on a missing row, an unfilled placeholder or a bad id", {
  expect_error(report_text("no_such_text"), "no_such_text")
  expect_error(report_text("preflight_detail_code_list_blank_row", row = 1L), "sheet")
  expect_error(report_text(NA_character_), "text_id")
  expect_error(report_text("preflight_title", lang = c("en", "fr")), "lang")
})

test_that("values read from the specification are quoted, a blank shown as (blank) (D12.55)", {
  expect_equal(
    report_text(
      "preflight_detail_dd_type_unknown",
      table_name = c("t", NA), attribute_name = "a", data_type = c(" text", NA)
    ),
    c(
      "t.a has type \" text\", which the type map doesn't list.",
      "(blank).a has type (blank), which the type map doesn't list."
    )
  )
})

test_that("an NA in another slot stops, and so do values of two lengths (D12.45)", {
  expect_error(
    report_text("preflight_detail_code_list_blank_row", row = NA_integer_, sheet = "s"),
    "row"
  )
  expect_error(
    report_text("preflight_detail_code_list_blank_row", row = 1:2, sheet = c("a", "b", "c")),
    "common length"
  )
})

test_that("a CSV's unknown warning and row shortfall fill their slots; NA stops (D12.58)", {
  expect_equal(
    report_text("preflight_detail_spec_csv_malformed_unknown", file = "a.csv", value = "Odd."),
    paste(
      "Reading a.csv gave a warning the package doesn't recognise, so some of its lines may",
      "not have been read: \"Odd.\"."
    )
  )
  expect_equal(
    report_text("preflight_detail_spec_csv_malformed_short", file = "a.csv", n = 3L, n_read = 2L),
    "a.csv has 3 records after its header, but only 2 were read."
  )
  expect_error(
    report_text("preflight_detail_spec_csv_malformed_short", file = "a.csv", n = 3L, n_read = NA),
    "n_read"
  )
})

test_that("numbers fill in full (D12.45)", {
  expect_equal(
    report_text(
      "preflight_detail_site_id_range_invalid_order",
      label = "AB", band_start = 1e5, band_end = 0.5
    ),
    "Band \"AB\" starts at 100000, after it ends at 0.5."
  )
})

test_that("fill_placeholders keeps plain text and gives nothing for no values", {
  expect_equal(fill_placeholders("Plain {not one", list()), "Plain {not one")
  expect_equal(fill_placeholders("Row {row}.", list(row = integer())), character())
})

test_that("a zero-length value beside a longer one stops as two lengths (D12.45)", {
  expect_error(
    fill_placeholders("Row {row} of {sheet}.", list(row = integer(), sheet = c("a", "b"))),
    "common length"
  )
  expect_equal(
    fill_placeholders("Row {row} of {sheet}.", list(row = integer(), sheet = "a")),
    character()
  )
})
