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
    report_text(
      "preflight_detail_code_list_blank_row",
      where = report_text("position_csv_one", rows = c(13L, 14L)), sheet = "visit_type"
    ),
    c("Sheet visit_type has a blank row: line 13.", "Sheet visit_type has a blank row: line 14.")
  )
  expect_equal(report_text("preflight_title"), "Pre-flight report")
})

test_that("report_text stops on a missing row, an unfilled placeholder or a bad id", {
  expect_error(report_text("no_such_text"), "no_such_text")
  expect_error(report_text("preflight_detail_code_list_blank_row", where = "line 1"), "sheet")
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
    report_text("preflight_detail_code_list_blank_row", where = NA_character_, sheet = "s"),
    "where"
  )
  expect_error(
    report_text(
      "preflight_detail_code_list_blank_row",
      where = c("line 1", "line 2"), sheet = c("a", "b", "c")
    ),
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

test_that("the engine's text is read once per session, and reset reads it again (D12.29)", {
  real_read <- read_csv_text
  reads <- 0L
  testthat::local_mocked_bindings(read_csv_text = function(path) {
    reads <<- reads + 1L
    real_read(path)
  })
  reset_report_texts()
  withr::defer(reset_report_texts())
  report_text("preflight_title")
  report_text("preflight_intro")
  expect_identical(report_texts(), report_texts())
  expect_equal(reads, 1L)
  reset_report_texts()
  report_text("preflight_title")
  expect_equal(reads, 2L)
})

test_that("a text is looked up in D14.8's order: caller, engine, caller English, engine", {
  text <- validate_text_table(data.frame(
    text_id = c("preflight_title", "preflight_title", "preflight_intro"),
    lang = c("fr", "en", "en"),
    text = c("Rapport de pre-vol", "Pre-flight page", "Our intro.")
  ))
  expect_equal(report_text("preflight_title", "fr", text = text), "Rapport de pre-vol")
  expect_equal(report_text("preflight_title", text = text), "Pre-flight page")
  # No French row anywhere: the caller's English wins over the engine's.
  expect_equal(report_text("preflight_intro", "fr", text = text), "Our intro.")
  # Neither table has the text in French or the caller's in English: the engine's English.
  expect_equal(report_text("preflight_summary_title", "fr", text = text), "Checks")
  expect_equal(report_text("preflight_summary_title", "fr"), "Checks")
  expect_error(report_text("no_such_text", "fr", text = text), "no row in language fr")
})

test_that("an engine row in the language comes before the caller's English (D14.8)", {
  reset_report_texts()
  withr::defer(reset_report_texts())
  engine <- data.table::copy(report_texts())
  french <- data.table::data.table(
    text_id = c("preflight_title", "preflight_intro"), lang = "fr",
    text = c("Moteur", "Moteur, introduction")
  )
  text_cache$engine <- rbind(engine, french)
  text <- validate_text_table(data.frame(
    text_id = c("preflight_title", "preflight_intro"), lang = c("en", "fr"),
    text = c("Caller English", "Caller French")
  ))
  expect_equal(report_text("preflight_title", "fr", text = text), "Moteur")
  # The caller's row in the language comes before the engine's row in it.
  expect_equal(report_text("preflight_intro", "fr", text = text), "Caller French")
})

test_that("text_ids lists the ids with a row in a language, in either table", {
  text <- validate_text_table(data.frame(text_id = "preflight_title", lang = "fr", text = "T"))
  expect_equal(text_ids("fr", text), "preflight_title")
  expect_equal(text_ids("fr"), character())
  expect_true("preflight_title" %in% text_ids("en"))
})

test_that("validate_text_table copies a table and refuses a malformed one", {
  given <- data.frame(lang = "fr", text = "T", text_id = "a")
  checked <- validate_text_table(given)
  expect_named(checked, c("text_id", "lang", "text"))
  # The values moved with their columns.
  expect_equal(as.list(checked), list(text_id = "a", lang = "fr", text = "T"))
  expect_null(validate_text_table(NULL))
  expect_error(validate_text_table(list(text_id = "a")), "exactly the columns")
  expect_error(validate_text_table(data.frame(text_id = "a", lang = "fr")), "exactly the columns")
  expect_error(
    validate_text_table(data.frame(text_id = "a", lang = "fr", text = "T", note = "x")),
    "exactly the columns"
  )
  # A column named twice (R12), whether or not another is then missing.
  expect_error(
    validate_text_table(
      data.frame(text_id = "a", lang = "fr", text = "T", text = "U", check.names = FALSE)
    ),
    "exactly the columns"
  )
  expect_error(
    validate_text_table(
      data.frame(text_id = "a", lang = "fr", lang = "en", check.names = FALSE)
    ),
    "exactly the columns"
  )
  expect_error(
    validate_text_table(data.frame(text_id = c("a", "b"), lang = c("fr", " "), text = c(NA, "T"))),
    "blank cells in rows 1, 2;"
  )
  # A table of no rows is a table, as a file with only its header is.
  empty <- validate_text_table(data.frame(text_id = "a", lang = "fr", text = "1")[0L, ])
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("text_id", "lang", "text"))
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  expect_error(
    validate_text_table(data.frame(text_id = c("a", "b"), lang = "fr", text = c("T", bad))),
    "Column `text` of `text` holds text that isn't valid UTF-8, in rows 2.",
    fixed = TRUE
  )
})

test_that("validate_text_table names a repeated text_id and lang, and caps a long list (D14.20)", {
  expect_error(
    validate_text_table(data.frame(text_id = c("a", "a"), lang = "fr", text = c("1", "2"))),
    "`text` has more than one row for text_id a in language fr.",
    fixed = TRUE
  )
  expect_error(
    validate_text_table(data.frame(
      text_id = c("a", "a", "b", "b"), lang = c("fr", "fr", "en", "en"), text = "T"
    )),
    "more than one row for each of these text_id (language) pairs: a (fr), b (en).",
    fixed = TRUE
  )
  many <- data.frame(text_id = letters[1:12], lang = "FR", text = "T")
  expect_error(
    validate_text_table(many),
    "in rows 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 and 2 more.",
    fixed = TRUE
  )
})

test_that("every list of rows or pairs in a message shows ten, then how many more", {
  first_ten <- paste(1:10, collapse = ", ")
  blank <- data.frame(text_id = letters[1:11], lang = "fr", text = NA_character_)
  expect_error(
    validate_text_table(blank),
    paste0("blank cells in rows ", first_ten, " and 1 more;"),
    fixed = TRUE
  )
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  invalid <- data.frame(text_id = letters[1:11], lang = "fr", text = bad)
  expect_error(
    validate_text_table(invalid),
    paste0("holds text that isn't valid UTF-8, in rows ", first_ten, " and 1 more."),
    fixed = TRUE
  )
  twice <- data.frame(text_id = rep(letters[1:11], each = 2L), lang = "fr", text = "T")
  expect_error(
    validate_text_table(twice),
    paste0("pairs: ", paste0(letters[1:10], " (fr)", collapse = ", "), " and 1 more."),
    fixed = TRUE
  )
})

test_that("validate_text_table changes nothing it is given and shares no column with it", {
  frame <- data.frame(lang = c("fr", "en"), text = c("T", "U"), text_id = c("a", "b"))
  # A data.table and a data.frame are both changed in place by data.table's functions.
  for (given in list(data.table::as.data.table(frame), frame)) {
    before <- data.table::copy(given)
    checked <- validate_text_table(given)
    # The caller's table keeps its columns, their order and their values.
    expect_equal(given, before)
    expect_identical(names(given), names(before))
    expect_equal(
      as.list(checked),
      list(text_id = c("a", "b"), lang = c("fr", "en"), text = c("T", "U"))
    )
    # No column of the result is the vector of the caller's column, so a change by
    # reference to the result can't reach the input.
    for (column in names(checked)) {
      expect_false(data.table::address(checked[[column]]) == data.table::address(given[[column]]))
    }
    data.table::set(checked, 1L, "text", "Changed")
    expect_equal(given$text, c("T", "U"))
  }
})

test_that("lookups leave the cached engine table as it was (D12.29)", {
  reset_report_texts()
  withr::defer(reset_report_texts())
  before <- data.table::copy(report_texts())
  address_before <- data.table::address(report_texts())
  text <- validate_text_table(data.frame(text_id = "preflight_title", lang = "fr", text = "T"))
  report_text("preflight_title")
  report_text("preflight_title", "fr", text = text)
  report_text("preflight_summary_title", "fr")
  text_template("preflight_intro", "en", text)
  text_ids("fr", text)
  text_ids("en")
  expect_identical(data.table::address(report_texts()), address_before)
  expect_equal(report_texts(), before)
})

test_that("validate_text_table refuses a column of more than one value per row", {
  given <- data.frame(text_id = c("a", "b"), lang = "fr", text = c("1", "2"))
  given$text <- matrix(c("1", "2", "3", "4"), 2)
  expect_error(
    validate_text_table(given), "Column `text` of `text` must hold one value per row.",
    fixed = TRUE
  )
  given$text <- list("x", c("y", "z"))
  expect_error(
    validate_text_table(given), "Column `text` of `text` must hold one value per row.",
    fixed = TRUE
  )
})

test_that("validate_text_table refuses a lang that isn't a language code, naming rows (D14.20)", {
  given <- data.frame(text_id = c("a", "b", "c"), lang = c("FR", "fr", "en-CA"), text = "T")
  expect_error(
    validate_text_table(given),
    "aren't a language code of two or three lower-case letters, in rows 1, 3."
  )
})

test_that("a stop for a missing value names the text and the language (D14.8)", {
  expect_error(
    report_text("preflight_detail_code_list_blank_row", where = "line 1"),
    "Report text preflight_detail_code_list_blank_row (language en) needs a value for sheet.",
    fixed = TRUE
  )
  # The language is the row's, so a fallback row says English in a French run.
  expect_error(
    report_text("preflight_detail_code_list_blank_row", "fr", where = "line 1"),
    "(language en) needs a value for sheet.",
    fixed = TRUE
  )
  french <- validate_text_table(data.frame(
    text_id = c("preflight_title", "preflight_intro"), lang = c("fr", "en"),
    text = c("Titre {zzz}", "Intro {zzz}")
  ))
  expect_error(
    report_text("preflight_title", "fr", text = french),
    "Report text preflight_title (language fr) needs a value for zzz.",
    fixed = TRUE
  )
  # The caller's English row in a French run is the one at fault, and says so.
  expect_error(
    report_text("preflight_intro", "fr", text = french),
    "Report text preflight_intro (language en) needs a value for zzz.",
    fixed = TRUE
  )
  expect_error(fill_placeholders("Row {row}.", list()), "Report text needs a value for row.")
})

test_that("a stop for values of two lengths or an NA names the text and the language", {
  expect_error(
    report_text(
      "preflight_detail_code_list_blank_row",
      where = c("line 1", "line 2"), sheet = c("a", "b", "c")
    ),
    "Report text preflight_detail_code_list_blank_row (language en) values must have length 1",
    fixed = TRUE
  )
  expect_error(
    report_text("preflight_detail_code_list_blank_row", where = NA_character_, sheet = "s"),
    "Report text preflight_detail_code_list_blank_row (language en) slot {where} has an NA value.",
    fixed = TRUE
  )
  # Called without a text id, the words are the plain ones.
  expect_error(
    fill_placeholders("Row {row} of {sheet}.", list(row = 1:2, sheet = c("a", "b", "c"))),
    "Report text values must have length 1 or one common length;",
    fixed = TRUE
  )
  expect_error(
    fill_placeholders("Row {row}.", list(row = NA_integer_)),
    "Report text slot {row} has an NA value.",
    fixed = TRUE
  )
})

test_that("a text with no row says so in English, or in the language and English", {
  expect_error(report_text("no_such_text"), "Report text no_such_text has no row in English.")
  expect_error(
    report_text("no_such_text", "fr"),
    "Report text no_such_text has no row in language fr or in English."
  )
})

test_that("two rows for one text and language in one table stop, whichever table (D14.8)", {
  reset_report_texts()
  withr::defer(reset_report_texts())
  engine <- data.table::copy(report_texts())
  twice <- data.table::data.table(text_id = "preflight_title", lang = "en", text = "Again")
  text_cache$engine <- rbind(engine, twice)
  expect_error(report_text("preflight_title"), "preflight_title has 2 rows in language en")
  reset_report_texts()
  # A caller's table that skipped validate_text_table() is still caught at the lookup.
  unchecked <- data.table::data.table(
    text_id = c("preflight_title", "preflight_title"), lang = "fr", text = c("1", "2")
  )
  expect_error(
    report_text("preflight_title", "fr", text = unchecked),
    "preflight_title has 2 rows in language fr"
  )
})

test_that("a blank rule_id shows as (blank), as the other names do (D12.55)", {
  expect_equal(
    report_text("preflight_stop_line", rule_id = c("a", NA), detail = "x"),
    c("a: x", "(blank): x")
  )
})
