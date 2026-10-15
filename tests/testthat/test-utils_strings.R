# Tests for the string helpers (plan 3.3, 3.6; D12.14, D12.27, D12.36, D12.45).

test_that("as_text gives UTF-8 text with blanks as NA", {
  expect_equal(as_text(c("a", "", NA)), c("a", NA, NA))
  expect_equal(as_text(factor(c("x", "y"))), c("x", "y"))
  expect_equal(as_text(c(1, -9)), c("1", "-9"))
  latin <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xE9)))
  Encoding(latin) <- "latin1"
  expect_identical(charToRaw(as_text(latin)), as.raw(c(0x63, 0x61, 0x66, 0xC3, 0xA9)))
  expect_equal(Encoding(as_text(latin)), "UTF-8")
})

test_that("as_text writes numbers in full, never in scientific notation (D12.45)", {
  expect_equal(
    as_text(c(1e5, 0.1, 0.1 + 0.2, 170.03, -1, NA)),
    c("100000", "0.1", "0.3", "170.03", "-1", NA)
  )
  expect_equal(as_text(c(2L, NA)), c("2", NA))
  expect_equal(as_text(as.Date("2020-01-02")), "2020-01-02")
})

test_that("as_text keeps no names or attributes (D12.45)", {
  expect_identical(as_text(c(a = 1.5, b = NA)), c("1.5", NA))
  expect_identical(as_text(c(a = 2L)), "2")
  expect_identical(as_text(matrix(c(1, 2), nrow = 1)), c("1", "2"))
  expect_identical(as_text(c(a = "x")), "x")
})

test_that("blank_to_na makes only cells empty after trimming NA (D12.27)", {
  expect_equal(
    blank_to_na(c("", "   ", "\t", "NA", " NT_PSP", NA)),
    c(NA, NA, NA, "NA", " NT_PSP", NA)
  )
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  expect_identical(blank_to_na(bad), bad)
  expect_equal(is_blank(c("", " ", "a", NA)), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("space_padding finds blank and padded values and trims them (D16.42, D16.44)", {
  nbsp <- intToUtf8(0xA0)
  em <- intToUtf8(0x2003)
  summer <- intToUtf8(c(0xE9, 0x74, 0xE9))
  text <- c(
    "", " \t", "AP", " AP", "AP\r\n", nbsp, paste0(em, " ", nbsp), paste0("AP", nbsp),
    paste0(em, summer, " "), "A B", "AP\f", NA
  )
  read <- space_padding(text)
  # A space, tab, CR or LF counts in plain ASCII text; any Unicode space in other text.
  expect_identical(
    read$blank, c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    read$padded, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(read$trimmed[c(3:5, 8:11)], c("AP", "AP", "AP", "AP", summer, "A B", "AP\f"))
  expect_true(is.na(read$trimmed[[12L]]))
  # is_blank() is unchanged for the specification reader: a no-break space isn't blank there.
  expect_false(is_blank(nbsp))
  expect_identical(space_padding(character()), list(
    blank = logical(), padded = logical(), trimmed = character()
  ))
})

test_that("space_padding reads latin1 as UTF-8 and other text byte by byte (D16.33, D16.36)", {
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  # A latin1 no-break space is a Unicode space once read as UTF-8; the value trimmed is UTF-8.
  latin <- iconv(paste0("AP", intToUtf8(0xA0)), "UTF-8", "latin1")
  # Text marked as bytes, or not valid UTF-8, has only its ASCII spaces counted, its bytes
  # trimmed and its mark kept.
  marked <- paste0(" ", intToUtf8(0xE9), " ")
  Encoding(marked) <- "bytes"
  invalid <- rawToChar(as.raw(c(0x20, 0x41, 0x97)))
  bare <- rawToChar(as.raw(c(0x41, 0x97, 0xA0)))
  expect_silent(read <- space_padding(c(latin, marked, invalid, bare)))
  expect_identical(read$padded, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(read$blank, c(FALSE, FALSE, FALSE, FALSE))
  expect_identical(read$trimmed[[1L]], "AP")
  expect_identical(charToRaw(read$trimmed[[2L]]), charToRaw(intToUtf8(0xE9)))
  expect_identical(Encoding(read$trimmed[[2L]]), "bytes")
  expect_identical(charToRaw(read$trimmed[[3L]]), as.raw(c(0x41, 0x97)))
})

test_that("space_padding reads unmarked UTF-8 as UTF-8 in a locale that isn't (D16.36, F1)", {
  # fread() leaves UTF-8 text unmarked. "voila" with a grave a ends in the bytes C3 A0, whose
  # A0 is a no-break space only when read byte by byte.
  voila <- intToUtf8(c(0x76, 0x6F, 0x69, 0x6C, 0xE0))
  text <- c(voila, intToUtf8(0xA0), paste0(intToUtf8(0x2003), "AP"), paste0(voila, " "))
  Encoding(text) <- "unknown"
  before <- Sys.getlocale("LC_CTYPE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", before)))
  set <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  testthat::skip_if(
    !nzchar(set) || l10n_info()[["UTF-8"]], "LC_CTYPE can't be set to a locale that isn't UTF-8."
  )
  read <- space_padding(text)
  expect_identical(read$blank, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(read$padded, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(lapply(read$trimmed[3:4], charToRaw), lapply(c("AP", voila), charToRaw))
  expect_true(all(validUTF8(read$trimmed)))
})

test_that("column letters and numbers convert both ways", {
  expect_equal(column_letters(c(1L, 26L, 27L, 52L, 703L)), c("A", "Z", "AA", "AZ", "AAA"))
  expect_equal(column_numbers(c("A", "z", "AA", "AAA", "a1")), c(1L, 26L, 27L, 703L, NA))
})

test_that("column numbers beyond the integer range are NA with no warning (D12.36)", {
  # FXSHRXW is the last letters name inside the integer range.
  expect_equal(column_numbers(c("FXSHRXW", "FXSHRXX")), c(.Machine$integer.max, NA))
  too_long <- c("ZZZZZZZ", "A", "ZZZZZZZZZZ", strrep("Z", 1000L))
  expect_no_warning(column_numbers(too_long))
  expect_equal(column_numbers(too_long), c(NA, 1L, NA, NA))
})

test_that("column_letters gives NA for NA (D12.36)", {
  expect_equal(column_letters(c(1L, NA, 27L)), c("A", NA, "AA"))
  expect_equal(column_letters(NA), NA_character_)
})

test_that("shorten_marked cuts a long value around its first marker (D12.28)", {
  expect_equal(shorten_marked("short ok<97>"), "short ok<97>")
  long <- paste0(strrep("a", 50), "ok<97>", strrep("b", 50))
  expect_equal(
    shorten_marked(long), paste0("...", strrep("a", 18), "ok<97>", strrep("b", 16), "...")
  )
})

test_that("mark_invalid_utf8 writes each invalid byte as <xx> and leaves valid text (D12.28)", {
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  expect_equal(mark_invalid_utf8(c("fine", bad, NA)), c("fine", "ok<97>", NA))
  expect_equal(mark_invalid_utf8(character()), character())
})

test_that("lang_pattern takes two or three lower-case letters and nothing else (D14.20)", {
  expect_equal(
    grepl(lang_pattern, c("en", "fra", "EN", "e", "engl", "en-CA", " en", "e1", "")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

# One value of each kind the encoding helpers meet: plain ASCII, valid UTF-8 unmarked and
# marked, latin1-marked text, text that isn't valid UTF-8, text marked as bytes, and NA.
encoding_kinds <- function() {
  unmarked <- "café"
  Encoding(unmarked) <- "unknown"
  bytes <- "café"
  Encoding(bytes) <- "bytes"
  c(
    plain = "cafe", unmarked = unmarked, utf8 = "café",
    latin1 = iconv("café", "UTF-8", "latin1"),
    invalid = rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9))), bytes = bytes, missing = NA
  )
}

# Each value's bytes, NA as NULL.
bytes_of <- function(x) lapply(x, function(value) if (is.na(value)) NULL else charToRaw(value))

test_that("utf8_marked() marks unmarked valid UTF-8 only, converting nothing (m63, D16.47)", {
  x <- encoding_kinds()
  out <- utf8_marked(x)
  expect_equal(
    unname(Encoding(out)),
    c("unknown", "UTF-8", "UTF-8", "latin1", "unknown", "bytes", "unknown")
  )
  expect_identical(bytes_of(out), bytes_of(x))
  expect_identical(utf8_marked(character()), character())
})

test_that("utf8_converted() converts what isn't valid UTF-8, never text marked as bytes (m63)", {
  skip_if_not(l10n_info()$`UTF-8`, "the session's locale isn't UTF-8")
  x <- encoding_kinds()
  out <- utf8_converted(x)
  expect_equal(out[["latin1"]], "café")
  expect_equal(Encoding(out[["latin1"]]), "UTF-8")
  # In a UTF-8 locale native text that isn't valid UTF-8 keeps its bytes, unreadable, though
  # enc2utf8() marks it UTF-8; every other value keeps its mark too.
  expect_identical(bytes_of(out[-4L]), bytes_of(x[-4L]))
  expect_identical(Encoding(out[-(4:5)]), Encoding(x[-(4:5)]))
  expect_identical(utf8_converted(character()), character())
})

test_that("bytes_unmarked() drops the bytes mark only, keeping the bytes (m63, D16.36)", {
  x <- encoding_kinds()
  out <- bytes_unmarked(x)
  expect_equal(Encoding(out[["bytes"]]), "unknown")
  expect_identical(bytes_of(out), bytes_of(x))
  expect_identical(Encoding(out[-6L]), Encoding(x[-6L]))
  expect_identical(bytes_unmarked(character()), character())
})
