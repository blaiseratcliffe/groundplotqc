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

test_that("column letters and numbers convert both ways", {
  expect_equal(column_letters(c(1L, 26L, 27L, 52L, 703L)), c("A", "Z", "AA", "AZ", "AAA"))
  expect_equal(column_numbers(c("A", "z", "AA", "AAA", "a1")), c(1L, 26L, 27L, 703L, NA))
})

test_that("shorten_marked cuts a long value around its first marker (D12.28)", {
  expect_equal(shorten_marked("short ok<97>"), "short ok<97>")
  long <- paste0(strrep("a", 50), "ok<97>", strrep("b", 50))
  expect_equal(
    shorten_marked(long), paste0("...", strrep("a", 18), "ok<97>", strrep("b", 16), "...")
  )
})
