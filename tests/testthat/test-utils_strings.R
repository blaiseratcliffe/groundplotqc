# Tests for the string helpers (plan 3.3, 3.6; D12.14, D12.27, D12.36).

test_that("as_text gives UTF-8 text with blanks as NA", {
  expect_equal(as_text(c("a", "", NA)), c("a", NA, NA))
  expect_equal(as_text(factor(c("x", "y"))), c("x", "y"))
  expect_equal(as_text(c(1, -9)), c("1", "-9"))
})

test_that("blank_to_na makes only cells empty after trimming NA (D12.27)", {
  expect_equal(
    blank_to_na(c("", "   ", "\t", "NA", " NT_PSP", NA)),
    c(NA, NA, NA, "NA", " NT_PSP", NA)
  )
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  expect_identical(blank_to_na(bad), bad)
})
