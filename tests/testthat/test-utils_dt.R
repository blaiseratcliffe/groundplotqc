# Tests for the data.table helpers (plan 3.6, 16.2; D12.14, D12.24).

write_bytes <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeBin(unlist(lapply(lines, function(x) c(x, charToRaw("\n")))), path)
  path
}

test_that("fread keeps a quoted field's doubled quotes, as read_csv_text() expects (D12.40)", {
  # read_csv_text() undoes them; a data.table that did so itself would make it cut a real
  # "" to one quote, so this fails first.
  expect_equal(data.table::fread(text = 'a\n"x""y"')$a, 'x""y')
})

test_that("read_csv_text reads a quoted blank and a spaces-only cell as NA (D12.27)", {
  path <- write_bytes(list(
    charToRaw("code,label"),
    charToRaw("A,\"\""),
    charToRaw("B,\"   \""),
    charToRaw("NA, NT_PSP")
  ))
  read <- read_csv_text(path)
  expect_equal(read$data$code, c("A", "B", "NA"))
  expect_equal(read$data$label, c(NA, NA, " NT_PSP"))
})

test_that("read_csv_text keeps quotes, spaces, NA text and leading zeros as written", {
  path <- write_bytes(list(
    charToRaw("code,label"),
    charToRaw("01,\" NT_PSP\""),
    charToRaw("NA,\"say \"\"hi\"\"\""),
    charToRaw("X,\"(T, S, O, L and X\"\").\"")
  ))
  read <- read_csv_text(path)
  expect_equal(read$data$code, c("01", "NA", "X"))
  expect_equal(read$data$label, c(" NT_PSP", "say \"hi\"", "(T, S, O, L and X\")."))
  expect_equal(nrow(read$invalid), 0L)
})

test_that("read_csv_text keeps an invalid byte as <97> and reports where", {
  path <- write_bytes(list(
    charToRaw("id,comments"),
    charToRaw("100.01,fine"),
    c(charToRaw("100.02,program"), as.raw(0x97), charToRaw(" confirm"))
  ))
  read <- read_csv_text(path)
  expect_equal(read$data$comments[[2L]], "program<97> confirm")
  expect_true(all(validUTF8(read$data$comments)))
  expect_equal(read$invalid$row, 2L)
  expect_equal(read$invalid$column, 2L)
})

test_that("fix_invalid_utf8 leaves valid text alone", {
  dt <- data.table::data.table(a = c("café", NA))
  expect_equal(nrow(fix_invalid_utf8(dt)), 0L)
  expect_equal(dt$a, c("café", NA))
})

test_that("fix_invalid_utf8 fixes a column name too, as row 0 (D12.28)", {
  bad_name <- rawToChar(as.raw(c(0x6E, 0x61, 0x97, 0x6D, 0x65)))
  Encoding(bad_name) <- "UTF-8"
  dt <- data.table::data.table(a = "x", b = "y")
  data.table::setnames(dt, "b", bad_name)
  found <- fix_invalid_utf8(dt)
  expect_equal(names(dt), c("a", "na<97>me"))
  expect_equal(found$row, 0L)
  expect_equal(found$column, 2L)
  expect_equal(found$value, "na<97>me")
})
