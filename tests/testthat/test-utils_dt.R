# Tests for the data.table helpers (plan 3.6, 16.2; D12.14, D12.24, D12.45, D12.54).

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
  expect_equal(nrow(read$malformed), 0L)
  expect_equal(read$lines, 2:4)
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

test_that("an invalid byte's value is the text kept, its quotes undone (D12.45)", {
  path <- write_bytes(list(
    charToRaw("id,comments"),
    c(charToRaw("1,\"say \"\"hi\"\""), as.raw(0x97), charToRaw("\""))
  ))
  read <- read_csv_text(path)
  expect_equal(read$data$comments, "say \"hi\"<97>")
  expect_equal(read$invalid$value, "say \"hi\"<97>")
})

test_that("read_csv_text reads line 1 as the header, comma-separated (D12.45)", {
  numbered <- read_csv_text(write_bytes(list(charToRaw("name,2019"), charToRaw("A,1"))))
  expect_named(numbered$data, c("name", "2019"))
  expect_equal(nrow(numbered$data), 1L)
  spaced <- read_csv_text(write_bytes(list(
    charToRaw("species name"), charToRaw("Lake trout"), charToRaw("Brook trout")
  )))
  expect_named(spaced$data, "species name")
  expect_equal(spaced$data[["species name"]], c("Lake trout", "Brook trout"))
})

test_that("header names have quotes undone and are made unique (D12.54)", {
  read <- read_csv_text(write_bytes(list(
    charToRaw("code,code,\"say \"\"hi\"\"\",,x"), charToRaw("A,B,C,D,E")
  )))
  expect_named(read$data, c("code", "code.1", "say \"hi\"", "V4", "x"))
})

test_that("a ragged line, a blank line, an empty file and a stray quote are malformed (D12.54)", {
  ragged <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,ok"), charToRaw("2,has, a comma"), charToRaw("3,x")
  )))
  expect_equal(nrow(ragged$data), 1L)
  expect_equal(ragged$malformed, data.table::data.table(kind = "fields", line = 3L, fields = 2L))
  blank <- read_csv_text(write_bytes(list(
    charToRaw("code,label"), charToRaw("A,x"), raw(0), charToRaw("B,y"), charToRaw("C,z")
  )))
  expect_equal(blank$malformed$line, 3L)
  footer <- read_csv_text(write_bytes(list(
    charToRaw("code,label"), charToRaw("A,x"), raw(0), charToRaw("B,y")
  )))
  expect_equal(footer$malformed, data.table::data.table(kind = "fields", line = 3L, fields = 2L))
  empty <- withr::local_tempfile(fileext = ".csv")
  file.create(empty)
  expect_equal(read_csv_text(empty)$malformed$kind, "empty")
  quote <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,\"open"), charToRaw("2,next")
  )))
  expect_equal(quote$malformed$kind, "quote")
  expect_no_warning(read_csv_text(empty))
})

test_that("each row's line counts the lines of a quoted cell above it (D12.54)", {
  read <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,\"two"), charToRaw("lines\""), charToRaw("2,x")
  )))
  expect_equal(read$lines, c(2L, 4L))
})

test_that("line 1 is the header even where fread() would skip it (D12.56)", {
  titled <- read_csv_text(write_bytes(list(
    charToRaw("Lookup export"), charToRaw("id,name"), charToRaw("1,a"), charToRaw("2,b")
  )))
  expect_named(titled$data, "Lookup export")
  expect_equal(nrow(titled$data), 0L)
  expect_equal(titled$malformed, data.table::data.table(kind = "fields", line = 2L, fields = 1L))
  # In a one-column file, fread() takes a last line with more fields as its header.
  one_column <- read_csv_text(write_bytes(list(
    charToRaw("code"), charToRaw("A"), charToRaw("B"), charToRaw("C"), charToRaw("D,E,F")
  )))
  expect_equal(one_column$data$code, c("A", "B", "C"))
  expect_equal(one_column$lines, 2:4)
  expect_equal(
    one_column$malformed, data.table::data.table(kind = "fields", line = 5L, fields = 1L)
  )
  blank_first <- read_csv_text(write_bytes(list(raw(0), charToRaw("id,name"), charToRaw("1,a"))))
  expect_equal(ncol(blank_first$data), 0L)
  expect_equal(
    blank_first$malformed, data.table::data.table(kind = "fields", line = 2L, fields = 0L)
  )
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

test_that("fix_invalid_utf8 reports a bad name and bad cells in reading order (D12.45)", {
  bad <- function(bytes) {
    x <- rawToChar(as.raw(bytes))
    Encoding(x) <- "UTF-8"
    x
  }
  dt <- data.table::data.table(a = c("x", bad(c(0x6F, 0x97))), b = c(bad(c(0x70, 0x97)), "y"))
  data.table::setnames(dt, "b", bad(c(0x62, 0x97)))
  found <- fix_invalid_utf8(dt)
  expect_equal(found$row, c(0L, 1L, 2L))
  expect_equal(found$column, c(2L, 2L, 1L))
  expect_equal(found$value, c("b<97>", "p<97>", "o<97>"))
})
