# Tests for the data.table helpers (plan 3.6, 16.2; D12.14, D12.24, D12.45, D12.54, D12.58,
# D12.59).

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
  expect_equal(
    ragged$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 3L, fields = 2L)
  )
  blank <- read_csv_text(write_bytes(list(
    charToRaw("code,label"), charToRaw("A,x"), raw(0), charToRaw("B,y"), charToRaw("C,z")
  )))
  expect_equal(blank$malformed$line, 3L)
  footer <- read_csv_text(write_bytes(list(
    charToRaw("code,label"), charToRaw("A,x"), raw(0), charToRaw("B,y")
  )))
  expect_equal(
    footer$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 3L, fields = 2L)
  )
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

test_that("a ragged line is named by its file line, past the lines of quoted cells (D12.54)", {
  # fread() counts records, so a quoted cell over several lines must not shift the line.
  multi <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,\"two"), charToRaw("lines\""), charToRaw("2"),
    charToRaw("3,x")
  )))
  expect_named(multi$data, c("id", "comments"))
  expect_equal(multi$lines, 2L)
  expect_equal(
    multi$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 4L, fields = 2L)
  )
  two_cells <- read_csv_text(write_bytes(list(
    charToRaw("id,note"), charToRaw("1,\"a"), charToRaw("b"), charToRaw("c\""),
    charToRaw("2,\"d"), charToRaw("e\""), charToRaw("3"), charToRaw("4,z")
  )))
  expect_equal(two_cells$lines, c(2L, 5L))
  expect_equal(
    two_cells$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 7L, fields = 2L)
  )
})

test_that("a header cell with a line break above a ragged line is read by its lines (D12.54)", {
  read <- read_csv_text(write_bytes(list(
    charToRaw("\"id"), charToRaw("x\",name"), charToRaw("1,a"), charToRaw("2"), charToRaw("3,b")
  )))
  expect_named(read$data, c("id\nx", "name"))
  expect_equal(read$lines, 3L)
  expect_equal(
    read$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 4L, fields = 2L)
  )
})

test_that("an invalid byte in the line fread() discards doesn't defeat the match (D12.54)", {
  # fread() quotes the discarded line in its warning, so the warning's text is invalid too.
  expect_no_warning(ragged <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), charToRaw("1,2"), c(charToRaw("bad"), as.raw(0x97), charToRaw(",x,y")),
    charToRaw("3,4"), charToRaw("5,6")
  ))))
  expect_equal(nrow(ragged$data), 1L)
  expect_equal(ragged$lines, 2L)
  expect_equal(
    ragged$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 3L, fields = 2L)
  )
  expect_no_warning(footer <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), charToRaw("1,2"), charToRaw("3,4"), c(charToRaw("Source: caf"), as.raw(0xE9))
  ))))
  expect_equal(nrow(footer$data), 2L)
  expect_equal(footer$lines, 2:3)
  expect_equal(
    footer$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 4L, fields = 2L)
  )
})

test_that("a trailing line of only spaces or tabs is ignored, as fread() ignores it (D12.54)", {
  for (end in c("   \n", "   ", "\t \t", "  \n\t\n")) {
    path <- withr::local_tempfile(fileext = ".csv")
    writeBin(charToRaw(paste0("a,b\n1,2\n", end)), path)
    read <- read_csv_text(path)
    expect_equal(nrow(read$data), 1L)
    expect_equal(nrow(read$malformed), 0L)
  }
  # In a one-column file fread() reads such a line as a row, so a skipped line above the
  # header is still counted.
  path <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("\nx\n   \n"), path)
  read <- read_csv_text(path)
  expect_equal(ncol(read$data), 0L)
  expect_equal(
    read$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 2L, fields = 0L)
  )
  # With no newline after it, fread() drops such a line even in a one-column file: the rows
  # above it stay, and no line is taken for one above the header (D12.58).
  writeBin(charToRaw("x\na\nb\n   "), path)
  read <- read_csv_text(path)
  expect_equal(read$data$x, c("a", "b"))
  expect_equal(nrow(read$malformed), 0L)
})

test_that("a folder is an error from the read, as a missing file is (D12.54)", {
  expect_no_warning(expect_error(read_csv_text(tempdir())))
})

test_that("a title line above a ragged line is still read as the header (D12.56)", {
  plain <- read_csv_text(write_bytes(list(
    charToRaw("title"), charToRaw("id,name"), charToRaw("1,a"), charToRaw("2"), charToRaw("3,c")
  )))
  expect_named(plain$data, "title")
  expect_equal(nrow(plain$data), 0L)
  expect_equal(
    plain$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 2L, fields = 1L)
  )
  multi <- read_csv_text(write_bytes(list(
    charToRaw("title"), charToRaw("id,note"), charToRaw("1,\"a"), charToRaw("b\""),
    charToRaw("2"), charToRaw("3,c")
  )))
  expect_named(multi$data, "title")
  expect_equal(nrow(multi$data), 0L)
  expect_equal(
    multi$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 2L, fields = 1L)
  )
})

test_that("a file of only blank lines or spaces is empty, never a stop (D12.54)", {
  blank_only <- list(
    list(raw(0)), list(raw(0), raw(0)), list(charToRaw("   \r")), list(charToRaw("\t "))
  )
  for (lines in blank_only) {
    read <- read_csv_text(write_bytes(lines))
    expect_equal(ncol(read$data), 0L)
    expect_equal(nrow(read$invalid), 0L)
    expect_equal(read$lines, integer())
    expect_equal(
      read$malformed[, c("kind", "line", "fields")],
      data.table::data.table(kind = "empty", line = NA_integer_, fields = NA_integer_)
    )
  }
})

test_that("a missing file is an error from the read, even with a space in its name (D12.45)", {
  expect_error(read_csv_text(file.path(tempdir(), "no such file.csv")))
})

test_that("fread()'s warnings are recognised whatever the session's language (D12.54)", {
  withr::local_envvar(LANGUAGE = "fr")
  expect_no_warning(ragged <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,ok"), charToRaw("2,has, a comma"), charToRaw("3,x")
  ))))
  expect_equal(
    ragged$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 3L, fields = 2L)
  )
  empty <- withr::local_tempfile(fileext = ".csv")
  file.create(empty)
  expect_no_warning(read <- read_csv_text(empty))
  expect_equal(read$malformed$kind, "empty")
  expect_no_warning(quote <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,\"open"), charToRaw("2,next")
  ))))
  expect_equal(quote$malformed$kind, "quote")
  expect_equal(Sys.getenv("LANGUAGE"), "fr")
})

test_that("the session's language is put back whether it was set or not, and after an error", {
  withr::local_envvar(LANGUAGE = NA)
  read_csv_text(write_bytes(list(charToRaw("a"), charToRaw("1"))))
  expect_true(is.na(Sys.getenv("LANGUAGE", unset = NA)))
  withr::local_envvar(LANGUAGE = "de")
  expect_error(read_csv_text(file.path(tempdir(), "no such file.csv")))
  expect_equal(Sys.getenv("LANGUAGE"), "de")
})

test_that("line 1 is the header even where fread() would skip it (D12.56)", {
  titled <- read_csv_text(write_bytes(list(
    charToRaw("Lookup export"), charToRaw("id,name"), charToRaw("1,a"), charToRaw("2,b")
  )))
  expect_named(titled$data, "Lookup export")
  expect_equal(nrow(titled$data), 0L)
  expect_equal(
    titled$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 2L, fields = 1L)
  )
  # In a one-column file, fread() takes a last line with more fields as its header.
  one_column <- read_csv_text(write_bytes(list(
    charToRaw("code"), charToRaw("A"), charToRaw("B"), charToRaw("C"), charToRaw("D,E,F")
  )))
  expect_equal(one_column$data$code, c("A", "B", "C"))
  expect_equal(one_column$lines, 2:4)
  expect_equal(
    one_column$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 5L, fields = 1L)
  )
  blank_first <- read_csv_text(write_bytes(list(raw(0), charToRaw("id,name"), charToRaw("1,a"))))
  expect_equal(ncol(blank_first$data), 0L)
  expect_equal(
    blank_first$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 2L, fields = 0L)
  )
})

test_that("malformed has a column for each slot its kinds fill, NA where unused (D12.58)", {
  ragged <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), charToRaw("1,2"), charToRaw("3"), charToRaw("4,5")
  )))
  expect_named(ragged$malformed, c("kind", "line", "fields", "n_records", "n_read", "value"))
  expect_equal(nrow(ragged$malformed), 1L)
  expect_true(all(is.na(unlist(ragged$malformed[, c("n_records", "n_read", "value")]))))
  clean <- read_csv_text(write_bytes(list(charToRaw("a,b"), charToRaw("1,2"))))
  expect_named(clean$malformed, c("kind", "line", "fields", "n_records", "n_read", "value"))
  expect_equal(nrow(clean$malformed), 0L)
})

test_that("one malformed line is one problem, never short or unknown beside it (D12.58)", {
  files <- list(
    ragged = c("id,comments", "1,ok", "2,has, a comma", "3,x"),
    blank_line = c("code,label", "A,x", "", "B,y", "C,z"),
    footer = c("code,label", "A,x", "", "B,y"),
    multi_line = c("id,comments", "1,\"two", "lines\"", "2", "3,x"),
    title = c("Lookup export", "id,name", "1,a", "2,b"),
    blank_first = c("", "id,name", "1,a"),
    one_column = c("code", "A", "B", "D,E,F"),
    quote = c("id,comments", "1,\"open", "2,next")
  )
  for (name in names(files)) {
    read <- read_csv_text(write_bytes(lapply(files[[name]], charToRaw)))
    expect_equal(read$malformed$kind, if (name == "quote") "quote" else "fields", info = name)
  }
  empty <- withr::local_tempfile(fileext = ".csv")
  file.create(empty)
  expect_equal(read_csv_text(empty)$malformed$kind, "empty")
})

test_that("a warning the reader doesn't know is one unknown problem, its text kept (D12.58)", {
  real_fread <- data.table::fread
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    warning(paste0("A new warning about ", bad), call. = FALSE, domain = NA)
    out
  })
  expect_no_warning(read <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), charToRaw("1,2"), charToRaw("3,4")
  ))))
  expect_equal(read$data$a, c("1", "3"))
  expect_equal(read$lines, 2:3)
  expect_equal(read$malformed$kind, "unknown")
  expect_true(is.na(read$malformed$line))
  # fread()'s text as kept, an invalid byte shown as <xx> (D12.24).
  expect_equal(read$malformed$value, "A new warning about ok<97>")
})

test_that("a reworded early stop is one unknown problem, never lines above the header (D12.58)", {
  real_fread <- data.table::fread
  local_mocked_bindings(fread = function(...) {
    withCallingHandlers(real_fread(...), warning = function(w) {
      warning(
        sub("Stopped early", "Halted", conditionMessage(w), fixed = TRUE),
        call. = FALSE, domain = NA
      )
      invokeRestart("muffleWarning")
    })
  })
  expect_no_warning(read <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,ok"), charToRaw("2,has, a comma"), charToRaw("3,x")
  ))))
  expect_named(read$data, c("id", "comments"))
  expect_equal(read$data$id, "1")
  expect_equal(read$lines, 2L)
  expect_equal(read$malformed$kind, "unknown")
  expect_match(read$malformed$value, "^Halted on line 3")
})

test_that("rows fread() leaves out without a warning are one short problem (D12.58)", {
  real_fread <- data.table::fread
  local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    out[seq_len(max(nrow(out) - 1L, 0L))]
  })
  # Three records after the header: a newline inside quotes and a blank line aren't records.
  expect_no_warning(read <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), charToRaw("1,2"), charToRaw("3,\"x"), charToRaw("y\""), charToRaw("5,6"),
    raw(0)
  ))))
  expect_equal(read$data$a, c("1", "3"))
  expect_equal(read$lines, 2:3)
  expect_equal(read$malformed$kind, "short")
  expect_equal(read$malformed$n_records, 3L)
  expect_equal(read$malformed$n_read, 2L)
  expect_true(is.na(read$malformed$line))
})

test_that("line 1 repeated where fread() takes its header is still a skip, never short (D12.58)", {
  # fread() skips to line 3, whose names are line 1's: the lines it skipped are found by
  # the line it skipped to, not taken for rows left out.
  read <- read_csv_text(write_bytes(list(
    charToRaw("id,name"), charToRaw("x"), charToRaw("id,name"), charToRaw("1,a")
  )))
  expect_equal(read$malformed$kind, "fields")
})

test_that("an invalid byte gives no unknown problem, nor an encoding warning beside it (D12.58)", {
  byte <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), c(charToRaw("1,a"), as.raw(0x97), charToRaw("b"))
  )))
  expect_equal(nrow(byte$invalid), 1L)
  expect_equal(nrow(byte$malformed), 0L)
  real_fread <- data.table::fread
  local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    warning("GB-18030 encoding detected, however fread() is unable to decode it.", call. = FALSE)
    out
  })
  # The encoding warning is spec_encoding_invalid's where the file has an invalid byte, and
  # an unknown problem where it has none.
  coded <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), c(charToRaw("1,"), as.raw(c(0xC4, 0xE3)))
  )))
  expect_equal(nrow(coded$invalid), 1L)
  expect_equal(nrow(coded$malformed), 0L)
  plain <- read_csv_text(write_bytes(list(charToRaw("a,b"), charToRaw("1,2"))))
  expect_equal(plain$malformed$kind, "unknown")
})

test_that("rows only the file's read leaves out are short through the skip check (D12.58)", {
  real_fread <- data.table::fread
  # Only the file's read loses its last row; the lines read on their own read in full.
  local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    if ("file" %in% names(list(...))) out[seq_len(max(nrow(out) - 1L, 0L))] else out
  })
  # A line is missing below fread()'s header, so the skip check runs: line 1 has fread()'s
  # names and line 2 doesn't, so the missing line is a row left out, not a skipped line.
  expect_no_warning(read <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), charToRaw("1,2"), charToRaw("3,4"), charToRaw("5,6")
  ))))
  expect_named(read$data, c("a", "b"))
  expect_equal(read$data$a, c("1", "3"))
  expect_equal(read$lines, 2:3)
  expect_equal(read$malformed$kind, "short")
  expect_equal(read$malformed$n_records, 3L)
  expect_equal(read$malformed$n_read, 2L)
})

test_that("a warning from reading one line on its own isn't the file's (D12.58)", {
  real_fread <- data.table::fread
  local_mocked_bindings(fread = function(...) {
    out <- real_fread(...)
    if ("file" %in% names(list(...))) {
      out[seq_len(max(nrow(out) - 1L, 0L))]
    } else {
      warning("A warning about one line.", call. = FALSE)
      out
    }
  })
  expect_no_warning(read <- read_csv_text(write_bytes(list(
    charToRaw("a,b"), charToRaw("1,2"), charToRaw("3,4"), charToRaw("5,6")
  ))))
  expect_equal(read$malformed$kind, "short")
})

test_that("a title line is the header beside an encoding warning and a bad byte (D12.56, D12.58)", {
  # fread() skips the GB-18030 mark with a warning; spec_encoding_invalid covers that
  # warning only where a bad byte it reads gives a finding.
  mark <- as.raw(c(0x84, 0x31, 0x95, 0x33))
  titled <- read_csv_text(write_bytes(list(
    c(mark, charToRaw("Title")), charToRaw("id,name"), c(charToRaw("1,a"), as.raw(0x97)),
    charToRaw("2,b")
  )))
  expect_named(titled$data, "Title")
  expect_equal(nrow(titled$data), 0L)
  # Line 2 and the lines after it aren't read, so neither is the bad byte on line 3, and
  # fread()'s warning is no other finding's.
  expect_equal(titled$malformed$kind, c("fields", "unknown"))
  expect_equal(titled$malformed$line, c(2L, NA))
  expect_equal(titled$malformed$fields, c(1L, NA))
  expect_match(titled$malformed$value[[2L]], "^GB-18030 encoding detected")
  expect_equal(nrow(titled$invalid), 0L)
  # With no bad byte the warning is unknown and the read is left as fread() made it.
  no_byte <- read_csv_text(write_bytes(list(
    c(mark, charToRaw("Title")), charToRaw("id,name"), charToRaw("1,a"), charToRaw("2,b")
  )))
  expect_equal(no_byte$malformed$kind, "unknown")
  # With no title line the bad byte is spec_encoding_invalid's, on its line.
  no_title <- read_csv_text(write_bytes(list(
    c(mark, charToRaw("id,name")), c(charToRaw("1,a"), as.raw(0x97)), charToRaw("2,b")
  )))
  expect_equal(nrow(no_title$malformed), 0L)
  expect_equal(no_title$invalid$row, 1L)
  expect_equal(no_title$lines, 2:3)
})

test_that("a session's warn = 2 doesn't stop the read, and its warn is put back (D12.59)", {
  withr::local_options(warn = 2)
  expect_no_error(ragged <- read_csv_text(write_bytes(list(
    charToRaw("id,comments"), charToRaw("1,ok"), charToRaw("2,has, a comma"), charToRaw("3,x")
  ))))
  expect_equal(
    ragged$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "fields", line = 3L, fields = 2L)
  )
  expect_equal(getOption("warn"), 2)
  expect_error(read_csv_text(tempdir()))
  expect_equal(getOption("warn"), 2)
})

test_that("fread()'s quote error on a one-column file is one quote problem, no rows (D12.59)", {
  # A blank line, then a line whose quote isn't closed or doubled: fread() stops with an
  # error, not a warning, on a file of one column.
  expect_no_warning(expect_no_error(read <- read_csv_text(write_bytes(list(
    charToRaw("code"), charToRaw("A"), raw(0), charToRaw("\"B\" extra")
  )))))
  expect_equal(
    read$malformed[, c("kind", "line", "fields")],
    data.table::data.table(kind = "quote", line = NA_integer_, fields = NA_integer_)
  )
  expect_equal(ncol(read$data), 0L)
  expect_equal(read$lines, integer())
  expect_equal(nrow(read$invalid), 0L)
  # Every other error still stops the read.
  expect_error(read_csv_text(file.path(tempdir(), "no such file.csv")))
})

test_that("a warning the reader doesn't know before the quote error is kept (D12.58, D12.59)", {
  real_fread <- data.table::fread
  local_mocked_bindings(fread = function(...) {
    warning("A new warning.", call. = FALSE)
    real_fread(...)
  })
  expect_no_warning(read <- read_csv_text(write_bytes(list(
    charToRaw("code"), charToRaw("A"), raw(0), charToRaw("\"B\" extra")
  ))))
  expect_equal(read$malformed$kind, c("quote", "unknown"))
  expect_equal(read$malformed$value, c(NA, "A new warning."))
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
