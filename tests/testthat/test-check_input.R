# Tests for the tables a conformance run reads, matched to its specification (plan 6.1,
# 6.2, 16.2; D7.5, D9.24, D16.9, D16.13, D16.14).

test_that("validate_tables takes a named list of data.frames and refuses anything else", {
  fish <- fx_fish_clean()
  expect_invisible(validate_tables(fish))
  expect_no_error(validate_tables(list()))
  expect_error(validate_tables(fish$stations), "must be a list of data.frames")
  expect_error(validate_tables(unname(fish)), "each named once")
  expect_error(validate_tables(setNames(fish, c("a", "a", "b"))), "each named once")
  expect_error(validate_tables(setNames(fish, c("a", " ", "b"))), "each named once")
  expect_error(validate_tables(list(a = 1:3)), "aren't data.frames: a")
  twice <- data.frame(x = 1, x = 2, check.names = FALSE)
  expect_error(validate_tables(list(t = twice)), "Table t has the columns x more than once")
  unnamed <- data.frame(x = 1)
  names(unnamed) <- ""
  expect_error(validate_tables(list(t = unnamed)), "has a column with no name")
  listed <- data.table::data.table(x = 1:2)
  listed$y <- list(1, 2)
  expect_error(validate_tables(list(t = listed)), "don't hold one value per row: y")
})

test_that("validate_tables refuses a table holding text marked as bytes (D16.38)", {
  marked <- "Hé"
  Encoding(marked) <- "bytes"
  refused <- function(table, columns) {
    paste0(
      "Table ", table, " has columns holding text marked as bytes: ", columns,
      "; read the table with its encoding, such as UTF-8 or latin1."
    )
  }
  spec <- fx_fish_spec()
  # A PK value: the run stops before any other check reads it.
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_id[[2L]] <- marked
  fish$hauls <- hauls
  expect_error(validate_tables(fish), refused("hauls", "haul_id"), fixed = TRUE)
  expect_error(gpq_check_conformance(fish, spec), refused("hauls", "haul_id"), fixed = TRUE)
  # A plain text column, and a factor's levels, each such column named in the table's order.
  hauls$gear <- structure(c(1L, 2L, 1L, 1L), levels = c("GN", marked), class = "factor")
  fish$hauls <- hauls
  expect_error(validate_tables(fish), refused("hauls", "haul_id, gear"), fixed = TRUE)
  fish <- fx_fish_clean()
  stations <- data.table::copy(fish$stations)
  stations$water_body[[1L]] <- marked
  fish$stations <- stations
  expect_error(
    gpq_check_conformance(fish, spec, layers = 1), refused("stations", "water_body"),
    fixed = TRUE
  )
  # Text marked latin1 or UTF-8 runs.
  stations$water_body <- c(iconv("Zürich", "UTF-8", "latin1"), "Málaga")
  fish$stations <- stations
  expect_invisible(validate_tables(fish))
  expect_no_error(gpq_check_conformance(fish, spec, layers = 1))
})

test_that("a type's family and a column's storage are named as 3.5 and the type map say", {
  expect_equal(
    type_family(c("integer", "double", "character", "character"), c(NA, NA, NA, "%Y-%m-%d")),
    c("numeric", "numeric", "character", "date")
  )
  expect_equal(storage_class(1L), "integer")
  expect_equal(storage_class(1.5), "double")
  expect_equal(storage_class("a"), "character")
  expect_equal(storage_class(NA), "logical")
  expect_equal(storage_class(factor("a")), "factor")
  expect_equal(storage_class(as.Date("2020-01-01")), "Date")
  expect_equal(storage_class(data.table::as.IDate("2020-01-01")), "IDate")
})

test_that("value_text keeps text as written and writes numbers in full", {
  expect_identical(value_text(c("", " a ", NA, "NA")), c("", " a ", NA, "NA"))
  expect_identical(value_text(c(1e5, 0.1, NA, NaN, Inf, -Inf)), c(
    "100000", "0.1", NA, "NaN", "Inf", "-Inf"
  ))
  expect_identical(value_text(c(1L, NA)), c("1", NA))
  expect_identical(value_text(factor(c("b", NA))), c("b", NA))
  expect_identical(value_text(as.Date("2020-06-01")), "2020-06-01")
  expect_identical(stored_text(factor("b")), "b")
  expect_null(stored_text(1:3))
})

test_that("coerce_column keeps a column of its class and names what a coercion loses (D16.9)", {
  same <- c(1.5, NA)
  expect_identical(coerce_column(same, "double")$value, same)
  expect_identical(coerce_column(1:2, "integer")$value, 1:2)
  expect_identical(coerce_column(1:2, "double")$value, c(1, 2))
  text <- c("12", "7.5", "abc", NA, "", "Inf", "NaN")
  as_double <- coerce_column(text, "double")
  expect_identical(as_double$value, c(12, 7.5, NA, NA, NA, Inf, NaN))
  expect_identical(as_double$lost, c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  as_integer <- coerce_column(text, "integer")
  expect_identical(as_integer$value, c(12L, NA, NA, NA, NA, NA, NA))
  expect_identical(as_integer$lost, c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))
  wide <- coerce_column(c(1, 2.5, 3e10, -Inf, NA), "integer")
  expect_identical(wide$value, c(1L, NA, NA, NA, NA))
  expect_identical(wide$lost, c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(coerce_column(factor(c("3", "x")), "integer")$lost, c(FALSE, TRUE))
  expect_identical(coerce_column(c(TRUE, NA), "integer")$lost, c(TRUE, FALSE))
  dates <- data.table::as.IDate(c("2020-06-01", NA))
  expect_identical(
    coerce_column(dates, "character", "%Y-%m-%d"),
    list(value = c("2020-06-01", NA), lost = c(FALSE, FALSE))
  )
  expect_identical(coerce_column(c(1.5, NA), "character")$value, c("1.5", NA))
  expect_identical(coerce_column(c("a", NA), "character")$lost, c(FALSE, FALSE))
})

test_that("coerce_column reads a factor, a date or a logical column once per distinct value", {
  # Each row takes its value's result, NA included (16.2).
  level <- factor(c("3", "x", "3", NA, "2.5"))
  expect_identical(coerce_column(level, "double")$value, c(3, NA, 3, NA, 2.5))
  expect_identical(coerce_column(level, "double")$lost, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(coerce_column(level, "integer")$value, c(3L, NA, 3L, NA, NA))
  expect_identical(coerce_column(level, "integer")$lost, c(FALSE, TRUE, FALSE, FALSE, TRUE))
  days <- data.table::as.IDate(c("2020-06-01", NA, "2020-06-01"))
  expect_identical(coerce_column(days, "integer")$lost, c(TRUE, FALSE, TRUE))
  stamps <- as.POSIXct(c("2020-06-01 10:00", "2020-06-02 10:00"), tz = "UTC")
  expect_identical(coerce_column(stamps, "double")$lost, c(TRUE, TRUE))
  expect_identical(
    coerce_column(c(TRUE, NA, FALSE, TRUE), "double")$lost, c(TRUE, FALSE, TRUE, TRUE)
  )
  # Six rows of three distinct values: value_text() writes three.
  real <- value_text
  given <- integer()
  testthat::local_mocked_bindings(value_text = function(x) {
    given <<- c(given, length(x))
    real(x)
  })
  repeated <- factor(c("3", "x", "3", "x", "2.5", "3"))
  expect_identical(
    coerce_column(repeated, "double")$lost, c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )
  expect_identical(given, 3L)
  # A date column stored as numbers, written as text for date_unparseable, is written once per
  # distinct value too: six rows, NA among them, of three distinct values.
  given <- integer()
  stored <- c(20200601L, 20200602L, 20200601L, NA, 20200602L, 20200601L)
  expect_identical(
    coerce_column(stored, "character", "%Y%m%d"),
    list(value = as.character(stored), lost = logical(6L))
  )
  expect_identical(given, 3L)
})

test_that("text is a number only when written as a decimal number (D16.42, m26)", {
  # A sign, digits, one point and an exponent, with spaces around; Inf and NaN spellings stay
  # numbers, value_not_finite's; hexadecimal and anything else is lost.
  text <- c(
    "0x10", "0X1p3", "0x1p9999", "1e3", " 7 ", "+.5", "-5.", "\t2E-2\n", "Inf", "-inf",
    "Infinity", "NaN", "1e", "1.2.3", paste0("5", intToUtf8(0xA0))
  )
  as_double <- coerce_column(text, "double")
  expect_identical(
    as_double$value, c(NA, NA, NA, 1000, 7, 0.5, -5, 0.02, Inf, -Inf, Inf, NaN, NA, NA, NA)
  )
  expect_identical(as_double$lost, rep(c(TRUE, FALSE, TRUE), c(3L, 9L, 3L)))
  as_integer <- coerce_column(c("0x10", "16", " 16 ", "1e1"), "integer")
  expect_identical(as_integer$value, c(NA, 16L, 16L, 10L))
  expect_identical(as_integer$lost, c(TRUE, FALSE, FALSE, FALSE))
})

test_that("coerce_column loses text that isn't valid UTF-8 without reading it (D16.33)", {
  # as.numeric() stops on such text in a UTF-8 locale.
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale, where R stops on it")
  invalid <- rawToChar(as.raw(c(0x32, 0x30, 0x97)))
  text <- c("12", invalid, NA)
  expect_identical(coerce_column(text, "double")$value, c(12, NA, NA))
  expect_identical(coerce_column(text, "double")$lost, c(FALSE, TRUE, FALSE))
  expect_identical(coerce_column(text, "integer")$value, c(12L, NA, NA))
  expect_identical(coerce_column(text, "integer")$lost, c(FALSE, TRUE, FALSE))
  # A factor's level can't be made by factor() from such text.
  level <- structure(1:2, levels = c("3", invalid), class = "factor")
  expect_identical(coerce_column(level, "double")$lost, c(FALSE, TRUE))
  # Latin-1-marked text is read as UTF-8 and text marked as bytes isn't read: neither is a
  # number (D16.36).
  marked <- "1é"
  Encoding(marked) <- "bytes"
  text <- c("12", iconv("1é", "UTF-8", "latin1"), marked, invalid)
  expect_silent(as_double <- coerce_column(text, "double"))
  expect_identical(as_double$value, c(12, NA, NA, NA))
  expect_identical(as_double$lost, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(coerce_column(text, "integer")$lost, c(FALSE, TRUE, TRUE, TRUE))
})

test_that("an exact name wins; a near one differs only in letter case or spaces (D16.14)", {
  matched <- match_columns(
    c("Depth ", "station_id", "water body", "STATION_ID", "extra"),
    c("station_id", "depth", "waterbody", "first_visit")
  )
  expect_identical(matched$column, c("station_id", "Depth ", "water body", NA))
  expect_identical(matched$near, c(FALSE, TRUE, TRUE, FALSE))
  expect_identical(matched$extra, c("STATION_ID", "extra"))
  # A name that isn't valid UTF-8 matches exactly or not at all, an attribute's too.
  bad <- rawToChar(as.raw(c(0x64, 0xe9, 0x70)))
  expect_identical(match_columns(c(bad, "x"), c(bad, "dep"))$column, c(bad, NA))
  unmatched <- match_columns(c("x", "dep"), c(bad, "dep"))
  expect_identical(unmatched$column, c(NA, "dep"))
  expect_identical(unmatched$near, c(FALSE, FALSE))
  expect_identical(unmatched$extra, "x")
  # Unicode spaces count in a name that isn't plain ASCII (D16.42, m21): a no-break space and
  # an em space.
  nbsp <- intToUtf8(0xA0)
  given <- c(
    paste0("dbh", nbsp), paste0("Sp", intToUtf8(c(0xE9, 0x2003)), "cies"), paste0("a", nbsp, "b")
  )
  spaced <- match_columns(given, c("dbh", paste0("sp", intToUtf8(0xE9), "cies"), "a b"))
  expect_identical(spaced$column, given)
  expect_identical(spaced$near, c(TRUE, TRUE, TRUE))
})

test_that("a name's Unicode spaces count in a locale that isn't UTF-8 (D16.36, F1)", {
  # Unmarked UTF-8, as fread() leaves a header, read as UTF-8 there too: "Voila" with an
  # a-grave, whose last byte reads as a no-break space byte by byte, is a near match of its
  # attribute in lower case, and a trailing no-break space is a space.
  given <- c(paste0("dbh", intToUtf8(0xA0)), intToUtf8(c(0x56, 0x6F, 0x69, 0x6C, 0xE0)))
  Encoding(given) <- "unknown"
  voila <- intToUtf8(c(0x76, 0x6F, 0x69, 0x6C, 0xE0))
  expect_identical(Encoding(voila), "UTF-8")
  before <- Sys.getlocale("LC_CTYPE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", before)))
  set <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  testthat::skip_if(
    !nzchar(set) || l10n_info()[["UTF-8"]], "LC_CTYPE can't be set to a locale that isn't UTF-8."
  )
  matched <- match_columns(given, c("dbh", voila))
  expect_identical(matched$column, given)
  expect_identical(matched$near, c(TRUE, TRUE))
})

test_that("fk_path walks the shortest path of keys, the dictionary's first key on a tie", {
  keys <- data.table::data.table(
    table_name = c("leaf", "leaf", "mid", "side", "top", "leaf"),
    attribute_name = c("mid_id", "side_id", "top_id", "top_id", "top_id", "leaf_id"),
    key_type = c("FK", "FK", "FK", "FK", "PK", "PK"), key_part = 1L,
    reference_table = c("mid", "side", "top", "top", NA, NA),
    reference_attribute = c("mid_id", "side_id", "top_id", "top_id", NA, NA)
  )
  path <- fk_path(keys, "leaf", "top")
  expect_equal(path$table_name, c("leaf", "mid"))
  expect_equal(path$reference_table, c("mid", "top"))
  expect_equal(fk_path(keys, "leaf", "side")$reference_table, "side")
  expect_null(fk_path(keys, "top", "leaf"))
  # The shortest path wins over the dictionary's first key: one step by top_id, though the key
  # to mid comes first.
  keys <- data.table::data.table(
    table_name = c("leaf", "leaf", "mid"), attribute_name = c("mid_id", "top_id", "top_id"),
    key_type = "FK", key_part = 1L, reference_table = c("mid", "top", "top"),
    reference_attribute = c("mid_id", "top_id", "top_id")
  )
  path <- fk_path(keys, "leaf", "top")
  expect_equal(path$attribute_name, "top_id")
  expect_equal(path$table_name, "leaf")
})

test_that("conformance_input matches tables and columns and lists what is missing or extra", {
  fish <- fx_fish_clean()
  fish$extra_table <- data.frame(a = 1)
  data.table::setnames(fish$hauls, "gear", "Gear")
  fish$catches$colour <- "red"
  fish$stations <- NULL
  input <- conformance_input(fish, fx_fish_spec())
  expect_equal(names(input$data), c("hauls", "catches"))
  expect_equal(input$missing, "stations")
  expect_equal(input$unexpected, "extra_table")
  expect_equal(input$given$table_name, c("hauls", "catches", "extra_table"))
  columns <- input$columns
  gear <- columns[columns$attribute_name == "gear"]
  expect_equal(gear$column, "Gear")
  expect_true(gear$near)
  expect_true(all(is.na(columns$column[columns$table_name == "stations"])))
  expect_false(any(columns$table_present[columns$table_name == "stations"]))
  expect_equal(input$extra$column, "colour")
  expect_equal(columns$family[columns$attribute_name == "haul_date"], "date")
  # hauls.haul_id and catches.catch_id are PKs; catches.haul_id is an FK.
  keyed <- columns$attribute_name %in% c("haul_id", "catch_id")
  expect_equal(columns$key_type[keyed], c("PK", "PK", "FK"))
  expect_true(columns$coded[columns$attribute_name == "species"])
  # No table given: every table is missing, and `given` keeps its columns.
  none <- conformance_input(list(), fx_fish_spec())
  expect_identical(
    none$given, data.table::data.table(table_name = character(), n_rows = integer())
  )
  expect_equal(none$missing, c("stations", "hauls", "catches"))
})

test_that("records find their group values through the shortest path, \"*\" where it breaks", {
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  # One catch points at no haul: its gear and water body can't be reached.
  catches <- data.table::copy(fish$catches)
  catches$haul_id[[1L]] <- "no_such_haul"
  fish$catches <- catches
  input <- conformance_input(fish, spec, group_cols = c("water_body", "gear"))
  stations <- input$groups$stations
  # A station can't reach gear (hauls point at stations, not the other way).
  expect_true(all(stations$values$gear == "*"))
  expect_equal(
    stations$values$water_body[stations$row_group], fish$stations$water_body
  )
  groups <- input$groups$catches
  reached <- groups$values[groups$row_group]
  expect_equal(reached$gear[[1L]], "*")
  expect_equal(reached$water_body[[1L]], "*")
  haul_at <- match(fish$catches$haul_id[-1L], fish$hauls$haul_id)
  expect_equal(reached$gear[-1L], fish$hauls$gear[haul_at])
  station_at <- match(fish$hauls$station_id[haul_at], fish$stations$station_id)
  expect_equal(reached$water_body[-1L], fish$stations$water_body[station_at])
  # Groups are numbered in the order of their text.
  ordered <- do.call(order, c(unname(as.list(groups$values)), method = "radix"))
  expect_equal(ordered, seq_len(nrow(groups$values)))
  # Without group columns every row is in one group.
  plain <- conformance_input(fish, spec)$groups$catches
  expect_equal(plain$row_group, rep(1L, nrow(fish$catches)))
  expect_null(plain$values)
})

test_that("values that read the same are one group: a blank, NA and a broken path all \"*\"", {
  fish <- fx_fish_clean()
  stations <- data.table::copy(fish$stations)
  stations$water_body <- c(" ", NA)
  fish$stations <- stations
  catches <- data.table::copy(fish$catches)
  catches$haul_id[[1L]] <- "no_such_haul"
  fish$catches <- catches
  input <- conformance_input(fish, fx_fish_spec(), group_cols = "water_body")
  expect_equal(input$groups$stations$values$water_body, "*")
  expect_equal(input$groups$catches$values$water_body, "*")
  expect_no_error(gpq_check_conformance(fish, fx_fish_spec(), group_cols = "water_body"))
})

test_that("an unmarked non-ASCII group value, as fread() leaves it, is a group as written", {
  # Valid UTF-8 left unmarked is marked UTF-8 in any locale, never converted (D16.47, I1);
  # order() refused it unmarked in a UTF-8 locale (D16.35).
  name <- "Zürich"
  Encoding(name) <- "unknown"
  expect_identical(Encoding(name), "unknown")
  fish <- fx_fish_clean()
  stations <- data.table::copy(fish$stations)
  stations$water_body <- c(name, "lake")
  fish$stations <- stations
  spec <- fx_fish_spec()
  values <- conformance_input(fish, spec, group_cols = "water_body")$groups$stations$values
  expect_identical(charToRaw(values$water_body[[1L]]), charToRaw(name))
  expect_identical(Encoding(values$water_body[[1L]]), "UTF-8")
  expect_equal(values$water_body[[2L]], "lake")
  results <- gpq_check_conformance(fish, spec, group_cols = "water_body", layers = 1)
  written <- vapply(results$results$water_body, function(v) paste(charToRaw(v), collapse = ""), "")
  expect_true(paste(charToRaw(name), collapse = "") %in% written)
})

test_that("a latin1-marked group value is converted to UTF-8 and written so (D16.36)", {
  name <- iconv("Zürich", "UTF-8", "latin1")
  # Latin-1 bytes that happen to be valid UTF-8 are converted too, never relabelled.
  odd <- iconv("Ã¼", "UTF-8", "latin1")
  expect_identical(Encoding(c(name, odd)), c("latin1", "latin1"))
  fish <- fx_fish_clean()
  stations <- data.table::copy(fish$stations)
  stations$water_body <- c(name, odd)
  fish$stations <- stations
  spec <- fx_fish_spec()
  values <- conformance_input(fish, spec, group_cols = "water_body")$groups$stations$values
  utf8 <- c("Zürich", "Ã¼")
  expect_identical(lapply(values$water_body, charToRaw), lapply(utf8, charToRaw))
  expect_identical(Encoding(values$water_body), c("UTF-8", "UTF-8"))
  results <- gpq_check_conformance(fish, spec, group_cols = "water_body", layers = 1)
  run_dir <- gpq_write_results(results, withr::local_tempdir())
  path <- file.path(run_dir, "tables", "results.csv")
  bytes <- readBin(path, "raw", file.size(path))
  for (text in utf8) {
    expect_true(length(grepRaw(charToRaw(text), bytes, fixed = TRUE)) > 0L, info = text)
  }
})

test_that("a group value that isn't valid UTF-8 is a caller error naming its column and row", {
  # Outside a UTF-8 locale the unmarked bytes below are native text, which converts (D16.36).
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  invalid <- rawToChar(as.raw(c(0x6C, 0x61, 0x97)))
  expect_false(validUTF8(invalid))
  spec <- fx_fish_spec()
  fish <- fx_fish_clean()
  stations <- data.table::copy(fish$stations)
  stations$water_body <- c("lake", invalid)
  fish$stations <- stations
  message <- paste0(
    "Group column water_body of table stations holds text that isn't valid UTF-8, ",
    "first in row 2; the table may have been read with the wrong encoding."
  )
  expect_error(conformance_input(fish, spec, group_cols = "water_body"), message, fixed = TRUE)
  # The run stops with it before any results are built.
  expect_error(
    gpq_check_conformance(fish, spec, group_cols = "water_body"), message,
    fixed = TRUE
  )
  # The first such row of the table the value is read from, however many follow it.
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$gear <- c("GN", invalid, "GN", invalid)
  fish$hauls <- hauls
  in_gear <- function(row) {
    paste0(
      "Group column gear of table hauls holds text that isn't valid UTF-8, first in row ",
      row, "; the table may have been read with the wrong encoding."
    )
  }
  expect_error(conformance_input(fish, spec, group_cols = "gear"), in_gear(2L), fixed = TRUE)
  # A latin1-marked value converts to UTF-8, so the first row it can't is named (D16.36).
  hauls$gear <- c(iconv("Gé", "UTF-8", "latin1"), "GN", invalid, "GN")
  fish$hauls <- hauls
  expect_error(conformance_input(fish, spec, group_cols = "gear"), in_gear(3L), fixed = TRUE)
  # Text marked as bytes has no encoding to convert from, even where its bytes are valid
  # UTF-8 (D16.36). The run refuses it before (D16.38), so this is the second line.
  marked <- "Gé"
  Encoding(marked) <- "bytes"
  expect_true(validUTF8(marked))
  hauls$gear <- c("GN", "GN", "GN", marked)
  fish$hauls <- hauls
  expect_error(conformance_input(fish, spec, group_cols = "gear"), in_gear(4L), fixed = TRUE)
  expect_error(
    gpq_check_conformance(fish, spec, group_cols = "gear"), "holding text marked as bytes: gear",
    fixed = TRUE
  )
})

test_that("a table name that isn't valid UTF-8 is checked, not a stop", {
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  tables <- c(fx_fish_clean(), structure(list(data.frame(a = 1)), names = bad))
  expect_invisible(validate_tables(tables))
  expect_error(validate_tables(structure(list(data.frame(a = 1)), names = " ")), "each named once")
})

test_that("a sentinel a key may hold reaches no parent", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  # "~" is the fish spec's character not-applicable sentinel, allowed in a foreign key.
  catches$haul_id[[2L]] <- "~"
  fish$catches <- catches
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_id[[1L]] <- "~"
  fish$hauls <- hauls
  input <- conformance_input(fish, fx_fish_spec(), group_cols = "gear")
  reached <- input$groups$catches$values$gear[input$groups$catches$row_group]
  expect_equal(reached[[2L]], "*")
})

test_that("every sentinel of a key's type is matched on neither side of a join (D16.42, m8)", {
  # "?", the fish spec's character missing sentinel, isn't one a foreign key may hold, and
  # still reaches no haul whose PK is "?".
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$haul_id[[2L]] <- "?"
  fish$catches <- catches
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_id[[1L]] <- "?"
  fish$hauls <- hauls
  input <- conformance_input(fish, fx_fish_spec(), group_cols = "gear")
  reached <- reach_column(input, "catches", "gear")
  expect_true(is.na(reached$rows[[2L]]))
  expect_false(1L %in% reached$rows)
  expect_equal(input$groups$catches$values$gear[input$groups$catches$row_group][[2L]], "*")
})

test_that("a key column is joined as given, its sentinels masked inside the match (R41, D16.26)", {
  fish <- fx_fish_clean()
  catches <- data.table::copy(fish$catches)
  catches$haul_id[[2L]] <- "~"
  fish$catches <- catches
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_id[[1L]] <- "~"
  fish$hauls <- hauls
  input <- conformance_input(fish, fx_fish_spec(), group_cols = "gear")
  keys <- key_values(input, "catches", "haul_id")
  expect_identical(data.table::address(keys$value), data.table::address(catches$haul_id))
  expect_identical(keys$open, c("?", "~"))
  expect_identical(catches$haul_id[[2L]], "~")
  # No key column is kept for the run (16.2).
  expect_identical(ls(input$cache), character())
  # The child's "~" doesn't reach the haul whose PK is "~".
  reached <- reach_column(input, "catches", "gear")
  expect_true(is.na(reached$rows[[2L]]))
  expect_false(1L %in% reached$rows)
  # A parent PK holding its type's sentinel is matched by no child, even by an FK typed apart
  # from it (D16.26): a text "-88" doesn't reach an integer PK -88.
  spec <- fx_fish_spec()
  typed <- data.table::copy(spec$attributes)
  whole <- typed[typed$attribute_name == "minutes"]
  at <- which(typed$table_name == "hauls" & typed$attribute_name == "haul_id")
  data.table::set(typed, i = at, j = "data_type", value = whole$data_type)
  data.table::set(typed, i = at, j = "r_class", value = whole$r_class)
  spec$attributes <- typed
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  catches <- data.table::copy(fish$catches)
  catches$haul_id <- as.character(match(catches$haul_id, hauls$haul_id))
  hauls$haul_id <- seq_len(nrow(hauls))
  hauls$haul_id[[1L]] <- -88L
  catches$haul_id[[1L]] <- "-88"
  fish$hauls <- hauls
  fish$catches <- catches
  reached <- reach_column(conformance_input(fish, spec), "catches", "gear")
  expect_true(is.na(reached$rows[[1L]]))
  expect_equal(reached$rows[catches$haul_id == "2"], rep(2L, sum(catches$haul_id == "2")))
  # A number-typed FK stored as text is read as numbers first (R41; D16.44, T1-Q1 (a)): "02"
  # reaches the PK stored as 2, and "-88" is still the sentinel.
  at <- which(typed$table_name == "catches" & typed$attribute_name == "haul_id")
  data.table::set(typed, i = at, j = "data_type", value = whole$data_type)
  data.table::set(typed, i = at, j = "r_class", value = whole$r_class)
  spec$attributes <- typed
  catches$haul_id <- sprintf("%02d", as.integer(catches$haul_id))
  fish$catches <- catches
  reached <- reach_column(conformance_input(fish, spec), "catches", "gear")
  expect_true(is.na(reached$rows[[1L]]))
  expect_true("02" %in% catches$haul_id)
  expect_equal(reached$rows[catches$haul_id == "02"], rep(2L, sum(catches$haul_id == "02")))
})

test_that("a text-typed key chain stored as numbers groups as its text twin (D16.42, m9)", {
  spec <- fx_fish_spec()
  # Character sentinels written in digits: as numbers they match only a number written as the
  # code, as in text ("007" isn't 7).
  sentinels <- data.table::copy(spec$sentinels)
  data.table::set(
    sentinels,
    i = which(sentinels$data_type == "character"), j = "value", value = c("-1", "007")
  )
  spec$sentinels <- sentinels
  fish <- fx_fish_clean()
  stations <- data.table::copy(fish$stations)
  hauls <- data.table::copy(fish$hauls)
  catches <- data.table::copy(fish$catches)
  # Every key stored as a number, each foreign key its parent's: haul 1 is numbered -1, the
  # sentinel, and its catches moved to haul 2; the last haul is numbered 7; catch 1 holds -1,
  # which reaches no haul, though one has the PK -1.
  hauls$station_id <- match(hauls$station_id, stations$station_id)
  at <- match(catches$haul_id, hauls$haul_id)
  stations$station_id <- seq_len(nrow(stations))
  numbered <- c(-1L, seq_len(nrow(hauls) - 1L)[-1L], 7L)
  hauls$haul_id <- numbered
  moved <- numbered
  moved[[1L]] <- 2L
  catches$haul_id <- moved[at]
  catches$haul_id[[1L]] <- -1L
  numbers <- list(stations = stations, hauls = hauls, catches = catches)
  twin <- lapply(numbers, data.table::copy)
  for (key in list(
    c("stations", "station_id"), c("hauls", "station_id"), c("hauls", "haul_id"),
    c("catches", "haul_id")
  )) {
    data.table::set(twin[[key[[1L]]]], j = key[[2L]], value = as.character(twin[[key]]))
  }
  group_cols <- c("water_body", "gear")
  stored <- conformance_input(numbers, spec, group_cols = group_cols)
  as_text <- conformance_input(twin, spec, group_cols = group_cols)
  for (table in names(numbers)) {
    expect_identical(stored$groups[[table]], as_text$groups[[table]], info = table)
  }
  reached <- stored$groups$catches$values$gear[stored$groups$catches$row_group]
  expect_equal(reached[[1L]], "*")
  expect_false(any(reached[catches$haul_id == 7L] == "*"))
  # Joined as stored: the key is the caller's column, no text made of it.
  keys <- key_values(stored, "catches", "haul_id")
  expect_identical(data.table::address(keys$value), data.table::address(catches$haul_id))
  # One side text and the other numbers: only the numeric side is written as text, the text
  # side read as it is, so a catch "07" reaches no haul 7.
  mixed <- numbers
  mixed$catches <- data.table::copy(twin$catches)
  seven <- which(catches$haul_id == 7L)[[1L]]
  data.table::set(mixed$catches, i = seven, j = "haul_id", value = "07")
  twin_seven <- lapply(twin, data.table::copy)
  data.table::set(twin_seven$catches, i = seven, j = "haul_id", value = "07")
  groups <- conformance_input(mixed, spec, group_cols = group_cols)$groups$catches
  expect_identical(
    groups, conformance_input(twin_seven, spec, group_cols = group_cols)$groups$catches
  )
  expect_equal(groups$values$gear[groups$row_group][[seven]], "*")
  # A numeric child against a text parent: the child is written as text.
  mixed <- numbers
  mixed$hauls <- twin$hauls
  expect_identical(
    conformance_input(mixed, spec, group_cols = group_cols)$groups$catches, as_text$groups$catches
  )
})

test_that("the problem unit's key is read where each record reaches it", {
  fish <- fx_fish_clean()
  input <- conformance_input(fish, fx_fish_spec(), key_col = "station_id")
  expect_equal(key_text(input, "stations", 1:2), fish$stations$station_id)
  haul_at <- match(fish$catches$haul_id, fish$hauls$haul_id)
  expect_equal(key_text(input, "catches", 1:3), fish$hauls$station_id[haul_at][1:3])
  expect_null(key_text(conformance_input(fish, fx_fish_spec()), "catches", 1L))
})

test_that("a record's PK is text, NA when missing, positional when missing or repeated", {
  fish <- fx_fish_clean()
  hauls <- data.table::copy(fish$hauls)
  hauls$haul_id[2:3] <- c("", hauls$haul_id[[1L]])
  fish$hauls <- hauls
  input <- conformance_input(fish, fx_fish_spec())
  pk <- record_pk_text(input, "hauls", 1:4)
  expect_identical(
    pk$record_pk, c(hauls$haul_id[[1L]], NA, hauls$haul_id[[1L]], hauls$haul_id[[4L]])
  )
  expect_identical(pk$positional, c(TRUE, TRUE, TRUE, FALSE))
  # A PK column not given: every PK is missing.
  fish$hauls$haul_id <- NULL
  gone <- record_pk_text(conformance_input(fish, fx_fish_spec()), "hauls", 1:2)
  expect_identical(gone$record_pk, c(NA_character_, NA_character_))
  expect_identical(gone$positional, c(TRUE, TRUE))
})

test_that("a composite PK's parts are joined with \"|\"", {
  spec <- fx_fish_spec()
  keys <- data.table::copy(spec$keys)
  keys <- rbind(keys, data.table::data.table(
    table_name = "catches", attribute_name = "species", key_type = "PK", key_part = 2L,
    reference_table = NA_character_, reference_attribute = NA_character_
  ))
  spec$keys <- keys
  fish <- fx_fish_clean()
  pk <- record_pk_text(conformance_input(fish, spec), "catches", 1:2)
  expect_identical(
    pk$record_pk, paste(fish$catches$catch_id[1:2], fish$catches$species[1:2], sep = "|")
  )
})

test_that("a record's PK is found whatever the keys table's other columns are named", {
  # Columns named like record_pk_text()'s variables are never read in their place.
  spec <- fx_fish_spec()
  keys <- data.table::copy(spec$keys)
  data.table::set(keys, j = c("table", "keys", "pk"), value = list("catches", "x", "y"))
  spec$keys <- keys
  fish <- fx_fish_clean()
  pk <- record_pk_text(conformance_input(fish, spec), "hauls", 1:2)
  expect_identical(pk$record_pk, fish$hauls$haul_id[1:2])
})

test_that("matching the input changes no table and copies none whole (16.2)", {
  fish <- fx_fish_clean()
  before <- lapply(fish, data.table::copy)
  catches_address <- data.table::address(fish$catches$weight)
  input <- conformance_input(
    fish, fx_fish_spec(),
    group_cols = c("water_body", "gear"), key_col = "station_id"
  )
  expect_identical(data.table::address(input$data$catches$weight), catches_address)
  # The PK helpers read the tables too.
  for (table in names(fish)) {
    record_pk_text(input, table, seq_len(nrow(fish[[table]])))
  }
  for (table in names(fish)) {
    expect_identical(fish[[table]], before[[table]], info = table)
    expect_null(data.table::indices(fish[[table]]), info = table)
  }
  # data.frames are kept as the caller's, never made data.tables.
  frames <- lapply(fx_fish_clean(), as.data.frame)
  frames_before <- lapply(frames, data.table::copy)
  input <- conformance_input(frames, fx_fish_spec(), group_cols = "gear", key_col = "station_id")
  expect_identical(input$data, frames)
  expect_identical(
    data.table::address(input$data$catches$weight), data.table::address(frames$catches$weight)
  )
  for (table in names(frames)) {
    record_pk_text(input, table, seq_len(nrow(frames[[table]])))
    expect_false(data.table::is.data.table(frames[[table]]), info = table)
    expect_identical(frames[[table]], frames_before[[table]], info = table)
  }
})
