# Tests for the tables a conformance run reads: CSV paths (plan 15, 9.5; D9.23, D17.1 to
# D17.5, D17.14, D17.16) and, from task 3, a DBI connection with table_map (D17.6).

run_id <- "20261014T120000Z_abc123"

# The fish survey's tables written as CSV files in `dir` with data.table::fwrite(): their
# paths, named by table.
fish_csv <- function(dir, tables = fx_fish_clean()) {
  paths <- file.path(dir, paste0(names(tables), ".csv"))
  names(paths) <- names(tables)
  for (name in names(tables)) {
    data.table::fwrite(tables[[name]], paths[[name]])
  }
  paths
}

# A file in `dir` holding `bytes`: its path.
raw_csv <- function(dir, file, bytes) {
  path <- file.path(dir, file)
  writeBin(bytes, path)
  path
}

test_that("CSV paths are read as text as written, and checked as the data.frames are (D9.23)", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  paths <- fish_csv(dir, fish)
  spec <- fx_fish_spec()
  read <- read_tables(as.list(paths), spec)
  expect_equal(read$text_tables, names(fish))
  for (name in names(fish)) {
    expect_true(all(vapply(read$tables[[name]], is.character, logical(1))))
    expect_equal(as.list(read$tables[[name]]), lapply(as.list(fish[[name]]), as.character))
  }
  expect_equal(read$inputs$table_name, names(fish))
  expect_equal(read$inputs$file, unname(basename(paths)))
  expect_equal(read$inputs$sha256, unname(tools::sha256sum(paths)))
  expect_equal(read$inputs$detail, paste(vapply(fish, nrow, 1L), "rows; separator comma"))
  results <- gpq_check_conformance(
    paths, spec,
    group_cols = "water_body", layers = 0:2, run_id = run_id
  )
  rows <- results$results
  typed <- rows$rule_id %chin% c("class_mismatch", "integer_stored_as_double")
  expect_true(all(rows$status[typed] == "not_run" & rows$not_run_reason[typed] == "text_input"))
  # One "*" row per column for each storage-class rule, not a row per group (D16.29, D17.15).
  expect_equal(sum(rows$rule_id == "class_mismatch"), 15L)
  expect_equal(sum(rows$rule_id == "integer_stored_as_double"), 4L)
  expect_true(all(rows$water_body[typed] == "*" & rows$n_not_evaluated[typed] == 1L))
  expect_equal(sum(rows$status == "fail"), 0L)
  expect_equal(nrow(results$issues), 0L)
  # Every other rule gives what it gives on the data.frames.
  framed <- gpq_check_conformance(
    fish, spec,
    group_cols = "water_body", layers = 0:2, run_id = run_id
  )$results
  kept <- c("rule_id", "table_name", "column_name", "water_body", "n_checked", "n_fail", "status")
  untyped <- function(x) {
    x[!x$rule_id %chin% c("class_mismatch", "integer_stored_as_double"), kept, with = FALSE]
  }
  expect_equal(untyped(rows), untyped(framed))
  inputs <- results$metadata$inputs
  expect_equal(inputs$input, paste0("tables:", names(fish)))
  expect_equal(inputs$file, unname(basename(paths)))
  expect_equal(inputs$sha256, unname(tools::sha256sum(paths)))
})

test_that("tables mixes data.frames and paths; a named character vector is all paths (D17.1)", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  paths <- fish_csv(dir, fish)
  mixed <- list(stations = fish$stations, hauls = paths[["hauls"]], catches = paths[["catches"]])
  stations_before <- data.table::copy(fish$stations)
  read <- read_tables(mixed, fx_fish_spec())
  # A data.frame given is kept as it is, never copied (16.2).
  expect_identical(data.table::address(read$tables$stations), data.table::address(fish$stations))
  expect_equal(read$text_tables, c("hauls", "catches"))
  results <- gpq_check_conformance(mixed, fx_fish_spec(), layers = 0:1, run_id = run_id)
  # The caller's list and its tables are as they were, and no index is left on the table given.
  expect_identical(names(mixed), c("stations", "hauls", "catches"))
  expect_identical(mixed$hauls, paths[["hauls"]])
  expect_identical(mixed$catches, paths[["catches"]])
  expect_identical(mixed$stations, stations_before)
  expect_identical(fish$stations, stations_before)
  expect_null(attr(fish$stations, "index"))
  inputs <- results$metadata$inputs
  expect_equal(inputs$file, c("in memory", "hauls.csv", "catches.csv"))
  expect_equal(is.na(inputs$sha256), c(TRUE, FALSE, FALSE))
  expect_equal(inputs$detail[[1L]], "2 rows")
  rows <- results$results[results$results$rule_id == "class_mismatch"]
  expect_equal(rows$status[rows$table_name == "stations"], rep("pass", 5L))
  read_as_text <- rows$table_name != "stations"
  expect_true(all(rows$status[read_as_text] == "not_run"))
  expect_true(all(rows$not_run_reason[read_as_text] == "text_input"))
  expect_equal(rows$n_not_evaluated[read_as_text], rep(1L, 10L))
  expect_equal(read_tables(paths, fx_fish_spec())$text_tables, names(fish))
})

test_that("a Windows-1252 CSV is converted per cell, the undefined bytes kept (D17.3)", {
  dir <- withr::local_tempdir()
  byte <- function(...) as.raw(c(...))
  stations <- raw_csv(dir, "stations.csv", c(
    charToRaw("station_id,station_name,water_body,depth,first_visit\nB"), byte(0xe9),
    charToRaw("cancour,A"), byte(0x81), charToRaw("B,Rivi"), byte(0xe8),
    charToRaw("re,1.5,2020-06-01\nS2,t2,LK,2.5,2020-06-02\n")
  ))
  hauls <- raw_csv(dir, "hauls.csv", c(
    charToRaw("haul_id,station_id,gear,minutes,haul_date\nH1,B"), byte(0xe9),
    charToRaw("cancour,GN,1,2020-06-01\nH2,S2,TN,2,2020-06-02\n")
  ))
  catches <- raw_csv(dir, "catches.csv", charToRaw(
    "catch_id,haul_id,species,fish_count,weight\nC1,H1,WAE,1,1.5\nC2,H2,NOP,2,2.5\n"
  ))
  paths <- c(stations = stations, hauls = hauls, catches = catches)
  read <- read_tables(paths, fx_fish_spec())
  first <- read$tables$stations[1L]
  expect_equal(first$station_id, "Bécancour")
  expect_equal(Encoding(first$station_id), "UTF-8")
  expect_equal(first$water_body, "Rivière")
  expect_equal(read$tables$hauls$station_id[[1L]], "Bécancour")
  # 0x81 has no Windows-1252 meaning: the cell keeps its bytes, never NA, on every platform.
  expect_identical(charToRaw(first$station_name), byte(0x41, 0x81, 0x42))
  expect_equal(read$inputs$detail, c(
    "2 rows; separator comma; 2 values converted from Windows-1252",
    "2 rows; separator comma; 1 value converted from Windows-1252",
    "2 rows; separator comma"
  ))
  results <- gpq_check_conformance(
    paths, fx_fish_spec(),
    group_cols = "water_body", layers = 0:2, run_id = run_id
  )
  # The group value runs, and the haul reaches it through the converted key.
  expect_true("Rivière" %in% results$results$water_body)
  hauls_rows <- results$results[results$results$table_name == "hauls"]
  expect_true(all(c("Rivière", "LK") %in% hauls_rows$water_body))
  out <- withr::local_tempdir()
  gpq_write_results(results, out)
  back <- gpq_read_results(file.path(out, run_id))
  expect_equal(back$results, results$results)
  expect_equal(back$issues, results$issues)
})

test_that("a header name holding a Windows-1252 byte is converted, and not counted (D17.3, R13)", {
  dir <- withr::local_tempdir()
  byte <- function(...) as.raw(c(...))
  # The header's fourth name holds 0xe9 ("dépth" in Windows-1252); the data hold no such byte.
  header_only <- raw_csv(dir, "header_only.csv", c(
    charToRaw("station_id,station_name,water_body,d"), byte(0xe9),
    charToRaw("pth,first_visit\nS1,t1,LK,1.5,2020-06-01\nS2,t2,RV,2.5,2020-06-02\n")
  ))
  read <- read_tables(list(stations = header_only), fx_fish_spec())
  name <- names(read$tables$stations)[[4L]]
  expect_equal(name, paste0("d", intToUtf8(0xe9), "pth"))
  expect_equal(Encoding(name), "UTF-8")
  expect_equal(names(read$tables$stations)[-4L], c(
    "station_id", "station_name", "water_body", "first_visit"
  ))
  # The header's conversion is no value converted: the count is of data values only.
  expect_equal(read$inputs$detail, "2 rows; separator comma")
  # With a data value converted beside it, the count is that value alone.
  both <- raw_csv(dir, "both.csv", c(
    charToRaw("station_id,station_name,water_body,d"), byte(0xe9),
    charToRaw("pth,first_visit\nB"), byte(0xe9), charToRaw("cancour,t1,LK,1.5,2020-06-01\n")
  ))
  read <- read_tables(list(stations = both), fx_fish_spec())
  expect_equal(names(read$tables$stations)[[4L]], paste0("d", intToUtf8(0xe9), "pth"))
  expect_equal(read$tables$stations$station_id, paste0("B", intToUtf8(0xe9), "cancour"))
  expect_equal(read$inputs$detail, "1 row; separator comma; 1 value converted from Windows-1252")
})

test_that("valid UTF-8 text is read back as written, with nothing converted (16.2, D17.3)", {
  dir <- withr::local_tempdir()
  hostile <- fx_hostile_text()
  stations <- data.table::data.table(
    station_id = paste0("S", seq_along(hostile)), station_name = hostile,
    water_body = "LK", depth = "1.5", first_visit = "2020-06-01"
  )
  path <- file.path(dir, "stations.csv")
  data.table::fwrite(stations, path)
  read <- read_tables(list(stations = path), fx_fish_spec())
  expect_equal(read$tables$stations$station_name, hostile)
  expect_equal(read$inputs$detail, paste(length(hostile), "rows; separator comma"))
})

test_that("from_windows_1252() converts only cells that aren't valid UTF-8 (D17.3)", {
  bytes <- function(...) rawToChar(as.raw(c(...)))
  x <- c("plain", "café", NA, bytes(0x63, 0x61, 0x66, 0xe9), bytes(0x41, 0x81), bytes(0x80))
  cells <- from_windows_1252(x)
  expect_equal(cells$rows, c(4L, 6L))
  expect_equal(cells$value, c("café", "€"))
  expect_true(all(Encoding(cells$value) == "UTF-8"))
  for (undefined in c(0x81, 0x8d, 0x8f, 0x90, 0x9d)) {
    expect_length(from_windows_1252(bytes(0x41, undefined, 0xe9))$rows, 0L)
  }
  expect_length(from_windows_1252(character())$rows, 0L)
})

test_that("an empty cell is NA, a quoted empty cell blank and NA text (D17.2)", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  stations <- raw_csv(dir, "stations.csv", charToRaw(paste0(
    "station_id,station_name,water_body,depth,first_visit\n",
    "S1,,LK,1.5,2020-06-01\nS2,\"\",RV,2.5,2020-06-02\nNA,t3,LK,3.5,2020-06-03\n"
  )))
  read <- read_tables(list(stations = stations), fx_fish_spec())$tables$stations
  expect_identical(read$station_name, c(NA, "", "t3"))
  expect_identical(read$station_id, c("S1", "S2", "NA"))
  results <- gpq_check_conformance(
    list(stations = stations, hauls = fish$hauls, catches = fish$catches), fx_fish_spec(),
    layers = 0:2, run_id = run_id
  )
  open <- results$issues[results$issues$status == "open" & results$issues$table_name == "stations"]
  open <- open[order(open$rule_id)]
  expect_equal(open$rule_id, c("blank_string", "na_text_in_id", "raw_na"))
  expect_equal(open$row_number, c(2L, 3L, 1L))
  expect_equal(open$value, c("", "NA", NA))
})

test_that("a header name holding a quote is read as written, as the data.frame has it (D17.2)", {
  dir <- withr::local_tempdir()
  frame <- data.table::data.table(station_id = "S1", note = "x")
  data.table::setnames(frame, "note", "say \"hi\"")
  path <- file.path(dir, "quoted_name.csv")
  data.table::fwrite(frame, path)
  read <- read_tables(list(stations = path), fx_fish_spec())$tables$stations
  expect_equal(names(read), names(frame))
  expect_equal(names(read)[[2L]], "say \"hi\"")
})

test_that("a value holding two quotes in a row is read back as written (D17.2)", {
  dir <- withr::local_tempdir()
  values <- c("say \"\"hi\"\"", "a\"\"b", "\"\"x", "x\"\"", "say \"hi\"", "\"")
  frame <- data.table::data.table(
    station_id = paste0("S", seq_along(values)), station_name = values,
    water_body = "LK", depth = "1.5", first_visit = "2020-06-01"
  )
  path <- file.path(dir, "stations.csv")
  data.table::fwrite(frame, path)
  read <- read_tables(list(stations = path), fx_fish_spec())$tables$stations
  expect_identical(read$station_name, values)
  expect_identical(read$station_id, frame$station_id)
})

test_that("a semicolon CSV is read with ';', its decimal commas lost in a number (D17.4)", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  stations <- raw_csv(dir, "stations.csv", charToRaw(paste0(
    "station_id;station_name;water_body;depth;first_visit\n",
    "S1;t1;LK;1,5;2020-06-01\nS2;t2;RV;2;2020-06-02\n"
  )))
  tables <- list(stations = stations, hauls = fish$hauls, catches = fish$catches)
  results <- gpq_check_conformance(tables, fx_fish_spec(), layers = 0:1, run_id = run_id)
  lost <- results$issues[results$issues$rule_id == "coercion_loss"]
  expect_equal(lost$column_name, "depth")
  expect_equal(lost$row_number, 1L)
  expect_equal(lost$value, "1,5")
  expect_equal(results$metadata$inputs$detail[[1L]], "2 rows; separator semicolon")
  # Each separator fread() finds is named by its word (D17.27).
  words <- c(tab = "\t", pipe = "|", space = " ")
  for (word in names(words)) {
    path <- raw_csv(dir, paste0(word, ".csv"), charToRaw(gsub(",", words[[word]], paste0(
      "station_id,station_name,water_body,depth,first_visit\n",
      "S1,t1,LK,1.5,2020-06-01\nS2,t2,RV,2.5,2020-06-02\n"
    ), fixed = TRUE)))
    read <- read_tables(list(stations = path), fx_fish_spec())
    expect_equal(read$inputs$detail, paste("2 rows; separator", word), info = word)
  }
})

test_that("a CSV fread() can't read cleanly stops the run before any check (D17.5)", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  header <- "station_id,station_name,water_body,depth,first_visit\n"
  bad <- list(
    ragged = paste0(header, "S1,t1,LK,1.5,2020-06-01\nS2,t2,RV,2.5,2020-06-02,extra\n"),
    short = paste0(header, "S1,t1,LK,1.5,2020-06-01\nS2,t2\n"),
    empty = ""
  )
  for (name in names(bad)) {
    path <- raw_csv(dir, paste0(name, ".csv"), charToRaw(bad[[name]]))
    tables <- list(stations = path, hauls = fish$hauls, catches = fish$catches)
    expect_error(
      gpq_check_conformance(tables, fx_fish_spec(), run_id = run_id),
      paste0(
        "Table stations: the file ", name, ".csv can't be read cleanly, so nothing was ",
        "checked. data.table::fread() said: "
      ),
      fixed = TRUE, info = name
    )
  }
  # A file that doesn't end with a line break may be cut off, inside a quoted last cell too,
  # which fread() reads without a word (D17.14); a CRLF ending is a line break.
  cut <- raw_csv(dir, "cut.csv", charToRaw(paste0(
    header, "S1,t1,LK,1.5,2020-06-01\nS2,t2,RV,2.5,\"2020-06"
  )))
  expect_error(
    gpq_check_conformance(
      list(stations = cut, hauls = fish$hauls, catches = fish$catches), fx_fish_spec(),
      run_id = run_id
    ),
    paste0(
      "Table stations: the file cut.csv doesn't end with a line break, so it may be cut off, ",
      "or was written without one; nothing was checked. Save it again, for example with ",
      "data.table::fwrite() or from Excel as CSV."
    ),
    fixed = TRUE
  )
  crlf <- raw_csv(dir, "crlf.csv", charToRaw(paste0(
    gsub("\n", "\r\n", header, fixed = TRUE), "S1,t1,LK,1.5,2020-06-01\r\n"
  )))
  expect_equal(read_tables(list(stations = crlf), fx_fish_spec())$tables$stations$depth, "1.5")
  # Under warn = 2 the stop is the same caller error.
  withr::local_options(warn = 2)
  path <- raw_csv(dir, "ragged2.csv", charToRaw(bad$ragged))
  expect_error(
    read_tables(list(stations = path), fx_fish_spec()),
    paste0(
      "Table stations: the file ragged2.csv can't be read cleanly, so nothing was checked. ",
      "data.table::fread() said: "
    ),
    fixed = TRUE
  )
})

test_that("a CSV whose header isn't its first line stops the run before any check (D17.16)", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  header <- "station_id,station_name,water_body,depth,first_visit\n"
  rows <- "S1,t1,LK,1.5,2020-06-01\nS2,t2,RV,2.5,2020-06-02\nS3,t3,LK,3.5,2020-06-03\n"
  # fread() would read each from a later line, without a word.
  moved <- list(
    ragged_first = paste0(header, "S0,t0,LK,0.5,2020-05-31,extra\n", rows),
    short_first = paste0(header, "S0,t0,LK\n", rows),
    short_header = paste0("station_id,station_name\n", rows),
    title = paste0("Fish stations 2020\n", header, rows)
  )
  for (name in names(moved)) {
    path <- raw_csv(dir, paste0(name, ".csv"), charToRaw(moved[[name]]))
    tables <- list(stations = path, hauls = fish$hauls, catches = fish$catches)
    expect_error(
      gpq_check_conformance(tables, fx_fish_spec(), run_id = run_id),
      paste0(
        "Table stations: the file ", name, ".csv can't be read cleanly, so nothing was ",
        "checked: data.table::fread() would take its header from a later line than the first. ",
        "Look for a title line above the header, or a line near the top with more or fewer ",
        "fields than the header."
      ),
      fixed = TRUE, info = name
    )
  }
  # A blank line above the header, a quoted line break in a header name, a quoted header of
  # one column (as write.csv() writes it) and a file of one column, a quoted separator in a
  # cell, are read from their first line.
  read_first <- list(
    blank_first = paste0("\n", header, rows),
    quoted_name = paste0("station_id,\"station\nname\",water_body,depth,first_visit\n", rows),
    quoted_one_column = "\"station_id\"\n\"S1\"\n",
    one_column = "station_id\nS1\n\"S,2\"\n"
  )
  for (name in names(read_first)) {
    path <- raw_csv(dir, paste0(name, ".csv"), charToRaw(read_first[[name]]))
    read <- read_tables(list(stations = path), fx_fish_spec())$tables$stations
    expect_equal(names(read)[[1L]], "station_id", info = name)
  }
  expect_equal(nrow(read), 2L)
  expect_equal(read$station_id, c("S1", "S,2"))
  # A file of one column has no separator to name in its inputs row.
  one <- read_tables(list(stations = file.path(dir, "one_column.csv")), fx_fish_spec())
  expect_equal(one$inputs$detail, "2 rows")
  # A file of one column has no separator, and fread() never moves its header: a title line
  # above it is read as the column's name, which Layer 0 reports (D17.24).
  titled <- raw_csv(dir, "titled.csv", charToRaw("Stations 2020\nstation_id\nS1\n"))
  read <- read_tables(list(stations = titled), fx_fish_spec())$tables$stations
  expect_equal(names(read), "Stations 2020")
  expect_equal(read[[1L]], c("station_id", "S1"))
})

test_that("tables that aren't data.frames or paths, or files that aren't there, are refused", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  expect_error(
    gpq_check_conformance(list(stations = 1, hauls = c("a", "b")), spec),
    paste0(
      "`tables` holds elements that are neither a data.frame nor the path of one CSV file: ",
      "stations, hauls."
    ),
    fixed = TRUE
  )
  absent <- file.path(dir, "nowhere.csv")
  expect_error(
    gpq_check_conformance(list(stations = absent, hauls = dir), spec),
    sprintf(
      "No file at the path given for these tables: stations (%s), hauls (%s).", absent, dir
    ),
    fixed = TRUE
  )
  # An unnamed list of paths is refused by validate_tables(), before any file is read (its
  # words in full from task 3, D17.27).
  expect_error(
    gpq_check_conformance(list(absent), spec),
    "`tables` must be a list of data.frames",
    fixed = TRUE
  )
})

test_that("a path with a space, a header-only file, a BOM and quoted cells are read as written", {
  dir <- withr::local_tempdir()
  folder <- file.path(dir, "fish survey")
  dir.create(folder)
  header <- "station_id,station_name,water_body,depth,first_visit\n"
  spaced <- raw_csv(folder, "stations.csv", charToRaw(paste0(header, "S1,t1,LK,1.5,2020-06-01\n")))
  read <- read_tables(list(stations = spaced), fx_fish_spec())
  expect_equal(read$tables$stations$station_id, "S1")
  expect_equal(read$inputs$file, "stations.csv")
  only_header <- raw_csv(dir, "header.csv", charToRaw(header))
  read <- read_tables(list(stations = only_header), fx_fish_spec())
  expect_equal(nrow(read$tables$stations), 0L)
  expect_equal(names(read$tables$stations), strsplit(trimws(header), ",")[[1L]])
  expect_equal(read$inputs$detail, "0 rows; separator comma")
  bom <- raw_csv(dir, "bom.csv", c(
    as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(paste0(header, "S1,t1,LK,1.5,2020-06-01\n"))
  ))
  read <- read_tables(list(stations = bom), fx_fish_spec())
  expect_equal(names(read$tables$stations)[[1L]], "station_id")
  quoted <- raw_csv(dir, "quoted.csv", charToRaw(paste0(
    header, "S1,\"two\nlines\",LK,1.5,2020-06-01\nS2,\"say \"\"hi\"\"\",RV,2.5,2020-06-02\n"
  )))
  read <- read_tables(list(stations = quoted), fx_fish_spec())$tables$stations
  expect_equal(read$station_name, c("two\nlines", "say \"hi\""))
})

test_that("a table of one row reads \"1 row\" in its inputs row, in memory or from a CSV (m5)", {
  dir <- withr::local_tempdir()
  fish <- fx_fish_clean()
  spec <- fx_fish_spec()
  fish$stations <- fish$stations[1L]
  # In memory: the one-row table beside tables of several rows.
  results <- gpq_check_conformance(fish, spec, layers = 0, run_id = run_id)
  expect_equal(results$metadata$inputs$detail, c("1 row", "4 rows", "8 rows"))
  # From CSV files: with a separator, with none, and with a converted value.
  paths <- fish_csv(dir, fish)
  read <- read_tables(as.list(paths), spec)
  expect_equal(
    read$inputs$detail,
    c("1 row; separator comma", "4 rows; separator comma", "8 rows; separator comma")
  )
  one_column <- raw_csv(dir, "one_column.csv", charToRaw("station_id\nS1\n"))
  expect_equal(read_tables(list(stations = one_column), spec)$inputs$detail, "1 row")
  converted <- raw_csv(dir, "converted.csv", c(
    charToRaw("station_id,station_name\nS1,B"), as.raw(0xe9), charToRaw("\n")
  ))
  expect_equal(
    read_tables(list(stations = converted), spec)$inputs$detail,
    "1 row; separator comma; 1 value converted from Windows-1252"
  )
})

# The fish survey's clean tables in an in-memory SQLite database: stations under its own name,
# hauls as fish_hauls, catches in the attached schema survey, and a table the spec lacks.
# The connection, closed when `env` ends.
fish_database <- function(env = parent.frame()) {
  fish <- fx_fish_clean()
  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(connection), envir = env)
  DBI::dbExecute(connection, "ATTACH DATABASE ':memory:' AS survey")
  DBI::dbWriteTable(connection, "stations", fish$stations)
  DBI::dbWriteTable(connection, "fish_hauls", fish$hauls)
  DBI::dbWriteTable(connection, DBI::Id(schema = "survey", table = "catches"), fish$catches)
  DBI::dbWriteTable(connection, "notes", data.frame(note = "t1"))
  connection
}

test_that("a connection's spec tables are read under table_map's names, a schema too (D17.6)", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  connection <- fish_database()
  before <- DBI::dbListTables(connection)
  results <- gpq_check_conformance(
    connection, fx_fish_spec(), c(hauls = "fish_hauls", catches = "survey.catches"),
    group_cols = "water_body", layers = 0:2, run_id = run_id
  )
  rows <- results$results
  # The database's other table raises nothing, and the stored types are checked: one passing
  # class_mismatch row per column, not a not_run row as for a table read from CSV.
  expect_equal(sum(rows$status == "fail"), 0L)
  expect_equal(nrow(results$issues), 0L)
  expect_false(any(rows$not_run_reason %in% "text_input"))
  expect_equal(rows$status[rows$rule_id == "class_mismatch"], rep("pass", 15L))
  expect_false("notes" %in% rows$table_name)
  inputs <- results$metadata$inputs
  expect_equal(inputs$input, paste0("tables:", c("stations", "hauls", "catches")))
  expect_true(all(inputs$file == "DBI connection, not hashed"))
  expect_true(all(is.na(inputs$sha256)))
  expect_equal(inputs$detail, c(
    "2 rows; table stations; driver SQLiteConnection",
    "4 rows; table fish_hauls; driver SQLiteConnection",
    "8 rows; table survey.catches; driver SQLiteConnection"
  ))
  # The connection is left open and unchanged.
  expect_true(DBI::dbIsValid(connection))
  expect_equal(DBI::dbListTables(connection), before)
})

test_that("a connection's table of one row reads \"1 row\" in its inputs row (m5)", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  connection <- fish_database()
  DBI::dbWriteTable(connection, "stations", fx_fish_clean()$stations[1L], overwrite = TRUE)
  results <- gpq_check_conformance(
    connection, fx_fish_spec(), c(hauls = "fish_hauls", catches = "survey.catches"),
    layers = 0, run_id = run_id
  )
  expect_equal(results$metadata$inputs$detail, c(
    "1 row; table stations; driver SQLiteConnection",
    "4 rows; table fish_hauls; driver SQLiteConnection",
    "8 rows; table survey.catches; driver SQLiteConnection"
  ))
})

test_that("a table_map schema the database lacks leaves its table missing, no stop (m12)", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  connection <- fish_database()
  results <- gpq_check_conformance(
    connection, fx_fish_spec(), c(hauls = "nope.fish_hauls", catches = "survey.catches"),
    layers = 0, run_id = run_id
  )
  missing <- results$results[results$results$rule_id == "table_missing"]
  expect_equal(
    missing$status[match(c("stations", "hauls", "catches"), missing$table_name)],
    c("pass", "fail", "pass")
  )
  expect_equal(results$metadata$inputs$input, paste0("tables:", c("stations", "catches")))
})

test_that("a wrong stored type is caught through a connection as it is in a data.frame (m13)", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  spec <- fx_fish_spec()
  fish <- fx_fish_clean()
  # fish_count, a whole number, stored as REAL with a fraction, and weight, a real number, as
  # TEXT.
  catches <- data.table::copy(fish$catches)
  catches$fish_count <- catches$fish_count + 0.5
  catches$weight <- as.character(catches$weight)
  fish$catches <- catches
  connection <- fish_database()
  DBI::dbWriteTable(
    connection, DBI::Id(schema = "survey", table = "catches"), catches,
    overwrite = TRUE
  )
  stored <- DBI::dbGetQuery(
    connection, "SELECT typeof(fish_count) AS count, typeof(weight) AS weight FROM survey.catches"
  )
  expect_equal(unique(stored$count), "real")
  expect_equal(unique(stored$weight), "text")
  from_db <- gpq_check_conformance(
    connection, spec, c(hauls = "fish_hauls", catches = "survey.catches"),
    group_cols = "water_body", layers = 0:1, run_id = run_id
  )
  framed <- gpq_check_conformance(
    fish, spec,
    group_cols = "water_body", layers = 0:1, run_id = run_id
  )
  expect_equal(from_db$results, framed$results)
  expect_equal(from_db$issues, framed$issues)
  open <- from_db$issues[from_db$issues$status == "open"]
  mismatch <- open[open$rule_id == "class_mismatch"]
  expect_equal(
    c(mismatch$table_name, mismatch$column_name, mismatch$value, mismatch$expected),
    c("catches", "weight", "character", "double")
  )
  swapped <- open[open$rule_id == "integer_stored_as_double"]
  expect_equal(
    c(swapped$table_name, swapped$column_name, swapped$value, swapped$expected),
    c("catches", "fish_count", "double", "integer")
  )
})

test_that("a spec table the database lacks is missing; table_map's faults are refused (D17.6)", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  connection <- fish_database()
  spec <- fx_fish_spec()
  # catches isn't mapped, so it is looked for as "catches", which the database lacks.
  results <- gpq_check_conformance(
    connection, spec, c(hauls = "fish_hauls"),
    layers = 0, run_id = run_id
  )
  missing <- results$results[results$results$rule_id == "table_missing"]
  expect_equal(missing$status[missing$table_name == "catches"], "fail")
  expect_equal(missing$status[missing$table_name != "catches"], c("pass", "pass"))
  # With table_map = NULL every table is looked for under its own name: only stations is there.
  unmapped <- gpq_check_conformance(connection, spec, NULL, layers = 0, run_id = run_id)
  missing <- unmapped$results[unmapped$results$rule_id == "table_missing"]
  expect_equal(
    missing$status[match(c("stations", "hauls", "catches"), missing$table_name)],
    c("pass", "fail", "fail")
  )
  expect_equal(unmapped$metadata$inputs$input, "tables:stations")
  expect_error(
    gpq_check_conformance(connection, spec, c(nets = "fish_nets")),
    "`table_map` names tables the specification doesn't have: nets.",
    fixed = TRUE
  )
  na_name <- c("fish_hauls", "fish_nets")
  names(na_name) <- c("hauls", NA)
  faults <- list(
    list(hauls = "fish_hauls"), "fish_hauls", c(hauls = NA_character_), c(hauls = ""),
    c(hauls = "fish_hauls", "fish_nets"), c(hauls = "fish_hauls", hauls = "fish_nets"),
    na_name, character(),
    # A value that starts or ends with a dot has an empty part (m11).
    c(hauls = ".fish_hauls"), c(hauls = "survey."), c(hauls = ".")
  )
  for (bad in faults) {
    expect_error(
      gpq_check_conformance(connection, spec, bad),
      "`table_map` must be NULL or a named character vector",
      fixed = TRUE
    )
  }
  expect_error(
    gpq_check_conformance(fx_fish_clean(), spec, table_map = c(stations = "st")),
    "`table_map` is only read when `tables` is a DBI connection.",
    fixed = TRUE
  )
})

test_that("a spec table whose own name holds a dot is read under that name, as given (D17.6)", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  # The fish spec with its table stations named "fish.stations".
  dictionary <- utils::read.csv(
    example_file("fish_dictionary.csv"),
    na.strings = "", stringsAsFactors = FALSE
  )
  dictionary[!is.na(dictionary) & dictionary == "stations"] <- "fish.stations"
  spec <- gpq_read_spec(
    dictionary = dictionary, code_lists = fx_fish_code_lists(),
    column_map = fx_fish_column_map(), type_map = gpq_type_map(example_file("fish_types.csv")),
    sentinels = gpq_sentinels(
      numeric = c(missing = -99, not_applicable = -88),
      character = c(missing = "?", not_applicable = "~"),
      date = c(missing = "?", not_applicable = "~")
    )
  )
  fish <- fx_fish_clean()
  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(connection))
  DBI::dbExecute(connection, "ATTACH DATABASE ':memory:' AS fish")
  # The table of that name in the main schema, and, as a trap, a table stations in a schema
  # fish, which a name split at its dot would reach.
  DBI::dbWriteTable(connection, "fish.stations", fish$stations)
  DBI::dbWriteTable(connection, DBI::Id(schema = "fish", table = "stations"), fish$stations[1L])
  DBI::dbWriteTable(connection, "hauls", fish$hauls)
  DBI::dbWriteTable(connection, "catches", fish$catches)
  results <- gpq_check_conformance(connection, spec, layers = 0:2, run_id = run_id)
  expect_equal(sum(results$results$status == "fail"), 0L)
  inputs <- results$metadata$inputs
  expect_equal(inputs$input, paste0("tables:", c("fish.stations", "hauls", "catches")))
  expect_equal(inputs$detail, c(
    "2 rows; table fish.stations; driver SQLiteConnection",
    "4 rows; table hauls; driver SQLiteConnection",
    "8 rows; table catches; driver SQLiteConnection"
  ))
})

test_that("tables that isn't a named list, paths or a connection is refused in full (D17.27)", {
  expect_error(
    gpq_check_conformance(fx_fish_clean()$stations, fx_fish_spec()),
    paste0(
      "`tables` must be a list of data.frames or CSV paths, each named once by its table, ",
      "a named character vector of paths, or a DBI connection."
    ),
    fixed = TRUE
  )
})

test_that("db_table_id() splits \"schema.table\" at its first dot (D17.6)", {
  testthat::skip_if_not_installed("DBI")
  expect_identical(db_table_id("catches"), "catches")
  expect_equal(db_table_id("survey.catches"), DBI::Id(schema = "survey", table = "catches"))
  expect_equal(
    db_table_id("survey.catches.2024"), DBI::Id(schema = "survey", table = "catches.2024")
  )
})
