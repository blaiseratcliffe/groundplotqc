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
  read <- read_tables(mixed, fx_fish_spec())
  # A data.frame given is kept as it is, never copied (16.2).
  expect_identical(data.table::address(read$tables$stations), data.table::address(fish$stations))
  expect_equal(read$text_tables, c("hauls", "catches"))
  results <- gpq_check_conformance(mixed, fx_fish_spec(), layers = 0:1, run_id = run_id)
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
      paste0("Table stations: the file ", name, ".csv can't be read cleanly"),
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
    "Table stations: the file cut.csv doesn't end with a line break, so it may be cut off",
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
    "Table stations: the file ragged2.csv can't be read cleanly",
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
        "checked: data.table::fread() would take its header from a later line than the first."
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
    "`tables` holds elements that are neither a data.frame nor the path of one CSV file: ",
    fixed = TRUE
  )
  absent <- file.path(dir, "nowhere.csv")
  expect_error(
    gpq_check_conformance(list(stations = absent, hauls = dir), spec),
    "No file at the path given for these tables: stations (",
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
