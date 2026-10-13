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
  # A name that isn't valid UTF-8 matches exactly or not at all.
  bad <- rawToChar(as.raw(c(0x64, 0xe9, 0x70)))
  expect_identical(match_columns(c(bad, "x"), c(bad, "dep"))$column, c(bad, NA))
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
  # Unmarked text is UTF-8 only in a UTF-8 locale, and order() refused it there (D16.35).
  testthat::skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
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
  # UTF-8 (D16.36).
  marked <- "Gé"
  Encoding(marked) <- "bytes"
  expect_true(validUTF8(marked))
  hauls$gear <- c("GN", "GN", "GN", marked)
  fish$hauls <- hauls
  expect_error(conformance_input(fish, spec, group_cols = "gear"), in_gear(4L), fixed = TRUE)
  expect_error(gpq_check_conformance(fish, spec, group_cols = "gear"), in_gear(4L), fixed = TRUE)
})

test_that("a table name that isn't valid UTF-8 is checked, not a stop", {
  bad <- rawToChar(as.raw(c(0x6F, 0x6B, 0x97)))
  Encoding(bad) <- "UTF-8"
  tables <- c(fx_fish_clean(), structure(list(data.frame(a = 1)), names = bad))
  expect_invisible(validate_tables(tables))
  expect_error(validate_tables(structure(list(data.frame(a = 1)), names = " ")), "each named once")
})

test_that("a sentinel a key may hold reaches no parent, nor does a key of the wrong class", {
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
  expect_identical(keys$open, "~")
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

test_that("matching the input changes no table and copies none whole (16.2)", {
  fish <- fx_fish_clean()
  before <- lapply(fish, data.table::copy)
  catches_address <- data.table::address(fish$catches$weight)
  input <- conformance_input(fish, fx_fish_spec(), group_cols = c("water_body", "gear"))
  expect_identical(data.table::address(input$data$catches$weight), catches_address)
  for (table in names(fish)) {
    expect_identical(fish[[table]], before[[table]], info = table)
    expect_null(data.table::indices(fish[[table]]), info = table)
  }
})
