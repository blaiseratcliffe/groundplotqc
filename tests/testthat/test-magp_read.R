# Tests for MAGPlot 2.0's delivery loader (plan 3.4; D7.34, D8.3, D21.11, D21.12). Each
# delivery is magp_example, or a copy changed in memory, saved by fx_magp_rdata() as the
# pipeline saves one, each table its own object.

test_that("magp_read_delivery returns each table as a data.table, in the dictionary's order", {
  path <- fx_magp_rdata()
  tables <- magp_read_delivery(path)
  # The dictionary's order is magp_example's.
  expect_identical(names(magp_example), unique(magp_spec()$attributes$table_name))
  expect_named(tables, names(magp_example))
  expect_true(all(vapply(tables, data.table::is.data.table, logical(1))))
  # c() keeps only the names, so each table is compared whole, the list's attributes apart.
  expect_equal(c(tables), c(magp_example))
  # No magp_meta in the file, none in the list (D21.12).
  expect_false("magp_meta" %in% names(tables))
})

test_that("magp_read_delivery gives the file's name, no folder, and its SHA-256 (D8.3)", {
  path <- fx_magp_rdata()
  tables <- magp_read_delivery(path)
  expect_identical(attr(tables, "source_file"), basename(path))
  expect_false(grepl("[/\\\\]", attr(tables, "source_file")))
  expect_identical(attr(tables, "sha256"), unname(tools::sha256sum(path)))
  expect_match(attr(tables, "sha256"), "^[0-9a-f]{64}$")
  # Another file's bytes give another hash.
  changed <- magp_example
  changed$magp_plots <- changed$magp_plots[-1L]
  expect_false(identical(
    attr(magp_read_delivery(fx_magp_rdata(changed)), "sha256"), attr(tables, "sha256")
  ))
})

test_that("a table saved as a data.frame comes back a data.table with its columns", {
  tables <- magp_example
  tables$magp_plots <- as.data.frame(magp_example$magp_plots)
  read <- magp_read_delivery(fx_magp_rdata(tables))
  expect_true(data.table::is.data.table(read$magp_plots))
  expect_equal(as.data.frame(read$magp_plots), as.data.frame(magp_example$magp_plots))
})

test_that("dd, lkp and other objects that aren't data.frames are dropped (D8.3, D21.12)", {
  tables <- magp_example
  # dd is dropped even as a data.frame; lkp, a character vector and a function are dropped. The
  # function is a base one, so save() writes no environment of this test with it.
  tables$dd <- data.frame(table_name = "magp_plots", attribute_name = "magp_plot_id")
  tables$lkp <- list(species = data.frame(code = "PL"))
  tables$notes <- "t1"
  tables$tally <- sum
  read <- magp_read_delivery(fx_magp_rdata(tables))
  expect_named(read, names(magp_example))
})

test_that("the file loads into an environment of its own, so the caller's objects stay (D7.34)", {
  # Objects of this frame named like three the delivery holds, and a delivery whose extra
  # data.frame is named like the loader's own argument, path: a load into the loader's frame
  # or into this one would overwrite them.
  dd <- "session dd"
  lkp <- "session lkp"
  magp_plots <- "session magp_plots"
  tables <- magp_example
  tables$dd <- data.frame(table_name = "magp_plots")
  tables$lkp <- list(species = data.frame(code = "PL"))
  tables$path <- data.frame(note = "t1")
  delivery <- fx_magp_rdata(tables)
  read <- magp_read_delivery(delivery)
  expect_identical(attr(read, "source_file"), basename(delivery))
  expect_named(read, c(names(magp_example), "path"))
  expect_equal(read$magp_plots, magp_example$magp_plots)
  expect_equal(as.data.frame(read$path), data.frame(note = "t1"))
  expect_identical(
    c(dd, lkp, magp_plots), c("session dd", "session lkp", "session magp_plots")
  )
})

test_that("an extra data.frame comes after the tables, in the file's order (D21.12)", {
  # One saved before the tables and one after: the extras keep the file's order, not the
  # alphabet's, and follow every table. c() drops the list's marker, so it is set again.
  tables <- c(list(zeta_notes = data.frame(note = "t1")), magp_example)
  attr(tables, "gpq_fixture") <- TRUE
  tables$alpha_notes <- data.table::data.table(note = c("t2", "t3"))
  read <- magp_read_delivery(fx_magp_rdata(tables))
  expect_named(read, c(names(magp_example), "zeta_notes", "alpha_notes"))
  expect_true(data.table::is.data.table(read$zeta_notes))
  expect_equal(as.data.frame(read$zeta_notes), data.frame(note = "t1"))
  expect_equal(read$alpha_notes, data.table::data.table(note = c("t2", "t3")))
})

test_that("magp_meta comes last, kept as the file holds it, whatever its shape (D21.12)", {
  # A made-up magp_meta: magp_example has none (D21.12). As a list, as a data.frame, which
  # stays one, and as NULL.
  shapes <- list(
    list(built = "2026-10-29", commit = "abc1234"),
    data.frame(built = "2026-10-29", commit = "abc1234"),
    NULL
  )
  for (meta in shapes) {
    tables <- magp_example
    tables["magp_meta"] <- list(meta)
    tables$extra_notes <- data.frame(note = "t1")
    read <- magp_read_delivery(fx_magp_rdata(tables))
    expect_named(read, c(names(magp_example), "extra_notes", "magp_meta"))
    expect_identical(read$magp_meta, meta)
  }
})

test_that("a delivery missing a table reads, the table left out (D21.12)", {
  tables <- magp_example
  tables$magp_tree_age <- NULL
  read <- magp_read_delivery(fx_magp_rdata(tables))
  expect_named(read, setdiff(names(magp_example), "magp_tree_age"))
  # One table is enough.
  read <- magp_read_delivery(fx_magp_rdata(fx_mark_fixture(magp_example["magp_plots"])))
  expect_named(read, "magp_plots")
})

test_that("magp_read_delivery stops on a path that isn't one .Rdata file R can load", {
  message <- "The file given must be one .Rdata file that R can load."
  dir <- withr::local_tempdir()
  text <- file.path(dir, "notes.Rdata")
  writeLines("not saved by R", text)
  rds <- file.path(dir, "plots.rds")
  saveRDS(magp_example$magp_plots, rds)
  empty <- file.path(dir, "empty.Rdata")
  writeBin(raw(), empty)
  saved <- fx_magp_rdata()
  given <- list(
    NULL, NA_character_, 1, c(saved, saved), file.path(dir, "absent.Rdata"), dir, text, rds,
    empty
  )
  for (bad in given) {
    # load()'s own warnings don't reach the caller: the error alone says what went wrong.
    expect_no_warning(expect_error(magp_read_delivery(bad), message, fixed = TRUE))
  }
})

test_that("W48 ends with load()'s own error where load() stopped on the file (D22.18)", {
  sentence <- "The file given must be one .Rdata file that R can load."
  dir <- withr::local_tempdir()
  # A delivery cut short, as a copy stopped part way leaves it: load() starts on its first
  # bytes, a saved file's, then stops. And two files load() stops on at once.
  saved <- fx_magp_rdata()
  cut <- file.path(dir, "cut.Rdata")
  writeBin(readBin(saved, "raw", file.size(saved) %/% 2L), cut)
  text <- file.path(dir, "notes.Rdata")
  writeLines("not saved by R", text)
  empty <- file.path(dir, "empty.Rdata")
  writeBin(raw(), empty)
  for (path in c(cut, text, empty)) {
    # load()'s error as load() gives it here, in the session's language, never written here.
    stopped <- tryCatch(suppressWarnings(load(path, envir = new.env())), error = function(e) e)
    expect_s3_class(stopped, "error")
    expect_identical(
      tryCatch(magp_read_delivery(path), error = conditionMessage),
      paste(sentence, "load() stopped with this error:", conditionMessage(stopped)),
      info = basename(path)
    )
  }
  # Where load() never ran, on a path that isn't one file, the sentence stands alone.
  for (bad in list(file.path(dir, "absent.Rdata"), dir, NA_character_)) {
    expect_identical(tryCatch(magp_read_delivery(bad), error = conditionMessage), sentence)
  }
})

test_that("magp_read_delivery stops on a file holding none of the 17 tables", {
  message <- paste(
    "%s holds none of MAGPlot 2.0's 17 tables as an object of its own; a delivery saves each",
    "table as its own object."
  )
  # The tables saved as one list object; the file's other objects alone; no object at all.
  path <- withr::local_tempfile(fileext = ".Rdata")
  delivery <- magp_example
  save(delivery, file = path)
  expect_error(magp_read_delivery(path), sprintf(message, basename(path)), fixed = TRUE)
  path <- withr::local_tempfile(fileext = ".Rdata")
  dd <- data.frame(table_name = "magp_plots")
  lkp <- list()
  magp_meta <- list(built = "2026-10-29")
  notes <- data.frame(note = "t1")
  save(dd, lkp, magp_meta, notes, file = path)
  expect_error(magp_read_delivery(path), sprintf(message, basename(path)), fixed = TRUE)
  path <- withr::local_tempfile(fileext = ".Rdata")
  save(list = character(), file = path)
  expect_error(magp_read_delivery(path), sprintf(message, basename(path)), fixed = TRUE)
})

test_that("magp_read_delivery stops on a table-named object that isn't a data.frame", {
  tables <- magp_example
  tables$magp_plots <- as.list(magp_example$magp_plots)
  tables$magp_sites <- "magp_sites"
  path <- fx_magp_rdata(tables)
  # Named in the dictionary's order.
  expect_error(
    magp_read_delivery(path),
    paste0(
      basename(path), " holds, under MAGPlot 2.0's table names, something other than a ",
      "data.frame: magp_sites, magp_plots. Each table must be a data.frame."
    ),
    fixed = TRUE
  )
})
