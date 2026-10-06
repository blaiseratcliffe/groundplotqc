# Tests for the sentinel table (plan 3.5; D7.4, D8.8, D9.18, D12.19, D12.54).

test_that("gpq_sentinels has no built-in values", {
  empty <- gpq_sentinels()
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("data_type", "role", "value", "allowed_in_pk", "allowed_in_fk"))
})

test_that("gpq_sentinels builds one row per family and role, with the engine's key flags", {
  s <- gpq_sentinels(
    numeric = c(missing = -99, not_applicable = -88),
    character = c(missing = "?", not_applicable = "~")
  )
  expect_equal(s$data_type, c("numeric", "numeric", "character", "character"))
  expect_equal(s$value, c("-99", "-88", "?", "~"))
  expect_equal(s$allowed_in_pk, rep(FALSE, 4L))
  expect_equal(s$allowed_in_fk, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("gpq_sentinels refuses unnamed or unknown roles", {
  expect_error(gpq_sentinels(numeric = c(-1, -9)), "numeric")
  expect_error(gpq_sentinels(date = c(absent = "X")), "date")
})

test_that("validate_sentinels takes a table such as MAGPlot's hand-kept one", {
  table <- data.frame(
    data_type = "numeric", role = "missing", value = "-1",
    allowed_in_pk = FALSE, allowed_in_fk = FALSE
  )
  expect_equal(validate_sentinels(table)$value, "-1")
  table$data_type <- "integer"
  # Its own check's wording: the columns check's message names data_type too.
  expect_error(validate_sentinels(table), "data_type is numeric, character or date")
})

test_that("validate_sentinels refuses what isn't one clean row per family and role (D12.54)", {
  expect_error(validate_sentinels(NULL), "data.frame")
  flags <- data.frame(
    data_type = "numeric", role = "missing", value = "-1", allowed_in_pk = "0",
    allowed_in_fk = "1"
  )
  expect_error(validate_sentinels(flags), "TRUE or FALSE")
  twice <- data.frame(
    data_type = "character", role = "missing", value = c("X", "Y"), allowed_in_pk = FALSE,
    allowed_in_fk = FALSE
  )
  expect_error(validate_sentinels(twice), "one row for each")
  bad <- rawToChar(as.raw(c(0x58, 0x97)))
  Encoding(bad) <- "UTF-8"
  expect_error(gpq_sentinels(character = c(missing = bad)), "UTF-8")
})

test_that("validate_sentinels refuses a data.frame with a repeated column, naming it (D12.54)", {
  repeated <- data.frame(
    data_type = "numeric", role = "missing", value = "-1", allowed_in_pk = FALSE,
    allowed_in_fk = FALSE, value = "-9", check.names = FALSE
  )
  expect_error(
    validate_sentinels(repeated), "A sentinel table's column names must not repeat: value.",
    fixed = TRUE
  )
})

test_that("a key flag is a logical or the text TRUE or FALSE, nothing else (D12.19)", {
  row <- function(pk, fk) {
    data.frame(
      data_type = "numeric", role = "missing", value = "-1", allowed_in_pk = pk, allowed_in_fk = fk
    )
  }
  from_logical <- validate_sentinels(row(FALSE, TRUE))
  expect_equal(c(from_logical$allowed_in_pk, from_logical$allowed_in_fk), c(FALSE, TRUE))
  from_text <- validate_sentinels(row("FALSE", "TRUE"))
  expect_equal(c(from_text$allowed_in_pk, from_text$allowed_in_fk), c(FALSE, TRUE))
  # as.logical() would take 0, 1, "T", "true" and "True" for flags, which the message doesn't
  # say.
  for (bad in list(0, 1, "0", "1", "T", "F", "true", "false", "True", "False", "yes", "", NA)) {
    expect_error(
      validate_sentinels(row(bad, FALSE)), "TRUE or FALSE",
      info = paste("pk", deparse(bad))
    )
    expect_error(
      validate_sentinels(row(FALSE, bad)), "TRUE or FALSE",
      info = paste("fk", deparse(bad))
    )
  }
})

test_that("validate_sentinels refuses a blank role or value, and takes a table with no rows", {
  row <- function(role = "missing", value = "-1") {
    data.frame(
      data_type = "numeric", role = role, value = value, allowed_in_pk = FALSE,
      allowed_in_fk = FALSE
    )
  }
  for (blank in c("", "   ", NA_character_)) {
    expect_error(
      validate_sentinels(row(role = blank)), "a role missing or not_applicable and no blank cell",
      info = paste("role", deparse(blank))
    )
    expect_error(
      validate_sentinels(row(value = blank)), "a role missing or not_applicable and no blank cell",
      info = paste("value", deparse(blank))
    )
  }
  none <- validate_sentinels(row()[0L, ])
  expect_equal(nrow(none), 0L)
  expect_named(none, c("data_type", "role", "value", "allowed_in_pk", "allowed_in_fk"))
})
