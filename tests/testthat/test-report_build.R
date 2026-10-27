# Tests for each report unit's file name (D20.8, D20.70).

test_that("unit_file_name() keeps safe ASCII and writes every other byte as %XX (D20.8)", {
  # A stem decoded: each %XX its byte, any other character itself.
  decode <- function(stem) {
    tokens <- regmatches(stem, gregexpr("%[0-9A-F]{2}|.", stem))[[1L]]
    escaped <- startsWith(tokens, "%")
    bytes <- raw(length(tokens))
    bytes[escaped] <- as.raw(strtoi(substring(tokens[escaped], 2L), 16L))
    bytes[!escaped] <- charToRaw(paste(tokens[!escaped], collapse = ""))
    text <- rawToChar(bytes)
    Encoding(text) <- "UTF-8"
    text
  }
  # MAGPlot's IDs, and any other unit of safe ASCII, are kept as they are.
  safe <- c("100", "220.01", "AB-3_x.v2")
  expect_identical(unit_file_name(safe), safe)
  units <- c(
    "%", "a b", "é", r"(/\:*?"<>|)", "\t", "a\nb", "\U0001F600", ";",
    ".a.", ".", "...", " lead", "trail ",
    "CON", "con.txt", "Com1", "LPT9.x.y", "nul", "aux.", "PRN",
    "CONX", "COM0", "COM10", "xCON", "CON-1", ".CON",
    "preflight", "PreFlight", "national_summary", "preflight.x",
    # The package's own file stems, in any case (D20.70, D20.73); a value only holding one isn't.
    "report", "Results", "ISSUES", "corrections", "issues.csv", "my_report"
  )
  stems <- unit_file_name(units)
  expect_identical(stems, c(
    "%25", "a%20b", "%C3%A9", "%2F%5C%3A%2A%3F%22%3C%3E%7C", "%09", "a%0Ab", "%F0%9F%98%80",
    "%3B", "%2Ea%2E", "%2E", "%2E.%2E", "%20lead", "trail%20",
    "%43ON", "%63on.txt", "%43om1", "%4CPT9.x.y", "%6Eul", "%61ux%2E", "%50RN",
    "CONX", "COM0", "COM10", "xCON", "CON-1", "%2ECON",
    "%70reflight", "%50reFlight", "%6Eational_summary", "preflight.x",
    "%72eport", "%52esults", "%49SSUES", "%63orrections", "issues.csv", "my_report"
  ))
  expect_identical(unit_file_name("Issues"), "%49ssues")
  expect_true(all(grepl("^[A-Za-z0-9._%-]+$", stems)))
  expect_identical(vapply(stems, decode, "", USE.NAMES = FALSE), units)
  # Text marked latin1 is encoded from its UTF-8.
  expect_identical(unit_file_name(iconv("été", "UTF-8", "latin1")), "%C3%A9t%C3%A9")
})

test_that("unit_file_name() stops on stems alike ignoring case or over 200 bytes (D20.8)", {
  expect_error(
    unit_file_name(c("x", "A", "b", "a")),
    paste0(
      "Report units A and a give the same file name, A.html, on a system that ignores ",
      "letter case; rename one of them."
    ),
    fixed = TRUE
  )
  # A reserved name's first character, encoded, keeps "CON" and "con" apart.
  expect_identical(unit_file_name(c("CON", "con")), c("%43ON", "%63on"))
  expect_identical(unit_file_name(strrep("x", 200L)), strrep("x", 200L))
  expect_error(
    unit_file_name(c("y", strrep("x", 201L))),
    paste0(
      "Report unit ", strrep("x", 201L), " gives a file name of 201 bytes once encoded, over ",
      "the limit of 200; use a shorter unit value."
    ),
    fixed = TRUE
  )
  # Bytes, not characters: 33 e-acutes are 198 bytes once encoded, 34 are 204.
  expect_identical(nchar(unit_file_name(strrep("é", 33L))), 198L)
  expect_error(unit_file_name(strrep("é", 34L)), "of 204 bytes once encoded", fixed = TRUE)
})

test_that("unit_file_name() folds letter case in ASCII, whatever the locale's rules (D20.8)", {
  # In a Turkish or Azeri UTF-8 locale on Linux, tolower() gives "I" a dotless i (U+0131).
  # The function runs here with a tolower() that does so, so "I" and "i" must still stop, on
  # any platform and in any locale.
  dotless <- function(x) base::tolower(gsub("I", "ı", x, fixed = TRUE))
  turkish <- unit_file_name
  environment(turkish) <- list2env(
    list(tolower = dotless),
    parent = environment(unit_file_name)
  )
  expect_error(
    turkish(c("I", "i")), "Report units I and i give the same file name, I.html,",
    fixed = TRUE
  )
  expect_error(turkish(c("ID1", "id1")), "Report units ID1 and id1 give", fixed = TRUE)
})
