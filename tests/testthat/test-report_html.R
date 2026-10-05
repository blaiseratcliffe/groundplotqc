# Tests for the HTML builders (plan 11.1, 11.2, 18.4; D8.18, D12.17, D12.21).

hostile <- c(
  "<script>alert(1)</script>", "a & b", "\"q\" 'r'", NA, "café",
  intToUtf8(0x1F600L), strrep("x", 10000L)
)

test_that("html_escape escapes the five characters and empties NA", {
  out <- html_escape(hostile)
  expect_equal(out[[1L]], "&lt;script&gt;alert(1)&lt;/script&gt;")
  expect_equal(out[[2L]], "a &amp; b")
  expect_equal(out[[3L]], "&quot;q&quot; &#39;r&#39;")
  expect_equal(out[[4L]], "")
  expect_equal(out[5:7], hostile[5:7])
})

test_that("html_escape writes an invalid byte as <xx>, escaped", {
  bad <- rawToChar(as.raw(c(0x61, 0x97)))
  Encoding(bad) <- "UTF-8"
  expect_equal(html_escape(bad), "a&lt;97&gt;")
})

test_that("html_table escapes every cell and caps with a note", {
  dt <- data.table::data.table(rule = c("<b>", "x", "y"), n = c(1L, NA, 3L))
  full <- html_table(dt, "Caption & more")
  expect_equal(lengths(regmatches(full, gregexpr("<tr>", full, fixed = TRUE))), 4L)
  expect_match(full, "<td>&lt;b&gt;</td>", fixed = TRUE)
  expect_match(full, "<td></td>", fixed = TRUE)
  expect_match(full, "Caption &amp; more", fixed = TRUE)
  capped <- html_table(dt, "c", row_cap = 2L, csv_name = "rows.csv")
  expect_equal(lengths(regmatches(capped, gregexpr("<tr>", capped, fixed = TRUE))), 3L)
  expect_match(capped, "Showing 2 of 3 rows; the full list is in rows.csv.", fixed = TRUE)
  expect_error(html_table(dt, "c", row_cap = 2L), "csv_name")
})

test_that("html_section checks its id and carries id and title", {
  section <- html_section("summary", "Checks <1>", "<p>x</p>")
  expect_equal(attr(section, "gpq_section"), c(id = "summary", title = "Checks <1>"))
  expect_match(section, "<h2>Checks &lt;1&gt;</h2>", fixed = TRUE)
  expect_error(html_section("Bad id", "t", ""), "id")
})

test_that("html_page is self-contained, with contents, footer and a small script", {
  page <- html_page(
    "Report <x>",
    list(html_section("summary", "Checks", "<p>a</p>"), html_section("rows", "Every row", "")),
    c("Package version" = "0.0.0.9000", "Files" = "a&b.csv")
  )
  expect_match(page, "^<!DOCTYPE html>")
  expect_match(page, "<meta charset=\"utf-8\">", fixed = TRUE)
  expect_match(page, "<a href=\"#summary\">Checks</a>", fixed = TRUE)
  expect_match(page, "<dd>a&amp;b.csv</dd>", fixed = TRUE)
  expect_false(grepl("(src|href)=\"https?:", page))
  # An inline handler is looked for inside tags only: escaped text may hold " onx=".
  expect_false(grepl("<[^>]*\\son[a-z]+=", page))
  expect_lt(nchar(page_script(), type = "bytes"), 4096L)
})
