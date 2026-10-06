# Tests for the HTML builders (plan 11.1, 11.2, 18.4; D8.18, D12.17, D12.21).

hostile <- c(
  "<script>alert(1)</script>", "a & b", "\"q\" 'r'", NA, "café",
  intToUtf8(0x1F600L), strrep("x", 10000L)
)

count_fixed <- function(pattern, x) {
  lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE)))
}

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
  bytes <- rawToChar(as.raw(c(0x61, 0x97)))
  Encoding(bytes) <- "bytes"
  expect_equal(html_escape(bytes), "a&lt;97&gt;")
})

test_that("html_escape keeps valid text marked latin1 as text", {
  latin1 <- iconv("café", "UTF-8", "latin1")
  expect_identical(Encoding(latin1), "latin1")
  expect_equal(html_escape(latin1), "café")
  expect_equal(html_escape(c(latin1, "<")), c("café", "&lt;"))
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

test_that("html_table needs csv_name whenever row_cap is given, capped or not", {
  dt <- data.table::data.table(rule = c("a", "b", "c"))
  expect_error(html_table(dt, "c", row_cap = 10L), "`row_cap` needs `csv_name`")
  expect_error(html_table(dt, "c", row_cap = 3L), "`row_cap` needs `csv_name`")
})

test_that("html_table needs csv_name to be one string wherever row_cap gives it a use", {
  dt <- data.table::data.table(rule = c("a", "b", "c"))
  # Capped or not: two names would make two notes, none an empty note, NA the lookup's error,
  # and a blank name an empty note or that same error.
  for (cap in c(2L, 10L)) {
    for (bad in list(c("a.csv", "b.csv"), character(0), NA_character_, 1, "", "  ")) {
      expect_error(
        html_table(dt, "c", row_cap = cap, csv_name = bad), "`csv_name` must be one string",
        info = paste(cap, deparse(bad))
      )
    }
  }
})

test_that("html_table adds no note when row_cap reaches the row count", {
  dt <- data.table::data.table(rule = c("a", "b", "c"))
  for (cap in c(3L, 10L)) {
    out <- html_table(dt, "c", row_cap = cap, csv_name = "rows.csv")
    expect_equal(count_fixed("<tr>", out), 4L)
    expect_false(grepl("rows.csv", out, fixed = TRUE))
  }
})

test_that("html_table with row_cap 0 shows the header and says so", {
  dt <- data.table::data.table(rule = c("a", "b", "c"))
  out <- html_table(dt, "c", row_cap = 0L, csv_name = "rows.csv")
  expect_equal(count_fixed("<tr>", out), 1L)
  expect_match(out, "Showing 0 of 3 rows; the full list is in rows.csv.", fixed = TRUE)
})

test_that("html_table writes a table with no rows or no columns without phantom cells", {
  no_rows <- html_table(data.table::data.table(x = character()), "c")
  expect_equal(count_fixed("<tr>", no_rows), 1L)
  expect_match(no_rows, "<th scope=\"col\" data-gpq-sort>x</th>", fixed = TRUE)
  expect_false(grepl("<td>", no_rows, fixed = TRUE))
  no_columns <- html_table(data.table::data.table(), "c")
  expect_false(grepl("<th ", no_columns, fixed = TRUE))
  expect_false(grepl("<td>", no_columns, fixed = TRUE))
})

test_that("html_table writes a double column's cells as text", {
  out <- html_table(data.table::data.table(x = c(0.5, 2.25)), "c")
  expect_match(out, "<td>0.5</td>", fixed = TRUE)
  expect_match(out, "<td>2.25</td>", fixed = TRUE)
})

test_that("html_table refuses bad arguments, naming the argument", {
  dt <- data.table::data.table(x = 1:3)
  expect_error(html_table(list(x = 1:3), "c"), "`dt` must be a data.frame")
  expect_error(html_table(dt, c("a", "b")), "`caption` must be one string")
  expect_error(html_table(dt, NA_character_), "`caption` must be one string")
  expect_error(html_table(dt, 1), "`caption` must be one string")
  for (bad in list(NA_integer_, -1L, c(1L, 2L), "2")) {
    expect_error(
      html_table(dt, "c", row_cap = bad, csv_name = "rows.csv"),
      "`row_cap` must be one non-negative number or NULL"
    )
  }
})

test_that("html_section checks its id and carries id and title", {
  section <- html_section("summary", "Checks <1>", "<p>x</p>")
  expect_equal(attr(section, "gpq_section"), c(id = "summary", title = "Checks <1>"))
  expect_match(section, "<h2>Checks &lt;1&gt;</h2>", fixed = TRUE)
  expect_error(html_section("Bad id", "t", ""), "lower-case")
  expect_error(html_section(c("a", "b"), "t", ""), "lower-case")
  expect_error(html_section(NA_character_, "t", ""), "lower-case")
  expect_error(html_section("a", c("t", "u"), ""), "`title` must be one string")
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
  expect_false(grepl("http", page, fixed = TRUE))
  expect_false(grepl("(href|src)=[\"']?//", page))
  for (pattern in c("url(", "@import", "<link", "<img", "<iframe")) {
    expect_false(grepl(pattern, page, fixed = TRUE), info = pattern)
  }
  # An inline handler is looked for inside tags only: escaped text may hold " onx=".
  expect_false(grepl("<[^>]*\\son[a-z]+=", page))
  expect_lt(nchar(page_script(), type = "bytes"), 4096L)
})

test_that("html_page escapes every value it shows, in every place it shows one", {
  script <- "<script>alert(1)</script>"
  image <- "<img src=x onerror=alert(1)>"
  shown_script <- "&lt;script&gt;alert(1)&lt;/script&gt;"
  shown_image <- "&lt;img src=x onerror=alert(1)&gt;"
  dt <- data.table::data.table(a = c(image, "b"), n = 1:2)
  data.table::setnames(dt, "a", script)
  meta <- image
  names(meta) <- script
  page <- html_page(
    script,
    list(html_section(
      "hostile", script, html_table(dt, script, row_cap = 1L, csv_name = image)
    )),
    meta
  )
  label <- report_text("report_filter_label")
  contents <- report_text("report_contents")
  expect_match(page, paste0("<title>", shown_script, "</title>"), fixed = TRUE)
  expect_match(page, paste0("<h1>", shown_script, "</h1>"), fixed = TRUE)
  expect_match(page, paste0("<a href=\"#hostile\">", shown_script, "</a>"), fixed = TRUE)
  expect_match(page, paste0("<h2>", shown_script, "</h2>"), fixed = TRUE)
  expect_match(page, paste0("<caption>", shown_script, "</caption>"), fixed = TRUE)
  expect_match(
    page, paste0("<th scope=\"col\" data-gpq-sort>", shown_script, "</th>"),
    fixed = TRUE
  )
  expect_match(page, paste0("<td>", shown_image, "</td>"), fixed = TRUE)
  expect_match(page, paste0("the full list is in ", shown_image, "."), fixed = TRUE)
  expect_match(page, paste0("<dt>", shown_script, "</dt><dd>", shown_image, "</dd>"), fixed = TRUE)
  expect_match(
    page, paste0("aria-label=\"", label, "\" placeholder=\"", label, "\""),
    fixed = TRUE
  )
  expect_match(
    page, paste0("<nav aria-label=\"", contents, "\"><h2>", contents, "</h2>"),
    fixed = TRUE
  )
  # The page's own script is the only one, and no hostile tag or handler is a real one.
  expect_equal(count_fixed("<script", page), 1L)
  expect_false(grepl("<img", page, fixed = TRUE))
  expect_false(grepl("<[^>]*onerror=", page))
})

test_that("html_page escapes a section id before it reaches the contents link", {
  odd <- structure("<section></section>", gpq_section = c(id = "x\" onclick=\"y", title = "t"))
  page <- html_page("T", list(odd))
  # The quotes are escaped, so the attribute can't close early and carry a handler.
  expect_match(page, "<a href=\"#x&quot; onclick=&quot;y\">t</a>", fixed = TRUE)
})

test_that("html_page writes no phantom contents or footer entries for empty input", {
  page <- html_page("T", list())
  expect_false(grepl("<li>", page, fixed = TRUE))
  expect_false(grepl("href=\"#\"", page, fixed = TRUE))
  expect_false(grepl("<nav", page, fixed = TRUE))
  expect_false(grepl("<dt>", page, fixed = TRUE))
})

test_that("html_page refuses bad arguments, naming the argument", {
  one <- html_section("a", "A", "")
  expect_error(html_page(c("T", "U"), list(one)), "`title` must be one string")
  expect_error(html_page(NA_character_, list(one)), "`title` must be one string")
  expect_error(html_page("T", one), "`sections` must be a list of sections")
  expect_error(html_page("T", list("<p>x</p>")), "`sections` must be a list of sections")
  expect_error(html_page("T", list(one, one)), "Section ids must be unique")
  expect_error(html_page("T", list(one), c("v1", "v2")), "`meta` must be named")
  expect_error(html_page("T", list(one), c(a = "v1", "v2")), "`meta` must be named")
})

test_that("the page's CSS wraps long values and the script marks sortable headers", {
  css <- page_css()
  expect_match(css, "overflow-wrap:anywhere", fixed = TRUE)
  expect_match(css, ".gpq-table{overflow-x:auto}", fixed = TRUE)
  # The scroll box would clip a focus ring drawn outside the first header, so it is drawn
  # inside, which keeps the layout as it is.
  expect_match(css, "th:focus-visible{outline-offset:-2px}", fixed = TRUE)
  # A header looks clickable only once the script has run.
  expect_match(css, ".gpq-js th[data-gpq-sort]{cursor:pointer", fixed = TRUE)
  expect_false(grepl("(^|\n)th\\[data-gpq-sort\\]", css))
  script <- page_script()
  expect_match(script, "document.documentElement.classList.add(\"gpq-js\")", fixed = TRUE)
  expect_match(script, "th.setAttribute(\"aria-sort\", \"none\")", fixed = TRUE)
})
