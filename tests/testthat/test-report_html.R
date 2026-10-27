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
  # n is a column of numbers, so its cells are right-aligned (R4); its NA is an empty cell.
  expect_match(full, "<td class=\"num\"></td>", fixed = TRUE)
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

test_that("html_table writes a double column's cells as text, right-aligned (R4)", {
  out <- html_table(data.table::data.table(x = c(0.5, 2.25)), "c")
  expect_match(out, "<td class=\"num\">0.5</td>", fixed = TRUE)
  expect_match(out, "<td class=\"num\">2.25</td>", fixed = TRUE)
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
    meta,
    subtitle = script
  )
  label <- report_text("report_filter_label")
  contents <- report_text("report_contents")
  expect_match(page, paste0("<title>", shown_script, "</title>"), fixed = TRUE)
  expect_match(page, paste0("<h1>", shown_script, "</h1>"), fixed = TRUE)
  # The masthead's line under the title, D20.66's subtitle.
  expect_match(page, paste0("<p>", shown_script, "</p>"), fixed = TRUE)
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
  # Table cells break a word only where it can't fit its box, so "Warning" and "Outcome" stay
  # whole at desktop width (D12.79); "anywhere" would shrink a column to one character.
  expect_match(css, "td,th{overflow-wrap:break-word}", fixed = TRUE)
  expect_false(grepl("td,th{overflow-wrap:anywhere}", css, fixed = TRUE))
  # The footer's and the facts' 64-character hashes sit in a <dd>, which would stretch the
  # page sideways at 360 px without a rule of its own.
  expect_match(css, "dd{overflow-wrap:anywhere}", fixed = TRUE)
  expect_match(css, ".gpq-table{overflow-x:auto;", fixed = TRUE)
  # The scroll box would clip a focus ring drawn outside a header, so it is drawn inside,
  # which keeps the layout as it is.
  expect_match(css, "th:focus-visible\\{[^}]*outline-offset:-[0-9]+px\\}")
  # A header looks clickable only once the script has run.
  expect_match(css, ".gpq-js th[data-gpq-sort]{cursor:pointer", fixed = TRUE)
  expect_false(grepl("(^|\n)th\\[data-gpq-sort\\]", css))
  script <- page_script()
  expect_match(script, "document.documentElement.classList.add(\"gpq-js\")", fixed = TRUE)
  expect_match(script, "th.setAttribute(\"aria-sort\", \"none\")", fixed = TRUE)
})

# ---- M7a task 6 ----

test_that("cut_cell_text cuts delivered text at cell_cap characters, before escaping (D20.39)", {
  ellipsis <- "…"
  # One character over the cap: the cap's characters, then the ellipsis. At the cap, NA and a
  # blank: as they are.
  expect_identical(cut_cell_text(strrep("a", 101L), 100L), paste0(strrep("a", 100L), ellipsis))
  expect_identical(
    cut_cell_text(c(strrep("b", 100L), NA, ""), 100L), c(strrep("b", 100L), NA, "")
  )
  # Quotes and "<" count one character each, before escaping: at the cap a value stays whole,
  # and one over it is cut between two of them, never inside an entity.
  marks <- strrep("\"<", 50L)
  expect_identical(cut_cell_text(marks, 100L), marks)
  cut <- cut_cell_text(paste0(marks, ">"), 100L)
  expect_identical(html_escape(cut), paste0(strrep("&quot;&lt;", 50L), ellipsis))
  table <- html_table(data.table::data.table(value = cut), "c")
  expect_match(table, paste0("<td>", strrep("&quot;&lt;", 50L), ellipsis, "</td>"), fixed = TRUE)
  # A character of two or of four bytes at the cut is kept whole, the result valid UTF-8.
  for (wide in c("é", "\U0001F600")) {
    kept <- cut_cell_text(paste0(strrep("a", 99L), wide, "b"), 100L)
    expect_identical(kept, paste0(strrep("a", 99L), wide, ellipsis), info = wide)
    expect_true(validUTF8(kept), info = wide)
  }
  # Text marked latin1, as utils::read.csv() can give it, is counted in characters too.
  latin1 <- iconv(paste0(strrep("a", 99L), "é", "b"), "UTF-8", "latin1")
  expect_identical(
    enc2utf8(cut_cell_text(latin1, 100L)), paste0(strrep("a", 99L), "é", ellipsis)
  )
  expect_identical(cut_cell_text("abc", 0L), ellipsis)
  for (bad in list(NA_integer_, -1L, c(1L, 2L), "100", NULL)) {
    expect_error(
      cut_cell_text("a", bad), "`cell_cap` must be one non-negative number.",
      fixed = TRUE, info = deparse(bad)
    )
  }
})

test_that("html_table right-aligns columns of numbers, header and cells, and nothing else (R4)", {
  dt <- data.table::data.table(
    text = "a", whole = 2L, part = 0.5, day = as.Date("2026-12-15"), kind = factor("k"),
    flag = TRUE, iday = data.table::as.IDate("2026-12-16"),
    time = as.POSIXct("2026-12-15 10:30:00", tz = "UTC")
  )
  out <- html_table(dt, "c")
  for (column in c("whole", "part")) {
    expect_match(
      out, paste0("<th scope=\"col\" data-gpq-sort class=\"num\">", column, "</th>"),
      fixed = TRUE, info = column
    )
  }
  for (column in c("text", "day", "kind", "flag", "iday", "time")) {
    expect_match(
      out, paste0("<th scope=\"col\" data-gpq-sort>", column, "</th>"),
      fixed = TRUE, info = column
    )
  }
  expect_match(
    out,
    paste0(
      "<tr><td>a</td><td class=\"num\">2</td><td class=\"num\">0.5</td><td>2026-12-15</td>",
      "<td>k</td><td>TRUE</td><td>2026-12-16</td><td>2026-12-15 10:30:00</td></tr>"
    ),
    fixed = TRUE
  )
  # A count stored as integer64, as fread() can give one, is a number too.
  testthat::skip_if_not_installed("bit64")
  big <- html_table(data.table::data.table(n = bit64::as.integer64("12345678901234")), "c")
  expect_match(big, "<th scope=\"col\" data-gpq-sort class=\"num\">n</th>", fixed = TRUE)
  expect_match(big, "<td class=\"num\">12345678901234</td>", fixed = TRUE)
})

test_that("html_table gives rows a class, a severity a label and a total row the foot (D20.37)", {
  dt <- data.table::data.table(
    severity = c("Error", "Warning", "x <y>", "Information", "Total"), n = c(3L, 2L, 1L, 1L, 7L)
  )
  classes <- c("sev-error", "sev-warning", NA, "sev-info", "total")
  out <- html_table(dt, "c", row_class = classes)
  expect_match(
    out,
    paste0(
      "<tr class=\"sev-error\"><td><span class=\"sev\">Error</span></td>",
      "<td class=\"num\">3</td></tr>"
    ),
    fixed = TRUE
  )
  expect_match(
    out, "<tr class=\"sev-warning\"><td><span class=\"sev\">Warning</span></td>",
    fixed = TRUE
  )
  # A row with no class keeps its first cell as text, and so does a class that isn't a
  # severity's.
  expect_match(out, "<tr><td>x &lt;y&gt;</td><td class=\"num\">1</td></tr>", fixed = TRUE)
  expect_match(
    out,
    paste0(
      "</tbody><tfoot><tr class=\"total\"><td>Total</td><td class=\"num\">7</td></tr>",
      "</tfoot></table>"
    ),
    fixed = TRUE
  )
  body <- regmatches(out, regexpr("<tbody>.*</tbody>", out))
  expect_false(grepl("Total", body, fixed = TRUE))
  # The cap counts and cuts the body only; the total row stays, outside the note's count.
  capped <- html_table(dt, "c", row_cap = 2L, csv_name = "rows.csv", row_class = classes)
  expect_match(capped, "Showing 2 of 4 rows; the full list is in rows.csv.", fixed = TRUE)
  expect_match(capped, "<tfoot><tr class=\"total\">", fixed = TRUE)
  expect_false(grepl("Information", capped, fixed = TRUE))
  # A total row before the cap's boundary: the body still shows the first two other rows, and
  # the total row stays in the foot.
  first <- html_table(
    data.table::data.table(
      severity = c("Total", "Error", "Warning", "x <y>", "Information"), n = c(7L, 3L, 2L, 1L, 1L)
    ),
    "c",
    row_cap = 2L, csv_name = "rows.csv",
    row_class = c("total", "sev-error", "sev-warning", NA, "sev-info")
  )
  expect_identical(
    regmatches(first, regexpr("<tbody>.*</tbody>", first)),
    paste0(
      "<tbody><tr class=\"sev-error\"><td><span class=\"sev\">Error</span></td>",
      "<td class=\"num\">3</td></tr><tr class=\"sev-warning\"><td><span class=\"sev\">Warning",
      "</span></td><td class=\"num\">2</td></tr></tbody>"
    )
  )
  expect_match(
    first,
    "<tfoot><tr class=\"total\"><td>Total</td><td class=\"num\">7</td></tr></tfoot>",
    fixed = TRUE
  )
  expect_match(first, "Showing 2 of 4 rows; the full list is in rows.csv.", fixed = TRUE)
  # No row_class: every row plain, as before.
  expect_false(grepl("<tr class=", html_table(dt, "c"), fixed = TRUE))
  wrong <- list(classes[-1L], c(classes[-1L], "Sev Error"), c(classes[-1L], "a\"b"), 1:5)
  wants <- paste(
    "`row_class` must be NULL or one class per row of `dt`, each NA or a lower-case name of",
    "letters, digits and hyphens."
  )
  for (bad in wrong) {
    expect_error(
      html_table(dt, "c", row_class = bad), wants,
      fixed = TRUE, info = deparse(bad)
    )
  }
})

test_that("html_table and html_page look up their words in the run's language (D14.7, D14.30)", {
  text <- validate_text_table(data.frame(
    text_id = c("report_filter_label", "report_rows_capped", "report_contents", "report_contents"),
    lang = c("fr", "fr", "fr", "en"),
    text = c("Filtrer", "{n} sur {total}, tout dans {csv_name}", "Sommaire", "On this page")
  ))
  dt <- data.table::data.table(x = c("a", "b"))
  out <- html_table(dt, "c", row_cap = 1L, csv_name = "rows.csv", lang = "fr", text = text)
  expect_match(out, "aria-label=\"Filtrer\" placeholder=\"Filtrer\"", fixed = TRUE)
  expect_match(out, "<p>1 sur 2, tout dans rows.csv</p>", fixed = TRUE)
  sections <- list(html_section("summary", "S", ""))
  page <- html_page("T", sections, lang = "fr", text = text)
  expect_match(page, "<html lang=\"fr\">", fixed = TRUE)
  expect_match(page, "<nav aria-label=\"Sommaire\"><h2>Sommaire</h2>", fixed = TRUE)
  # In English, the caller's English row; with no table, the engine's.
  expect_match(html_page("T", sections, text = text), "<h2>On this page</h2>", fixed = TRUE)
  english <- html_page("T", sections)
  expect_match(english, "<html lang=\"en\">", fixed = TRUE)
  expect_match(english, paste0("<h2>", report_text("report_contents"), "</h2>"), fixed = TRUE)
  # A language without rows falls back to English text, the page still marked in it.
  expect_match(html_page("T", sections, lang = "de"), "<html lang=\"de\">", fixed = TRUE)
  for (bad in list("EN", c("en", "fr"), NA_character_, 1)) {
    expect_error(
      html_page("T", sections, lang = bad),
      "`lang` must be one language code of two or three lower-case letters.",
      fixed = TRUE, info = deparse(bad)
    )
  }
})

test_that("provider-facing builders pass lang and text to every lookup and table (D14.7)", {
  root <- testthat::test_path("..", "..")
  testthat::skip_if_not(
    file.exists(file.path(root, "R", "report_html.R")), "the source tree isn't here"
  )
  # The provider-facing builders: the pages and tables, the sections (tasks 9 and 10) and the
  # issues CSV (task 8), each read once its file exists.
  files <- file.path(root, "R", c("report_html.R", "report_sections.R", "report_csv.R"))
  files <- files[file.exists(files)]
  # The functions whose every call passes the run's language and the caller's text: the
  # lookup, the three helpers that look up the blank text (D14.30), and the two builders that
  # look words up.
  checked <- c(
    "report_text", "fill_placeholders", "quote_value", "blank_as_text", "html_table", "html_page"
  )
  is_checked <- function(part) is.symbol(part) && as.character(part) %in% checked
  direct <- list()
  handed <- list()
  # Every call in the code, in function bodies and argument defaults too: a call of one of
  # them, or a call handing one to another function (vapply() and the like), an assignment
  # that defines one apart.
  defining <- c("<-", "=", "<<-")
  walk <- function(x) {
    if (is.call(x)) {
      parts <- as.list(x)
      head <- parts[[1L]]
      defines <- is.symbol(head) && as.character(head) %in% defining
      if (is_checked(head)) {
        direct[[length(direct) + 1L]] <<- x
      } else if (!defines && any(vapply(parts[-1L], is_checked, logical(1)))) {
        handed[[length(handed) + 1L]] <<- x
      }
      lapply(parts, walk)
    } else if (is.pairlist(x)) {
      lapply(as.list(x), walk)
    }
    invisible(NULL)
  }
  for (file in files) {
    lapply(as.list(parse(file, keep.source = FALSE)), walk)
  }
  shown <- function(calls) {
    vapply(calls, function(call) paste(deparse(call), collapse = " "), character(1))
  }
  # report_html.R's three lookups at least: the cap's note, the filter's label, the contents.
  expect_gte(length(direct), 3L)
  # Arguments matched as R matches them, a lang given by position included; `...` passed on
  # by a wrapper is left out, since match.call() can't expand it here.
  given <- lapply(direct, function(call) {
    call <- call[!vapply(as.list(call), identical, logical(1), quote(...))]
    names(match.call(get(as.character(call[[1L]])), call))
  })
  lacking <- !vapply(given, function(args) all(c("lang", "text") %in% args), logical(1))
  expect_identical(shown(direct[lacking]), character())
  # A call handing one on passes both through its own arguments.
  lacking <- !vapply(handed, function(call) all(c("lang", "text") %in% names(call)), logical(1))
  expect_identical(shown(handed[lacking]), character())
})

test_that("html_bar draws one bar per value, as wide as its share of max (11.2, D20.37)", {
  out <- html_bar(c(17L, 15L, 0L), 17L, c("A <b>", "B", "C"))
  expect_equal(count_fixed("<div class=\"bars\">", out), 1L)
  expect_equal(count_fixed("<div class=\"bar\">", out), 3L)
  styles <- regmatches(out, gregexpr("style=\"[^\"]*\"", out))[[1L]]
  expect_identical(
    styles, c("style=\"width:100.0%\"", "style=\"width:88.2%\"", "style=\"width:0.0%\"")
  )
  # The width is the page's only inline style, and each value is shown as text.
  expect_equal(count_fixed("style=", out), 3L)
  expect_match(out, "<span class=\"bar-label\">A &lt;b&gt;</span>", fixed = TRUE)
  expect_match(out, "<span class=\"bar-value\">15</span>", fixed = TRUE)
  # A severity's class colours its bar and shows its label as the severity's; NA, none.
  coloured <- html_bar(c(3, 1e5), 1e5, c("Error", "Other"), c("sev-error", NA))
  expect_match(
    coloured,
    paste0(
      "<div class=\"bar sev-error\"><span class=\"bar-label\">",
      "<span class=\"sev\">Error</span></span>"
    ),
    fixed = TRUE
  )
  expect_match(coloured, "<div class=\"bar\"><span class=\"bar-label\">Other</span>", fixed = TRUE)
  expect_match(coloured, "<span class=\"bar-value\">100000</span>", fixed = TRUE)
  expect_equal(count_fixed("bar sev-info", html_bar(1:2, 2L, c("a", "b"), "sev-info")), 2L)
  # A max of 0 draws no width; no value draws nothing.
  expect_match(html_bar(0L, 0L, "a"), "width:0.0%", fixed = TRUE)
  expect_identical(html_bar(integer(), 0L, character()), "")
  value_wants <- "`value` must be numbers, none of them NA or below 0."
  expect_error(html_bar(c(1L, NA), 2L, c("a", "b")), value_wants, fixed = TRUE)
  expect_error(html_bar(-1L, 2L, "a"), value_wants, fixed = TRUE)
  expect_error(html_bar("1", 2L, "a"), value_wants, fixed = TRUE)
  max_wants <- "`max` must be one number, not NA, that no value exceeds."
  expect_error(html_bar(3L, 2L, "a"), max_wants, fixed = TRUE)
  expect_error(html_bar(1L, c(2L, 3L), "a"), max_wants, fixed = TRUE)
  expect_error(html_bar(1:2, 2L, "a"), "`label` must be text, one label per value")
  class_wants <- paste(
    "`bar_class` must be NULL, one class or one per value, each NA or a lower-case name of",
    "letters, digits and hyphens."
  )
  expect_error(
    html_bar(1:2, 2L, c("a", "b"), c("sev-error", "sev-info", "sev-flag")), class_wants,
    fixed = TRUE
  )
  expect_error(html_bar(1L, 2L, "a", "Sev"), class_wants, fixed = TRUE)
})

test_that("html_facts lists facts and html_details folds a part, both escaped (D20.37, D20.40)", {
  facts <- html_facts(c("Run ID" = "r<1>", "Rule set & version" = NA, "Note" = ""))
  expect_identical(facts, paste0(
    "<div class=\"facts\"><dl><dt>Run ID</dt><dd>r&lt;1&gt;</dd>",
    "<dt>Rule set &amp; version</dt><dd></dd><dt>Note</dt><dd></dd></dl></div>"
  ))
  # A list of single values: a number written in full, a date as text.
  listed <- html_facts(list(Rows = 1e5, Day = as.Date("2026-12-15")))
  expect_match(listed, "<dt>Rows</dt><dd>100000</dd><dt>Day</dt><dd>2026-12-15</dd>", fixed = TRUE)
  expect_identical(html_facts(character()), "")
  facts_wants <- "`values` must be named, with no empty name, and hold one value per name."
  expect_error(html_facts(c("a", "b")), facts_wants, fixed = TRUE)
  expect_error(html_facts(c(a = "1", "2")), facts_wants, fixed = TRUE)
  expect_error(html_facts(list(a = 1:2)), facts_wants, fixed = TRUE)
  folded <- html_details("Settings (2) <x>", c("<p>a</p>", "<p>b</p>"))
  expect_identical(
    folded, "<details><summary>Settings (2) &lt;x&gt;</summary><p>a</p><p>b</p></details>"
  )
  for (bad in list(c("a", "b"), NA_character_, 1)) {
    expect_error(html_details(bad, ""), "`summary` must be one string", info = deparse(bad))
  }
})

test_that("html_page: a masthead with its line, the contents beside the sections (D20.30)", {
  sections <- list(
    html_section("glance", "At a glance", "<p>a</p>"), html_section("guide", "Guide", "")
  )
  page <- html_page(
    "Report <1>", sections, c("Run ID" = "r1", "Package version" = "0.1"),
    subtitle = "Run r1 & more"
  )
  expect_match(
    page, "<header class=\"masthead\"><h1>Report &lt;1&gt;</h1><p>Run r1 &amp; more</p></header>",
    fixed = TRUE
  )
  layout <- regexpr("<div class=\"layout\">\n<nav ", page, fixed = TRUE)
  expect_true(layout > 0L && layout < regexpr("<main>", page, fixed = TRUE))
  expect_match(
    page, "<li><a href=\"#glance\">At a glance</a></li><li><a href=\"#guide\">Guide</a></li>",
    fixed = TRUE
  )
  expect_match(
    page,
    paste0(
      "<footer><dl><div><dt>Run ID</dt><dd>r1</dd></div>",
      "<div><dt>Package version</dt><dd>0.1</dd></div></dl></footer>"
    ),
    fixed = TRUE
  )
  # No line given, none shown, as on pre-flight's page.
  expect_match(
    html_page("T", sections), "<header class=\"masthead\"><h1>T</h1></header>",
    fixed = TRUE
  )
  for (bad in list(c("a", "b"), NA_character_, 1)) {
    expect_error(
      html_page("T", sections, subtitle = bad), "`subtitle` must be one string or NULL",
      info = deparse(bad)
    )
  }
})

test_that("page_css() sets the template's colours and each severity's once (D20.37, D20.49)", {
  css <- page_css()
  for (colour in c("#07333E", "#10566F", "#CAC244", "#C8CBCB", "#5F7A7C", "#1B1B1A")) {
    expect_equal(count_fixed(colour, css), 1L, info = colour)
  }
  # Each severity's colours are set once, on its class, which a row and a bar both carry.
  for (severity in c("error", "warning", "flag", "info")) {
    expect_equal(count_fixed(paste0(".sev-", severity, "{"), css), 1L, info = severity)
  }
  # A filter box stays hidden without the script, though its own rule sets a display.
  expect_match(css, "[hidden]{display:none!important}", fixed = TRUE)
  expect_match(css, "@media print{", fixed = TRUE)
  expect_match(css, "@media (max-width:", fixed = TRUE)
  expect_match(css, "font-family:Arial,", fixed = TRUE)
  # A bar's value column holds a six-digit count in two columns and on a narrow screen, and in
  # print a short page ends where its content does.
  expect_match(
    css, ".cols .bar{grid-template-columns:9rem minmax(8rem,1fr) 3.75rem}",
    fixed = TRUE
  )
  expect_match(
    css, ".bar,.cols .bar{grid-template-columns:7rem minmax(3rem,1fr) 3.75rem}}",
    fixed = TRUE
  )
  expect_match(css, "main{border:0;padding:0;min-height:0}", fixed = TRUE)
  for (pattern in c("url(", "@import", "//")) {
    expect_false(grepl(pattern, css, fixed = TRUE), info = pattern)
  }
})
