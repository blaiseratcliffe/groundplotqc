# Base-R HTML builders (plan 11.1, 11.2; D8.18, D12.17, D12.21, D14.7, D14.30, D20.30,
# D20.37, D20.39, D20.49). Every value reaches HTML through html_escape(). Pages are
# self-contained, in the NRCan Dark-green template's colours: inline CSS and a hand-written
# script under 4 KB that sorts tables by a header click and filters rows; without JavaScript
# the tables read as they are, the filter boxes stay hidden and a folded part opens on a click.

#' Text escaped for HTML
#'
#' NA as "", text in another encoding converted to UTF-8, invalid bytes as `<xx>`, then
#' `& < > " '` escaped.
#' @noRd
html_escape <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  bad <- which(!validUTF8(x))
  if (length(bad) > 0L) {
    x[bad] <- mark_invalid_utf8(x[bad])
  }
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

#' Delivered text cut at `cell_cap` characters, a cut value ending with an ellipsis (D20.39)
#'
#' Characters are counted before the text is escaped for HTML, and as characters, never
#' bytes, so a cut splits neither an entity nor a UTF-8 character; html_table() escapes the
#' result. A value of `cell_cap` characters or fewer, and NA, stay as they are, and so does a
#' value whose characters can't be counted (not valid in its encoding), which html_escape()
#' then marks. Plain text in, plain text out; the issues CSV is never cut.
#' @noRd
cut_cell_text <- function(x, cell_cap) {
  ok <- is.numeric(cell_cap) && length(cell_cap) == 1L && !is.na(cell_cap) && cell_cap >= 0
  if (!ok) {
    stop("`cell_cap` must be one non-negative number.", call. = FALSE)
  }
  x <- as.character(x)
  n <- nchar(x, type = "chars", allowNA = TRUE)
  long <- which(!is.na(n) & n > cell_cap)
  if (length(long) > 0L) {
    x[long] <- paste0(substr(x[long], 1L, cell_cap), "\u{2026}")
  }
  x
}

#' A sortable, filterable HTML table
#'
#' Every cell escaped; a `row_cap` caps the rows shown, with a note naming `csv_name`, which
#' a `row_cap` always needs, as one string, whether or not the table reaches it. A column of
#' numbers (integer or double) is right-aligned, its header too (R4). `row_class` gives each
#' row one CSS class or NA (D20.37): a severity's class ("sev-error", "sev-warning",
#' "sev-flag" or "sev-info") shows the row's first cell, the severity's word, as a label in
#' the severity's colour; a row of class "total" goes in the table's foot, after the body, so
#' it is never sorted, filtered or capped and isn't counted in the note. The note and the
#' filter box's label are in `lang`, from the caller's `text` table where it has them (D14.7,
#' D14.30).
#' @noRd
html_table <- function(dt, caption, row_cap = NULL, csv_name = NULL, row_class = NULL,
                       lang = "en", text = NULL) {
  if (!is.data.frame(dt)) {
    stop("`dt` must be a data.frame or data.table.", call. = FALSE)
  }
  if (!is.character(caption) || length(caption) != 1L || is.na(caption)) {
    stop("`caption` must be one string.", call. = FALSE)
  }
  if (!is.null(row_cap)) {
    ok <- is.numeric(row_cap) && length(row_cap) == 1L && !is.na(row_cap) && row_cap >= 0
    if (!ok) {
      stop("`row_cap` must be one non-negative number or NULL.", call. = FALSE)
    }
    if (is.null(csv_name)) {
      stop("`row_cap` needs `csv_name`, the file that holds the full list.", call. = FALSE)
    }
    ok <- is.character(csv_name) && length(csv_name) == 1L && !is.na(csv_name) &&
      !is_blank(csv_name)
    if (!ok) {
      stop("`csv_name` must be one string.", call. = FALSE)
    }
  }
  if (!is.null(row_class)) {
    # Each distinct class checked once: a row_class as long as the issues holds only a few.
    distinct <- unique(row_class)
    ok <- is.character(row_class) && length(row_class) == nrow(dt) &&
      all(is.na(distinct) | grepl("^[a-z][a-z0-9-]*$", distinct))
    if (!ok) {
      stop(
        paste(
          "`row_class` must be NULL or one class per row of `dt`, each NA or a lower-case",
          "name of letters, digits and hyphens."
        ),
        call. = FALSE
      )
    }
  }
  # A total row stays out of the body, so the script neither sorts nor filters it (D20.37).
  foot <- if (is.null(row_class)) logical(nrow(dt)) else row_class %chin% "total"
  body <- which(!foot)
  total <- length(body)
  capped <- !is.null(row_cap) && total > row_cap
  shown <- if (capped) body[seq_len(row_cap)] else body
  numbers <- vapply(dt, is.numeric, logical(1))
  header <- if (length(dt) > 0L) {
    opening <- fifelse(
      numbers, "<th scope=\"col\" data-gpq-sort class=\"num\">", "<th scope=\"col\" data-gpq-sort>"
    )
    paste0(opening, html_escape(names(dt)), "</th>", collapse = "")
  } else {
    ""
  }
  cell_opening <- fifelse(numbers, "<td class=\"num\">", "<td>")
  # The rows `at` as HTML, read column by column: each cell escaped, a row with a severity's
  # class showing its first cell as the severity's label.
  rows_html <- function(at) {
    if (length(at) == 0L || length(dt) == 0L) {
      return("")
    }
    classes <- if (is.null(row_class)) rep(NA_character_, length(at)) else row_class[at]
    cells <- lapply(seq_along(dt), function(j) {
      column <- dt[[j]]
      value <- html_escape(if (length(at) == length(column)) column else column[at])
      if (j == 1L) {
        label <- startsWith(classes, "sev-") %in% TRUE
        value[label] <- paste0("<span class=\"sev\">", value[label], "</span>")
      }
      paste0(cell_opening[[j]], value, "</td>")
    })
    opening <- fifelse(is.na(classes), "<tr>", paste0("<tr class=\"", classes, "\">"))
    paste0(opening, do.call(paste0, unname(cells)), "</tr>", collapse = "")
  }
  ending <- if (any(foot)) paste0("<tfoot>", rows_html(which(foot)), "</tfoot>") else ""
  note <- if (capped) {
    paste0("<p>", html_escape(report_text(
      "report_rows_capped", lang,
      n = length(shown), total = total, csv_name = csv_name, text = text
    )), "</p>")
  } else {
    ""
  }
  label <- html_escape(report_text("report_filter_label", lang, text = text))
  paste0(
    "<div class=\"gpq-table\">",
    "<input type=\"search\" data-gpq-filter hidden aria-label=\"", label,
    "\" placeholder=\"", label, "\">",
    "<table data-gpq-table><caption>", html_escape(caption), "</caption>",
    "<thead><tr>", header, "</tr></thead><tbody>", rows_html(shown), "</tbody>", ending,
    "</table>", note, "</div>"
  )
}

#' Bars of values, each as wide as its share of `max`, with its label and its value as text
#' (plan 11.1, 11.2; D20.37)
#'
#' Vectorised: one bar per element of `value`, in its order, all in one block of bars (one
#' chart). `label` gives each bar's label, escaped; the value beside it is text, so the chart
#' reads without its colours. A bar's width is its percentage of `max`, to one decimal, the
#' page's only inline style; with `max` 0 every bar has no width. `bar_class`, one class or
#' one per value (NA for none), colours a bar as html_table()'s `row_class` colours a row: a
#' severity's class colours the bar and shows its label as the severity's label; NULL leaves
#' every bar petrol. No value gives "".
#' @noRd
html_bar <- function(value, max, label, bar_class = NULL) {
  ok <- is.numeric(value) && !anyNA(value) && all(value >= 0)
  if (!ok) {
    stop("`value` must be numbers, none of them NA or below 0.", call. = FALSE)
  }
  ok <- is.numeric(max) && length(max) == 1L && !is.na(max) && all(value <= max)
  if (!ok) {
    stop("`max` must be one number, not NA, that no value exceeds.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != length(value)) {
    stop("`label` must be text, one label per value.", call. = FALSE)
  }
  if (!is.null(bar_class)) {
    ok <- is.character(bar_class) && length(bar_class) %in% c(1L, length(value)) &&
      all(is.na(bar_class) | grepl("^[a-z][a-z0-9-]*$", bar_class))
    if (!ok) {
      stop(
        paste(
          "`bar_class` must be NULL, one class or one per value, each NA or a lower-case name",
          "of letters, digits and hyphens."
        ),
        call. = FALSE
      )
    }
  }
  if (length(value) == 0L) {
    return("")
  }
  classes <- if (is.null(bar_class)) NA_character_ else bar_class
  classes <- rep_len(classes, length(value))
  width <- if (max > 0) 100 * value / max else numeric(length(value))
  shown <- html_escape(label)
  severity <- startsWith(classes, "sev-") %in% TRUE
  shown[severity] <- paste0("<span class=\"sev\">", shown[severity], "</span>")
  bars <- paste0(
    "<div class=\"", fifelse(is.na(classes), "bar", paste("bar", classes)), "\">",
    "<span class=\"bar-label\">", shown, "</span>",
    "<span class=\"bar-track\"><span class=\"bar-fill\" style=\"width:",
    sprintf("%.1f", width), "%\"></span></span>",
    "<span class=\"bar-value\">", html_escape(as_text(value)), "</span></div>",
    collapse = ""
  )
  paste0("<div class=\"bars\">", bars, "</div>")
}

#' A list of named facts: each name a term, each value its description, both escaped
#' (D20.37, D20.40)
#'
#' `values` is a named vector or list holding one value per name, written as text (a number
#' in full), NA or a blank shown empty; with no value, "".
#' @noRd
html_facts <- function(values) {
  if (length(values) == 0L) {
    return("")
  }
  terms <- names(values)
  ok <- !is.null(terms) && !anyNA(terms) && all(nzchar(terms)) && all(lengths(values) == 1L)
  if (!ok) {
    stop("`values` must be named, with no empty name, and hold one value per name.", call. = FALSE)
  }
  shown <- vapply(values, as_text, character(1), USE.NAMES = FALSE)
  paste0(
    "<div class=\"facts\"><dl>",
    paste0("<dt>", html_escape(terms), "</dt><dd>", html_escape(shown), "</dd>", collapse = ""),
    "</dl></div>"
  )
}

#' A part folded until clicked, which a browser opens without JavaScript (D20.37, D20.40)
#'
#' `summary`, the line that stays shown, is escaped here; `body` is trusted HTML, built with
#' the builders in this file.
#' @noRd
html_details <- function(summary, body) {
  if (!is.character(summary) || length(summary) != 1L || is.na(summary)) {
    stop("`summary` must be one string.", call. = FALSE)
  }
  paste0(
    "<details><summary>", html_escape(summary), "</summary>", paste(body, collapse = ""),
    "</details>"
  )
}

#' An HTML section carrying its id and title for the contents list
#'
#' `body` is trusted HTML, built with the builders in this file: a value that goes into it
#' goes through `html_escape()` first. `title` is escaped here.
#' @noRd
html_section <- function(id, title, body) {
  if (!is.character(id) || length(id) != 1L || !grepl("^[a-z][a-z0-9-]*$", id)) {
    stop("`id` must be one lower-case name of letters, digits and hyphens.", call. = FALSE)
  }
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    stop("`title` must be one string.", call. = FALSE)
  }
  structure(
    paste0(
      "<section id=\"", id, "\"><h2>", html_escape(title), "</h2>",
      paste(body, collapse = ""), "</section>"
    ),
    gpq_section = c(id = id, title = title)
  )
}

#' A self-contained HTML page from sections (plan 11.1, 11.2; D20.30, D20.49)
#'
#' A masthead holding the title and, when `subtitle` is given, one line under it (a provider
#' report's run line; none on pre-flight's page); a contents list beside the sections,
#' linking to each section's id (none when there is no section); a footer of the named `meta`
#' values; inline CSS and script. `lang` is the page's language, written in `<html lang>`,
#' and the contents' heading is looked up in it, from the caller's `text` table where it has
#' that row (D14.7, D14.30).
#' @noRd
html_page <- function(title, sections, meta = character(), subtitle = NULL, lang = "en",
                      text = NULL) {
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    stop("`title` must be one string.", call. = FALSE)
  }
  is_section <- function(s) {
    info <- attr(s, "gpq_section")
    is.character(s) && length(s) == 1L && is.character(info) &&
      all(c("id", "title") %in% names(info))
  }
  ok <- is.list(sections) && all(vapply(sections, is_section, logical(1)))
  if (!ok) {
    stop("`sections` must be a list of sections built by `html_section()`.", call. = FALSE)
  }
  if (length(meta) > 0L) {
    meta_names <- names(meta)
    ok <- !is.null(meta_names) && !anyNA(meta_names) && all(nzchar(meta_names))
    if (!ok) {
      stop("`meta` must be named, with no empty name.", call. = FALSE)
    }
  }
  ok <- is.null(subtitle) ||
    (is.character(subtitle) && length(subtitle) == 1L && !is.na(subtitle))
  if (!ok) {
    stop("`subtitle` must be one string or NULL.", call. = FALSE)
  }
  ok <- is.character(lang) && length(lang) == 1L && !is.na(lang) && grepl(lang_pattern, lang)
  if (!ok) {
    stop("`lang` must be one language code of two or three lower-case letters.", call. = FALSE)
  }
  labels <- vapply(sections, function(s) attr(s, "gpq_section")[["title"]], character(1))
  ids <- vapply(sections, function(s) attr(s, "gpq_section")[["id"]], character(1))
  if (anyDuplicated(ids) > 0L) {
    stop("Section ids must be unique.", call. = FALSE)
  }
  nav <- if (length(ids) > 0L) {
    heading <- html_escape(report_text("report_contents", lang, text = text))
    links <- paste0(
      "<li><a href=\"#", html_escape(ids), "\">", html_escape(labels), "</a></li>",
      collapse = ""
    )
    paste0(
      "<nav aria-label=\"", heading, "\"><h2>", heading, "</h2><ol>", links, "</ol></nav>\n"
    )
  } else {
    ""
  }
  # Each fact a term and its value, kept together where the footer's line wraps.
  footer <- if (length(meta) > 0L) {
    paste0(
      "<dl>",
      paste0(
        "<div><dt>", html_escape(names(meta)), "</dt><dd>", html_escape(meta), "</dd></div>",
        collapse = ""
      ),
      "</dl>"
    )
  } else {
    ""
  }
  line <- if (is.null(subtitle)) "" else paste0("<p>", html_escape(subtitle), "</p>")
  paste0(
    "<!DOCTYPE html>\n<html lang=\"", html_escape(lang), "\">\n<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", html_escape(title), "</title>\n<style>", page_css(), "</style>\n</head>\n<body>\n",
    "<header class=\"masthead\"><h1>", html_escape(title), "</h1>", line, "</header>\n",
    "<div class=\"layout\">\n", nav,
    "<main>\n", paste(vapply(sections, as.character, character(1)), collapse = "\n"), "\n</main>\n",
    "</div>\n<footer>", footer, "</footer>\n<script>", page_script(),
    "</script>\n</body>\n</html>\n"
  )
}

#' The pages' inline CSS (plan 11.2's look; D20.37, D20.49)
#'
#' The NRCan Dark-green template's colours, Arial; a masthead, a contents sidebar that sits
#' on top on a narrow screen and is left out of print, dark header rows, striped rows, numbers
#' right-aligned, each severity's colours set once on its class, bars, fact lists, folded
#' parts and a visible keyboard focus.
#' @noRd
page_css <- function() {
  paste(
    # The template's deep green, petrol, olive, light grey, grey-teal and near-black, with
    # white and three tints of them for backgrounds and rules.
    ":root{--deep:#07333E;--petrol:#10566F;--olive:#CAC244;--mist:#C8CBCB;--sage:#5F7A7C;",
    "--ink:#1B1B1A;--paper:#FFFFFF;--wash:#F2F4F4;--rule:#DCDFDF;--tint:#E8F0F2}",
    "*{box-sizing:border-box}",
    "html{font-size:93.75%}",
    "body{margin:0;font-family:Arial,'Helvetica Neue',Helvetica,sans-serif;color:var(--ink);",
    "background:var(--wash);line-height:1.45}",
    "a{color:var(--petrol)}",
    "a:focus-visible,summary:focus-visible,input:focus-visible{outline:3px solid var(--petrol);",
    "outline-offset:2px}",
    # Drawn inside the header, so the table's scroll box doesn't clip it.
    "th:focus-visible{outline:3px solid var(--olive);outline-offset:-3px}",
    ".masthead{background:var(--deep);color:var(--paper);padding:1.25rem 1.5rem 1.1rem;",
    "border-bottom:5px solid var(--olive)}",
    ".masthead h1{margin:0;font-size:1.65rem;letter-spacing:-.01em}",
    ".masthead p{margin:.3rem 0 0;color:var(--mist);font-size:.95rem}",
    ".layout{display:flex;align-items:flex-start}",
    ".layout>nav{flex:0 0 14rem;position:sticky;top:0;max-height:100vh;overflow-y:auto;",
    "padding:1.25rem 1rem 1.25rem 1.5rem}",
    "nav h2{font-size:.85rem;color:var(--petrol);margin:0 0 .5rem}",
    "nav ol{list-style:none;margin:0;padding:0}",
    "nav a{display:block;padding:.3rem .5rem;border-left:3px solid transparent;",
    "border-radius:3px;color:var(--ink);text-decoration:none;font-size:.93rem}",
    "nav a:hover{border-left-color:var(--petrol);background:var(--tint)}",
    "main{flex:1 1 auto;min-width:0;min-height:100vh;background:var(--paper);",
    "padding:1.25rem 2.5rem 2rem;border-left:1px solid var(--rule)}",
    "section{margin:0 0 2.25rem}",
    "section>h2{font-size:1.25rem;margin:0 0 .75rem;padding-bottom:.35rem;",
    "border-bottom:2px solid var(--deep)}",
    "h3{font-size:1rem;margin:1.25rem 0 .4rem;color:var(--deep)}",
    "p{max-width:72ch;margin:.5rem 0}",
    ".lead{font-size:1.05rem}",
    ".lead strong{color:var(--deep)}",
    ".note{border:1px solid #B9CFD6;border-left:5px solid var(--petrol);background:var(--tint);",
    "padding:.6rem .85rem;border-radius:3px}",
    ".gpq-table{overflow-x:auto;margin:.4rem 0 1.4rem}",
    ".gpq-table>p{font-size:.85rem;margin:.3rem 0 0}",
    "table{border-collapse:collapse;font-size:.88rem;min-width:24rem}",
    "caption{text-align:left;font-weight:700;color:var(--deep);padding:.2rem 0 .4rem;",
    "font-size:.95rem}",
    "th,td{padding:.35rem .6rem;text-align:left;vertical-align:top;",
    "border-bottom:1px solid var(--rule)}",
    # A cell breaks a word only where it can't fit its box (D12.79).
    "td,th{overflow-wrap:break-word}",
    "thead th{background:var(--deep);color:var(--paper);border-bottom:0;white-space:nowrap}",
    "tbody tr:nth-child(even){background:var(--wash)}",
    ".num{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}",
    "tr.total td{font-weight:700;border-top:2px solid var(--deep)}",
    ".wrap{overflow-wrap:anywhere}",
    # A header looks clickable only once the script has run.
    ".gpq-js th[data-gpq-sort]{cursor:pointer}",
    "th[aria-sort=ascending]::after{content:' \\25B2';font-size:.7em}",
    "th[aria-sort=descending]::after{content:' \\25BC';font-size:.7em}",
    "input[data-gpq-filter]{display:block;margin:0 0 .35rem;padding:.3rem .5rem;font:inherit;",
    "font-size:.85rem;border:1px solid var(--sage);border-radius:3px;width:15rem;max-width:100%}",
    # A filter box stays hidden until the script shows it, whatever its own display.
    "[hidden]{display:none!important}",
    # Each severity's colours, set once on its class and read by a row's label and a bar.
    ".sev-error{--sev-bg:var(--deep);--sev-ink:var(--paper)}",
    ".sev-warning{--sev-bg:var(--olive);--sev-ink:var(--ink)}",
    ".sev-flag{--sev-bg:var(--sage);--sev-ink:var(--paper)}",
    ".sev-info{--sev-bg:var(--mist);--sev-ink:var(--ink)}",
    ".sev{display:inline-block;white-space:nowrap;padding:0 .45rem;border-radius:2px;",
    "font-size:.8rem;font-weight:700;line-height:1.5;background:var(--sev-bg);",
    "color:var(--sev-ink)}",
    ".facts{background:var(--wash);border-left:5px solid var(--deep);padding:.75rem 1rem;",
    "margin:.25rem 0 1rem;max-width:56rem}",
    ".facts dl{display:grid;grid-template-columns:max-content minmax(0,1fr);",
    "gap:.3rem 1.5rem;margin:0}",
    ".facts dt{color:var(--petrol);font-weight:700}",
    ".facts dd{margin:0;min-width:0}",
    # A 64-character hash in a <dd> would stretch the page sideways on a narrow screen.
    "dd{overflow-wrap:anywhere}",
    "details{margin:.4rem 0;max-width:56rem}",
    "details>summary{cursor:pointer;font-weight:700;color:var(--deep);padding:.3rem 0}",
    ".bars{margin:.3rem 0 1.4rem;max-width:44rem}",
    ".bar{display:grid;grid-template-columns:11rem minmax(4rem,1fr) 4.5rem;align-items:center;",
    "gap:.6rem;margin:.25rem 0;font-size:.9rem}",
    ".bar-label{overflow-wrap:anywhere}",
    ".bar-track{background:var(--wash);height:1rem;border-radius:2px;overflow:hidden}",
    ".bar-fill{display:block;height:100%;background:var(--sev-bg,var(--petrol))}",
    ".bar-value{text-align:right;font-variant-numeric:tabular-nums;font-weight:700}",
    ".cols{display:flex;flex-wrap:wrap;gap:0 2.5rem}",
    ".cols>div{flex:1 1 30rem;min-width:0;max-width:44rem}",
    ".cols>.gpq-table{flex:0 1 auto}",
    ".cols .bar{grid-template-columns:9rem minmax(8rem,1fr) 3.75rem}",
    ".guide dt{font-weight:700;color:var(--deep);margin-top:.6rem}",
    ".guide dd{margin:.1rem 0 0;max-width:72ch}",
    "footer{padding:1rem 1.5rem 2rem;color:var(--petrol);font-size:.85rem}",
    "footer dl{display:flex;flex-wrap:wrap;gap:.25rem 2rem;margin:0}",
    "footer dl>div{display:flex;gap:.4rem;min-width:0}",
    "footer dd{margin:0;color:var(--ink);font-weight:700}",
    "@media (max-width:56rem){.layout{display:block}",
    ".layout>nav{position:static;max-height:none;border-bottom:1px solid var(--rule)}",
    "main{border-left:0;padding:1rem}",
    ".bar,.cols .bar{grid-template-columns:7rem minmax(3rem,1fr) 3.75rem}}",
    "@media print{body{background:#fff}.layout{display:block}",
    ".layout>nav,input[data-gpq-filter]{display:none!important}",
    "main{border:0;padding:0;min-height:0}",
    ".masthead,thead th,.sev,.bar-fill{-webkit-print-color-adjust:exact;",
    "print-color-adjust:exact}tr{break-inside:avoid}}",
    sep = "\n"
  )
}

#' The pages' hand-written sort-and-filter script, under 4 KB
#' @noRd
page_script <- function() {
  paste(
    "(function () {",
    "  \"use strict\";",
    "  function text(row, i) { var c = row.cells[i]; return c ? c.textContent.trim() : \"\"; }",
    "  function order(a, b) {",
    "    var x = Number(a), y = Number(b);",
    "    if (a !== \"\" && b !== \"\" && !isNaN(x) && !isNaN(y)) { return x - y; }",
    "    return a.localeCompare(b);",
    "  }",
    "  var tables = document.querySelectorAll(\"table[data-gpq-table]\");",
    "  Array.prototype.forEach.call(tables, function (table) {",
    "    var heads = table.querySelectorAll(\"th[data-gpq-sort]\");",
    "    Array.prototype.forEach.call(heads, function (th, i) {",
    "      th.tabIndex = 0;",
    "      th.setAttribute(\"aria-sort\", \"none\");",
    "      function sort() {",
    "        var up = th.getAttribute(\"aria-sort\") !== \"ascending\";",
    "        Array.prototype.forEach.call(heads, function (h) {",
    "          h.setAttribute(\"aria-sort\", \"none\");",
    "        });",
    "        th.setAttribute(\"aria-sort\", up ? \"ascending\" : \"descending\");",
    "        var body = table.tBodies[0];",
    "        var rows = Array.prototype.slice.call(body.rows);",
    "        rows.sort(function (r, s) {",
    "          var d = order(text(r, i), text(s, i));",
    "          return up ? d : -d;",
    "        });",
    "        rows.forEach(function (r) { body.appendChild(r); });",
    "      }",
    "      th.addEventListener(\"click\", sort);",
    "      th.addEventListener(\"keydown\", function (e) {",
    "        if (e.key === \"Enter\" || e.key === \" \") { e.preventDefault(); sort(); }",
    "      });",
    "      document.documentElement.classList.add(\"gpq-js\");",
    "    });",
    "  });",
    "  var boxes = document.querySelectorAll(\"input[data-gpq-filter]\");",
    "  Array.prototype.forEach.call(boxes, function (box) {",
    "    var table = box.parentNode.querySelector(\"table[data-gpq-table]\");",
    "    if (!table) { return; }",
    "    box.hidden = false;",
    "    box.addEventListener(\"input\", function () {",
    "      var q = box.value.toLowerCase();",
    "      Array.prototype.forEach.call(table.tBodies[0].rows, function (r) {",
    "        r.hidden = q !== \"\" && r.textContent.toLowerCase().indexOf(q) === -1;",
    "      });",
    "    });",
    "  });",
    "})();",
    sep = "\n"
  )
}
