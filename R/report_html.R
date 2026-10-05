# Base-R HTML builders (plan 11.1, 11.2; D8.18, D12.17, D12.21). Every value reaches
# HTML through html_escape(). Pages are self-contained: inline CSS and a hand-written
# script under 4 KB that sorts tables by a header click and filters rows; without
# JavaScript the tables read as they are and the filter boxes stay hidden.

#' Text escaped for HTML
#'
#' NA as "", invalid bytes as `<xx>`, then `& < > " '` escaped.
#' @noRd
html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  bad <- which(!validUTF8(x))
  if (length(bad) > 0L) {
    x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
  }
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

#' A sortable, filterable HTML table
#'
#' Every cell escaped; capped at `row_cap` rows with a note naming `csv_name`.
#' @noRd
html_table <- function(dt, caption, row_cap = NULL, csv_name = NULL) {
  total <- nrow(dt)
  capped <- !is.null(row_cap) && total > row_cap
  if (capped && is.null(csv_name)) {
    stop("A capped table needs `csv_name`.", call. = FALSE)
  }
  shown <- if (capped) seq_len(row_cap) else seq_len(total)
  header <- paste0(
    "<th scope=\"col\" data-gpq-sort>", html_escape(names(dt)), "</th>",
    collapse = ""
  )
  cells <- lapply(dt, function(column) paste0("<td>", html_escape(column[shown]), "</td>"))
  rows <- if (length(shown) > 0L && length(cells) > 0L) {
    paste0("<tr>", do.call(paste0, unname(cells)), "</tr>", collapse = "")
  } else {
    ""
  }
  note <- if (capped) {
    paste0("<p>", html_escape(report_text(
      "report_rows_capped",
      n = length(shown), total = total, csv_name = csv_name
    )), "</p>")
  } else {
    ""
  }
  label <- html_escape(report_text("report_filter_label"))
  paste0(
    "<div class=\"gpq-table\">",
    "<input type=\"search\" data-gpq-filter hidden aria-label=\"", label,
    "\" placeholder=\"", label, "\">",
    "<table data-gpq-table><caption>", html_escape(caption), "</caption>",
    "<thead><tr>", header, "</tr></thead><tbody>", rows, "</tbody></table>",
    note, "</div>"
  )
}

#' An HTML section carrying its id and title for the contents list
#' @noRd
html_section <- function(id, title, body) {
  if (!is.character(id) || length(id) != 1L || !grepl("^[a-z][a-z0-9-]*$", id)) {
    stop("`id` must be one lower-case name of letters, digits and hyphens.", call. = FALSE)
  }
  structure(
    paste0(
      "<section id=\"", id, "\"><h2>", html_escape(title), "</h2>",
      paste(body, collapse = ""), "</section>"
    ),
    gpq_section = c(id = id, title = title)
  )
}

#' A self-contained HTML page from sections
#'
#' Contents list from the sections' ids and titles, inline CSS and script, and a footer of
#' the named `meta` values.
#' @noRd
html_page <- function(title, sections, meta = character()) {
  labels <- vapply(sections, function(s) attr(s, "gpq_section")[["title"]], character(1))
  ids <- vapply(sections, function(s) attr(s, "gpq_section")[["id"]], character(1))
  contents <- paste0("<li><a href=\"#", ids, "\">", html_escape(labels), "</a></li>", collapse = "")
  footer <- if (length(meta) > 0L) {
    paste0(
      "<dl>",
      paste0(
        "<dt>", html_escape(names(meta)), "</dt><dd>", html_escape(meta), "</dd>",
        collapse = ""
      ),
      "</dl>"
    )
  } else {
    ""
  }
  heading <- html_escape(report_text("report_contents"))
  paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", html_escape(title), "</title>\n<style>", page_css(), "</style>\n</head>\n<body>\n",
    "<header><h1>", html_escape(title), "</h1></header>\n",
    "<nav aria-label=\"", heading, "\"><h2>", heading, "</h2><ol>", contents, "</ol></nav>\n",
    "<main>\n", paste(vapply(sections, as.character, character(1)), collapse = "\n"), "\n</main>\n",
    "<footer>", footer, "</footer>\n<script>", page_script(), "</script>\n</body>\n</html>\n"
  )
}

#' The pages' inline CSS
#' @noRd
page_css <- function() {
  paste(
    "body{font-family:system-ui,sans-serif;margin:1.5rem;line-height:1.4;color:#1a1a1a}",
    "table{border-collapse:collapse;margin:.5rem 0 1.5rem}",
    "caption{text-align:left;font-weight:600;padding:.25rem 0}",
    "th,td{border:1px solid #c8c8c8;padding:.25rem .5rem;text-align:left;vertical-align:top}",
    "th[data-gpq-sort]{cursor:pointer;background:#f2f2f2}",
    "th[aria-sort=ascending]::after{content:' \\25B2'}",
    "th[aria-sort=descending]::after{content:' \\25BC'}",
    "input[data-gpq-filter]{margin:.5rem 0;padding:.25rem}",
    "[hidden]{display:none}",
    "footer{margin-top:2rem;font-size:.9rem;color:#444}",
    "dt{font-weight:600}",
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
    "      function sort() {",
    "        var up = th.getAttribute(\"aria-sort\") !== \"ascending\";",
    "        Array.prototype.forEach.call(heads, function (h) {",
    "          h.removeAttribute(\"aria-sort\");",
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
