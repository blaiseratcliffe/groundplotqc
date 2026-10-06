# The pre-flight files (plan 4.2, 9.5, 11.5; D2.11, D7.11, D12.17): metadata/preflight.csv,
# the table as returned, and reports/preflight.html, a self-contained page for MAGPlot
# only (13). Uncapped at M2 (D12.17).

#' metadata/preflight.csv and reports/preflight.html under output_dir
#' @noRd
write_preflight_files <- function(results, spec, output_dir) {
  csv <- file.path(output_dir, "metadata", "preflight.csv")
  html <- file.path(output_dir, "reports", "preflight.html")
  dir.create(dirname(csv), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(html), recursive = TRUE, showWarnings = FALSE)
  fwrite(results, csv, na = "", quote = TRUE)
  # The page's text is UTF-8 already, and its bytes are written as they are, so a session
  # in another locale doesn't turn "é" into "<U+00E9>".
  connection <- file(html, open = "wb")
  on.exit(close(connection))
  writeLines(enc2utf8(preflight_html(results, spec)), connection, useBytes = TRUE)
  invisible(c(csv = csv, html = html))
}

#' The pre-flight page: a summary per check, then every row, uncapped (D12.17)
#' @noRd
preflight_html <- function(results, spec) {
  label <- function(columns) {
    vapply(paste0("preflight_column_", columns), report_text, character(1), USE.NAMES = FALSE)
  }
  # The page shows outcomes and not-run reasons as text, by reference on the page's own
  # tables; preflight.csv, the returned table and the conditions keep the codes (D12.56).
  show_codes <- function(table) {
    prefixes <- c(outcome = "preflight_outcome_", not_run_reason = "preflight_reason_")
    for (column in names(prefixes)) {
      codes <- table[[column]]
      known <- unique(codes[!is.na(codes)])
      if (length(known) == 0L) {
        next
      }
      keys <- paste0(prefixes[[column]], known)
      texts <- vapply(keys, report_text, character(1), USE.NAMES = FALSE)
      set(table, j = column, value = texts[match(codes, known)])
    }
    table
  }
  summary <- results[!duplicated(rule_id), list(rule_id, outcome, n_findings, not_run_reason)]
  summary[, description := vapply(rule_id, report_text, character(1), USE.NAMES = FALSE)]
  setcolorder(summary, c("rule_id", "description"))
  show_codes(summary)
  setnames(summary, label(names(summary)))
  rows <- show_codes(copy(results))
  setnames(rows, label(names(rows)))
  files <- spec$manifest[file != "in memory"]
  # A file without a hash is named alone; with no file read the entry is empty (R19).
  listed <- fifelse(
    is.na(files$sha256), files$file, paste0(files$file, " (", files$sha256, ")")
  )
  meta <- c(
    as.character(utils::packageVersion("groundplotqc")), paste(listed, collapse = "; ")
  )
  names(meta) <- c(report_text("preflight_meta_package"), report_text("preflight_meta_files"))
  html_page(
    report_text("preflight_title"),
    list(
      html_section("summary", report_text("preflight_summary_title"), c(
        paste0("<p>", html_escape(report_text("preflight_intro")), "</p>"),
        html_table(summary, report_text("preflight_summary_title"))
      )),
      html_section(
        "rows", report_text("preflight_rows_title"),
        html_table(rows, report_text("preflight_rows_title"))
      )
    ),
    meta
  )
}
