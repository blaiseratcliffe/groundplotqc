# The pre-flight files (plan 4.2, 9.5, 11.5; D2.11, D7.11, D12.17, D12.67, D14.11, D14.12,
# D14.14): metadata/preflight.csv, the table as returned, and reports/preflight.html, a
# self-contained page that never goes to a provider (13). Uncapped (D12.17).

#' metadata/preflight.csv and reports/preflight.html under output_dir
#' @noRd
write_preflight_files <- function(results, spec, output_dir) {
  csv <- file.path(output_dir, "metadata", "preflight.csv")
  html <- file.path(output_dir, "reports", "preflight.html")
  # Both folders are made, or the call stops naming the one it couldn't, before any file.
  for (folder in dirname(c(csv, html))) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(folder)) {
      stop(
        "Can't create the folder ", folder, " for the pre-flight files; a file of that name ",
        "may be in the way.",
        call. = FALSE
      )
    }
  }
  fwrite(results, csv, na = "", quote = TRUE)
  # The page is built before its file opens, so an error in building it leaves no empty
  # page behind.
  page <- preflight_html(results, spec)
  # The page's text is UTF-8 already, and its bytes are written as they are, so a session
  # in another locale doesn't turn an accented letter into text such as "<U+00E9>".
  connection <- file(html, open = "wb")
  on.exit(close(connection))
  writeLines(enc2utf8(page), connection, useBytes = TRUE)
  invisible(c(csv = csv, html = html))
}

#' The page's result line: stopped, passed with warnings or passed, then the checks that
#' didn't run, if any (D12.67 (2); D14.11, D14.14)
#' @noRd
preflight_result_line <- function(results) {
  stops <- results$outcome == "stop"
  warns <- results$outcome == "warn"
  n_warnings <- sum(warns)
  n_warning_checks <- length(unique(results$rule_id[warns]))
  line <- if (any(stops)) {
    n_findings <- sum(stops)
    n_checks <- length(unique(results$rule_id[stops]))
    if (n_warnings > 0L) {
      report_text(
        "preflight_result_stop",
        n_findings = n_findings, n_checks = n_checks, n_warnings = n_warnings,
        n_warning_checks = n_warning_checks
      )
    } else {
      report_text("preflight_result_stop_only", n_findings = n_findings, n_checks = n_checks)
    }
  } else if (n_warnings > 0L) {
    report_text(
      "preflight_result_warn",
      n_warnings = n_warnings, n_warning_checks = n_warning_checks
    )
  } else {
    report_text("preflight_result_pass")
  }
  n_not_run <- sum(results$outcome == "not_run")
  if (n_not_run > 0L) {
    line <- paste(line, report_text("preflight_result_not_run", n_not_run = n_not_run))
  }
  line
}

#' The pre-flight page: the result line, a summary per check, then every row, uncapped
#' (D12.17)
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
  # Each row repeats its check's total, so its column says so (D14.12).
  row_labels <- label(names(rows))
  row_labels[names(rows) == "n_findings"] <- report_text("preflight_rows_column_n_findings")
  setnames(rows, row_labels)
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
        paste0("<p>", html_escape(preflight_result_line(results)), "</p>"),
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
