# The provider report's sections (plan 11.3; D8.18, D14.7, D20.28, D20.30, D20.39 to D20.47,
# D20.53 to D20.56). Each builder takes `(tables, text, settings)` and returns html_section()
# under its fixed id (D20.30). `tables` holds the unit's routed results, issues and corrections
# with what the report shows of the run, as gpq_build_reports() makes it (D20.61); `text` is
# the caller's text table or NULL; `settings` holds the resolved lang, row_cap, example_cap and
# cell_cap, the run's column_level_min_n and the unit's CSV name (D20.68). A builder reads only
# these tables, never the data (11.1), and passes the run's `lang` and the caller's `text` to
# every report_text() call (D14.7, D14.30). Every value reaches HTML through html_escape() or a
# builder of R/report_html.R that calls it (11.1), and delivered text in a table cell or a bar
# label is cut at `cell_cap` characters before it is escaped (cut_cell_text(), D20.39).
# Sections 2 to 6 show open issues and section 10 the not-evaluated ones; each open issue is
# listed in exactly one of sections 3 to 6 (D20.28).

#' Section 1, "About this report": the run's facts, then its specification files and its
#' settings, each folded (D20.40)
#'
#' The facts, as a list: the report unit with its column (none without group columns, D20.29),
#' the run ID, its start and finish in UTC, one row per input table with its file and SHA-256
#' or, for a table given in memory or read from a connection, how it came and what the run
#' recorded of it, the rule set's name and version (none without a rule set), the package
#' version, the git commit (or that there is none) and the R version. Then the specification
#' files the run read from a file (file and date) and the resolved settings (value and the tier
#' each came from; only the settings setting_definitions() names, a severity entry by its part
#' before the colon, D20.68), each in a part folded until clicked. Never the run's list of
#' units, its pre-flight, its clashes or the specification files' SHA-256 (D20.40).
#' @noRd
section_header <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  metadata <- tables$metadata
  run <- metadata$run
  # A run's time as recorded, such as "2026-10-08T14:03:12Z", shown "2026-10-08 14:03:12".
  utc <- function(time) sub("^([0-9-]+)T([0-9:]+)Z$", "\\1 \\2", time)
  terms <- c(
    if (!is.null(tables$unit)) {
      report_text("report_header_unit", lang, column = tables$group_cols[[1L]], text = text)
    },
    word("report_meta_run_id"), word("report_header_started"), word("report_header_finished")
  )
  facts <- c(tables$unit, run$run_id, utc(run$run_start), utc(run$run_end))
  # Each input table, recorded as "tables:<name>" (R/check_conformance.R): a file's name and
  # SHA-256, else how the table came ("in memory" or a connection) with what the run recorded
  # of it, its rows and, for a connection, the database's table and the driver.
  inputs <- metadata$inputs
  hashed <- !is.na(inputs$sha256)
  how <- fifelse(
    is.na(inputs$detail), inputs$file, paste(inputs$file, inputs$detail, sep = "; ")
  )
  shown <- character(nrow(inputs))
  shown[hashed] <- report_text(
    "report_header_input_file", lang,
    file = inputs$file[hashed], sha256 = inputs$sha256[hashed], text = text
  )
  shown[!hashed] <- report_text(
    "report_header_input_detail", lang,
    detail = how[!hashed], text = text
  )
  terms <- c(terms, report_text(
    "report_header_table", lang,
    table = sub("^tables:", "", inputs$input), text = text
  ))
  facts <- c(facts, shown)
  rules <- metadata$rules
  if (nrow(rules) > 0L) {
    terms <- c(terms, word("report_header_rule_set"))
    # D20.88: the name fills {rule_set_name}, shown as written, a blank as "(blank)".
    facts <- c(facts, report_text(
      "report_header_rule_set_value", lang,
      rule_set_name = rules$name[[1L]], version = rules$version[[1L]], text = text
    ))
  }
  commit <- run$git_commit
  if (is.na(commit)) {
    commit <- word("report_header_no_commit")
  }
  terms <- c(
    terms, word("report_meta_package"), word("report_header_git_commit"),
    word("report_header_r_version")
  )
  facts <- c(facts, run$package_version, commit, run$r_version)
  names(facts) <- terms
  parts <- html_facts(facts)
  # The specification files read from a file, those with a SHA-256 (the hash itself isn't
  # shown, D20.40); a date as the manifest records it from the file's name, "20261005", shown
  # "2026-10-05".
  files <- metadata$spec_manifest
  at <- which(!is.na(files$sha256))
  if (length(at) > 0L) {
    caption <- word("report_header_spec_caption")
    date <- files$file_date[at]
    dated <- as.Date(date, format = "%Y%m%d")
    listed <- data.table(files$file[at], fifelse(is.na(dated), date, format(dated)))
    setnames(listed, c(word("report_column_file"), word("report_column_date")))
    parts <- c(parts, html_details(
      paste0(caption, " (", length(at), ")"),
      html_table(listed, caption, lang = lang, text = text)
    ))
  }
  # Only the settings the registry defines, a severity entry by the part before its colon, so
  # no other row, such as the unit's CSV name, shows as a setting (D20.68).
  chosen <- metadata$settings
  chosen <- chosen[sub(":.*$", "", chosen$setting) %chin% names(setting_definitions())]
  if (nrow(chosen) > 0L) {
    caption <- word("report_header_settings_caption")
    tiers <- c(
      argument = "report_tier_argument", "rule set" = "report_tier_rule_set",
      option = "report_tier_option", "built-in" = "report_tier_built_in"
    )
    tier <- vapply(tiers, word, "")
    listed <- data.table(chosen$setting, chosen$value, unname(tier[chosen$tier]))
    setnames(listed, c(
      word("report_column_setting"), word("report_column_value"), word("report_column_tier")
    ))
    parts <- c(parts, html_details(
      paste0(caption, " (", nrow(chosen), ")"),
      html_table(listed, caption, lang = lang, text = text)
    ))
  }
  html_section("header", word("report_section_header"), parts)
}

#' Section 2, "At a glance": the unit's open issues counted, each once (D20.41, D20.55)
#'
#' A line of the open issues by class; bars by severity; a table by layer and severity, one row
#' for each layer the unit's results or open issues have, with a total row and column; a table
#' by class, three rows (D20.55); then bars by each breakdown column (the group columns after
#' the report unit) and by table. A column-level issue counts once (D20.41). Breakdown values
#' come from the issues, never the results (D20.23): "*" shows as W4, and any other value is
#' cut at `cell_cap` (D20.39). A list of bars shows at most `row_cap` values, most issues
#' first, with a line saying the issues CSV has them all (D20.90).
#' @noRd
section_glance <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  issues <- tables$issues
  open <- which(issues$status %chin% "open")
  severities <- c("error", "warning", "flag", "info")
  classes <- c("source", "harmonization", "unclassified")
  severity <- issues$severity[open]
  layer <- issues$layer[open]
  by_class <- tabulate(match(issues$issue_class[open], classes), length(classes))
  lead <- report_text(
    "report_glance_summary", lang,
    n = length(open), n_source = by_class[[1L]], n_harmonization = by_class[[2L]],
    n_review = by_class[[3L]], text = text
  )
  severity_words <- vapply(paste0("report_severity_", severities), word, "", USE.NAMES = FALSE)
  by_severity <- tabulate(match(severity, severities), length(severities))
  severity_bars <- html_bar(
    by_severity, max(by_severity), severity_words,
    bar_class = paste0("sev-", severities)
  )
  layers <- sort(unique(c(tables$results$layer, unique(layer))))
  counts <- lapply(severities, function(each) {
    n <- tabulate(match(layer[severity == each], layers), length(layers))
    c(n, sum(n))
  })
  by_layer <- as.data.table(c(
    list(c(
      vapply(paste0("report_layer_", layers, recycle0 = TRUE), word, "", USE.NAMES = FALSE),
      word("report_column_total")
    )),
    counts, list(Reduce(`+`, counts))
  ))
  setnames(by_layer, c(word("report_column_layer"), severity_words, word("report_column_total")))
  # The total row goes in the table's foot, outside its sort and cap (html_table()'s class
  # "total").
  layer_table <- html_table(
    by_layer, word("report_glance_layer_caption"),
    row_class = c(rep(NA_character_, length(layers)), "total"), lang = lang, text = text
  )
  by_class_table <- data.table(
    vapply(paste0("report_class_", classes), word, "", USE.NAMES = FALSE), by_class
  )
  setnames(by_class_table, c(word("report_column_class"), word("report_column_issues")))
  class_table <- html_table(
    by_class_table, word("report_glance_class_caption"),
    lang = lang, text = text
  )
  not_linked <- word("report_not_linked")
  # One chart of the open issues by a column's values, most issues first, then by value, "*"
  # after the values with as many issues, at most row_cap values (D20.90); a group value is
  # delivered text, so it is cut, and "*" shows as W4.
  chart <- function(heading, values, delivered) {
    if (length(values) == 0L) {
      return(character())
    }
    counted <- data.table(value = values)[, list(n = .N), by = "value"]
    counted <- counted[order(-counted$n, counted$value == "*", counted$value, method = "radix")]
    total <- nrow(counted)
    shown <- counted[seq_len(min(total, settings$row_cap))]
    label <- shown$value
    if (delivered) {
      label <- cut_cell_text(label, settings$cell_cap) # D20.59: the cut's name
      label[shown$value == "*"] <- not_linked
    }
    c(
      paste0("<h3>", html_escape(heading), "</h3>"),
      if (nrow(shown) > 0L) html_bar(shown$n, shown$n[[1L]], label),
      if (total > nrow(shown)) {
        # report_bars_capped (D20.90): the values shown, and the CSV that has them all.
        paste0("<p>", html_escape(report_text(
          "report_bars_capped", lang,
          n = nrow(shown), csv_name = settings$csv_name, text = text
        )), "</p>")
      }
    )
  }
  breakdowns <- unlist(lapply(tables$group_cols[-1L], function(column) {
    chart(
      report_text("report_glance_bars_column", lang, column = column, text = text),
      issues[[column]][open], TRUE
    )
  }))
  by_table <- chart(word("report_glance_bars_table"), issues$table_name[open], FALSE)
  html_section("glance", word("report_section_glance"), c(
    paste0("<p class=\"lead\">", html_escape(lead), "</p>"),
    paste0("<h3>", html_escape(word("report_glance_severity_bars")), "</h3>"), severity_bars,
    "<div class=\"cols\">", layer_table, class_table, "</div>",
    if (length(by_table) > 0L) {
      c(
        "<div class=\"cols\">",
        if (length(breakdowns) > 0L) c("<div>", breakdowns, "</div>"),
        "<div>", by_table, "</div></div>"
      )
    }
  ))
}

#' Section 14, "How to read this report": short paragraphs, each only where the report has
#' something it explains, a layer's line only for a layer the run ran (D20.47)
#'
#' Always: what the report is, naming the unit's CSV. With a results row: the layers' lead-in
#' and a line for each layer the unit's results have a row of. With an open issue: severities,
#' then classes. With a Layer 2 results row (the missing-value code checks): missing-value
#' codes. With a key status in section 3's "Each issue" table (an open record-level source
#' issue with a `provider_key_status`): provider keys. With a row in section 10 (a check that
#' didn't run, a not-evaluated issue, or records a check that ran didn't reach or couldn't
#' judge): not evaluated, not reached and not run. With an open column-level issue:
#' column-level issues, with the run's `column_level_min_n`. With an open issue or a row in
#' section 10: the caps. The layers are a list of terms; the body is wrapped in the page's
#' "guide" class.
#' @noRd
section_guide <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  paragraph <- function(x) paste0("<p>", html_escape(x), "</p>")
  results <- tables$results
  issues <- tables$issues
  open <- issues$status %chin% "open"
  ran <- !results$status %chin% "not_run"
  listed <- any(!ran) || any(issues$status %chin% "not_evaluated") ||
    any(ran & (results$n_unchecked > 0L | results$n_not_evaluated_own > 0L), na.rm = TRUE)
  keyed <- any(
    open & issues$level %chin% "record" & issues$issue_class %chin% "source" &
      !is.na(issues$provider_key_status)
  )
  body <- paragraph(report_text(
    "report_guide_about", lang,
    csv_name = settings$csv_name, text = text # D20.68
  ))
  layers <- sort(unique(results$layer))
  if (length(layers) > 0L) {
    # Each layer's name and line as a term and its description, escaped.
    terms <- vapply(paste0("report_layer_", layers), word, "", USE.NAMES = FALSE)
    lines <- vapply(paste0("report_guide_layer_", layers), word, "", USE.NAMES = FALSE)
    body <- c(
      body, paragraph(word("report_guide_layers")),
      paste0(
        "<dl>",
        paste0("<dt>", html_escape(terms), "</dt><dd>", html_escape(lines), "</dd>", collapse = ""),
        "</dl>"
      )
    )
  }
  if (any(open)) {
    body <- c(
      body, paragraph(word("report_guide_severities")), paragraph(word("report_guide_classes"))
    )
  }
  if (any(results$layer == 2L)) {
    body <- c(body, paragraph(word("report_guide_sentinels")))
  }
  if (keyed) {
    body <- c(body, paragraph(word("report_guide_keys")))
  }
  if (listed) {
    body <- c(body, paragraph(word("report_guide_not_evaluated")))
  }
  if (any(open & issues$level %chin% "column")) {
    body <- c(body, paragraph(report_text(
      "report_guide_column_level", lang,
      min_n = settings$column_level_min_n, text = text
    )))
  }
  if (any(open) || listed) {
    body <- c(body, paragraph(report_text(
      "report_guide_caps", lang,
      row_cap = settings$row_cap, example_cap = settings$example_cap,
      cell_cap = settings$cell_cap, text = text
    )))
  }
  html_section(
    "guide", word("report_section_guide"), c("<div class=\"guide\">", body, "</div>")
  )
}
