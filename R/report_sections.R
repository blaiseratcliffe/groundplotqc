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
    # The values read as UTF-8, so the radix sort's byte order is C order whatever a value's
    # encoding mark, without resting on data.table's optimisation (D20.42, R94).
    counted <- counted[order(
      -counted$n, counted$value == "*", enc2utf8(counted$value),
      method = "radix"
    )]
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

#' The "By check" table of sections 3 to 5: one row per check, table and column (D4.18,
#' D20.39, D20.42, D20.43)
#'
#' `issues` holds the section's issues, with at least `issue_id`, `severity`, `rule_id`,
#' `table_name`, `column_name` and `value`. Each row: the severity, the check, what it means
#' (the rule's text, from the registry's `message_id`), the table and column, the issues, and
#' up to `example_cap` distinct values in the order the run found them (by issue ID), joined
#' with "; " and cut as one cell at `cell_cap` (D20.39 counts the examples as one cell). Sorted
#' by severity (error, warning, flag, info), most issues first, then check, table and column;
#' capped at `row_cap`, the note naming the unit's CSV (D20.68).
#' @noRd
issue_summary <- function(issues, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  severities <- c("error", "warning", "flag", "info")
  keys <- c("severity", "rule_id", "table_name", "column_name")
  group <- as.integer(frankv(issues, cols = keys, ties.method = "dense"))
  n_groups <- max(c(0L, group))
  first <- match(seq_len(n_groups), group)
  n <- tabulate(group, n_groups)
  # Each check's distinct values, NA left out, in issue ID order, the first example_cap of them
  # (as the run takes its results' examples), read once for every check (16.2).
  in_order <- order(issues$issue_id)
  seen <- unique(data.table(group = group[in_order], value = issues$value[in_order]))
  seen <- seen[!is.na(seen$value)]
  seen <- seen[rowid(seen$group) <= settings$example_cap]
  examples <- rep(NA_character_, n_groups)
  joined <- vapply(split(seen$value, seen$group), paste, "", collapse = "; ")
  examples[as.integer(names(joined))] <- joined
  examples <- cut_cell_text(examples, settings$cell_cap) # D20.59: the cut's name
  registry <- rule_registry()
  message_ids <- registry$message_id[match(issues$rule_id[first], registry$rule_id)]
  messages <- vapply(unique(message_ids), word, "")
  severity <- issues$severity[first]
  # The text keys read as UTF-8, so the radix sort's byte order is C order whatever a key's
  # encoding mark (D20.42, R94).
  order_by <- order(
    match(severity, severities), -n, enc2utf8(issues$rule_id[first]),
    enc2utf8(issues$table_name[first]), enc2utf8(issues$column_name[first]),
    method = "radix"
  )
  severity_words <- vapply(paste0("report_severity_", severities), word, "", USE.NAMES = FALSE)
  shown <- data.table(
    severity_words[match(severity, severities)], issues$rule_id[first],
    unname(messages[message_ids]), issues$table_name[first], issues$column_name[first], n,
    examples
  )[order_by]
  setnames(shown, c(
    word("report_column_severity"), word("report_column_check"), word("report_column_meaning"),
    word("report_column_table"), word("report_column_column"), word("report_column_issues"),
    word("report_column_examples")
  ))
  html_table(
    shown, word("report_summary_caption"), settings$row_cap, settings$csv_name, # D20.68
    row_class = paste0("sev-", severity[order_by]), lang = lang, text = text
  )
}

#' The "Each issue" table of section 3: one row per issue (D20.17, D20.39, D20.42, D20.44,
#' D20.56)
#'
#' `issues` holds the section's record-level issues, with at least `issue_id`, `severity`,
#' `rule_id`, `table_name`, `column_name`, `provider_key`, `provider_key_status`, `record_pk`,
#' `value` and `expected`, and the columns to show after them, in order, under their raw names
#' (D20.56): every column that isn't an issues column of results_schema(), such as the
#' breakdown columns and the problem unit's key column, whatever their names. `raw_values` holds
#' the lineage input's raw value of each issue's cell, one per row of `issues`, NA where it has
#' none (D20.17); it comes apart from `issues`, so a caller's column of any name keeps its place
#' (D20.123). Each row: severity, check, table, column, provider key and its status, record ID,
#' value, raw value, expected value, then those columns, "*" in them shown as W4. Delivered text
#' is cut at `cell_cap` (D20.39). A column empty in every row is left out (D20.42, D20.44); the
#' CSV keeps every column. Sorted by severity, check, table, column and provider key, then
#' record ID and issue ID; capped at `row_cap`, the note naming the unit's CSV (D20.68).
#' @noRd
issue_records <- function(issues, raw_values, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  cut <- function(x) cut_cell_text(x, settings$cell_cap) # D20.59: the cut's name
  severities <- c("error", "warning", "flag", "info")
  statuses <- c("decoded", "partial", "raw", "fallback_pk", "no_lineage_spec")
  extra <- setdiff(names(issues), names(results_schema()$issues))
  not_linked <- word("report_not_linked")
  linked <- function(x) {
    shown <- cut(x)
    shown[x %chin% "*"] <- not_linked
    shown
  }
  severity_words <- vapply(paste0("report_severity_", severities), word, "", USE.NAMES = FALSE)
  status_words <- vapply(paste0("report_key_", statuses), word, "", USE.NAMES = FALSE)
  columns <- c(
    list(
      severity_words[match(issues$severity, severities)], issues$rule_id, issues$table_name,
      issues$column_name, cut(issues$provider_key),
      status_words[match(issues$provider_key_status, statuses)], cut(issues$record_pk),
      cut(issues$value), cut(as.character(raw_values)), cut(issues$expected)
    ),
    lapply(extra, function(column) linked(issues[[column]]))
  )
  headers <- c(
    word("report_column_severity"), word("report_column_check"), word("report_column_table"),
    word("report_column_column"), word("report_column_provider_key"),
    word("report_column_key_status"), word("report_column_record_pk"),
    word("report_column_issue_value"), word("report_column_raw_value"),
    word("report_column_expected"), extra
  )
  # A column empty in every row is left out (D20.42, D20.44), such as the raw value when no
  # lineage was given; the CSV keeps every column.
  kept <- nrow(issues) == 0L | !vapply(columns, function(x) all(is.na(x) | !nzchar(x)), TRUE)
  # The text keys read as UTF-8, so the radix sort's byte order is C order whatever a key's
  # encoding mark, as the provider CSV sorts them (D20.42, R94).
  order_by <- order(
    match(issues$severity, severities), enc2utf8(issues$rule_id), enc2utf8(issues$table_name),
    enc2utf8(issues$column_name), enc2utf8(issues$provider_key), enc2utf8(issues$record_pk),
    issues$issue_id,
    method = "radix"
  )
  shown <- as.data.table(lapply(columns[kept], function(x) x[order_by]))
  setnames(shown, headers[kept])
  html_table(
    shown, word("report_records_caption"), settings$row_cap, settings$csv_name, # D20.68
    row_class = paste0("sev-", issues$severity[order_by]), lang = lang, text = text
  )
}

#' Section 3, "Issues for you to act on": the unit's open issues of class source (D20.17,
#' D20.28, D20.42)
#'
#' The opening line; first the count of open column-level source issues, pointing to section
#' 6 (D20.28), so a whole-column problem isn't passed over; then the record-level ones in
#' issue_summary()'s "By check" table and issue_records()' "Each issue" table, which shows each
#' cell's raw value from `tables$lineage`, joined on table, record and column for an issue
#' whose key names one record (D20.17, D20.117) and passed as `raw_values` (D20.123), the
#' breakdown columns (the group columns after the report unit) and the problem unit's key
#' column. With no source issue, W101 alone.
#' @noRd
section_actions <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  issues <- tables$issues
  ours <- issues$status %chin% "open" & issues$issue_class %chin% "source"
  record <- which(ours & issues$level %chin% "record")
  whole <- sum(ours & issues$level %chin% "column")
  title <- word("report_section_actions")
  if (length(record) == 0L && whole == 0L) {
    return(html_section(
      "actions", title, paste0("<p>", html_escape(word("report_actions_empty")), "</p>")
    ))
  }
  body <- paste0("<p>", html_escape(word("report_actions_intro")), "</p>")
  if (whole > 0L) {
    # Section 6's name is a link to it, wrapped once the line is escaped.
    section <- word("report_section_column_level")
    line <- html_escape(report_text(
      "report_column_level_pointer", lang,
      n = whole, section = section, text = text
    ))
    link <- paste0("<a href=\"#column-level\">", html_escape(section), "</a>")
    body <- c(body, paste0("<p>", sub(html_escape(section), link, line, fixed = TRUE), "</p>"))
  }
  if (length(record) > 0L) {
    columns <- c(
      "issue_id", "severity", "rule_id", "table_name", "column_name", "provider_key",
      "provider_key_status", "record_pk", "value", "expected"
    )
    shown <- issues[record, unique(c(columns, tables$group_cols[-1L], tables$key_col)),
      with = FALSE
    ]
    # Each cell's raw value, by one key join on table, record and column (16.2): only cells of
    # the unit's own issues can meet a lineage row (D20.17), and only an issue whose key names
    # one record is on a cell (`pk_positional` FALSE, D20.117). The values go to
    # issue_records() apart, so a caller's column named raw_value keeps its place (D20.123).
    raw_values <- rep(NA_character_, length(record))
    lineage <- tables$lineage
    if (!is.null(lineage)) {
      cells <- c("table_name", "record_pk", "column_name")
      keyed <- which(issues$pk_positional[record] %in% FALSE)
      pairs <- key_matches(
        lapply(cells, function(column) shown[[column]][keyed]), keyed, lineage, cells
      )
      pairs <- pairs[!duplicated(pairs$row)]
      raw_values[pairs$row] <- lineage$raw_value[pairs$at]
    }
    body <- c(
      body, issue_summary(shown, text, settings),
      issue_records(shown, raw_values, text, settings)
    )
  }
  html_section("actions", title, body)
}

#' Section 4, "Issues being fixed in harmonization": the unit's open record-level issues of
#' class harmonization in a "By check" table, the records in the CSV (D20.43)
#'
#' The opening line in a note, the summary, then the count of open column-level harmonization
#' issues pointing to section 6 (D20.28). With none of either, W111 alone.
#' @noRd
section_harmonization <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  issues <- tables$issues
  ours <- issues$status %chin% "open" & issues$issue_class %chin% "harmonization"
  record <- which(ours & issues$level %chin% "record")
  whole <- sum(ours & issues$level %chin% "column")
  title <- word("report_section_harmonization")
  if (length(record) == 0L && whole == 0L) {
    return(html_section(
      "harmonization", title,
      paste0("<p>", html_escape(word("report_harmonization_empty")), "</p>")
    ))
  }
  body <- paste0("<p class=\"note\">", html_escape(word("report_harmonization_intro")), "</p>")
  if (length(record) > 0L) {
    columns <- c("issue_id", "severity", "rule_id", "table_name", "column_name", "value")
    body <- c(body, issue_summary(issues[record, columns, with = FALSE], text, settings))
  }
  if (whole > 0L) {
    # Section 6's name is a link to it, wrapped once the line is escaped.
    section <- word("report_section_column_level")
    line <- html_escape(report_text(
      "report_column_level_pointer", lang,
      n = whole, section = section, text = text
    ))
    link <- paste0("<a href=\"#column-level\">", html_escape(section), "</a>")
    body <- c(body, paste0("<p>", sub(html_escape(section), link, line, fixed = TRUE), "</p>"))
  }
  html_section("harmonization", title, body)
}

#' Section 5, "Issues under review": the unit's open record-level issues of class
#' unclassified in a "By check" table, the records in the CSV (D4.3, D20.43)
#'
#' The opening line in a note, the summary, then the count of open column-level unclassified
#' issues pointing to section 6 (D20.28). With none of either, W121 alone.
#' @noRd
section_under_review <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  issues <- tables$issues
  ours <- issues$status %chin% "open" & issues$issue_class %chin% "unclassified"
  record <- which(ours & issues$level %chin% "record")
  whole <- sum(ours & issues$level %chin% "column")
  title <- word("report_section_under_review")
  if (length(record) == 0L && whole == 0L) {
    return(html_section(
      "under-review", title,
      paste0("<p>", html_escape(word("report_under_review_empty")), "</p>")
    ))
  }
  body <- paste0("<p class=\"note\">", html_escape(word("report_under_review_intro")), "</p>")
  if (length(record) > 0L) {
    columns <- c("issue_id", "severity", "rule_id", "table_name", "column_name", "value")
    body <- c(body, issue_summary(issues[record, columns, with = FALSE], text, settings))
  }
  if (whole > 0L) {
    # Section 6's name is a link to it, wrapped once the line is escaped.
    section <- word("report_section_column_level")
    line <- html_escape(report_text(
      "report_column_level_pointer", lang,
      n = whole, section = section, text = text
    ))
    link <- paste0("<a href=\"#column-level\">", html_escape(section), "</a>")
    body <- c(body, paste0("<p>", sub(html_escape(section), link, line, fixed = TRUE), "</p>"))
  }
  html_section("under-review", title, body)
}

#' Section 6, "Issues that cover a whole column": every open column-level issue of the three
#' classes in one table (6.3; D16.8, D20.11, D20.44, D20.45)
#'
#' One row per issue: severity, class, check, what it means, table, column, the records it
#' covers (`n_records`; none for a unit rule's issue, the registry's `unit`, as in section 10,
#' D16.8, D20.122), value, expected value and the breakdown columns under their raw names
#' (D20.56), "*" in them shown as W4; delivered text cut at `cell_cap` (D20.39). Sorted by
#' severity, class (source, harmonization, unclassified), table and column, then check and
#' issue ID; a column empty in every row left out (D20.44); capped at `row_cap`. It holds the
#' unit rules' "*" issues (D16.8) and the unit's own undeclared_code_set issue (D20.11). With
#' none, W133 alone.
#' @noRd
section_column_level <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  cut <- function(x) cut_cell_text(x, settings$cell_cap) # D20.59: the cut's name
  issues <- tables$issues
  severities <- c("error", "warning", "flag", "info")
  classes <- c("source", "harmonization", "unclassified")
  title <- word("report_section_column_level")
  at <- which(
    issues$status %chin% "open" & issues$level %chin% "column" & issues$issue_class %chin% classes
  )
  if (length(at) == 0L) {
    return(html_section(
      "column-level", title, paste0("<p>", html_escape(word("report_column_level_empty")), "</p>")
    ))
  }
  # The text keys read as UTF-8, so the radix sort's byte order is C order whatever a key's
  # encoding mark (D20.42, R94).
  at <- at[order(
    match(issues$severity[at], severities), match(issues$issue_class[at], classes),
    enc2utf8(issues$table_name[at]), enc2utf8(issues$column_name[at]),
    enc2utf8(issues$rule_id[at]), issues$issue_id[at],
    method = "radix"
  )]
  registry <- rule_registry()
  message_ids <- registry$message_id[match(issues$rule_id[at], registry$rule_id)]
  messages <- vapply(unique(message_ids), word, "")
  # A unit rule's issue shows no record count, as section 10 shows none: the rule counts a
  # table or column (D16.8), and its "*" issue goes to every unit, so its count would be the
  # whole delivery's (D20.122).
  records <- issues$n_records[at]
  records[registry$unit[match(issues$rule_id[at], registry$rule_id)] %in% TRUE] <- NA_integer_
  not_linked <- word("report_not_linked")
  breakdowns <- tables$group_cols[-1L]
  severity_words <- vapply(paste0("report_severity_", severities), word, "", USE.NAMES = FALSE)
  class_words <- vapply(paste0("report_class_", classes), word, "", USE.NAMES = FALSE)
  columns <- c(
    list(
      severity_words[match(issues$severity[at], severities)],
      class_words[match(issues$issue_class[at], classes)], issues$rule_id[at],
      unname(messages[message_ids]), issues$table_name[at], issues$column_name[at],
      records, cut(issues$value[at]), cut(issues$expected[at])
    ),
    lapply(breakdowns, function(column) {
      shown <- cut(issues[[column]][at])
      shown[issues[[column]][at] %chin% "*"] <- not_linked
      shown
    })
  )
  headers <- c(
    word("report_column_severity"), word("report_column_class"), word("report_column_check"),
    word("report_column_meaning"), word("report_column_table"), word("report_column_column"),
    word("report_column_records"), word("report_column_issue_value"),
    word("report_column_expected"), breakdowns
  )
  # A column empty in every row is left out (D20.44); the CSV keeps every column. Only a text
  # column can hold "", so nzchar() reads the text columns alone (16.2).
  empty <- function(x) if (is.character(x)) all(is.na(x) | !nzchar(x)) else all(is.na(x))
  kept <- !vapply(columns, empty, TRUE)
  shown <- as.data.table(columns[kept])
  setnames(shown, headers[kept])
  html_section("column-level", title, c(
    paste0("<p>", html_escape(word("report_column_level_intro")), "</p>"),
    html_table(
      shown, word("report_records_caption"), settings$row_cap, settings$csv_name, # D20.68
      row_class = paste0("sev-", issues$severity[at]), lang = lang, text = text
    )
  ))
}

#' Section 10, "Checks not run, and records not evaluated or not reached": one table by check
#' and reason, its records summed over the unit's groups (D20.15, D20.23, D20.46, D20.53,
#' D20.54)
#'
#' A row per check, table, column and reason, from the unit's routed results (counts only,
#' D20.23) and its not-evaluated issues:
#' - a check that didn't run, by its not-run reason (W145 to W160); its records are those the
#'   results row counts unchecked (`n_unchecked`) or not evaluated with no not-evaluated issue
#'   (`n_not_evaluated_own`), the records of its not-evaluated issues being shown under their
#'   roots; a unit rule counts tables or columns, not records (D16.8), so it shows none;
#' - not evaluated because a check failed on the same value first, one row per root check
#'   (W143, the root read from the unit's issues by `root_issue_id`), each issue's records
#'   (`n_records` for a column-level one, else 1);
#' - a check that ran and couldn't judge some records (`n_not_evaluated_own`, W161, D20.54);
#' - a check that ran and didn't reach some records (`n_unchecked`, W144, D20.15).
#' Sorted by table, column and check, then the reasons in that order; at most `row_cap` rows,
#' those with the most records, shown in that order, a unit rule's row ranking as 0 (D20.91,
#' D20.94), with a line saying how many of how many it shows, which names no file, since
#' the CSV holds only the not-evaluated issues (D20.91). Above it, the self-test's line when
#' the run's self-test lists rules (9.7). With no row and no self-test line, W159.
#' @noRd
section_not_evaluated <- function(tables, text, settings) {
  lang <- settings$lang
  word <- function(text_id) report_text(text_id, lang, text = text)
  results <- tables$results
  issues <- tables$issues
  registry <- rule_registry()
  title <- word("report_section_not_evaluated")
  not_run <- results$status %chin% "not_run"
  unit_rule <- registry$unit[match(results$rule_id, registry$rule_id)] %in% TRUE
  unchecked <- fcoalesce(as.integer(results$n_unchecked), 0L)
  own <- fcoalesce(as.integer(results$n_not_evaluated_own), 0L) # D20.62
  # A row of the table: its kind (1 not run, 2 not evaluated, 3 couldn't judge, 4 not
  # reached), check, table, column, reason and records.
  part <- function(kind, at, reason, records, from = results) {
    data.table(
      kind = rep(kind, length(at)), rule_id = from$rule_id[at], table_name = from$table_name[at],
      column_name = from$column_name[at], reason = reason, n = records
    )
  }
  stopped <- which(not_run)
  reasons <- unique(results$not_run_reason[stopped])
  reason_words <- vapply(
    paste0("report_reason_", reasons, recycle0 = TRUE), word, "",
    USE.NAMES = FALSE
  )
  # The not-evaluated issues by check, table, column and root check, their records summed
  # (`n_records` for a column-level one, else 1), so each group's reason is filled once (16.2).
  waiting <- which(issues$status %chin% "not_evaluated")
  held <- data.table(
    rule_id = issues$rule_id[waiting], table_name = issues$table_name[waiting],
    column_name = issues$column_name[waiting],
    root = issues$rule_id[match(issues$root_issue_id[waiting], issues$issue_id)],
    n = fifelse(issues$level[waiting] %chin% "column", issues$n_records[waiting], 1L)
  )[, list(n = sum(n)), by = c("rule_id", "table_name", "column_name", "root")]
  judged <- which(!not_run & own > 0L)
  reached <- which(!not_run & unchecked > 0L)
  rows <- rbindlist(list(
    part(
      1L, stopped, reason_words[match(results$not_run_reason[stopped], reasons)],
      fifelse(unit_rule[stopped], NA_integer_, unchecked[stopped] + own[stopped])
    ),
    part(
      2L, seq_len(nrow(held)),
      report_text("report_reason_not_evaluated", lang, rule_id = held$root, text = text),
      held$n,
      from = held
    ),
    part(
      3L, judged, rep(word("report_reason_not_evaluated_own"), length(judged)), own[judged]
    ),
    part(4L, reached, rep(word("report_reason_unchecked"), length(reached)), unchecked[reached])
  ))
  rows <- rows[, list(n = sum(n)), by = c("kind", "rule_id", "table_name", "column_name", "reason")]
  # The text keys read as UTF-8, so the radix sort's byte order is C order whatever a key's
  # encoding mark (D20.42, R94).
  rows <- rows[order(
    enc2utf8(rows$table_name), enc2utf8(rows$column_name), enc2utf8(rows$rule_id), rows$kind,
    enc2utf8(rows$reason),
    method = "radix"
  )]
  self_test <- tables$metadata$self_test
  incomplete <- if (nrow(self_test) > 0L) {
    paste0("<p>", html_escape(report_text(
      "report_self_test_incomplete", lang,
      rule_ids = paste(self_test$rule_id, collapse = ", "), text = text
    )), "</p>")
  }
  if (nrow(rows) == 0L) {
    empty <- if (is.null(incomplete)) {
      paste0("<p>", html_escape(word("report_not_evaluated_empty")), "</p>")
    }
    return(html_section("not-evaluated", title, c(incomplete, empty)))
  }
  total <- nrow(rows)
  if (total > settings$row_cap) {
    # Capped, the rows with the most records stay, shown in the order above, so the largest
    # gaps are never the ones cut (D20.91). A unit rule's row, which counts no records, ranks
    # as 0 (D20.94), and rows of as many records rank in the order above (R73).
    ranked <- order(-fcoalesce(rows$n, 0L), seq_len(total), method = "radix")
    rows <- rows[sort(ranked[seq_len(settings$row_cap)])]
  }
  shown <- data.table(rows$table_name, rows$column_name, rows$rule_id, rows$reason, rows$n)
  setnames(shown, c(
    word("report_column_table"), word("report_column_column"), word("report_column_check"),
    word("report_column_reason"), word("report_column_records")
  ))
  html_section("not-evaluated", title, c(
    paste0("<p>", html_escape(word("report_not_evaluated_intro")), "</p>"), incomplete,
    html_table(shown, word("report_not_evaluated_caption"), lang = lang, text = text),
    if (total > nrow(rows)) {
      # report_rows_shown (D20.91): how many rows of how many, naming no file.
      paste0("<p>", html_escape(report_text(
        "report_rows_shown", lang,
        n = nrow(rows), total = total, text = text
      )), "</p>")
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
