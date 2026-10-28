# The provider issues CSV (plan 11.6; D8.18, D20.31 to D20.33, D20.35, D20.36): one row per
# issue routed to a report unit (route_unit()), 11.6's columns with the provider key's parts
# split into `key_<label>` columns, the rule's text as `message`, sorted as the report sorts,
# and every text cell guarded so a spreadsheet doesn't read it as a formula. M7a task 11's
# gpq_build_reports() writes it with fwrite(bom = TRUE), so Excel reads its accents (D8.18).

#' One report unit's issues CSV, as a table (11.6; D20.31 to D20.33, D20.36; D20.63)
#'
#' `issues` are the unit's routed issues (route_unit()); `group_cols` the group columns to
#' write, report unit first (gpq_build_reports() gives the run's, as 11.6 lists them);
#' `key_col` the problem unit's key column, or NULL; `lang` and `text` as report_text() takes
#' them. One row per issue, open or not evaluated, codes as
#' stored (D20.32). The columns, in 11.6's order: the group columns, then the key column
#' (D20.74); `table_name`, `column_name`, `rule_id`, `severity`, `issue_class`; `message`, the
#' rule's text (report_text() of its registry `message_id`, looked up once per rule; no rule's
#' text has a slot); the `key_<label>` columns (split_provider_key()), their tables in C order
#' and each table's labels in part order (D20.71), then `provider_key` and `provider_key_status`
#' (D20.75); `record_pk`, `value`, `expected`; `proposed_value` and `easy_fix`, empty until M11
#' (11.6); `status`, `change`, `fingerprint` and `n_records`. `reaches_other_unit` isn't
#' written (D20.33). The rows go in the order of the report's "Each issue" tables (D20.42,
#' issue_records()): severity (error, warning, flag, info), then rule, table, column and
#' provider key in C order, an issue with no provider key last, then record ID and issue ID
#' (R65). Every text cell is guarded (guard_csv_text(), D20.36), in the table made here, with
#' no second copy (D20.92). A caller's column can't be named `message` or start with `key_`
#' (results_schema(), refused at the run's start, D20.76), so no key part's column meets a
#' group or key column. Returns a new data.table.
#' @noRd
provider_csv_table <- function(issues, group_cols, key_col, lang = "en", text = NULL) {
  # As issue_records() sorts the "Each issue" tables, ties broken by record ID, then issue ID
  # (D20.32, R65).
  order_by <- order(
    match(issues$severity, c("error", "warning", "flag", "info")), issues$rule_id,
    issues$table_name, issues$column_name, issues$provider_key, issues$record_pk,
    issues$issue_id,
    na.last = TRUE, method = "radix"
  )
  # A column of the issues in the CSV's row order.
  sorted <- function(column) issues[[column]][order_by]
  front <- c(
    unique(c(group_cols, key_col)), "table_name", "column_name", "rule_id", "severity",
    "issue_class"
  )
  csv <- setDT(structure(lapply(front, sorted), names = front))
  # The rule's text, once per rule (D20.32).
  registry <- rule_registry()
  rule_id <- csv$rule_id
  rules <- unique(rule_id)
  messages <- vapply(rules, function(rule) {
    report_text(registry$message_id[[match(rule, registry$rule_id)]], lang, text = text)
  }, character(1), USE.NAMES = FALSE)
  set(csv, j = "message", value = messages[match(rule_id, rules)])
  # The key parts, one table at a time in C order, so a label takes the place of its first
  # table and part (D20.71); each key is split once.
  provider_key <- sorted("provider_key")
  status <- sorted("provider_key_status")
  tables <- csv$table_name
  for (table in sort(unique(tables), method = "radix")) {
    here <- which(tables == table)
    parts <- split_provider_key(provider_key[here], status[here])
    for (column in names(parts)) {
      if (is.null(csv[[column]])) {
        set(csv, j = column, value = rep(NA_character_, nrow(csv)))
      }
      set(csv, i = here, j = column, value = parts[[column]])
    }
  }
  set(csv, j = "provider_key", value = provider_key)
  set(csv, j = "provider_key_status", value = status)
  for (column in c("record_pk", "value", "expected")) {
    set(csv, j = column, value = sorted(column))
  }
  set(csv, j = "proposed_value", value = rep(NA_character_, nrow(csv)))
  set(csv, j = "easy_fix", value = rep(NA, nrow(csv)))
  for (column in c("status", "change", "fingerprint", "n_records")) {
    set(csv, j = column, value = sorted(column))
  }
  # Every column here is a new vector, none the issues' own, so the guard works in place.
  guard_csv_text(csv)
  csv
}

#' Each provider key's parts as columns, one per label (D19.47, D20.31, D20.35; D20.37)
#'
#' A `decoded` or `partial` key is its parts written `<label>=<value>` and joined by "; "
#' (decode_records()), a source part labelled with its provider's `table.column` and a derived
#' part with its bare attribute name (D19.47). The decoder gives `raw` to a record whose part
#' or label holds "; " or "=" (D20.35), so such a key splits back exactly: on "; ", then each
#' part at its "=". Every other key (`raw`, `fallback_pk`, `no_lineage_spec` or NA) leaves its
#' parts empty, and so does a key that still doesn't split exactly (a part without exactly one
#' "=" after its label, or a label given twice), never a guessed split (D20.31 (a)). Returns a
#' data.table of one row per key and one character column per label, named `key_` and the
#' label, NA where a key has no such part: the labels in the order of their first part
#' position among the keys, labels of one position in C order (D20.72). With no key that
#' splits, a table with no columns.
#' @noRd
split_provider_key <- function(provider_key, status) {
  n <- length(provider_key)
  splits <- which(status %chin% c("decoded", "partial") & !is.na(provider_key))
  pieces <- strsplit(provider_key[splits], "; ", fixed = TRUE)
  count <- lengths(pieces)
  flat <- unlist(pieces, use.names = FALSE)
  row <- rep(splits, count)
  position <- sequence(count)
  at <- regexpr("=", flat, fixed = TRUE)
  label <- substr(flat, 1L, at - 1L)
  value <- substr(flat, at + 1L, .Machine$integer.max)
  # A key with a part that has no label, more than one "=", or a label given twice keeps all
  # its parts empty.
  odd <- at < 2L | grepl("=", value, fixed = TRUE) | duplicated(data.table(row, label))
  kept <- !row %in% row[odd]
  row <- row[kept]
  label <- label[kept]
  value <- value[kept]
  labels <- unique(label[order(position[kept], label, method = "radix")])
  column <- match(label, labels)
  parts <- lapply(seq_along(labels), function(k) {
    cells <- rep(NA_character_, n)
    here <- which(column == k)
    cells[row[here]] <- value[here]
    cells
  })
  setDT(structure(parts, names = sprintf("key_%s", labels)))
}

#' A table's text cells guarded so a spreadsheet shows them as text (D20.32, D20.36)
#'
#' Every cell of a character column that starts with =, +, @, a tab or a carriage return, or
#' with - where the whole cell isn't a number, gets an apostrophe before it. A number is a
#' minus, digits with an optional point and fraction (or a point and digits), and an optional
#' exponent, and nothing else, so "-5" and "-1.5e3" stay as they are, and "-x", "-", "-Inf"
#' and "-5 " are guarded. Other columns, NA cells and empty ones are left as they are. The
#' cells are matched byte by byte, the marks being ASCII, so a cell's encoding doesn't matter.
#' `dt` is changed by reference (set()), with no second copy of the table (D20.92), so it must
#' be a table no one else holds, such as the one provider_csv_table() has just made. Returns
#' `dt`, invisibly.
#' @noRd
guard_csv_text <- function(dt) {
  number <- "^-([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"
  for (k in seq_along(dt)) {
    cells <- dt[[k]]
    if (!is.character(cells)) {
      next
    }
    # The cells starting with a mark, then the number pattern on those starting with "-" only,
    # never on every cell (m14).
    marked <- which(grepl("^[-=+@\t\r]", cells, useBytes = TRUE))
    minus <- marked[grepl("^-", cells[marked], useBytes = TRUE)]
    numbers <- minus[grepl(number, cells[minus], useBytes = TRUE)]
    risky <- setdiff(marked, numbers)
    if (length(risky) > 0L) {
      set(dt, i = risky, j = k, value = paste0("'", cells[risky]))
    }
  }
  invisible(dt)
}
