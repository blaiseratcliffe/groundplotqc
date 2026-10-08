# Pre-flight checks of the rule set, the settings and the report text (plan 4.2; D9.21,
# D14.5 to D14.8, D14.13, D14.19, D14.21). Each takes the spec and pre-flight's context and
# returns findings_of(...) or the reason it couldn't run, as the checks in R/preflight.R do.
# A finding on a rule-set row read from a file names the file and the row's line (D14.19);
# the other findings concern no file, so `file` is NA.

#' What pre-flight's checks read besides the spec (D14.6, D14.13)
#'
#' The rule set and the text table as checked, the severity setting's argument and option
#' entries, the resolved run language and the registry. Caller errors in any of them stop
#' here, before a check runs.
#' @noRd
preflight_context <- function(rules = NULL, settings = list(), text = NULL) {
  rules <- validate_rule_set(rules)
  check_settings(settings, rules)
  list(
    rules = rules, severity = severity_entries(settings),
    lang = resolve_setting("lang", settings, rules)$value,
    text = validate_text_table(text), registry = rule_registry()
  )
}

#' The rule set's rules rows, numbered as R counts rows, each placed for a finding (D14.19)
#'
#' Columns added: `file` and `source_cell` ("<file>:<line>") from the component's origin, NA
#' without one, and `position`, where the row is as report text: the line of its file, or
#' its row as R counts rows. No rows without a rule set.
#' @noRd
rule_set_rows <- function(context) {
  rows <- context$rules$rules
  if (is.null(rows)) {
    rows <- empty_table(rule_set_schema()$rules)
  }
  file <- attr(rows, "source_file", exact = TRUE)
  lines <- attr(rows, "source_lines", exact = TRUE)
  out <- cbind(data.table(row = seq_len(nrow(rows))), rows)
  if (is.null(file)) {
    set(out, j = c("file", "source_cell"), value = list(
      rep(NA_character_, nrow(out)), rep(NA_character_, nrow(out))
    ))
    set(out, j = "position", value = report_text("rule_set_position_memory", row = out$row))
  } else {
    set(out, j = c("file", "source_cell"), value = list(
      rep(file, nrow(out)), paste0(file, ":", lines, recycle0 = TRUE)
    ))
    set(out, j = "position", value = report_text(
      "rule_set_position_file",
      line = lines, file = file
    ))
  }
  out
}

#' Where a severity setting entry came from, as report text
#' @noRd
setting_where <- function(tier) {
  fifelse(
    tier == "argument", report_text("preflight_setting_argument"),
    report_text("preflight_setting_option")
  )
}

#' Which of `values` aren't allowed for their rules, by allowed_overrides()'s table
#' @noRd
not_allowed <- function(allowed, rule_ids, values) {
  asked <- data.table(rule_id = rule_ids, value = values)
  is.na(allowed[asked, on = c("rule_id", "value"), which = TRUE, mult = "first"])
}

#' The allowed values of each rule, ", "-joined, for a finding's detail
#'
#' A loop over the rule set's rows, which are configuration, never data (16.2).
#' @noRd
allowed_text <- function(allowed, rule_ids) {
  vapply(rule_ids, function(id) {
    paste(allowed$value[allowed$rule_id == id], collapse = ", ")
  }, "", USE.NAMES = FALSE)
}

#' The checks of the rule set and the severity setting (4.2; D9.21, D14.5, D14.19)
#' @noRd
preflight_rule_set_checks <- function() {
  list(
    rule_set_unknown_column = function(spec, context) {
      if (is.null(context$rules)) {
        return("no_input")
      }
      rows <- rule_set_rows(context)
      a <- spec$attributes
      every_table <- rows$table_name %chin% "*"
      # A blank table names no table, even beside a dictionary row with a blank one.
      no_table <- !every_table &
        (is.na(rows$table_name) | !rows$table_name %chin% unique(a$table_name))
      every_attribute <- rows$attribute_name %chin% "*"
      pairs <- unique(a[, list(table_name, attribute_name)])
      in_table <- !is.na(rows$attribute_name) & !is.na(
        pairs[rows, on = c("table_name", "attribute_name"), which = TRUE, mult = "first"]
      )
      in_any <- !is.na(rows$attribute_name) & rows$attribute_name %chin% a$attribute_name
      no_attribute <- !no_table & !every_attribute & fifelse(every_table, !in_any, !in_table)
      by_table <- rows[no_table]
      by_attribute <- rows[no_attribute & !every_table]
      by_any <- rows[no_attribute & every_table]
      rbindlist(list(
        findings_of(
          by_table$file,
          report_text(
            "preflight_detail_rule_set_unknown_column_table",
            position = by_table$position, table_name = by_table$table_name
          ),
          by_table$source_cell
        ),
        findings_of(
          by_attribute$file,
          report_text(
            "preflight_detail_rule_set_unknown_column_attribute",
            position = by_attribute$position, table_name = by_attribute$table_name,
            attribute_name = by_attribute$attribute_name
          ),
          by_attribute$source_cell
        ),
        findings_of(
          by_any$file,
          report_text(
            "preflight_detail_rule_set_unknown_column_any",
            position = by_any$position, attribute_name = by_any$attribute_name
          ),
          by_any$source_cell
        )
      ))
    },
    rule_id_unknown = function(spec, context) {
      if (is.null(context$rules) && nrow(context$severity) == 0L) {
        return("no_input")
      }
      registered <- context$registry$rule_id
      rows <- rule_set_rows(context)
      rows <- rows[!rows$rule_id %chin% registered]
      entries <- context$severity[!context$severity$rule_id %chin% registered]
      rbindlist(list(
        findings_of(
          rows$file,
          report_text(
            "preflight_detail_rule_id_unknown",
            position = rows$position, rule_id = rows$rule_id
          ),
          rows$source_cell
        ),
        findings_of(NA_character_, report_text(
          "preflight_detail_rule_id_unknown_setting",
          where = setting_where(entries$tier), rule_id = entries$rule_id
        ))
      ))
    },
    rule_set_override_invalid = function(spec, context) {
      if (is.null(context$rules) && nrow(context$severity) == 0L) {
        return("no_input")
      }
      registry <- context$registry
      allowed <- allowed_overrides(registry)
      preflight_ids <- registry$rule_id[registry$check_type == "preflight"]
      open_ids <- setdiff(registry$rule_id, preflight_ids)
      rows <- rule_set_rows(context)
      # A rule set can't change a pre-flight check, whatever its row says (D14.5).
      # Each subset's test is worked out before the brackets, so no name inside them can be
      # taken for a column (severity and class are columns of the rows).
      is_fixed <- rows$rule_id %chin% preflight_ids
      is_open <- rows$rule_id %chin% open_ids
      fixed <- rows[is_fixed]
      bad_severity <- is_open & !is.na(rows$severity) &
        not_allowed(allowed$severity, rows$rule_id, rows$severity)
      bad_class <- is_open & !is.na(rows$class) &
        not_allowed(allowed$class, rows$rule_id, rows$class)
      with_severity <- rows[bad_severity]
      with_class <- rows[bad_class]
      entries <- context$severity
      entry_fixed <- entries[entries$rule_id %chin% preflight_ids]
      bad_entry <- entries$rule_id %chin% open_ids &
        not_allowed(allowed$severity, entries$rule_id, entries$severity)
      entry_open <- entries[bad_entry]
      rbindlist(list(
        findings_of(
          fixed$file,
          report_text(
            "preflight_detail_rule_set_override_invalid_preflight",
            position = fixed$position, rule_id = fixed$rule_id
          ),
          fixed$source_cell
        ),
        findings_of(
          with_severity$file,
          report_text(
            "preflight_detail_rule_set_override_invalid_severity",
            position = with_severity$position, rule_id = with_severity$rule_id,
            value = with_severity$severity,
            allowed = allowed_text(allowed$severity, with_severity$rule_id)
          ),
          with_severity$source_cell
        ),
        findings_of(
          with_class$file,
          report_text(
            "preflight_detail_rule_set_override_invalid_class",
            position = with_class$position, rule_id = with_class$rule_id,
            value = with_class$class, allowed = allowed_text(allowed$class, with_class$rule_id)
          ),
          with_class$source_cell
        ),
        findings_of(NA_character_, report_text(
          "preflight_detail_rule_set_override_invalid_setting_preflight",
          where = setting_where(entry_fixed$tier), rule_id = entry_fixed$rule_id
        )),
        findings_of(NA_character_, report_text(
          "preflight_detail_rule_set_override_invalid_setting",
          where = setting_where(entry_open$tier), rule_id = entry_open$rule_id,
          value = entry_open$severity,
          allowed = allowed_text(allowed$severity, entry_open$rule_id)
        ))
      ))
    }
  )
}

# ---- task 6 ----

#' The checks of the report text (4.2; D7.6, D14.7, D14.8, D14.13, D14.21)
#'
#' text_id_missing and text_id_fallback read every registered rule's message_id:
#' text_id_missing in English, and text_id_fallback, in a run language other than English,
#' in that language, a row in the caller's table or the engine's counting (the first two
#' steps of D14.8's lookup). text_slot_unknown reads the caller's table: each {slot} of a
#' row that the engine's English row of the same text_id lacks is a finding; fewer slots
#' are fine, and a text_id the engine lacks is skipped (D14.21).
#' @noRd
preflight_text_checks <- function() {
  list(
    text_id_missing = function(spec, context) {
      registry <- context$registry
      has_text <- registry$message_id %chin% text_ids("en", context$text)
      no_text <- registry[!has_text]
      findings_of(NA_character_, report_text(
        "preflight_detail_text_id_missing",
        rule_id = no_text$rule_id, message_id = no_text$message_id
      ))
    },
    text_id_fallback = function(spec, context) {
      if (identical(context$lang, "en")) {
        return(no_findings())
      }
      registry <- context$registry
      has_row <- registry$message_id %chin% text_ids(context$lang, context$text)
      no_row <- registry[!has_row]
      findings_of(NA_character_, report_text(
        "preflight_detail_text_id_fallback",
        rule_id = no_row$rule_id, message_id = no_row$message_id, language = context$lang
      ))
    },
    text_slot_unknown = function(spec, context) {
      text <- context$text
      if (is.null(text)) {
        return(no_findings())
      }
      engine <- report_texts()
      # Each subset's test is worked out before the brackets: `text` is also a column there.
      is_english <- engine$lang == "en"
      english <- engine[is_english]
      known <- text$text_id %chin% english$text_id
      used <- text_slots(text[known])
      package_slots <- text_slots(english)
      unknown <- unique(used[!package_slots, on = c("text_id", "slot")])
      findings_of(NA_character_, report_text(
        "preflight_detail_text_slot_unknown",
        message_id = unknown$text_id, language = unknown$lang, slot = unknown$slot
      ))
    }
  )
}

#' Each {slot} the rows of a text table use: one row per text_id, lang and slot, the slot
#' written with its braces (D14.21)
#' @noRd
text_slots <- function(texts) {
  found <- regmatches(texts$text, gregexpr("\\{[a-z0-9_]+\\}", texts$text))
  n <- lengths(found)
  data.table(
    text_id = rep(texts$text_id, n), lang = rep(texts$lang, n),
    slot = as.character(unlist(found))
  )
}
