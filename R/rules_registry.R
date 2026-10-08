# The rule registry (plan 9.7; D7.25, D8.11, D9.21, D14.5, D14.10): what each rule is, one
# row per rule ID, in package code. The rule set's rules component says where and how a rule
# applies (R/rules_set.R); the two share only rule_id. Each milestone registers its rules
# here through the add-rule skill.

#' The rule registry: one row per registered rule
#'
#' Columns: `rule_id`; `stage` (`conformance`, `plausibility`); `layer` (0 to 7, NA for a
#' pre-flight check, D7.23); `check_type` (`preflight` for the checks of 4.2);
#' `default_severity` (`error`, `warning`, `flag`, `info`; a pre-flight check's `error`
#' stops and its `warning` warns, and no rule set or setting changes either, D14.5);
#' `default_class` (`source`, `harmonization`, `depends`, or `none` for information rules
#' and pre-flight checks, which write no issues); `inputs` (what the rule reads, "; "-joined:
#' specification components, `rules`, `settings:<name>`, `text`); `strategy` (`none` for a
#' rule that proposes nothing); `message_id` (its text_id in the report text). Layer 6 and 7
#' rules add their reason codes at M12 (D9.33).
#' @noRd
rule_registry <- function() {
  preflight <- function(rule_id, severity, inputs) {
    data.table(
      rule_id = rule_id, stage = "conformance", layer = NA_integer_, check_type = "preflight",
      default_severity = severity, default_class = "none", inputs = inputs,
      strategy = "none", message_id = rule_id
    )
  }
  lists <- "code_list_map; code_lists; non_code_sheets"
  rbindlist(list(
    preflight("dd_duplicate_attribute", "error", "attributes"),
    preflight("dd_type_unknown", "error", "attributes; type_map; read_findings"),
    preflight("dd_type_column_ambiguous", "error", "read_findings"),
    preflight("dd_pk_missing", "error", "attributes; keys"),
    preflight("dd_fk_target_missing", "error", "attributes; keys"),
    preflight("code_list_missing", "error", "code_list_map"),
    preflight("code_column_missing", "error", "code_list_map; code_lists"),
    preflight("code_list_duplicate_code", "warning", lists),
    preflight("code_list_blank_row", "warning", lists),
    preflight("code_list_unreferenced", "error", paste0("code_list_sheets; ", lists)),
    preflight("code_list_empty_column", "warning", lists),
    preflight("spec_clash_resolved", "warning", "clashes"),
    preflight("spec_clash_unresolved", "error", "clashes; read_findings"),
    preflight("datasets_row_missing", "warning", "read_findings"),
    preflight("spec_encoding_invalid", "error", "read_findings"),
    preflight("spec_csv_malformed", "error", "read_findings"),
    preflight("site_id_range_invalid", "error", "read_findings"),
    preflight("lineage_spec_unparseable", "error", "attributes; lineage_spec; read_findings"),
    preflight("lineage_name_unknown", "warning", "attributes; lineage_spec"),
    preflight("lineage_spec_row_unflagged", "warning", "attributes; lineage_spec"),
    preflight("lineage_id_unflagged", "warning", "attributes"),
    preflight("crosswalk_unreadable", "error", "read_findings"),
    # ---- task 5 ----
    preflight("rule_set_unknown_column", "error", "attributes; rules"),
    preflight("rule_id_unknown", "error", "rules; settings:severity"),
    preflight("rule_set_override_invalid", "error", "rules; settings:severity"),
    # ---- task 6 ----
    preflight("text_id_missing", "error", "text"),
    preflight("text_id_fallback", "warning", "text; settings:lang"),
    preflight("text_slot_unknown", "error", "text")
  ))
}

#' The severities and classes a rule-set row or the severity setting may give each rule
#'
#' One row per rule and allowed value: severity `error` or `warning` for a conformance rule,
#' `flag` for a plausibility rule, `info` for an information rule (its default severity
#' `info`); class `source`, `harmonization` or `depends`, `none` for an information rule
#' (4.2; D8.12, D9.21). A pre-flight check takes none (D14.5).
#' @noRd
allowed_overrides <- function(registry) {
  open <- registry[registry$check_type != "preflight"]
  info <- open$default_severity == "info"
  plausibility <- !info & open$stage == "plausibility"
  conformance <- !info & !plausibility
  pairs <- function(ids, values) {
    data.table(
      rule_id = rep(ids, each = length(values)), value = rep(values, times = length(ids))
    )
  }
  list(
    severity = rbindlist(list(
      pairs(open$rule_id[info], "info"), pairs(open$rule_id[plausibility], "flag"),
      pairs(open$rule_id[conformance], c("error", "warning"))
    )),
    class = rbindlist(list(
      pairs(open$rule_id[info], "none"),
      pairs(open$rule_id[!info], c("source", "harmonization", "depends"))
    ))
  )
}
