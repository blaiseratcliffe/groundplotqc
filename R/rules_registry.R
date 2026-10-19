# The rule registry (plan 9.7; D7.25, D8.11, D9.21, D14.5, D14.10, D18.49): what each rule is, one
# row per rule ID, in package code. The rule set's rules component says where and how a rule
# applies (R/rules_set.R); the two share only rule_id. Each milestone registers its rules
# here through the add-rule skill.

# The registry, built once per session (D18.49), as report_texts() keeps the engine's text
# table (D12.29).
registry_cache <- new.env(parent = emptyenv())

#' The rule registry, built once per session (D18.49)
#'
#' The table (build_rule_registry()) is shared by every call, so a caller never changes it by
#' reference.
#' @noRd
rule_registry <- function() {
  if (is.null(registry_cache$registry)) {
    registry_cache$registry <- build_rule_registry()
  }
  registry_cache$registry
}

#' The rule registry: one row per registered rule
#'
#' Columns: `rule_id`; `stage` (`conformance`, `plausibility`); `layer` (0 to 7, NA for a
#' pre-flight check, D7.23); `check_type` (`preflight` for the checks of 4.2);
#' `default_severity` (`error`, `warning`, `flag`, `info`; a pre-flight check's `error`
#' stops and its `warning` warns, and no rule set or setting changes either, D14.5);
#' `default_class` (`source`, `harmonization`, `depends`, or `none` for information rules
#' and pre-flight checks, which write no issues); `inputs` (what the rule reads, "; "-joined:
#' specification components, `rules`, `settings:<name>`, `text`); `strategy` (`none` for a
#' rule that proposes nothing); `message_id` (its text_id in the report text); `rule_version`
#' (from 1, raised by the add-rule skill whenever the rule's check behaves differently, so its
#' rule fingerprint changes, D15.6); `unit` (TRUE for a rule that checks a table or a column
#' as one unit, D16.8, whose not_run rows are one "*" row each, D16.29, D17.15). Layer 6 and 7
#' rules add their reason codes at M12 (D9.33).
#' Conformance data rules have check type `conformance` (8.2) and the strategy 6.1's Fix
#' column gives (`sentinel_fix`, `trim_whitespace`, else `none`), proposed from M11.
#' @noRd
build_rule_registry <- function() {
  preflight <- function(rule_id, severity, inputs) {
    data.table(
      rule_id = rule_id, stage = "conformance", layer = NA_integer_, check_type = "preflight",
      default_severity = severity, default_class = "none", inputs = inputs,
      strategy = "none", message_id = rule_id, rule_version = 1L, unit = FALSE
    )
  }
  # Every conformance rule reads the severity setting, which gives its severity (D16.6), so
  # its fingerprint takes in the rule's entry (D16.54); the settings entries follow the
  # specification components, in their names' order.
  conformance <- function(rule_id, layer, severity, class, inputs, strategy = "none",
                          unit = FALSE, rule_version = 1L) {
    read <- strsplit(inputs, "; ", fixed = TRUE)[[1L]]
    named <- startsWith(read, "settings:")
    settings <- sort(unique(c(read[named], "settings:severity")), method = "radix")
    inputs <- paste(c(read[!named], settings), collapse = "; ")
    data.table(
      rule_id = rule_id, stage = "conformance", layer = layer, check_type = "conformance",
      default_severity = severity, default_class = class, inputs = inputs,
      strategy = strategy, message_id = rule_id, rule_version = rule_version, unit = unit
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
    # ---- M5: DD ranges and formats, key pairs, the walk, one-to-many IDs (4.2; D18.9, D18.36) ----
    preflight("dd_format_invalid", "error", "read_findings"),
    preflight("key_pair_invalid", "error", "read_findings"),
    preflight("reach_invalid", "error", "read_findings"),
    preflight(
      "one_to_many_invalid", "warning", "attributes; id_band_columns; lineage_spec; one_to_many"
    ),
    # ---- task 5 ----
    preflight("rule_set_unknown_column", "error", "attributes; rules"),
    preflight("rule_id_unknown", "error", "rules; settings:severity"),
    preflight("rule_set_override_invalid", "error", "rules; settings:severity"),
    preflight("rule_set_duplicate_row", "error", "rules"),
    # ---- task 6 ----
    preflight("text_id_missing", "error", "text"),
    preflight("text_id_fallback", "warning", "text; settings:lang"),
    preflight("text_slot_unknown", "error", "text"),
    # ---- M4a: Layer 0, structure (6.1; D2.34, D8.11, D16.8) ----
    conformance("table_missing", 0L, "error", "harmonization", "attributes", unit = TRUE),
    conformance("table_unexpected", 0L, "warning", "harmonization", "attributes", unit = TRUE),
    conformance("column_missing", 0L, "error", "harmonization", "attributes", unit = TRUE),
    conformance("column_unexpected", 0L, "warning", "harmonization", "attributes", unit = TRUE),
    conformance("column_name_mismatch", 0L, "error", "harmonization", "attributes", unit = TRUE),
    # ---- M4a: Layer 1, data types (6.1; D2.33, D8.8, D8.11, D16.8, D16.9) ----
    # Here and in Layer 2, every rule with record outcomes reads column_level_min_n, which
    # decides whether its failures are record issues or one column-level issue (6.3; D16.47,
    # m60); a unit rule's outcomes are never collapsed.
    conformance(
      "class_mismatch", 1L, "error", "harmonization", "attributes; type_map",
      unit = TRUE
    ),
    # Version 2 (M5): a key of only Unicode spaces is left to Layer 4 (D18.62, D18.63).
    conformance(
      "coercion_loss", 1L, "error", "harmonization",
      paste(
        "attributes; keys; type_map; sentinels; codes; code_list_map;",
        "settings:column_level_min_n; settings:rogue_codes; settings:unknown_codes"
      ),
      rule_version = 2L
    ),
    conformance(
      "date_unparseable", 1L, "error", "depends",
      paste(
        "attributes; keys; type_map; sentinels; codes; code_list_map;",
        "settings:column_level_min_n; settings:rogue_codes; settings:unknown_codes"
      ),
      rule_version = 2L
    ),
    conformance(
      "integer_stored_as_double", 1L, "warning", "harmonization", "attributes; type_map",
      unit = TRUE
    ),
    conformance(
      "value_not_finite", 1L, "error", "harmonization",
      "attributes; type_map; settings:column_level_min_n"
    ),
    # ---- M4a: Layer 2, missing-value codes (6.1; D1.8a, D7.4, D7.9, D8.8, D8.11, D16.11) ----
    conformance(
      "raw_na", 2L, "error", "harmonization",
      paste(
        "attributes; keys; type_map; sentinels; settings:column_level_min_n;",
        "settings:raw_na_policy"
      ),
      "sentinel_fix"
    ),
    conformance(
      "blank_string", 2L, "error", "harmonization",
      "attributes; keys; type_map; sentinels; settings:column_level_min_n", "sentinel_fix"
    ),
    conformance(
      "sentinel_wrong_type", 2L, "error", "harmonization",
      "attributes; type_map; sentinels; settings:column_level_min_n; settings:unknown_codes",
      "sentinel_fix"
    ),
    conformance(
      "sentinel_rogue_code", 2L, "error", "depends",
      paste(
        "attributes; keys; type_map; sentinels; codes; settings:column_level_min_n;",
        "settings:rogue_codes; settings:unknown_codes"
      )
    ),
    conformance(
      "species_unknown_code", 2L, "error", "harmonization",
      "attributes; type_map; sentinels; settings:column_level_min_n; settings:unknown_codes"
    ),
    conformance(
      "whitespace_padding", 2L, "warning", "harmonization",
      "attributes; keys; code_list_map; settings:column_level_min_n", "trim_whitespace"
    ),
    conformance(
      "na_text_in_id", 2L, "error", "harmonization", "attributes; keys; settings:column_level_min_n"
    ),
    # ---- M5: Layer 3, value domains (6.1; D8.24, D18.9, D18.10) ----
    # Every Layer 3 record rule reads the sentinels and the unknown codes, which it leaves to
    # Layer 2 (D18.9).
    conformance(
      "code_not_in_list", 3L, "error", "depends",
      paste(
        "attributes; type_map; sentinels; codes; code_list_map; settings:column_level_min_n;",
        "settings:unknown_codes"
      )
    ),
    # The range rules leave the coordinate attributes to coordinate_out_of_bounds (D18.9).
    conformance(
      "value_below_min", 3L, "error", "depends",
      paste(
        "attributes; type_map; sentinels; settings:column_level_min_n;",
        "settings:coordinate_attributes; settings:unknown_codes"
      )
    ),
    conformance(
      "value_above_max", 3L, "error", "depends",
      paste(
        "attributes; type_map; sentinels; settings:column_level_min_n;",
        "settings:coordinate_attributes; settings:unknown_codes"
      )
    ),
    conformance(
      "string_too_long", 3L, "error", "depends",
      "attributes; type_map; sentinels; settings:column_level_min_n; settings:unknown_codes"
    ),
    conformance(
      "pattern_mismatch", 3L, "error", "depends",
      "attributes; type_map; sentinels; settings:column_level_min_n; settings:unknown_codes"
    ),
    conformance(
      "decimals_exceeded", 3L, "warning", "depends",
      "attributes; type_map; sentinels; settings:column_level_min_n; settings:unknown_codes"
    ),
    conformance(
      "site_id_out_of_range", 3L, "error", "harmonization",
      paste(
        "attributes; keys; type_map; sentinels; id_bands; id_band_columns;",
        "settings:column_level_min_n; settings:unknown_codes"
      )
    ),
    conformance(
      "coordinate_out_of_bounds", 3L, "error", "depends",
      paste(
        "attributes; type_map; sentinels; settings:column_level_min_n;",
        "settings:coordinate_attributes; settings:unknown_codes"
      )
    ),
    # ---- M5: Appendix E's candidate (D8.24, D9.26; class proposed in the PR, Y6) ----
    conformance(
      "undeclared_code_set", 3L, "warning", "harmonization",
      paste(
        "attributes; keys; type_map; sentinels; code_list_map; settings:code_set_max_values;",
        "settings:code_set_skip_attributes; settings:code_set_skip_pattern;",
        "settings:unknown_codes"
      ),
      unit = TRUE
    ),
    # ---- M5: Layer 4, keys and relationships (6.1; D7.3, D7.9, D16.2, D18.10, D18.33) ----
    # Each record rule reads the keys and the sentinels' allowed_in_pk and allowed_in_fk (3.5).
    conformance(
      "pk_duplicate", 4L, "error", "harmonization",
      "attributes; keys; type_map; sentinels; settings:column_level_min_n"
    ),
    conformance(
      "pk_missing", 4L, "error", "harmonization",
      "attributes; keys; type_map; sentinels; settings:column_level_min_n"
    ),
    conformance(
      "fk_orphan", 4L, "error", "harmonization",
      "attributes; keys; type_map; sentinels; settings:column_level_min_n"
    ),
    conformance(
      "fk_unassigned", 4L, "error", "harmonization",
      "attributes; keys; type_map; sentinels; settings:column_level_min_n"
    ),
    conformance(
      "fk_sentinel_unverified", 4L, "warning", "harmonization",
      "attributes; keys; type_map; sentinels; settings:column_level_min_n"
    ),
    conformance("fk_not_applicable_mismatch", 4L, "error", "harmonization", "attributes; keys"),
    conformance(
      "src_id_duplicate", 4L, "warning", "depends",
      paste(
        "attributes; code_lists; id_band_columns; keys; lineage_spec; one_to_many; type_map;",
        "sentinels; settings:column_level_min_n"
      )
    ),
    conformance(
      "key_incoherent", 4L, "error", "harmonization",
      "attributes; keys; type_map; sentinels; key_pairs; settings:column_level_min_n"
    ),
    conformance(
      "record_unreachable", 4L, "error", "harmonization",
      "attributes; keys; type_map; sentinels; reach; settings:column_level_min_n"
    ),
    conformance(
      "parent_without_children", 4L, "info", "none", "attributes; keys; type_map; sentinels"
    )
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
