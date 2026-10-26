# Fingerprints (plan 9.6; D4.2, D4.7, D8.16, D9.22, D15.6 to D15.9): an issue's readable
# fingerprint, which the comparison with a previous run and the acknowledgements match on
# (M8a), and a rule's definition fingerprint, which tells a change made by rules or
# specification from a change in the data (D4.7). A run's issues have the pk: form;
# gpq_decode_lineage() fills the provider key where it decodes and sets the fingerprint from
# it, keeping the pk: form where another record of the table holds the same key (D19.15).
# M9 and M13 add the matrix and threshold versions to the rule fingerprint.

#' The SHA-256 of a raw vector, as 64 lower-case hex characters
#' @noRd
sha256_bytes <- function(bytes) {
  unname(tools::sha256sum(bytes = bytes))
}

#' Each issue's fingerprint and its pk: form
#'
#' `issues` holds `rule_id`, `table_name`, `column_name`, `record_pk` and `provider_key`.
#' The pk: form is `rule_id|table_name|column_name|pk:<record_pk>`, NA where the record's
#' PK is missing (`record_pk` NA, D15.9). The fingerprint is
#' `rule_id|table_name|column_name|<provider_key>` where the provider key is filled, and the
#' pk: form otherwise (D4.2, D9.22). Readable text, never hashed (D8.16). Returns a new
#' data.table with the columns `fingerprint` and `fingerprint_pk`, one row per issue.
#' @noRd
issue_fingerprint <- function(issues) {
  # The text up to the last part, for the given rows only.
  stem <- function(rows) {
    paste(issues$rule_id[rows], issues$table_name[rows], issues$column_name[rows], sep = "|")
  }
  pk <- rep(NA_character_, length(issues$rule_id))
  with_pk <- which(!is.na(issues$record_pk))
  pk[with_pk] <- paste0(stem(with_pk), "|pk:", issues$record_pk[with_pk])
  fingerprint <- pk
  with_provider <- which(!is.na(issues$provider_key))
  fingerprint[with_provider] <- paste0(
    stem(with_provider), "|", issues$provider_key[with_provider]
  )
  data.table(fingerprint = fingerprint, fingerprint_pk = pk)
}

#' Each rule's definition fingerprint: the SHA-256 of what it is and what it reads
#'
#' For each rule ID, the hash of, in this order (D4.7, D15.6, D15.7): its registry row,
#' `rule_version` included; the rows of each rule-set component with a `rule_id` column
#' that name the rule or "*", in 3.5's order, a component none of whose rows names the
#' rule adding nothing, so no rule set, an empty one and an unrelated one give one hash
#' (D15.22); then, for each entry of its registry
#' `inputs` in order, the SHA-256 of a spec component it names, or the value of a setting
#' it names as `settings:<name>` (the severity setting's value being the rule's own entry,
#' the argument's over the option's). Every table goes in as the compiled writer's bytes
#' (compiled_bytes()), so a spec built in memory and its compiled copy give the same
#' fingerprint. An input entry that is neither (`rules`, `text`) adds nothing. `rules` is a
#' rule set, checked here; `settings` as gpq_preflight() takes them. Returns a named
#' character vector, one hash per rule ID; an unregistered rule ID is a developer error,
#' and so is a spec lacking a component a rule reads, the stop naming the component and the
#' rules that read it.
#' @noRd
rule_fingerprint <- function(rule_ids, spec, rules = NULL, settings = list(),
                             registry = rule_registry()) {
  rules <- validate_rule_set(rules)
  check_settings(settings, rules)
  entries <- severity_entries(settings)
  components <- names(spec_schema())
  at <- match(rule_ids, registry$rule_id)
  rows <- registry[at]
  if (anyNA(rows$rule_id)) {
    stop(sprintf(
      "Rules %s aren't registered.",
      paste(rule_ids[is.na(rows$rule_id)], collapse = ", ")
    ), call. = FALSE)
  }
  inputs <- strsplit(rows$inputs, "; ", fixed = TRUE)
  # Each spec component a rule reads is hashed once, however many rules read it.
  needed <- intersect(components, unlist(inputs))
  lacking <- needed[!needed %chin% names(spec)]
  if (length(lacking) > 0L) {
    readers <- vapply(lacking, function(name) {
      reading <- vapply(inputs, function(listed) name %chin% listed, logical(1))
      sprintf("%s (read by %s)", name, paste(unique(rule_ids[reading]), collapse = ", "))
    }, character(1))
    stop(sprintf(
      "The specification lacks %s.", paste(readers, collapse = "; ")
    ), call. = FALSE)
  }
  spec_hashes <- vapply(needed, function(name) {
    sha256_bytes(compiled_bytes(spec[[name]]))
  }, character(1))
  label <- function(text) charToRaw(paste0(text, "\n"))
  hashes <- vapply(seq_along(rule_ids), function(k) {
    id <- rule_ids[[k]]
    parts <- list(label("registry"), compiled_bytes(rows[k]))
    for (name in names(rules)) {
      component <- rules[[name]]
      if (!"rule_id" %in% names(component)) {
        next
      }
      naming <- component$rule_id %chin% c(id, "*")
      if (!any(naming)) {
        next
      }
      parts <- c(parts, list(label(paste0("rules:", name)), compiled_bytes(component[naming])))
    }
    for (input in inputs[[k]]) {
      if (input %chin% components) {
        parts <- c(parts, list(label(paste0("spec:", input, "=", spec_hashes[[input]]))))
      } else if (startsWith(input, "settings:")) {
        setting <- substring(input, nchar("settings:") + 1L)
        value <- if (setting == "severity") {
          entries$severity[match(id, entries$rule_id)]
        } else {
          written <- setting_text(resolve_setting(setting, settings, rules)$value)
          # A value with no element is labelled with nothing, as it was before setting_text(),
          # so that it differs from the one code "NA" (setting_text() never gives "" itself).
          if (is.na(written)) "" else written
        }
        parts <- c(parts, list(label(paste0(input, "=", value))))
      }
    }
    sha256_bytes(unlist(parts, use.names = FALSE))
  }, character(1))
  names(hashes) <- rule_ids
  hashes
}
