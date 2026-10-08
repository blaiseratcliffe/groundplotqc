# Settings (plan 3.5, 8.5; D3.16, D9.21, D14.6, D14.9): each resolves argument > rule set >
# option (groundplotqc.<setting>) > built-in, and the tier it came from is kept for the run
# metadata (9.5). Only the settings something reads are declared: lang and severity from
# M3a, each later milestone adding its own with the feature that reads it (D14.9).

#' The declared settings: the type a rule-set row gives, the built-in value, a test of a
#' value and what the test wants
#'
#' `severity` is a named character vector by rule ID; its rule-set tier is the rules rows'
#' `severity` column, not a settings row (3.5, D9.21), so it has no row type. Each test checks
#' for invalid UTF-8 before it reads the text.
#' @noRd
setting_definitions <- function() {
  list(
    lang = list(
      type = "character", built_in = "en",
      valid = function(x) {
        is.character(x) && is.null(dim(x)) && length(x) == 1L && !is.na(x) && validUTF8(x) &&
          grepl(lang_pattern, x)
      },
      wants = "one language code of two or three lower-case letters, such as \"en\""
    ),
    severity = list(
      type = NA_character_, built_in = structure(character(), names = character()),
      valid = function(x) {
        is.character(x) && is.null(dim(x)) && !anyNA(x) && !is.null(names(x)) &&
          !anyNA(names(x)) && all(validUTF8(x)) && all(validUTF8(names(x))) &&
          all(nzchar(trimws(names(x)))) && !anyDuplicated(names(x))
      },
      wants = "a character vector of severities named by rule ID, each name once"
    )
  )
}

#' The settings argument and the rule set's settings rows checked against the declared
#' settings
#'
#' Caller errors, in English (D12.37): a settings argument that isn't a list with unique
#' names, a setting no one declared, a rule-set row for a setting the rule set can't give,
#' twice, or of another type.
#' @noRd
check_settings <- function(settings = list(), rules = NULL) {
  definitions <- setting_definitions()
  names_ok <- length(settings) == 0L || (
    !is.null(names(settings)) && !anyNA(names(settings)) && all(nzchar(names(settings))) &&
      !anyDuplicated(names(settings))
  )
  named <- is.list(settings) && !is.data.frame(settings) && names_ok
  if (!named) {
    stop("`settings` must be a list of settings, each named once.", call. = FALSE)
  }
  unknown <- setdiff(names(settings), names(definitions))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "`settings` names settings the package doesn't have: %s. Its settings are %s.",
      paste(mark_invalid_utf8(unknown), collapse = ", "), paste(names(definitions), collapse = ", ")
    ), call. = FALSE)
  }
  rows <- rules$settings
  if (is.null(rows) || nrow(rows) == 0L) {
    return(invisible(NULL))
  }
  # A blank setting cell is NA once the rule set is validated; messages show it as (blank).
  shown <- function(x) fifelse(is.na(x), "(blank)", x)
  takes_rows <- names(definitions)[!is.na(vapply(definitions, `[[`, "", "type"))]
  refused <- setdiff(rows$setting, takes_rows)
  if (length(refused) > 0L) {
    stop(sprintf(
      paste(
        "The rule set's settings rows name settings a rule set can't give there: %s.",
        "A settings row can give only %s."
      ),
      paste(shown(refused), collapse = ", "), paste(takes_rows, collapse = ", ")
    ), call. = FALSE)
  }
  twice <- unique(rows$setting[duplicated(rows$setting)])
  if (length(twice) > 0L) {
    stop(sprintf(
      "The rule set's settings rows give %s more than once.", paste(shown(twice), collapse = ", ")
    ), call. = FALSE)
  }
  wanted <- vapply(definitions[rows$setting], `[[`, "", "type")
  wrong <- is.na(rows$type) | rows$type != wanted
  if (any(wrong)) {
    # Each setting is told with its own type.
    stop(sprintf(
      "The rule set's settings rows give the wrong type for %s.",
      paste0(shown(rows$setting[wrong]), ", which takes ", wanted[wrong], collapse = "; ")
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' One setting's value and the tier it came from: argument, rule set, option or built-in
#'
#' The value is checked against its definition, a bad one a caller error naming the setting
#' and its tier. The rule-set tier is a settings row, its value returned as the text it
#' holds, as `character` is the only type so far. Call check_settings() first.
#'
#' The severity setting isn't resolved here: it is one value per rule, with a tier for each,
#' so asking for it is a developer error pointing to severity_entries() (D14.29).
#' @noRd
resolve_setting <- function(name, settings = list(), rules = NULL) {
  definition <- setting_definitions()[[name]]
  if (is.null(definition)) {
    stop(sprintf("No setting is named %s.", name), call. = FALSE)
  }
  if (name == "severity") {
    stop(
      "The severity setting is resolved per rule by severity_entries(), not by resolve_setting().",
      call. = FALSE
    )
  }
  rows <- rules$settings
  row <- if (!is.null(rows) && !is.na(definition$type)) which(rows$setting == name)
  option_name <- paste0("groundplotqc.", name)
  option <- getOption(option_name)
  if (!is.null(settings[[name]])) {
    value <- settings[[name]]
    tier <- "argument"
  } else if (length(row) == 1L) {
    value <- rows$value[[row]]
    tier <- "rule set"
  } else if (!is.null(option)) {
    value <- option
    tier <- "option"
  } else {
    value <- definition$built_in
    tier <- "built-in"
  }
  if (!definition$valid(value)) {
    # The option tier names the option, as a finding on the severity setting does.
    shown_tier <- if (tier == "option") paste("option", option_name) else tier
    stop(sprintf(
      "Setting %s (%s) must be %s.", name, shown_tier, definition$wants
    ), call. = FALSE)
  }
  list(value = value, tier = tier)
}

#' The severity setting's entries, as an argument and as the option, one row per rule
#'
#' Its rule-set tier is the rules rows' severity column, read by the checks themselves. A
#' value of the wrong shape is a caller error naming its tier; what each entry says is
#' pre-flight's to check (rule_id_unknown, rule_set_override_invalid; D9.21).
#' @noRd
severity_entries <- function(settings = list()) {
  definition <- setting_definitions()$severity
  tiers <- list(argument = settings[["severity"]], option = getOption("groundplotqc.severity"))
  parts <- lapply(names(tiers), function(tier) {
    x <- tiers[[tier]]
    if (is.null(x)) {
      return(NULL)
    }
    if (!definition$valid(x)) {
      shown_tier <- if (tier == "option") "option groundplotqc.severity" else tier
      stop(sprintf(
        "Setting severity (%s) must be %s.", shown_tier, definition$wants
      ), call. = FALSE)
    }
    data.table(rule_id = names(x), severity = unname(x), tier = tier)
  })
  rbindlist(c(
    list(data.table(rule_id = character(), severity = character(), tier = character())), parts
  ))
}
