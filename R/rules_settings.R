# Settings (plan 3.5, 8.5; D3.16, D9.21, D14.6, D14.9, D16.11, D16.12): each resolves
# argument > rule set > option (groundplotqc.<setting>) > built-in, and the tier it came from
# is kept for the run metadata (9.5). Only the settings something reads are declared: lang
# and severity from M3a; example_cap, column_level_min_n, raw_na_policy, rogue_codes and
# unknown_codes from M4a, read by the conformance checks; coordinate_attributes,
# code_set_max_values, code_set_skip_attributes and code_set_skip_pattern from M5, read by
# Layer 3 (D18.9, D18.20), and the design candidates' eight, read by Layer 4 (D18.39). A
# rule-set row gives a setting as text: a whole number for type "integer", several values
# "; "-joined, a named value as "name=value" (D16.12).

#' The declared settings: the type a rule-set row gives, the built-in value, a test of a
#' value, what the test wants, and how a rule-set row's text becomes a value
#'
#' `severity` is a named character vector by rule ID; its rule-set tier is the rules rows'
#' `severity` column, not a settings row (3.5, D9.21), so it has no row type. Each test checks
#' for invalid UTF-8 before it reads the text. `from_text` reads a row's value (NA for a
#' blank cell): a whole number for an integer setting (anything else NA, which the test
#' refuses), "; "-joined parts for several values, "name=value" parts for named ones (D16.12).
#' The code settings refuse, in every tier, a code that is empty, padded with spaces or holds
#' ";" ("=" too for unknown codes), and an unknown code's name empty, padded or holding ";"
#' or "=", so a typo in a rule-set row stops rather than giving a code no value matches; a
#' row ending in "; " has an empty last code (D16.21, D16.23, D16.24).
#' @noRd
setting_definitions <- function() {
  text_ok <- function(x) {
    is.character(x) && is.null(dim(x)) && !anyNA(x) && all(validUTF8(x))
  }
  # Valid UTF-8 only; `marks` a pattern no element may hold, NULL for none.
  plain <- function(x, marks = NULL) {
    padded <- !nzchar(x) | x != trimws(x)
    !any(padded) && (is.null(marks) || !any(grepl(marks, x)))
  }
  whole <- function(x, lowest) {
    is.numeric(x) && is.null(dim(x)) && length(x) == 1L && !is.na(x) && is.finite(x) &&
      x == trunc(x) && x >= lowest && x <= .Machine$integer.max
  }
  parts <- function(text) {
    if (is.na(text)) {
      return(character())
    }
    pieces <- strsplit(text, "; ", fixed = TRUE)[[1L]]
    # strsplit() drops a trailing empty part, which is refused as any empty code is (D16.23).
    if (endsWith(text, "; ")) c(pieces, "") else pieces
  }
  as_whole <- function(text) {
    if (!is.na(text) && grepl("^[0-9]+$", text)) as.numeric(text) else NA_real_
  }
  # The design candidates' settings (D18.39): a table setting names one table,
  # design_signature_by one attribute, the others any number of attributes; none built in.
  one_name <- function(wants) {
    list(
      type = "character", built_in = character(),
      valid = function(x) text_ok(x) && is.null(names(x)) && length(x) <= 1L && plain(x, ";"),
      wants = wants, from_text = parts
    )
  }
  attribute_names <- list(
    type = "character", built_in = character(),
    valid = function(x) text_ok(x) && is.null(names(x)) && plain(x, ";"),
    wants = paste(
      "a character vector of attribute names, none of them NA, empty, padded with spaces",
      "or holding \";\""
    ),
    from_text = parts
  )
  list(
    lang = list(
      type = "character", built_in = "en",
      valid = function(x) {
        is.character(x) && is.null(dim(x)) && length(x) == 1L && !is.na(x) && validUTF8(x) &&
          grepl(lang_pattern, x)
      },
      wants = "one language code of two or three lower-case letters, such as \"en\"",
      from_text = function(text) text
    ),
    severity = list(
      type = NA_character_, built_in = structure(character(), names = character()),
      valid = function(x) {
        is.character(x) && is.null(dim(x)) && !anyNA(x) && !is.null(names(x)) &&
          !anyNA(names(x)) && all(validUTF8(x)) && all(validUTF8(names(x))) &&
          all(nzchar(trimws(names(x)))) && !anyDuplicated(names(x))
      },
      wants = "a character vector of severities named by rule ID, each name once",
      from_text = NULL
    ),
    example_cap = list(
      type = "integer", built_in = 5L, valid = function(x) whole(x, 0),
      wants = "one whole number, 0 or more", from_text = as_whole
    ),
    column_level_min_n = list(
      type = "integer", built_in = 10L, valid = function(x) whole(x, 1),
      wants = "one whole number, 1 or more", from_text = as_whole
    ),
    raw_na_policy = list(
      type = "character", built_in = "when_sentinels",
      valid = function(x) {
        is.character(x) && is.null(dim(x)) && length(x) == 1L && !is.na(x) &&
          x %chin% c("when_sentinels", "always", "never")
      },
      wants = "one of \"when_sentinels\", \"always\" or \"never\"",
      from_text = function(text) text
    ),
    rogue_codes = list(
      type = "character", built_in = c("-999", "UNK", "NA", "N/A"),
      valid = function(x) text_ok(x) && is.null(names(x)) && plain(x, ";"),
      wants = paste(
        "a character vector of codes, none of them NA, empty, padded with spaces",
        "or holding \";\""
      ),
      from_text = parts
    ),
    unknown_codes = list(
      type = "character", built_in = structure(character(), names = character()),
      valid = function(x) {
        text_ok(x) && plain(x, "[;=]") && (length(x) == 0L || (
          !is.null(names(x)) && !anyNA(names(x)) && all(validUTF8(names(x))) &&
            plain(names(x), "[;=]")
        ))
      },
      wants = paste(
        "a character vector of codes named by attribute, such as c(colour = \"unknown\"),",
        "no code NA, empty, padded with spaces or holding \";\" or \"=\",",
        "and no name empty, padded with spaces or holding \";\" or \"=\""
      ),
      from_text = function(text) {
        pieces <- parts(text)
        named <- grepl("=", pieces, fixed = TRUE)
        # fifelse() keeps a row with no part character, where ifelse() gives logical(0).
        structure(
          fifelse(named, sub("^[^=]*=", "", pieces), NA_character_),
          names = fifelse(named, sub("=.*$", "", pieces), NA_character_)
        )
      }
    ),
    # The attributes coordinate_out_of_bounds checks and the range rules leave (D18.9).
    coordinate_attributes = list(
      type = "character", built_in = character(),
      valid = function(x) text_ok(x) && is.null(names(x)) && plain(x, ";"),
      wants = paste(
        "a character vector of attribute names, none of them NA, empty, padded with spaces",
        "or holding \";\""
      ),
      from_text = parts
    ),
    # undeclared_code_set's (D18.20, D18.25): the most distinct values a judged column may
    # hold, and the attributes it never judges, by name and by one pattern.
    code_set_max_values = list(
      type = "integer", built_in = 25L, valid = function(x) whole(x, 1),
      wants = "one whole number, 1 or more", from_text = as_whole
    ),
    code_set_skip_attributes = list(
      type = "character", built_in = character(),
      valid = function(x) text_ok(x) && is.null(names(x)) && plain(x, ";"),
      wants = paste(
        "a character vector of attribute names, none of them NA, empty, padded with spaces",
        "or holding \";\""
      ),
      from_text = parts
    ),
    code_set_skip_pattern = list(
      type = "character", built_in = character(),
      valid = function(x) {
        text_ok(x) && is.null(names(x)) && length(x) <= 1L && (length(x) == 0L || tryCatch(
          is.logical(grepl(x, "")),
          error = function(e) FALSE, warning = function(w) FALSE
        ))
      },
      wants = "one regular expression R can read, or none",
      from_text = function(text) if (is.na(text)) character() else text
    ),
    design_geometry_table = one_name(
      "one table name, not NA, empty, padded with spaces or holding \";\", or none"
    ),
    design_geometry_by = attribute_names,
    design_geometry_columns = attribute_names,
    subplot_label_table = one_name(
      "one table name, not NA, empty, padded with spaces or holding \";\", or none"
    ),
    subplot_label_columns = attribute_names,
    design_signature_table = one_name(
      "one table name, not NA, empty, padded with spaces or holding \";\", or none"
    ),
    design_signature_by = one_name(
      "one attribute name, not NA, empty, padded with spaces or holding \";\", or none"
    ),
    design_signature_columns = attribute_names
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
#' and its tier. The rule-set tier is a settings row, its text read by the definition's
#' `from_text` (D16.12). Call check_settings() first.
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
    value <- definition$from_text(rows$value[[row]])
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

#' A setting's value as one text, as settings.csv and a rule's fingerprint write it
#'
#' The elements "; "-joined, each "name=value" when the value has names (D16.12; M3b's R13),
#' a number written in full (D12.45); NA when the value has no element.
#' @noRd
setting_text <- function(value) {
  if (length(value) == 0L) {
    return(NA_character_)
  }
  text <- as_text(value)
  if (!is.null(names(value))) {
    text <- paste0(names(value), "=", text)
  }
  paste(text, collapse = "; ")
}
