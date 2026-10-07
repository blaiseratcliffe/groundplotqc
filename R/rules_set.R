# The rule set (plan 3.5; D3.16, D7.25, D9.20, D14.2, D14.19): a named list of tables, read
# from CSV, the rules component required. The rule set says where and how registered rules
# apply (R/rules_registry.R says what they are); the two share only rule_id. The schemas of
# meta, rules and settings are checked here; the other components of 3.5 pass through as
# tables until the milestone that reads one checks its schema. A component read from a file
# keeps its origin, the file's name and each row's line, so a finding can place a row.

#' The rule set's components in 3.5's order, with the columns and classes checked
#'
#' A component whose schema is NULL is passed through unchecked.
#' @noRd
rule_set_schema <- function() {
  chr <- "character"
  list(
    meta = c(rule_set_name = chr, version = chr, date = chr, spec_version = chr),
    rules = c(
      rule_id = chr, table_name = chr, attribute_name = chr, severity = chr, class = chr,
      enabled = "logical"
    ),
    crossfield = NULL,
    strategies = NULL,
    tolerances = NULL,
    buffers = NULL,
    settings = c(setting = chr, value = chr, type = chr)
  )
}

#' A rule set checked and copied, its components in 3.5's order
#'
#' NULL stays NULL. Each checked component becomes a new data.table with exactly its
#' schema's columns, each holding one value per row: text as UTF-8 with blanks NA, `enabled`
#' logical, read from TRUE or FALSE in any case. A component keeps its origin
#' (carry_origin()). The caller's tables are never changed. A rule set that isn't a named
#' list of tables with a rules component, or a component without exactly its columns, is a
#' caller error (D12.33's pattern); what the rows say is pre-flight's to check (4.2).
#' @noRd
validate_rule_set <- function(rules) {
  if (is.null(rules)) {
    return(NULL)
  }
  schema <- rule_set_schema()
  named <- is.list(rules) && !is.data.frame(rules) && !is.null(names(rules)) &&
    !anyNA(names(rules)) && all(nzchar(names(rules)))
  if (!named) {
    stop("`rules` must be a named list of tables, one per rule-set component.", call. = FALSE)
  }
  unknown <- setdiff(names(rules), names(schema))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "`rules` has components a rule set doesn't have: %s. Its components are %s.",
      paste(mark_invalid_utf8(unknown), collapse = ", "), paste(names(schema), collapse = ", ")
    ), call. = FALSE)
  }
  if (anyDuplicated(names(rules)) > 0L) {
    stop("`rules` names a component more than once.", call. = FALSE)
  }
  if (!"rules" %in% names(rules)) {
    stop("`rules` must have a rules component.", call. = FALSE)
  }
  present <- names(schema)[names(schema) %in% names(rules)]
  out <- lapply(present, function(name) {
    rule_set_component(rules[[name]], name, schema[[name]])
  })
  names(out) <- present
  if (!is.null(out$meta) && nrow(out$meta) != 1L) {
    stop("The rule set's meta component must have one row.", call. = FALSE)
  }
  out
}

#' One rule-set component as a new data.table, checked against its columns, its origin kept
#' @noRd
rule_set_component <- function(x, name, columns) {
  if (!is.data.frame(x)) {
    stop(sprintf("Rule-set component %s must be a data.frame.", name), call. = FALSE)
  }
  if (is.null(columns)) {
    return(carry_origin(copy(as.data.table(x)), x))
  }
  missing <- setdiff(names(columns), names(x))
  extra <- setdiff(names(x), names(columns))
  repeated <- unique(names(x)[duplicated(names(x))])
  if (length(missing) > 0L || length(extra) > 0L || length(repeated) > 0L) {
    problems <- c(
      if (length(missing) > 0L) paste("missing", paste(missing, collapse = ", ")),
      if (length(extra) > 0L) paste("extra", paste(mark_invalid_utf8(extra), collapse = ", ")),
      if (length(repeated) > 0L) {
        paste("repeated", paste(mark_invalid_utf8(repeated), collapse = ", "))
      }
    )
    stop(sprintf(
      "Rule-set component %s must have exactly the columns %s (%s).",
      name, paste(names(columns), collapse = ", "), paste(problems, collapse = "; ")
    ), call. = FALSE)
  }
  # A list column, or a matrix one, would be written as text over more or fewer rows than
  # the component has.
  for (column in names(columns)) {
    value <- x[[column]]
    if (is.list(value) || !is.null(dim(value)) || length(value) != nrow(x)) {
      stop(sprintf(
        "Rule-set component %s, column %s, must hold one value per row.", name, column
      ), call. = FALSE)
    }
  }
  table <- as.data.table(lapply(names(columns), function(column) as_text(x[[column]])))
  setnames(table, names(columns))
  for (column in names(columns)) {
    text <- table[[column]]
    if (!all(validUTF8(text[!is.na(text)]))) {
      stop(sprintf(
        "Rule-set component %s, column %s, holds text that isn't valid UTF-8.", name, column
      ), call. = FALSE)
    }
  }
  if ("enabled" %in% names(columns)) {
    enabled <- toupper(table$enabled)
    bad <- which(is.na(enabled) | !enabled %chin% c("TRUE", "FALSE"))
    if (length(bad) > 0L) {
      stop(sprintf(
        "Rule-set component %s has enabled values that aren't TRUE or FALSE, in rows %s.",
        name, paste(bad, collapse = ", ")
      ), call. = FALSE)
    }
    set(table, j = "enabled", value = enabled == "TRUE")
  }
  carry_origin(table, x)
}

#' A component's origin set by reference: the name of the file it was read from and the
#' file line each row starts on, or none where both are NULL (D14.19)
#'
#' Only on a table the package made, never a caller's. Returns `table`.
#' @noRd
set_origin <- function(table, file, lines) {
  setattr(table, "source_file", file)
  setattr(table, "source_lines", lines)
  table
}

#' A new component's origin carried over from the table it was made from, when that has one:
#' a file name of valid UTF-8 and one whole line number from 1 up per row; otherwise none
#' (D14.19)
#' @noRd
carry_origin <- function(table, from) {
  file <- attr(from, "source_file", exact = TRUE)
  lines <- attr(from, "source_lines", exact = TRUE)
  kept <- is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file) &&
    validUTF8(file) && is.numeric(lines) && length(lines) == nrow(table) && !anyNA(lines) &&
    all(lines >= 1 & lines == trunc(lines))
  set_origin(table, if (kept) file, if (kept) as.integer(lines))
}

#' Text with each byte that isn't valid UTF-8 written as <xx>, for a caller error (D12.28)
#' @noRd
mark_invalid_utf8 <- function(x) {
  iconv(x, "UTF-8", "UTF-8", sub = "byte")
}

#' A rule set read from a folder of rules_<component>.csv files
#'
#' `dir` is one folder holding rules_rules.csv, or the call stops. Each file present is read
#' as text and must read cleanly; each component keeps its file's name and each row's line
#' (D14.19); then validate_rule_set() checks the whole.
#' @noRd
read_rule_set <- function(dir) {
  found <- is.character(dir) && length(dir) == 1L && !is.na(dir) && dir.exists(dir) &&
    file.exists(file.path(dir, "rules_rules.csv"))
  if (!found) {
    where <- if (is.character(dir) && length(dir) == 1L) dir else deparse1(dir)
    stop(sprintf("No rule-set file rules_rules.csv in %s.", where), call. = FALSE)
  }
  components <- names(rule_set_schema())
  files <- paste0("rules_", components, ".csv")
  given <- file.exists(file.path(dir, files))
  read <- lapply(files[given], function(file) {
    path <- file.path(dir, file)
    text <- read_csv_text(path)
    if (nrow(text$malformed) > 0L || nrow(text$invalid) > 0L) {
      stop(sprintf("The rule-set file %s doesn't read cleanly.", path), call. = FALSE)
    }
    set_origin(text$data, file, text$lines)
  })
  names(read) <- components[given]
  validate_rule_set(read)
}
