# The rule set (plan 3.5; D3.16, D7.25, D9.20, D14.2, D14.19): a named list of tables, read
# from CSV, the rules component required. The rule set says where and how registered rules
# apply (R/rules_registry.R says what they are); the two share only rule_id. The schemas of
# meta, rules and settings are checked here; the other components of 3.5 pass through as
# tables until the milestone that reads one checks its schema. A component read from a file
# keeps its origin, the file's name and each row's line, so a finding can place a row, while
# its rows stay as they were read (D16.15). rule_row_values() gives a rule's severity, class
# and enabled at a table and attribute from the most specific rows (D16.6).

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
#' schema's columns, each holding one value per row: text as UTF-8 with blanks NA, a column
#' the schema declares logical (`enabled`) logical, read from TRUE or FALSE, the ASCII
#' letters in any case. A component keeps its origin (carry_origin()), which places a bad
#' `enabled` cell by its file line in the stop (D14.26). The caller's tables are never
#' changed. A rule set that isn't a named list of tables with a rules component, or a
#' component without exactly its columns, is a caller error (D12.33's pattern); what the
#' rows say is pre-flight's to check (4.2).
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
  # The repeated components, in 3.5's order.
  twice <- intersect(names(schema), names(rules)[duplicated(names(rules))])
  if (length(twice) > 0L) {
    stop(
      "`rules` names a component more than once: ", paste(twice, collapse = ", "), ".",
      call. = FALSE
    )
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
        "Column `%s` of rule-set component %s must hold one value per row.", column, name
      ), call. = FALSE)
    }
  }
  table <- as.data.table(lapply(names(columns), function(column) as_text(x[[column]])))
  setnames(table, names(columns))
  table <- carry_origin(table, x)
  # The first ten of a list for a message, then how many more.
  listed <- function(items, n = 10L) {
    if (length(items) <= n) {
      return(paste(items, collapse = ", "))
    }
    paste0(paste(items[seq_len(n)], collapse = ", "), " and ", length(items) - n, " more")
  }
  # Where the cells at `bad` are, for a message: by their file lines when the component has
  # its origin, by R's row numbers otherwise (D14.26).
  places <- function(bad) {
    origin_file <- attr(table, "source_file", exact = TRUE)
    if (is.null(origin_file)) {
      return(paste(if (length(bad) == 1L) "row" else "rows", listed(bad)))
    }
    listed(sprintf("line %d of %s", attr(table, "source_lines", exact = TRUE)[bad], origin_file))
  }
  for (column in names(columns)) {
    bad <- which(!validUTF8(table[[column]]))
    if (length(bad) > 0L) {
      stop(sprintf(
        "Column `%s` of rule-set component %s holds text that isn't valid UTF-8, in %s.",
        column, name, places(bad)
      ), call. = FALSE)
    }
  }
  # A column the schema declares logical is read from TRUE or FALSE, the ASCII letters in any
  # case: toupper() would fold other letters too, such as the long s.
  for (column in names(columns)[columns == "logical"]) {
    flag <- chartr(paste(letters, collapse = ""), paste(LETTERS, collapse = ""), table[[column]])
    bad <- which(is.na(flag) | !flag %chin% c("TRUE", "FALSE"))
    if (length(bad) > 0L) {
      stop(sprintf(
        "Rule-set component %s has %s values that aren't TRUE or FALSE, in %s.",
        name, column, places(bad)
      ), call. = FALSE)
    }
    set(table, j = column, value = flag == "TRUE")
  }
  # The origin's hash is of the rows as returned, a logical column read (D16.15), so a later
  # check of this component keeps the origin.
  set_origin(
    table, attr(table, "source_file", exact = TRUE), attr(table, "source_lines", exact = TRUE)
  )
}

#' A component's origin set by reference: the name of the file it was read from, the file
#' line each row starts on, and the SHA-256 of the rows as they are now (`source_sha256`), or
#' none of the three where `file` is NULL (D14.19, D16.15)
#'
#' Only on a table the package made, never a caller's. Returns `table`.
#' @noRd
set_origin <- function(table, file, lines) {
  setattr(table, "source_file", file)
  setattr(table, "source_lines", if (!is.null(file)) lines)
  setattr(
    table, "source_sha256",
    if (!is.null(file)) sha256_bytes(compiled_bytes(as.data.table(table)))
  )
  table
}

#' A new component's origin carried over from the table it was made from, when that has one
#' and its rows are still the ones its origin was set on: a file name of valid UTF-8, one
#' whole line number from 1 up per row, and `source_sha256` matching the rows; otherwise none
#' (D14.19, D16.15)
#'
#' A component edited, reordered or filtered after it was read loses its origin, so its
#' findings give R's row numbers rather than another row's line.
#' @noRd
carry_origin <- function(table, from) {
  file <- attr(from, "source_file", exact = TRUE)
  lines <- attr(from, "source_lines", exact = TRUE)
  recorded <- attr(from, "source_sha256", exact = TRUE)
  kept <- is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file) &&
    validUTF8(file) && is.numeric(lines) && length(lines) == nrow(table) && !anyNA(lines) &&
    all(lines >= 1 & lines <= .Machine$integer.max & lines == trunc(lines)) &&
    is.character(recorded) && length(recorded) == 1L &&
    identical(recorded, sha256_bytes(compiled_bytes(as.data.table(from))))
  set_origin(table, if (kept) file, if (kept) as.integer(lines))
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

#' A checked rule set as one text, in bytes (D15.8)
#'
#' For each component it has, in 3.5's order, the component's name on a line, then the
#' component as compiled_bytes() writes it. A rule set read from files and the same rows
#' given in memory give the same bytes: a component's origin isn't part of it. A rule set
#' with no components gives no bytes, still a raw vector.
#' @noRd
rule_set_bytes <- function(rules) {
  c(raw(), unlist(lapply(names(rules), function(name) {
    c(charToRaw(paste0(name, "\n")), compiled_bytes(rules[[name]]))
  }), use.names = FALSE))
}

#' A rule's severity, class and enabled at each table and attribute asked, from the rule
#' set's rules rows (D16.6)
#'
#' For each of the three, the most specific matching row that gives a value: the table and
#' attribute, then the table and "*", then "*" and the attribute, then "*" and "*"; NA where
#' no row gives one (the caller falls back to the option entry and the registry, or to TRUE
#' for enabled). `rows` is a checked rules component or NULL; `rule_ids`, `table_names` and
#' `column_names` are one per question. Returns a data.table with `severity`, `class` and
#' `enabled`, one row per question. pre-flight's rule_set_duplicate_row stops a rule set with
#' two rows at one level.
#' @noRd
rule_row_values <- function(rows, rule_ids, table_names, column_names) {
  n <- length(rule_ids)
  out <- data.table(
    severity = rep(NA_character_, n), class = rep(NA_character_, n), enabled = rep(NA, n)
  )
  if (is.null(rows) || nrow(rows) == 0L || n == 0L) {
    return(out)
  }
  levels <- list(c(TRUE, TRUE), c(TRUE, FALSE), c(FALSE, TRUE), c(FALSE, FALSE))
  join_on <- c("rule_id", "table_name", "attribute_name")
  for (level in levels) {
    asked <- data.table(
      rule_id = rule_ids,
      table_name = if (level[[1L]]) table_names else rep("*", n),
      attribute_name = if (level[[2L]]) column_names else rep("*", n)
    )
    at <- rows[asked, on = join_on, which = TRUE, mult = "first"]
    for (field in c("severity", "class", "enabled")) {
      value <- rows[[field]][at]
      open <- which(is.na(out[[field]]) & !is.na(value))
      set(out, i = open, j = field, value = value[open])
    }
  }
  out
}
