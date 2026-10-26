# Conditions in D8.10's restricted grammar (plan 5.5, 10.1; D8.10, D19.12, D19.37, D19.43; R3):
# one comparison on a column of the record, on a parent's column through a chain of foreign
# keys in D18.1's " > " notation (parse_chain()), or on a derived flag the caller supplies as
# flag.<name>. The lineage spec's `when` uses them from M6, checked by lineage_when_invalid;
# M10a's applies_when rules reuse the parser and the evaluator.

#' The literals of a condition as written: text in double or single quotes holding no quote of
#' its kind, or a number; several joined by ", "
#'
#' Returns the literals' text, the quotes dropped and a number kept as written, or NULL where
#' `text` isn't one or more literals so joined.
#' @noRd
condition_literals <- function(text) {
  pattern <- "\"[^\"]*\"|'[^']*'|-?[0-9]+([.][0-9]+)?"
  found <- regmatches(text, gregexpr(pattern, text))[[1L]]
  # The literals must give the text back exactly, so nothing between or around them is lost.
  if (length(found) == 0L || !identical(paste(found, collapse = ", "), text)) {
    return(NULL)
  }
  quoted <- grepl("^[\"']", found)
  found[quoted] <- substr(found[quoted], 2L, nchar(found[quoted]) - 1L)
  found
}

#' One condition parsed against the dictionary (D8.10; R3)
#'
#' `text` is one of `<operand> == <literal>`, `<operand> != <literal>`,
#' `<operand> %in% c(<literal>, ...)`, `is_present(<operand>)` and `is_sentinel(<operand>)`,
#' with one space on each side of an operator and none inside the brackets' ends; no `&`, `|`
#' or `!`. `<operand>` is an attribute of `table`, a chain of its foreign keys joined by " > "
#' ending in an attribute, or `flag.<name>`, a flag the caller supplies for `table` (D19.43).
#' A literal is text in double or single quotes, or a number, compared with the value's text
#' as written (condition_rows()). Returns `list(operator, operand, values)`: `operator` one of
#' "==", "!=", "%in%", "is_present" and "is_sentinel"; `operand` parse_chain()'s result, or
#' `list(flag = <name>)`; `values` the literals' text. NULL where `text` isn't such a
#' condition.
#' @noRd
parse_condition <- function(text, table, keys, attributes) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    return(NULL)
  }
  operand_of <- function(x) {
    flag <- regmatches(x, regexec("^flag[.]([A-Za-z_][A-Za-z0-9_]*)$", x))[[1L]]
    if (length(flag) == 2L) {
      return(list(flag = flag[[2L]]))
    }
    parse_chain(x, table, keys, attributes)
  }
  test <- regmatches(text, regexec("^(is_present|is_sentinel)[(](.+)[)]$", text))[[1L]]
  if (length(test) == 3L) {
    operand <- operand_of(test[[3L]])
    if (is.null(operand)) {
      return(NULL)
    }
    return(list(operator = test[[2L]], operand = operand, values = character()))
  }
  # The first operator with a space on each side splits the text, so a literal may hold one.
  compare <- regmatches(text, regexec("^(.+?) (==|!=|%in%) (.+)$", text, perl = TRUE))[[1L]]
  if (length(compare) != 4L) {
    return(NULL)
  }
  operand <- operand_of(compare[[2L]])
  right <- compare[[4L]]
  if (compare[[3L]] == "%in%") {
    inside <- regmatches(right, regexec("^c[(](.+)[)]$", right))[[1L]]
    values <- if (length(inside) == 2L) condition_literals(inside[[2L]])
  } else {
    values <- condition_literals(right)
    if (length(values) != 1L) {
      values <- NULL
    }
  }
  if (is.null(operand) || is.null(values)) {
    return(NULL)
  }
  list(operator = compare[[3L]], operand = operand, values = values)
}

#' Whether a condition holds for some rows of a table (D8.10, D19.43; R3)
#'
#' `condition` is parse_condition()'s result for `table`; `rows` are rows of the table, NA
#' for none; `flags` is flag_values()'s result. An attribute or chain is read where the
#' record's foreign keys reach it (chain_rows(), one key join per hop), a flag at the record's
#' key; each value is written as text for these rows only (value_text(), 16.2) and compared as
#' written, a number literal with the number's text ("1" with 1, not with 1.0). A row whose
#' value is NA, whose chain breaks, or which has no flag holds no condition, `!=` included.
#' `is_present` holds where the value isn't blank (D16.42) or a missing-value code of its type,
#' and `is_sentinel` where it is such a code; a flag's codes are the character ones. A
#' condition reading a flag that `flags` doesn't give for `table` is a caller's error (D19.43).
#' Returns one logical per row.
#' @noRd
condition_rows <- function(input, table, condition, rows, flags = list()) {
  operand <- condition$operand
  if (!is.null(operand$flag)) {
    given <- flags[[table]]
    if (is.null(given) || !operand$flag %chin% names(given$values)) {
      # W170
      stop(sprintf(
        "A lineage condition reads flag.%s, which `flags` doesn't give for table %s.",
        operand$flag, table
      ), call. = FALSE)
    }
    text <- given$values[[operand$flag]][given$at[rows]]
    family <- "character"
  } else {
    reached <- chain_rows(input, table, operand$steps)
    text <- value_text(reached_values(
      input, list(home = operand$table, rows = reached[rows]), operand$attribute
    ))
    columns <- input$columns
    family <- columns$family[
      columns$table_name == operand$table & columns$attribute_name == operand$attribute
    ]
  }
  sentinels <- input$spec$sentinels
  codes <- sentinels$value[sentinels$data_type == family[[1L]]]
  known <- !is.na(text)
  switch(condition$operator,
    "==" = known & text == condition$values,
    "!=" = known & text != condition$values,
    "%in%" = known & text %chin% condition$values,
    is_present = known & !space_padding(text)$blank & !text %chin% codes,
    is_sentinel = known & text %chin% codes
  )
}
