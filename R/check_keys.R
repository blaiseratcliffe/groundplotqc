# Layer 4, keys and relationships (plan 6.1; D2.28 to D2.32, D7.3, D7.7, D7.9, D8.24, D9.26,
# D12.31, D12.75, D16.2, D18.1, D18.3, D18.4, D18.10, D18.14, D18.15, D18.33 to D18.43): each
# table's primary key present and unique, each foreign key naming a parent row, the redundant
# keys key_pairs names agreeing, src_*_id values that repeat, the records no root reaches, the
# parents no child points at, and the three design candidates of Appendix E. A key is read as
# given: NA, a blank (any Unicode space in text that isn't plain ASCII, D16.42, D18.15) and the
# key type's sentinels are kept apart as 3.5's allowed_in_pk and allowed_in_fk say. A value
# that failed Layers 1 to 3 is masked by the run (6.2). The engine names no table, column or
# code: the keys come from the dictionary, the pairs and the walk from the specification, and
# the candidates' tables and columns from the settings (CLAUDE.md "Code").
#
# Task cuts (M5): task 9 writes the file with check_keys() returning an empty list; task 10
# adds key_rows(), pk_holders() and the rules pk_duplicate to fk_not_applicable_mismatch, with
# the locals rows_of(), data_of(), edge_of(), parent_missing() and parent_rows(); task 11
# key_incoherent; task 12 record_labels(), the local units_of() and src_id_duplicate; task 13
# reach_rows(), record_unreachable and parent_without_children; task 14 disagreeing_rows(), the
# local by_groups() and the three design candidates. Each cut keeps the rules in this order, the
# registry's, and each later task inserts its rule at its place.

#' A key column's missing values, and those holding a sentinel its role allows
#'
#' `x` is the column as given and `row` its attribute's row of the input's columns table;
#' `allowed` names the sentinels' column that says where they may stand (`allowed_in_pk` or
#' `allowed_in_fk`, 3.5). `missing` gives the rows that are NA, blank (any Unicode space in
#' text that isn't plain ASCII, D16.42, D18.15) or hold a sentinel of the key's type that
#' `allowed` refuses; `open` the rows holding one it allows. A sentinel is matched as
#' rows_holding() matches it, and in a number-typed key stored as text also as a number, so
#' "-9.0" is -9 (R29); in a key stored as numbers whose type isn't a number type, where the
#' value's text is the code, so a real 0 isn't the date code "00000000" (m39).
#' @noRd
key_rows <- function(x, row, sentinels, allowed) {
  own <- sentinels[sentinels$data_type == row$family]
  allows <- own[[allowed]] %in% TRUE
  stored_numbers <- is.numeric(x) && !is.object(x)
  number <- if (row$family == "numeric" && !stored_numbers && nrow(own) > 0L) {
    coerce_column(x, "double")$value
  }
  holding <- function(codes) {
    if (length(codes) == 0L) {
      return(integer())
    }
    hit <- if (stored_numbers && row$family != "numeric") {
      which(value_text(x) %chin% codes)
    } else {
      rows_holding(x, codes)
    }
    if (!is.null(number)) {
      numbers <- suppressWarnings(as.numeric(codes))
      hit <- union(hit, which(number %in% numbers[!is.na(numbers)]))
    }
    hit
  }
  blank <- integer()
  text <- stored_text(x)
  if (!is.null(text)) {
    # Only the values that may be blank are read by space_padding() (16.2).
    maybe <- which(grepl("^[ \t\r\n]*$|[^\001-\177]", text, useBytes = TRUE))
    blank <- maybe[space_padding(text[maybe])$blank]
  }
  list(
    missing = sort(unique(c(which(is.na(x)), blank, holding(own$value[!allows])))),
    open = sort(holding(own$value[allows]))
  )
}

#' Each row of a table's first row holding the same primary key value (D18.14)
#'
#' A child joined to its parent reaches the parent's first holder of its key (chain_rows());
#' the holders let it reach every holder of a repeated key. NA for a row whose key is NA,
#' blank (as space_padding() reads it) or a sentinel of its type, which no child can name
#' (D16.26, D18.31), and for every row of a table not given or whose key isn't one column, which
#' no foreign key follows (D18.42). Kept in the input's cache, by table.
#' @noRd
pk_holders <- function(input, table) {
  name <- paste("pk_holders", table, sep = "\r")
  kept <- input$cache[[name]]
  if (!is.null(kept)) {
    return(kept)
  }
  keys <- input$spec$keys
  # Worked out before the brackets, so no name inside them is taken for a column.
  is_pk <- keys$table_name == table & keys$key_type == "PK"
  pk <- keys$attribute_name[is_pk]
  n <- if (table %chin% names(input$data)) nrow(input$data[[table]]) else 0L
  holders <- rep(NA_integer_, n)
  if (length(pk) == 1L && n > 0L) {
    found <- key_values(input, table, pk)
    value <- found$value
    blanks <- character()
    if (is.character(value)) {
      maybe <- which(grepl("^[ \t\r\n]*$|[^\001-\177]", value, useBytes = TRUE))
      blanks <- unique(value[maybe][space_padding(value[maybe])$blank])
    }
    holders <- match(value, value, incomparables = c(NA, found$open, blanks))
  }
  assign(name, holders, envir = input$cache)
  holders
}

#' Layer 4's outcomes, by rule
#'
#' `pk_duplicate` per table: every record holding a primary key value another record holds,
#' a composite key compared whole and reported on its first part, its value the parts joined
#' with "|" (D18.10 (2), D18.42); a key NA, blank or a sentinel is pk_missing's, left alone.
#' `pk_missing` per key part: NA, blank or a sentinel `allowed_in_pk` refuses (D7.9, D18.15).
#' `fk_orphan` per foreign key to a parent's PK: a value naming no parent row, expected the
#' parent table; a sentinel `allowed_in_fk` allows counts as not evaluated, with no issue
#' (D7.3, D7.7, D18.33); NA, blanks and refused sentinels are fk_unassigned's (D7.9);
#' not_run (no_table, no_column) without the parent's key. `fk_unassigned` per foreign key:
#' NA, blank or a sentinel `allowed_in_fk` refuses (D2.28, D18.10 (1), D18.15).
#' `fk_sentinel_unverified` per foreign key of a number type: each record holding a sentinel
#' `allowed_in_fk` allows, never looked up (D12.31, D16.2, D18.33). `fk_not_applicable_mismatch`
#' per foreign key: not_run, no_matrix, until a matrix says where "Z" belongs (D7.3).
#' @noRd
check_keys <- function(input, context) {
  spec <- context$spec
  keys <- spec$keys
  columns <- input$columns
  sentinels <- spec$sentinels
  supplied <- names(input$data)
  column_key <- paste(columns$table_name, columns$attribute_name, sep = "\r")
  # The columns table's rows of some attributes, in their order; NA rows for unknown ones.
  rows_of <- function(table, attribute) {
    columns[match(paste(table, attribute, sep = "\r"), column_key)]
  }
  # An attribute's column as given, NULL where its table or column wasn't given.
  data_of <- function(table, attribute) {
    row <- rows_of(table, attribute)
    if (!table %chin% supplied || is.na(row$column)) NULL else input$data[[table]][[row$column]]
  }
  is_pk <- keys$key_type == "PK"
  primary <- keys[is_pk]
  primary <- primary[order(match(primary$table_name, columns$table_name), primary$key_part)]
  is_first <- primary$key_part == 1L
  firsts <- primary[is_first]
  is_fk <- keys$key_type == "FK"
  foreign <- keys[is_fk]
  is_edge <- !is.na(foreign$reference_table) & !is.na(foreign$reference_attribute)
  edges <- foreign[is_edge]
  edge_of <- function(table, attribute) {
    edges[edges$table_name == table & edges$attribute_name == attribute]
  }
  # Why a foreign key can't be followed: its parent table or key not given; NULL when it can.
  parent_missing <- function(edge) {
    if (!edge$reference_table %chin% supplied) {
      return("no_table")
    }
    if (is.null(data_of(edge$reference_table, edge$reference_attribute))) "no_column" else NULL
  }
  # Each child row's parent row along one foreign key, matched once per key (16.2).
  joined <- new.env(parent = emptyenv())
  parent_rows <- function(edge) {
    name <- paste(edge$table_name, edge$attribute_name, sep = "\r")
    if (is.null(joined[[name]])) {
      assign(name, chain_rows(input, edge$table_name, edge), envir = joined)
    }
    joined[[name]]
  }
  list(
    pk_duplicate = rule_outcomes(
      "pk_duplicate", rows_of(firsts$table_name, firsts$attribute_name), input,
      function(table, attribute, x, row) {
        parts <- primary[primary$table_name == table]
        values <- lapply(parts$attribute_name, function(part) data_of(table, part))
        missing <- logical(length(x))
        for (k in seq_along(values)) {
          if (is.null(values[[k]])) {
            missing[] <- TRUE
          } else {
            part_row <- rows_of(table, parts$attribute_name[[k]])
            missing[key_rows(values[[k]], part_row, sentinels, "allowed_in_pk")$missing] <- TRUE
          }
        }
        compared <- which(!missing)
        # The caller's columns are read where they are; a subset is taken only where some
        # key is missing, and a composite key is a table made by reference over its parts.
        keyed <- if (length(values) == 1L) {
          if (length(compared) == length(x)) x else x[compared]
        } else {
          setDT(lapply(values, function(v) if (length(compared) == length(x)) v else v[compared]))
        }
        fail <- compared[duplicated(keyed) | duplicated(keyed, fromLast = TRUE)]
        value <- if (length(values) == 1L) {
          value_text(x[fail])
        } else {
          do.call(paste, c(lapply(values, function(v) value_text(v[fail])), sep = "|"))
        }
        record_outcome("pk_duplicate", table, attribute, fail, value)
      }
    ),
    pk_missing = rule_outcomes(
      "pk_missing", rows_of(primary$table_name, primary$attribute_name), input,
      function(table, attribute, x, row) {
        fail <- key_rows(x, row, sentinels, "allowed_in_pk")$missing
        record_outcome("pk_missing", table, attribute, fail, value_text(x[fail]))
      }
    ),
    fk_orphan = rule_outcomes(
      "fk_orphan", rows_of(edges$table_name, edges$attribute_name), input,
      function(table, attribute, x, row) {
        edge <- edge_of(table, attribute)
        reason <- parent_missing(edge)
        if (!is.null(reason)) {
          return(not_run_outcome("fk_orphan", table, attribute, reason))
        }
        found <- key_rows(x, row, sentinels, "allowed_in_fk")
        orphan <- which(is.na(parent_rows(edge)))
        fail <- orphan[!orphan %in% c(found$missing, found$open)]
        record_outcome(
          "fk_orphan", table, attribute, fail, value_text(x[fail]), edge$reference_table,
          not_evaluated = found$open
        )
      }
    ),
    fk_unassigned = rule_outcomes(
      "fk_unassigned", rows_of(foreign$table_name, foreign$attribute_name), input,
      function(table, attribute, x, row) {
        fail <- key_rows(x, row, sentinels, "allowed_in_fk")$missing
        record_outcome("fk_unassigned", table, attribute, fail, value_text(x[fail]))
      }
    ),
    fk_sentinel_unverified = local({
      scope <- rows_of(foreign$table_name, foreign$attribute_name)
      rule_outcomes(
        "fk_sentinel_unverified", scope[scope$family == "numeric"], input,
        function(table, attribute, x, row) {
          fail <- key_rows(x, row, sentinels, "allowed_in_fk")$open
          record_outcome("fk_sentinel_unverified", table, attribute, fail, value_text(x[fail]))
        }
      )
    }),
    fk_not_applicable_mismatch = rule_outcomes(
      "fk_not_applicable_mismatch", rows_of(foreign$table_name, foreign$attribute_name), input,
      function(table, attribute, x, row) {
        not_run_outcome("fk_not_applicable_mismatch", table, attribute, "no_matrix")
      }
    )
  )
}
