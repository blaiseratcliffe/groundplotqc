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
#' value's text is the code, so a real 0 isn't the date code "00000000" (m39), only the values
#' near a code that reads as a number written as text (16.2).
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
      # Plain numbers aren't all written as text (16.2): only NaN and the values whose
      # 15-digit text may be a code are, as domain_skipped() does (R84). A value written as a
      # number's code lies within 1e-14 of it, relative, so the wider margin only narrows what
      # is written; the text decides.
      numbers <- suppressWarnings(as.numeric(codes))
      near <- is.nan(x)
      for (code in numbers[!is.na(numbers)]) {
        margin <- if (is.finite(code)) abs(code) * 1e-12 else 0
        near <- near | x == code | abs(x - code) <= margin
      }
      maybe <- which(near)
      maybe[value_text(x[maybe]) %chin% codes]
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

#' Each record's A2 label (D18.6, D18.37, D18.38)
#'
#' The record reaches the last attribute of id_band_columns' key chain by D16.13's shortest
#' path (reach_column()); that value is looked up in the key's column of the labels sheet
#' (`labels_sheet` in the code_lists component), and the label is the same row's
#' `labels_column`. NA where id_band_columns has no row, the record reaches no value, or the
#' sheet holds no row of it.
#' @noRd
record_labels <- function(input, table) {
  spec <- input$spec
  bands <- spec$id_band_columns
  n <- nrow(input$data[[table]])
  if (nrow(bands) == 0L) {
    return(rep(NA_character_, n))
  }
  chain <- parse_chain(
    bands$key_attribute[[1L]], bands$table_name[[1L]], spec$keys, spec$attributes
  )
  if (is.null(chain)) {
    return(rep(NA_character_, n))
  }
  key <- chain$attribute
  reached <- reach_column(input, table, key)
  if (is.na(reached$home)) {
    return(rep(NA_character_, n))
  }
  # The key's home table written as text and labelled once, then read at each record's row
  # there, so only the home table's values are converted (16.2).
  home_n <- if (reached$home %chin% names(input$data)) nrow(input$data[[reached$home]]) else 0L
  home <- list(home = reached$home, rows = seq_len(home_n))
  value <- value_text(reached_values(input, home, key))
  lists <- spec$code_lists
  # Worked out before the brackets, so no name inside them is taken for a column.
  in_sheet <- lists$sheet == bands$labels_sheet[[1L]] & lists$source_row > 1L
  sheet <- lists[in_sheet]
  # Worked out before the brackets too: `sheet` is also a column of the code lists.
  is_key <- sheet$sheet_column == key
  is_label <- sheet$sheet_column == bands$labels_column[[1L]]
  keyed <- sheet[is_key]
  labelled <- sheet[is_label]
  label <- labelled$value[match(keyed$source_row, labelled$source_row)]
  home_label <- label[match(value, keyed$value, incomparables = NA)]
  home_label[reached$rows]
}

#' Layer 4's outcomes, by rule
#'
#' `pk_duplicate` per table: every record holding a primary key value another record holds,
#' a composite key compared whole and reported on its first part, its value the parts joined
#' with "|" (D18.10 (2), D18.42); a key NA, blank or a sentinel, allowed or not, is left
#' alone, pk_missing's where `allowed_in_pk` refuses it; not_run (no_column) where a part's
#' column wasn't given (D9.24). `pk_missing` per key part: NA, blank or a sentinel
#' `allowed_in_pk` refuses (D7.9, D18.15). `fk_orphan` per foreign key to a parent's PK: a
#' value naming no parent row, expected the parent table; a sentinel `allowed_in_fk` allows
#' counts as not evaluated, with no issue (D7.3, D7.7, D18.33); NA, blanks and refused
#' sentinels are fk_unassigned's (D7.9); not_run (no_table, no_column) without the parent's
#' key. `fk_unassigned` per foreign key: NA, blank or a sentinel `allowed_in_fk` refuses
#' (D2.28, D18.10 (1), D18.15).
#' `fk_sentinel_unverified` per foreign key of a number type: each record holding a sentinel
#' `allowed_in_fk` allows, never looked up (D12.31, D16.2, D18.33). `fk_not_applicable_mismatch`
#' per foreign key: not_run, no_matrix, until a matrix says where "Z" belongs (D7.3).
#' `src_id_duplicate` per ID-marked attribute outside the keys: every record whose value
#' another record of its table and report unit (the first group column) holds; NA, blanks,
#' sentinels and the records of a contributor `one_to_many` documents for the attribute are
#' left alone, and a record whose unit is "*" isn't checked (D2.30, D18.3, D18.6, D18.35).
#' `key_incoherent` per table and first attribute of a `key_pairs` left side: a record whose
#' two sides of a pair differ, compared as text; its value and expected the failing pairs'
#' sides labelled by the left side's last attribute; a side whose chain breaks, or whose value
#' is NA, blank or a sentinel, isn't compared, and a record none of whose pairs was compared
#' isn't checked (D18.1, D18.40).
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
  # Each row's report unit, the first group column's text ("*" where it reaches none); one
  # unit for all without group columns.
  units_of <- function(table, n) {
    if (length(input$group_cols) == 0L) {
      return(rep("1", n))
    }
    grouping <- input$groups[[table]]
    grouping$values[[input$group_cols[[1L]]]][grouping$row_group]
  }
  list(
    pk_duplicate = rule_outcomes(
      "pk_duplicate", rows_of(firsts$table_name, firsts$attribute_name), input,
      function(table, attribute, x, row) {
        parts <- primary[primary$table_name == table]
        values <- lapply(parts$attribute_name, function(part) data_of(table, part))
        # A key a part of whose column wasn't given compares nothing (D9.24).
        if (any(vapply(values, is.null, logical(1)))) {
          return(not_run_outcome("pk_duplicate", table, attribute, "no_column"))
        }
        missing <- logical(length(x))
        for (k in seq_along(values)) {
          # NA, blanks and every sentinel of the part's type, allowed or not, hold no key.
          part_row <- rows_of(table, parts$attribute_name[[k]])
          found <- key_rows(values[[k]], part_row, sentinels, "allowed_in_pk")
          missing[c(found$missing, found$open)] <- TRUE
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
    ),
    src_id_duplicate = local({
      scope <- columns[columns$id_marked & is.na(columns$key_type)]
      # A row pre-flight finds can't apply is left unapplied, its repeats warning (D18.36).
      applies <- is.na(one_to_many_problems(spec))
      documented <- spec$one_to_many[applies]
      # Each table's labels, found once for all its documented attributes (16.2).
      labelled <- new.env(parent = emptyenv())
      labels_of <- function(table) {
        if (is.null(labelled[[table]])) {
          assign(table, record_labels(input, table), envir = labelled)
        }
        labelled[[table]]
      }
      rule_outcomes("src_id_duplicate", scope, input, function(table, attribute, x, row) {
        found <- key_rows(x, row, sentinels, "allowed_in_fk")
        # NA, blanks and every sentinel of the type (the type's missing and not-applicable
        # codes) are no IDs (D18.3).
        left_alone <- logical(length(x))
        left_alone[c(found$missing, found$open)] <- TRUE
        # The records of a contributor whose A2 documents this ID as one-to-many, found
        # through the record's A2 label (D18.3, D18.6).
        is_documented <- documented$table_name == table & documented$attribute_name == attribute
        if (any(is_documented)) {
          labels <- labels_of(table)
          left_alone <- left_alone | labels %chin% documented$contributor_label[is_documented]
        }
        unit <- units_of(table, length(x))
        star <- unit == "*"
        compared <- which(!left_alone & !star)
        # Made by reference, the units and the caller's column read where they are when every
        # record is compared (16.2).
        every_row <- length(compared) == length(x)
        pool <- setDT(list(
          unit = if (every_row) unit else unit[compared], value = if (every_row) x else x[compared]
        ))
        fail <- compared[duplicated(pool) | duplicated(pool, fromLast = TRUE)]
        record_outcome(
          "src_id_duplicate", table, attribute, fail, value_text(x[fail]),
          unchecked = which(star)
        )
      })
    }),
    key_incoherent = if (!given(spec, "key_pairs")) {
      list(not_run_outcome("key_incoherent", "*", "*", "no_input"))
    } else {
      pairs <- spec$key_pairs
      first <- sub(" > .*$", "", pairs$left)
      heads <- unique(data.table(table_name = pairs$table_name, attribute_name = first))
      scope <- rows_of(heads$table_name, heads$attribute_name)
      rule_outcomes(
        "key_incoherent", scope[!is.na(scope$table_name)], input,
        function(table, attribute, x, row) {
          mine <- which(pairs$table_name == table & first == attribute)
          n <- length(x)
          differs <- list()
          labels <- character()
          left_values <- list()
          right_values <- list()
          any_compared <- logical(n)
          for (k in mine) {
            left <- parse_chain(pairs$left[[k]], table, keys, spec$attributes)
            right <- parse_chain(pairs$right[[k]], table, keys, spec$attributes)
            # A row that doesn't parse is key_pair_invalid's, which stops pre-flight.
            if (is.null(left) || is.null(right)) {
              next
            }
            # A side of one attribute is the column as given, read where it is (16.2): the
            # left one is `x` itself.
            lv <- if (nrow(left$steps) == 0L) x else chain_values(input, table, left)
            rv <- if (nrow(right$steps) == 0L) data_of(table, right$attribute)
            if (is.null(rv)) {
              rv <- chain_values(input, table, right)
            }
            # A side whose chain breaks gives NA; NA, blanks and sentinels of the side's own
            # attribute's type aren't compared (D18.1).
            skipped <- logical(n)
            for (side in list(list(lv, left), list(rv, right))) {
              found <- key_rows(
                side[[1L]], rows_of(side[[2L]]$table, side[[2L]]$attribute), sentinels,
                "allowed_in_fk"
              )
              skipped[c(found$missing, found$open)] <- TRUE
            }
            # Compared as text, the rows compared only: as stored where both sides are plain
            # numbers or both text; otherwise each distinct stored value of a side that isn't
            # text is written once, by its dense rank, so equal values give one text (16.2).
            compared <- which(!skipped)
            every_row <- length(compared) == n
            lc <- if (every_row) lv else lv[compared]
            rc <- if (every_row) rv else rv[compared]
            same <- if (is.numeric(lc) && !is.object(lc) && is.numeric(rc) && !is.object(rc)) {
              lc == rc
            } else if (is.character(lc) && is.character(rc)) {
              lc == rc
            } else {
              written <- lapply(list(lc, rc), function(v) {
                if (is.character(v) || length(v) == 0L) {
                  return(value_text(v))
                }
                rank <- frankv(v, ties.method = "dense")
                value_text(v[match(seq_len(max(rank)), rank)])[rank]
              })
              written[[1L]] == written[[2L]]
            }
            any_compared <- any_compared | !skipped
            differ <- logical(n)
            differ[compared[!same %in% TRUE]] <- TRUE
            differs[[length(differs) + 1L]] <- differ
            labels <- c(labels, left$attribute)
            left_values[[length(left_values) + 1L]] <- lv
            right_values[[length(right_values) + 1L]] <- rv
          }
          fail <- if (length(differs) == 0L) integer() else which(Reduce(`|`, differs))
          # Each failing pair's sides, labelled by its left side's last attribute, in
          # key_pairs' order (D18.40).
          value <- character(length(fail))
          expected <- character(length(fail))
          for (k in seq_along(differs)) {
            hit <- differs[[k]][fail]
            joiner <- fifelse(nzchar(value[hit]), "; ", "")
            value[hit] <- paste0(
              value[hit], joiner, labels[[k]], "=", value_text(left_values[[k]][fail][hit])
            )
            expected[hit] <- paste0(
              expected[hit], joiner, labels[[k]], "=", value_text(right_values[[k]][fail][hit])
            )
          }
          record_outcome(
            "key_incoherent", table, attribute, fail, value, expected,
            unchecked = which(!any_compared)
          )
        }
      )
    }
  )
}
