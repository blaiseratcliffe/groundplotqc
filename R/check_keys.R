# Layer 4, keys and relationships (plan 6.1; D2.28 to D2.32, D7.3, D7.7, D7.9, D8.24, D9.26,
# D12.31, D12.75, D16.2, D18.1, D18.3, D18.4, D18.10, D18.14, D18.15, D18.33 to D18.43): each
# table's primary key present and unique, each foreign key naming a parent row, the redundant
# keys key_pairs names agreeing, src_*_id values that repeat, the records no root reaches, the
# parents no child points at, and the three design candidates of Appendix E. A key is read as
# given: NA, a blank (any Unicode space in text that isn't plain ASCII, D16.42, D18.15) and the
# key type's sentinels are kept apart as 3.5's allowed_in_pk and allowed_in_fk say. A value
# that failed Layers 1 to 3 is masked by the run (6.2), but for a Layer 3 failure in a key
# column, which is still checked as a key (D18.76). The engine names no table, column or
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
#' near a code that reads as a number written as text (16.2). A key stored as integer64, as a
#' DBI query of a BIGINT column returns it, is read as stored numbers are (D18.64, m55).
#' @noRd
key_rows <- function(x, row, sentinels, allowed) {
  own <- sentinels[sentinels$data_type == row$family]
  allows <- own[[allowed]] %in% TRUE
  stored_numbers <- is.numeric(x) && (!is.object(x) || inherits(x, "integer64"))
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
      # is written; the text decides. integer64 is compared by its own methods, within its
      # range only, so no value overflows; it holds no NaN.
      numbers <- suppressWarnings(as.numeric(codes))
      near <- is.nan(x)
      for (code in numbers[!is.na(numbers)]) {
        margin <- if (is.finite(code)) abs(code) * 1e-12 else 0
        if (!is.object(x)) {
          near <- near | x == code | abs(x - code) <= margin
        } else if (abs(code) + margin < 2^63) {
          near <- near | (x >= code - margin & x <= code + margin)
        }
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
#' the holders let it reach every holder of a repeated key. NA for a row whose key is NA, NaN,
#' blank (as space_padding() reads it) or a sentinel of its type, which no child can name
#' (D16.26, D18.31), masked after a plain match() with %in% in the key's own type (16.2), as
#' match()'s `incomparables` isn't always honoured when it holds more than one value (D18.73);
#' and for every row of a table not given or whose key isn't one column, which no foreign key
#' follows (D18.42). Kept in the input's cache, by table.
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
    # NULL unless the key is text, so the codes keep a number key's type (16.2).
    blanks <- NULL
    if (is.character(value)) {
      maybe <- which(grepl("^[ \t\r\n]*$|[^\001-\177]", value, useBytes = TRUE))
      blanks <- unique(value[maybe][space_padding(value[maybe])$blank])
    }
    holders <- match(value, value)
    # NaN, which match() keeps apart from NA, holds no key either, as key_rows() reads it (m85).
    masked <- c(NA, found$open, blanks)
    if (is.double(value)) {
      masked <- c(masked, NaN)
    }
    # An integer key is read against integer codes, so %in% copies no key to doubles (16.2):
    # only a whole code an integer holds can equal one; -99.5 isn't -99 (m86).
    if (is.integer(value)) {
      whole <- is.na(masked) | (masked == trunc(masked) & abs(masked) <= .Machine$integer.max)
      masked <- as.integer(masked[whole])
    }
    holders[value %in% masked] <- NA_integer_
  }
  assign(name, holders, envir = input$cache)
  holders
}

#' The rows the reach input's links reach from its roots, table by table (D18.4, D18.14)
#'
#' Every row of a `root` table is reached. A `from_parent` link reaches a row of its table
#' when the row its foreign key names is reached; a `from_child` link reaches a row of the
#' parent table when a reached row of its table names it. A child naming a repeated key
#' reaches every holder of it, and is reached when any holder is (pk_holders()). The links
#' are passed over until a pass reaches nothing new; each link's rows are matched once, before
#' the passes, one key join each (chain_rows(), 16.2). An exempt table is walked by no link.
#' A root not given, as record_unreachable's second walk takes each linked table not given
#' (D18.66), reaches every row its links can: a child row holding a key, or a parent row whose
#' key a child can hold, a key being neither NA, blank nor a missing-value code of the child's
#' or the parent's type, as a join reads it (D18.75). A root row naming an attribute, as that
#' walk takes each link's key column not given (D18.68), makes each link through that column
#' reach, once a row on its near side that holds a key is reached, every row on its far side
#' that holds one; a row whose key column isn't given counts as holding one. When a second walk
#' will follow, the joins are kept in the input's cache, so it makes none again. Returns a
#' logical per row for each table the walk names but the exempt ones, a table not given having
#' none.
#' @noRd
reach_rows <- function(input, reach) {
  keys <- input$spec$keys
  is_edge <- keys$key_type == "FK" & !is.na(keys$reference_table) &
    !is.na(keys$reference_attribute)
  edges <- keys[is_edge]
  exempt <- reach$table_name[reach$how %chin% "exempt"]
  is_link <- reach$how %chin% c("from_parent", "from_child")
  links <- reach[is_link]
  at <- match(
    paste(links$table_name, links$attribute_name, sep = "\r"),
    paste(edges$table_name, edges$attribute_name, sep = "\r")
  )
  # A row naming no foreign key to a parent's PK is reach_invalid's; it reaches nothing.
  links <- links[!is.na(at)]
  link_edges <- edges[at[!is.na(at)]]
  walked <- setdiff(
    unique(c(reach$table_name[!reach$how %chin% "exempt"], link_edges$reference_table)), exempt
  )
  walked <- walked[!is.na(walked)]
  given <- names(input$data)
  size <- function(table) if (table %chin% given) nrow(input$data[[table]]) else 0L
  # The roots: tables, and in a second walk the key columns not given (D18.68).
  is_root <- reach$how %chin% "root"
  root_tables <- reach$table_name[is_root & is.na(reach$attribute_name)]
  root_columns <- paste(reach$table_name, reach$attribute_name, sep = "\r")[
    is_root & !is.na(reach$attribute_name)
  ]
  reached <- lapply(walked, function(table) rep(table %chin% root_tables, size(table)))
  names(reached) <- walked
  followed <- !links$table_name %chin% exempt & !link_edges$reference_table %chin% exempt
  child_given <- links$table_name %chin% given
  parent_given <- link_edges$reference_table %chin% given
  # Each link's foreign key and its parent's key: whether the column was given.
  column_key <- paste(input$columns$table_name, input$columns$attribute_name, sep = "\r")
  child_at <- paste(links$table_name, links$attribute_name, sep = "\r")
  parent_at <- paste(link_edges$reference_table, link_edges$reference_attribute, sep = "\r")
  child_column <- !is.na(input$columns$column[match(child_at, column_key)])
  parent_column <- !is.na(input$columns$column[match(parent_at, column_key)])
  between <- followed & child_given & parent_given
  usable <- which(between & child_column & parent_column)
  # The followed links a second walk starts from or follows loosely: from a table not given
  # into one given (D18.66), or between tables given with a key column not given (D18.68).
  # record_unreachable finds the same links to decide whether a second walk runs; the two are
  # kept in step (m83).
  from_absent <- followed & fifelse(
    links$how == "from_parent", child_given & !parent_given, parent_given & !child_given
  )
  broken <- between & !(child_column & parent_column)
  # A link's join, kept in the input's cache when such a link makes a second walk follow,
  # where it stays until the run ends; made afresh otherwise, as nothing reads it again.
  keep <- any(from_absent | broken)
  joined <- function(edge) {
    name <- paste("reach_rows", edge$table_name, edge$attribute_name, sep = "\r")
    rows <- input$cache[[name]]
    if (is.null(rows)) {
      rows <- chain_rows(input, edge$table_name, edge)
      if (keep) {
        assign(name, rows, envir = input$cache)
      }
    }
    rows
  }
  prepared <- lapply(usable, function(k) {
    edge <- link_edges[k]
    list(
      how = links$how[[k]], child = edge$table_name, parent = edge$reference_table,
      parent_row = joined(edge), holders = pk_holders(input, edge$reference_table)
    )
  })
  # The missing-value codes of the other side's type, read from the columns table, in the type
  # of a side's key: numbers for a key of numbers, as key_values() gives a key's own codes;
  # text otherwise, a number type's codes written as value_text() writes numbers, as
  # chain_rows() compares them (D16.26, D18.75).
  sentinels <- input$spec$sentinels
  child_family <- input$columns$family[match(child_at, column_key)]
  parent_family <- input$columns$family[match(parent_at, column_key)]
  codes_as <- function(family, value) {
    codes <- sentinels$value[sentinels$data_type == family]
    if (!is.numeric(value) && family != "numeric") {
      return(codes)
    }
    number <- suppressWarnings(as.numeric(codes))
    if (family != "numeric") {
      number <- number[!is.na(number) & value_text(number) == codes]
    }
    if (is.numeric(value)) number else value_text(number)
  }
  # Whether each row of a link's child holds a key: not NA, blank or a sentinel of its type or
  # its parent's, the keys chain_rows() masks on the child's side; and of its parent: one
  # pk_holders() holds, not a sentinel of the child's type. Read with %in% in the key's own
  # type (16.2), as match()'s `incomparables` isn't always honoured when it holds more than one
  # value (D18.73). Every row, where the key column isn't given.
  child_keys <- function(k) {
    table <- links$table_name[[k]]
    if (!child_column[[k]]) {
      return(rep(TRUE, size(table)))
    }
    found <- key_values(input, table, links$attribute_name[[k]])
    value <- found$value
    # NULL unless the key is text, so the codes keep a number key's type (16.2).
    blanks <- NULL
    if (is.character(value)) {
      maybe <- which(grepl("^[ \t\r\n]*$|[^\001-\177]", value, useBytes = TRUE))
      blanks <- unique(value[maybe][space_padding(value[maybe])$blank])
    }
    # NaN, which match() keeps apart from NA, is no key either, as key_rows() reads it (m85).
    masked <- c(NA, found$open, codes_as(parent_family[[k]], value), blanks)
    if (is.double(value)) {
      masked <- c(masked, NaN)
    }
    !value %in% masked
  }
  parent_keys <- function(k) {
    parent <- link_edges$reference_table[[k]]
    if (!parent_column[[k]]) {
      return(rep(TRUE, size(parent)))
    }
    value <- key_values(input, parent, link_edges$reference_attribute[[k]])$value
    !is.na(pk_holders(input, parent)) & !value %in% codes_as(child_family[[k]], value)
  }
  # A root table not given reaches, through each followed link from it, every row holding a
  # key on the link's far side (D18.66).
  absent <- setdiff(root_tables, given)
  for (k in which(from_absent)) {
    edge <- link_edges[k]
    if (links$how[[k]] == "from_parent" && edge$reference_table %chin% absent) {
      reached[[edge$table_name]] <- reached[[edge$table_name]] | child_keys(k)
    } else if (links$how[[k]] == "from_child" && edge$table_name %chin% absent) {
      reached[[edge$reference_table]] <- reached[[edge$reference_table]] | parent_keys(k)
    }
  }
  # A link whose missing key column a root row names, followed loosely (D18.68): once a row on
  # its near side holding a key is reached, every row on its far side holding one is.
  rooted <- (!child_column & child_at %chin% root_columns) |
    (!parent_column & parent_at %chin% root_columns)
  loose <- lapply(which(broken & rooted), function(k) {
    down <- links$how[[k]] == "from_parent"
    child <- links$table_name[[k]]
    parent <- link_edges$reference_table[[k]]
    list(
      from = if (down) parent else child, to = if (down) child else parent,
      near = if (down) parent_keys(k) else child_keys(k),
      far = if (down) child_keys(k) else parent_keys(k)
    )
  })
  repeat {
    changed <- FALSE
    for (link in prepared) {
      child <- reached[[link$child]]
      parent <- reached[[link$parent]]
      if (link$how == "from_parent") {
        # A parent row stands for every holder of its key: reached when any of them is.
        any_holder <- logical(length(parent))
        any_holder[link$holders[parent & !is.na(link$holders)]] <- TRUE
        new <- !child & !is.na(link$parent_row) & any_holder[link$parent_row] %in% TRUE
        if (any(new)) {
          reached[[link$child]][new] <- TRUE
          changed <- TRUE
        }
      } else {
        named <- logical(length(parent))
        named[link$parent_row[child & !is.na(link$parent_row)]] <- TRUE
        new <- !parent & named[link$holders] %in% TRUE
        if (any(new)) {
          reached[[link$parent]][new] <- TRUE
          changed <- TRUE
        }
      }
    }
    for (link in loose) {
      if (any(reached[[link$from]] & link$near)) {
        new <- link$far & !reached[[link$to]]
        if (any(new)) {
          reached[[link$to]][new] <- TRUE
          changed <- TRUE
        }
      }
    }
    if (!changed) {
      break
    }
  }
  reached
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

#' The rows whose group holds more than one distinct value
#'
#' `groups` gives each row's group, NA for a row in none, and `value` each row's value, compared
#' as given, NA a value of its own (Appendix E K1, K8).
#' @noRd
disagreeing_rows <- function(groups, value) {
  grouped <- which(!is.na(groups))
  if (length(grouped) == 0L) {
    return(integer())
  }
  # The vectors are read where they are, a subset taken only where some row is in no group,
  # and the pairs are a table made by reference over them (16.2).
  every_row <- length(grouped) == length(groups)
  group <- if (every_row) groups else groups[grouped]
  pairs <- setDT(list(group = group, value = if (every_row) value else value[grouped]))
  distinct <- pairs[!duplicated(pairs)]
  several <- tabulate(distinct$group, max(group)) > 1L
  grouped[several[group]]
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
#' per foreign key: not_run, no_matrix, until a matrix says where the type's not-applicable
#' code belongs (D7.3).
#' `src_id_duplicate` per ID-marked attribute outside the keys: every record whose value
#' another record of its table and report unit (the first group column) holds; NA, blanks,
#' sentinels and the records of a contributor `one_to_many` documents for the attribute are
#' left alone, and a record whose unit is "*" isn't checked (D2.30, D18.3, D18.6, D18.35).
#' `key_incoherent` per table and first attribute of a `key_pairs` left side: a record whose
#' two sides of a pair differ, compared as text; its value and expected the failing pairs'
#' sides labelled by the left side's last attribute; a side whose chain breaks, or whose value
#' is NA, blank or a sentinel, isn't compared, and a record none of whose pairs was compared
#' isn't checked (D18.1, D18.40). `record_unreachable` per table the `reach` input walks: a row
#' no link reaches, on its PK's first part; a table with a `from_parent` link leaves its
#' unreached rows unchecked, each left to the FK rules or under an unreached parent; a row
#' only a linked table or a link's key column not given could reach, found by a second walk
#' taking each such table or column as a root, is unchecked; a record pk_missing reports is not
#' evaluated, rooted there; not_run (no_table) where a root table wasn't given (D18.4, D18.14,
#' D18.43, D18.66, D18.68).
#' `parent_without_children` per foreign key to a parent's PK: the parent rows no child row
#' names, counted in the parent rows' own groups, with no issue (D18.10 (4), D18.34). The
#' candidates read their tables and columns from the
#' settings, each name the dictionary's (pre-flight stops on an argument's or the rule set's
#' unknown name and conformance_context() drops an option's, D18.45, D18.47), one not_run row
#' (no_input) without them: `design_geometry_inconsistent` per
#' compared column, every record of a `design_geometry_by` group holding more than one
#' distinct value, expected the group's values as given; `subplot_label_ambiguous` on the last
#' of `subplot_label_columns`, every record whose values there a record of another PK holds,
#' the key compared by its parts as given, a record whose key is NA, blank or a missing-value
#' code a key can't hold not grouped (R45), not_run (no_column) without a column of the key;
#' `design_duplicate_signature` on the table `design_signature_by` names, every row whose
#' frames, written as text and sorted, another row of its report unit has too, expected the
#' others' keys (Appendix E K1, K8, K10; D8.24, D9.26, D18.39). A number-typed column is
#' compared by its numbers, one stored as text too, text that doesn't coerce as written
#' (D18.80); an `expected` list shows its first example_cap values in C order and the count of
#' the rest (D18.81), a blank value as the blank text (D18.83), and is NA at example_cap 0
#' (D18.82). A record whose grouping value is NA, blank or a sentinel isn't grouped.
#' @noRd
check_keys <- function(input, context) {
  # data.table's ranks and repeats read doubles exactly here, whatever setNumericRounding()
  # the session has set, so two values that differ only in their last digits stay two; the
  # session's setting is put back on exit (m67).
  rounding <- setNumericRounding(0L)
  on.exit(setNumericRounding(rounding), add = TRUE)
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
  # Each row's group by `by`'s values in a table, NA where one of them is NA, blank or a
  # sentinel; or why the groups can't be made.
  by_groups <- function(table, by) {
    if (!table %chin% supplied) {
      return("no_table")
    }
    rows <- rows_of(rep(table, length(by)), by)
    if (anyNA(rows$table_name) || anyNA(rows$column)) {
      return("no_column")
    }
    values <- lapply(rows$column, function(column) input$data[[table]][[column]])
    missing <- logical(length(values[[1L]]))
    for (k in seq_along(values)) {
      found <- key_rows(values[[k]], rows[k], sentinels, "allowed_in_fk")
      missing[c(found$missing, found$open)] <- TRUE
    }
    groups <- rep(NA_integer_, length(missing))
    present <- which(!missing)
    # The columns are read where they are, a subset taken only where some row is left out
    # (16.2).
    if (length(present) > 0L && length(present) == length(missing)) {
      groups <- frankv(values, ties.method = "dense")
    } else if (length(present) > 0L) {
      groups[present] <- frankv(lapply(values, function(v) v[present]), ties.method = "dense")
    }
    groups
  }
  # A column's values as the design candidates compare them (D18.80): a number-typed column
  # stored as text, or as a factor, by its numbers, as Layer 1 coerces them, and text that
  # doesn't coerce as written, each distinct pair one dense rank, NA a value of its own; any
  # other column as given.
  as_compared <- function(x, row) {
    if (row$family != "numeric" || !(is.character(x) || is.factor(x))) {
      return(x)
    }
    number <- coerce_column(x, row$r_class)$value
    # Text only where no number was read (16.2).
    written <- rep(NA_character_, length(x))
    lost <- which(is.na(number))
    written[lost] <- stored_text(x[lost])
    frankv(list(number, written), ties.method = "dense", na.last = TRUE)
  }
  # The most values an `expected` list shows: example_cap (D18.81).
  most <- context$example_cap
  # An `expected` list ending, where `rest` values are left out, with their count (D18.81); at
  # example_cap 0 no value is shown and `expected` is NA, as `examples` is (D18.82).
  with_rest <- function(shown, rest) {
    if (most == 0L) {
      return(rep(NA_character_, length(shown)))
    }
    capped <- which(rest > 0L)
    if (length(capped) > 0L) {
      shown[capped] <- report_text("expected_capped", values = shown[capped], n = rest[capped])
    }
    shown
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
        # key is missing, and a composite key is a table made by reference over its parts,
        # as is integer64, whose own duplicated() ignores fromLast.
        keyed <- if (length(values) == 1L) {
          if (length(compared) == length(x)) x else x[compared]
        } else {
          setDT(lapply(values, function(v) if (length(compared) == length(x)) v else v[compared]))
        }
        if (inherits(keyed, "integer64")) {
          keyed <- setDT(list(keyed))
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
    },
    record_unreachable = local({
      if (!given(spec, "reach")) {
        list(not_run_outcome("record_unreachable", "*", "*", "no_input"))
      } else {
        reach <- spec$reach
        roots <- unique(reach$table_name[reach$how %chin% "root"])
        no_root <- !all(roots %chin% supplied)
        reached <- if (!no_root) reach_rows(input, reach)
        from_above <- unique(reach$table_name[reach$how %chin% "from_parent"])
        # The tables checked: those a row names, but the exempt ones, and those a from_child
        # link reaches. A from_parent link's parent no row names isn't checked; its children
        # are then under unreached rows.
        exempt <- reach$table_name[reach$how %chin% "exempt"]
        is_child_link <- reach$how %chin% "from_child"
        targets <- edges$reference_table[match(
          paste(reach$table_name[is_child_link], reach$attribute_name[is_child_link], sep = "\r"),
          paste(edges$table_name, edges$attribute_name, sep = "\r")
        )]
        walked <- setdiff(
          unique(c(reach$table_name[!reach$how %chin% "exempt"], targets)), c(exempt, NA)
        )
        walked <- walked[order(match(walked, columns$table_name))]
        firsts <- primary[primary$key_part == 1L & primary$table_name %chin% walked]
        scope <- rows_of(firsts$table_name, firsts$attribute_name)
        # The tables not given that a followed link leads from into a table given (D18.66), and
        # the key columns not given of a followed link between tables given (D18.68): only then
        # a second walk, each taken as a root, finds the rows only they could reach. reach_rows()
        # finds the same links to follow them; the two are kept in step (m83).
        at <- match(
          paste(reach$table_name, reach$attribute_name, sep = "\r"),
          paste(edges$table_name, edges$attribute_name, sep = "\r")
        )
        is_link <- reach$how %chin% c("from_parent", "from_child") & !is.na(at)
        ends <- edges[at[is_link]]
        how <- reach$how[is_link]
        followed <- !ends$table_name %chin% exempt & !ends$reference_table %chin% exempt
        child_given <- ends$table_name %chin% supplied
        parent_given <- ends$reference_table %chin% supplied
        absent <- unique(c(
          ends$reference_table[followed & how == "from_parent" & child_given & !parent_given],
          ends$table_name[followed & how == "from_child" & parent_given & !child_given]
        ))
        between <- followed & child_given & parent_given
        no_key <- between & is.na(rows_of(ends$table_name, ends$attribute_name)$column)
        no_parent_key <- between &
          is.na(rows_of(ends$reference_table, ends$reference_attribute)$column)
        lost <- unique(data.table(
          table_name = c(ends$table_name[no_key], ends$reference_table[no_parent_key]),
          attribute_name = c(ends$attribute_name[no_key], ends$reference_attribute[no_parent_key])
        ))
        possible <- if (!no_root && (length(absent) > 0L || nrow(lost) > 0L)) {
          reach_rows(input, rbind(reach, data.table(
            table_name = c(absent, lost$table_name),
            attribute_name = c(rep(NA_character_, length(absent)), lost$attribute_name),
            how = "root"
          )))
        }
        rule_outcomes("record_unreachable", scope, input, function(table, attribute, x, row) {
          if (no_root) {
            return(not_run_outcome("record_unreachable", table, attribute, "no_table"))
          }
          unreached <- which(!reached[[table]])
          # A row only a table not given could reach is left unchecked (D18.21, D18.66).
          could <- logical(length(unreached))
          if (!is.null(possible)) {
            could <- possible[[table]][unreached]
          }
          # A table reached from above leaves each unreached row to the FK rules or under an
          # unreached parent: reported once, at its topmost row (D18.4, D18.43).
          above <- table %chin% from_above
          fail <- if (above) integer() else unreached[!could]
          record_outcome(
            "record_unreachable", table, attribute, fail, value_text(x[fail]),
            unchecked = if (above) unreached else unreached[could], masked_by = "pk_missing"
          )
        })
      }
    }),
    parent_without_children = rule_outcomes(
      "parent_without_children", rows_of(edges$table_name, edges$attribute_name), input,
      function(table, attribute, x, row) {
        edge <- edge_of(table, attribute)
        reason <- parent_missing(edge)
        if (!is.null(reason)) {
          return(not_run_outcome("parent_without_children", table, attribute, reason))
        }
        holders <- pk_holders(input, edge$reference_table)
        named <- logical(length(holders))
        pointed <- parent_rows(edge)
        named[pointed[!is.na(pointed)]] <- TRUE
        count_outcome(
          "parent_without_children", table, attribute, edge$reference_table,
          which(!named[holders] %in% TRUE)
        )
      }
    ),
    design_geometry_inconsistent = local({
      table <- context$design_geometry_table
      by <- context$design_geometry_by
      compared <- context$design_geometry_columns
      if (length(table) == 0L || length(by) == 0L || length(compared) == 0L) {
        list(not_run_outcome("design_geometry_inconsistent", "*", "*", "no_input"))
      } else {
        # Each name is the dictionary's: pre-flight stops on an argument's or the rule set's
        # unknown name and conformance_context() drops an option's (D18.45, D18.47).
        scope <- rows_of(rep(table, length(compared)), compared)
        groups <- by_groups(table, by)
        rule_outcomes(
          "design_geometry_inconsistent", scope, input,
          function(table, attribute, x, row) {
            if (is.character(groups)) {
              return(not_run_outcome("design_geometry_inconsistent", table, attribute, groups))
            }
            fail <- disagreeing_rows(groups, as_compared(x, row))
            text <- value_text(x[fail])
            # Each failing group's distinct values as given, in C order (radix), the first
            # example_cap of them shown and the rest counted (D18.81).
            at <- groups[fail]
            pairs <- setDT(list(group = at, value = text))
            distinct <- pairs[!duplicated(pairs)]
            ordered <- order(distinct$group, distinct$value, method = "radix", na.last = TRUE)
            listed_values <- distinct$value[ordered]
            # A blank value, as the checks read spaces, reads as the blank text (D18.83).
            maybe <- which(grepl("^[ \t\r\n]*$|[^\001-\177]", listed_values, useBytes = TRUE))
            blank <- maybe[space_padding(listed_values[maybe])$blank]
            listed_values[blank] <- report_text("preflight_blank_value")
            by_group <- split(listed_values, distinct$group[ordered])
            count <- lengths(by_group)
            shown <- vapply(by_group, function(values) {
              paste(values[seq_len(min(length(values), most))], collapse = "; ")
            }, "")
            listed <- with_rest(unname(shown), count - pmin(count, most))
            record_outcome(
              "design_geometry_inconsistent", table, attribute, fail, text,
              listed[match(at, as.integer(names(by_group)))]
            )
          }
        )
      }
    }),
    subplot_label_ambiguous = local({
      table <- context$subplot_label_table
      labelled <- context$subplot_label_columns
      if (length(table) == 0L || length(labelled) == 0L) {
        list(not_run_outcome("subplot_label_ambiguous", "*", "*", "no_input"))
      } else {
        scope <- rows_of(table, labelled[[length(labelled)]])
        rule_outcomes(
          "subplot_label_ambiguous", scope, input,
          function(table, attribute, x, row) {
            groups <- by_groups(table, labelled)
            if (is.character(groups)) {
              return(not_run_outcome("subplot_label_ambiguous", table, attribute, groups))
            }
            parts <- primary[primary$table_name == table]
            keyed <- lapply(parts$attribute_name, function(part) data_of(table, part))
            # A table given without a column of its key compares nothing (D9.24), as for
            # pk_duplicate.
            if (any(vapply(keyed, is.null, logical(1)))) {
              return(not_run_outcome("subplot_label_ambiguous", table, attribute, "no_column"))
            }
            # A record whose key is NA, blank or a missing-value code a key can't hold has none:
            # it is pk_missing's (R45), and isn't grouped.
            for (k in seq_along(keyed)) {
              part_row <- rows_of(table, parts$attribute_name[[k]])
              groups[key_rows(keyed[[k]], part_row, sentinels, "allowed_in_pk")$missing] <- NA
            }
            # Each record's key by its parts' values as given, as pk_duplicate compares them
            # (D18.42): one dense rank, the columns read where they are and a subset taken only
            # where some record isn't grouped, so no key is written as text (16.2).
            grouped <- which(!is.na(groups))
            key <- rep(NA_integer_, length(x))
            if (length(grouped) > 0L && length(grouped) == length(x)) {
              key <- frankv(keyed, ties.method = "dense")
            } else if (length(grouped) > 0L) {
              key[grouped] <- frankv(lapply(keyed, function(v) v[grouped]), ties.method = "dense")
            }
            fail <- disagreeing_rows(groups, key)
            record_outcome("subplot_label_ambiguous", table, attribute, fail, value_text(x[fail]))
          }
        )
      }
    }),
    design_duplicate_signature = local({
      table <- context$design_signature_table
      by <- context$design_signature_by
      signed <- context$design_signature_columns
      if (length(table) == 0L || length(by) == 0L || length(signed) == 0L) {
        list(not_run_outcome("design_duplicate_signature", "*", "*", "no_input"))
      } else {
        # `by` is a foreign key of the table linked to its parent's key, and each column the
        # table's, as for design_geometry_inconsistent (D18.45, D18.47).
        edge <- edge_of(table, by)
        scope <- rows_of(edge$reference_table, edge$reference_attribute)
        rule_outcomes(
          "design_duplicate_signature", scope, input, function(parent, attribute, x, row) {
            if (!table %chin% supplied) {
              return(not_run_outcome("design_duplicate_signature", parent, attribute, "no_table"))
            }
            parts <- rows_of(rep(table, length(signed)), signed)
            if (anyNA(parts$column) || is.null(data_of(table, by))) {
              return(not_run_outcome("design_duplicate_signature", parent, attribute, "no_column"))
            }
            owner <- parent_rows(edge)
            kept <- which(!is.na(owner))
            signature <- rep(NA_character_, length(x))
            if (length(kept) > 0L) {
              # Each of the table's rows by its signed values, written as text but a
              # number-typed one compared by its number (D18.80): one dense rank over the
              # columns, read where they are, so no value can make two rows one (16.2).
              every_row <- length(kept) == length(owner)
              signed_values <- lapply(seq_len(nrow(parts)), function(k) {
                column <- input$data[[table]][[parts$column[[k]]]]
                values <- if (every_row) column else column[kept]
                if (parts$family[[k]] == "numeric" || is.character(values) || is.factor(values)) {
                  return(as_compared(values, parts[k]))
                }
                # Any other column as D18.39 writes a row's values, as text: each distinct value
                # written once and matched back by its rank (16.2).
                rank <- frankv(values, ties.method = "dense", na.last = TRUE)
                distinct <- values[match(seq_len(max(c(0L, rank))), rank)]
                frankv(value_text(distinct), ties.method = "dense", na.last = TRUE)[rank]
              })
              ranked <- frankv(signed_values, ties.method = "dense", na.last = TRUE)
              mine <- if (every_row) owner else owner[kept]
              # Each owner's signature: its rows' ranks, sorted and joined (Appendix E K10).
              ordered <- order(mine, ranked, method = "radix")
              joined_rows <- split(ranked[ordered], mine[ordered])
              signature[as.integer(names(joined_rows))] <- vapply(
                joined_rows, paste, "",
                collapse = " "
              )
            }
            unit <- units_of(parent, length(x))
            star <- !is.na(signature) & unit == "*"
            compared <- which(!is.na(signature) & !star)
            fail <- integer()
            ids <- character()
            others <- character()
            if (length(compared) > 0L) {
              # Each compared owner's group, by its unit and signature: one dense rank.
              group <- frankv(list(unit[compared], signature[compared]), ties.method = "dense")
              size <- tabulate(group)
              twinned <- size[group] > 1L
              fail <- compared[twinned]
              at <- group[twinned]
              ids <- value_text(x[fail])
              # Each failing owner's twins in C order (radix), the first example_cap shown and
              # the rest counted (D18.81), built a place at a time over every row together.
              ordered <- order(at, ids, method = "radix")
              sorted_ids <- ids[ordered]
              place <- integer(length(fail))
              place[ordered] <- rowid(at[ordered])
              first <- match(at, at[ordered])
              twins <- size[at] - 1L
              shown <- pmin(twins, most)
              others <- character(length(fail))
              for (j in seq_len(max(c(0L, shown)))) {
                more <- which(shown >= j)
                # The j-th twin: the group's j-th member, or the next once past the row itself.
                twin <- sorted_ids[first[more] + j - 1L + (j >= place[more])]
                others[more] <- if (j == 1L) twin else paste0(others[more], "; ", twin)
              }
              others <- with_rest(others, twins - shown)
            }
            record_outcome(
              "design_duplicate_signature", parent, attribute, fail, ids, others,
              unchecked = which(star)
            )
          }
        )
      }
    })
  )
}
