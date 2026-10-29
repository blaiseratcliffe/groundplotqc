# Lineage decoding (plan 9.3, 9.6, 10.1, 10.2; D8.17, D19.10 to D19.17, D19.41 to D19.48): each
# record-level issue's src_*_id, read from the caller's tables at its row_number, split on the
# lineage spec's separator into the parts its contributor's entry expects and labelled with the
# provider's columns; its fingerprint then taken from the decoded key. The tables are read,
# never changed or copied whole (16.2); only the issues are set, by reference.

#' The attribute a table's records are decoded by: its first ID-marked attribute, in the
#' dictionary's order, that the lineage spec gives an id entry (R4); NA where none
#' @noRd
source_id_attribute <- function(spec, table) {
  a <- spec$attributes
  lineage <- spec$lineage_spec
  is_id <- lineage$spec_type %chin% "id"
  entries <- paste(lineage$table_name, lineage$attribute_name, sep = "\r")[is_id]
  hit <- which(
    a$table_name %chin% table & a$id_marked %in% TRUE &
      paste(a$table_name, a$attribute_name, sep = "\r") %chin% entries
  )
  if (length(hit) == 0L) NA_character_ else a$attribute_name[[hit[[1L]]]]
}

#' Each key_col value's A2 label (D18.6, D19.11)
#'
#' Where `key_col` is the last attribute of id_band_columns' key chain, a value is looked up in
#' that column of the labels sheet and its label is the same row's `labels_column`; otherwise
#' the value is its own label. NA for a value that is NA or that the sheet doesn't hold. The one
#' reading of the labels sheet: Layer 4's record_labels() calls it too (D19.49), reading a value
#' "*" or blank as given, so the decoder makes those NA before it calls this (D9.24, D19.69).
#' @noRd
contributor_labels <- function(spec, key_col, values) {
  bands <- spec$id_band_columns
  if (nrow(bands) == 0L) {
    return(values)
  }
  chain <- parse_chain(
    bands$key_attribute[[1L]], bands$table_name[[1L]], spec$keys, spec$attributes
  )
  if (is.null(chain) || !identical(chain$attribute, key_col)) {
    return(values)
  }
  lists <- spec$code_lists
  # Worked out before the brackets, so no name inside them is taken for a column.
  in_sheet <- lists$sheet == bands$labels_sheet[[1L]] & lists$source_row > 1L
  sheet <- lists[in_sheet]
  is_key <- sheet$sheet_column == key_col
  is_label <- sheet$sheet_column == bands$labels_column[[1L]]
  keyed <- sheet[is_key]
  labelled <- sheet[is_label]
  label <- labelled$value[match(keyed$source_row, labelled$source_row)]
  label[match(values, keyed$value, incomparables = NA)]
}

#' The lineage spec's id rows for one contributor label, table and attribute, conditioned rows
#' included, in the component's order
#' @noRd
lineage_entry <- function(spec, label, table, attribute) {
  lineage <- spec$lineage_spec
  hit <- lineage$contributor_label %chin% label & lineage$table_name %chin% table &
    lineage$attribute_name %chin% attribute & lineage$spec_type %chin% "id"
  lineage[hit]
}

#' An entry's parts in order, a nested ID expanded into its own (D8.17; R6)
#'
#' A derived part naming an attribute that has an id entry for the same contributor is that
#' entry's parts, recursively; its conditions are read on the record's parent in the nested
#' entry's table. Each part is `list(table, rows)`: the table its conditions read, and its
#' rows of the lineage spec, the conditioned ones first in file order, then A2's own (D19.12).
#' NULL where a nested ID can't expand: an entry in more than one table, one without parts
#' (blank, no_id, not_applicable or unparseable), one with labelled alternatives (D19.48), or
#' one nested in itself; the ID is then raw (D19.16).
#' @noRd
entry_slots <- function(spec, label, entry, table, seen = character()) {
  lineage <- spec$lineage_spec
  slots <- list()
  for (k in sort(unique(entry$part_order))) {
    # Worked out before the brackets, so no name inside them is taken for a column.
    is_part <- entry$part_order == k
    rows <- entry[is_part]
    is_own <- is.na(rows$when)
    own <- rows[is_own]
    nests <- own$part_kind[[1L]] %chin% "derived" & lineage$contributor_label %chin% label &
      lineage$attribute_name %chin% own$part_source[[1L]] & lineage$spec_type %chin% "id"
    nested <- lineage[nests]
    if (nrow(nested) == 0L) {
      slots[[length(slots) + 1L]] <- list(table = table, rows = rows)
      next
    }
    expandable <- !own$part_source[[1L]] %chin% seen && uniqueN(nested$table_name) == 1L &&
      all(nested$id_status %chin% c("single", "composite")) && all(is.na(nested$alternative))
    if (!expandable) {
      return(NULL)
    }
    inner <- entry_slots(
      spec, label, nested, nested$table_name[[1L]], c(seen, own$part_source[[1L]])
    )
    if (is.null(inner)) {
      return(NULL)
    }
    slots <- c(slots, inner)
  }
  slots
}

#' Each ID split on the separator into its parts, an empty part kept (D8.17, D19.13)
#'
#' The separator is fixed text, not a pattern. A trailing separator is read as an empty last
#' part, so "4021_1_" has three parts.
#' @noRd
split_src_id <- function(text, separator) {
  strsplit(paste0(text, separator), separator, fixed = TRUE)
}

#' Each record's label for each part: A2's own part, or the first conditioned row whose
#' condition holds for the record (D19.12, D19.43; R3, R8)
#'
#' `slots` is entry_slots()'s result for `table`, `rows` the records' rows there. A part of a
#' nested ID reads its conditions on the record's parent in that ID's table, reached by the
#' routes of foreign keys (walk_fk_paths(), D20.51); a record no route reaches holds none. A
#' condition that doesn't parse holds for no record. Returns one character vector per part.
#' @noRd
label_parts <- function(input, table, rows, slots, flags) {
  spec <- input$spec
  lapply(slots, function(slot) {
    candidates <- slot$rows
    is_own <- is.na(candidates$when)
    label <- rep(candidates$part_source[is_own][[1L]], length(rows))
    conditioned <- which(!is_own)
    if (length(conditioned) == 0L) {
      return(label)
    }
    at <- rows
    if (!identical(slot$table, table)) {
      at <- walk_fk_paths(input, table, slot$table)$rows[rows]
    }
    open <- rep(TRUE, length(rows))
    for (i in conditioned) {
      condition <- parse_condition(
        candidates$when[[i]], slot$table, spec$keys, spec$attributes
      )
      if (is.null(condition)) {
        next
      }
      hold <- open & condition_rows(input, slot$table, condition, at, flags)
      label[hold] <- candidates$part_source[[i]]
      open <- open & !hold
    }
    label
  })
}

#' Some records of a table decoded: each one's provider key and its status (9.3; D19.13,
#' D19.15, D19.16, D19.47, D19.48, D20.35; R4 to R7)
#'
#' `rows` are rows of `table`, `labels` each record's A2 label (contributor_labels()). In order:
#' `fallback_pk`, no key, where the table has no ID attribute to decode (source_id_attribute())
#' or its column wasn't given, where the record's ID is NA, blank (D16.42) or a missing-value
#' code of its type, or where the label's entry has no ID (blank, no_id, not_applicable);
#' `no_lineage_spec`, the ID as delivered, where the record has no label, its label no entry,
#' or the entry is unparseable; `raw`, the ID as delivered, where the entry has labelled
#' alternatives (D19.48), a nested ID can't expand, there is no separator to split a composite
#' by, the ID doesn't split into the expected number of parts, or a part or its label holds
#' "; " or "=", which would keep the key from splitting back into its parts exactly (D20.35);
#' otherwise `decoded`, each part as `<label>=<value>` joined by "; ", a source part labelled
#' with its provider's `table.column` and a derived one with its bare attribute name (D19.47),
#' or `partial`, so written, where a part is blank or any missing-value code the spec has, of
#' any type (R5). A single ID isn't split (D19.13). Returns `list(provider_key, status)`.
#' @noRd
decode_records <- function(input, table, rows, labels, flags) {
  spec <- input$spec
  n <- length(rows)
  provider_key <- rep(NA_character_, n)
  status <- rep("fallback_pk", n)
  attribute <- source_id_attribute(spec, table)
  if (n == 0L || is.na(attribute)) {
    return(list(provider_key = provider_key, status = status))
  }
  columns <- input$columns
  at <- which(columns$table_name == table & columns$attribute_name == attribute)
  column <- if (length(at) == 1L) columns$column[[at]] else NA_character_
  if (is.na(column)) {
    return(list(provider_key = provider_key, status = status))
  }
  text <- value_text(input$data[[table]][[column]][rows])
  sentinels <- spec$sentinels
  codes <- sentinels$value[sentinels$data_type == columns$family[[at]]]
  usable <- !is.na(text) & !space_padding(text)$blank & !text %chin% codes
  any_code <- unique(sentinels$value)
  # Text holding what joins a key, "; " between its parts or "=" in each, looked for in the
  # bytes, as both are ASCII (D20.35).
  holds_joiner <- function(x) {
    grepl("; ", x, fixed = TRUE, useBytes = TRUE) | grepl("=", x, fixed = TRUE, useBytes = TRUE)
  }
  # A record with no label, or whose label the lineage spec gives no id entry for this
  # attribute, has no lineage spec: set in one step, so the loop below runs over the spec's
  # contributors only (16.2).
  lineage <- spec$lineage_spec
  has_entry <- lineage$spec_type %chin% "id" & lineage$table_name %chin% table &
    lineage$attribute_name %chin% attribute & !is.na(lineage$contributor_label)
  entry_labels <- unique(lineage$contributor_label[has_entry])
  no_entry <- usable & (is.na(labels) | !labels %chin% entry_labels)
  status[no_entry] <- "no_lineage_spec"
  for (label in unique(labels[usable & !no_entry])) {
    here <- which(usable & labels %chin% label)
    entry <- lineage_entry(spec, label, table, attribute)
    if (entry$id_status[[1L]] == "unparseable") {
      status[here] <- "no_lineage_spec"
      next
    }
    if (entry$id_status[[1L]] %chin% c("blank", "no_id", "not_applicable")) {
      next
    }
    slots <- if (all(is.na(entry$alternative))) entry_slots(spec, label, entry, table)
    if (is.null(slots)) {
      status[here] <- "raw"
      next
    }
    if (length(slots) == 1L) {
      parts <- list(text[here])
      fits <- rep(TRUE, length(here))
    } else {
      separator <- entry$separator[[1L]]
      pieces <- if (!is.na(separator)) split_src_id(text[here], separator)
      fits <- if (is.null(pieces)) rep(FALSE, length(here)) else lengths(pieces) == length(slots)
      # Each kept ID has exactly length(slots) pieces, so part k is every length(slots)-th
      # piece from the k-th: no pass over the issues' pieces for each part (16.2).
      flat <- as.character(unlist(pieces[fits], use.names = FALSE))
      parts <- lapply(seq_along(slots), function(k) {
        flat[seq.int(k, by = length(slots), length.out = sum(fits))]
      })
    }
    status[here[!fits]] <- "raw"
    ok <- here[fits]
    if (length(ok) == 0L) {
      next
    }
    part_labels <- label_parts(input, table, rows[ok], slots, flags)
    pairs <- lapply(seq_along(slots), function(k) paste0(part_labels[[k]], "=", parts[[k]]))
    provider_key[ok] <- do.call(paste, c(pairs, sep = "; "))
    missing_part <- Reduce(`|`, lapply(parts, function(part) {
      space_padding(part)$blank | part %chin% any_code
    }))
    # A part or a label holding "; " or "=" would split back from the key otherwise than it
    # was joined, so the record is shown as delivered, raw (D20.31 (a), D20.35).
    unsplittable <- Reduce(`|`, lapply(seq_along(slots), function(k) {
      holds_joiner(parts[[k]]) | holds_joiner(part_labels[[k]])
    }))
    status[ok] <- fifelse(unsplittable, "raw", fifelse(missing_part, "partial", "decoded"))
  }
  # A record shown as delivered keeps its ID's text as its key.
  delivered <- status %chin% c("no_lineage_spec", "raw")
  provider_key[delivered] <- text[delivered]
  list(provider_key = provider_key, status = status)
}

#' The caller's flags checked and joined to their tables by key (D19.42, D19.43; R2)
#'
#' `flags` is NULL or a list named by table, each name once, each element a data.frame holding
#' the table's primary-key columns, named as the dictionary names them, and at least one flag
#' column. An element's keys are written as the run writes record_pk (record_pk_text(): the
#' parts' text joined "|"), so a number-typed or integer64 key meets its own row. A key given
#' twice in one element, or one the table doesn't hold, is a caller's error. Returns, per
#' table, `at` (each of the table's rows' row in the element, NA where none: a record whose key
#' is missing gets no flag, and every holder of a repeated key gets that key's flags) and
#' `values` (each flag column as text, value_text()).
#' @noRd
flag_values <- function(flags, input) {
  if (is.null(flags)) {
    return(list())
  }
  named <- is.list(flags) && !is.data.frame(flags) && length(flags) > 0L &&
    !is.null(names(flags)) && !anyNA(names(flags)) && all(nzchar(names(flags))) &&
    !anyDuplicated(names(flags)) && all(vapply(flags, is.data.frame, logical(1)))
  if (!named) {
    # W171
    stop("`flags` must be NULL or a list of data.frames, each named once by its table.",
      call. = FALSE
    )
  }
  keys <- input$spec$keys
  out <- list()
  for (table in names(flags)) {
    if (!table %chin% names(input$data)) {
      # W172
      stop(sprintf(
        "`flags` names table %s, which isn't a table of the specification given in `tables`.",
        table
      ), call. = FALSE)
    }
    # Worked out before the brackets, so no name inside them is taken for a column.
    is_pk <- keys$table_name == table & keys$key_type == "PK"
    pk <- keys[is_pk]
    pk_columns <- pk$attribute_name[order(pk$key_part)]
    element <- flags[[table]]
    flag_columns <- setdiff(names(element), pk_columns)
    if (!all(pk_columns %in% names(element)) || length(flag_columns) == 0L) {
      # W173
      stop(sprintf(
        "`flags`' table for %s must hold its primary key (%s) and at least one flag column.",
        table, paste(pk_columns, collapse = ", ")
      ), call. = FALSE)
    }
    parts <- lapply(pk_columns, function(column) value_text(element[[column]]))
    if (length(parts) > 1L) {
      parts <- lapply(parts, utf8_marked)
    }
    given <- do.call(paste, c(parts, sep = "|"))
    given[Reduce(`|`, lapply(parts, function(part) is.na(part) | is_blank(part)))] <- NA_character_
    twice <- given[duplicated(given)]
    if (length(twice) > 0L) {
      # W174 (R2)
      stop(sprintf(
        "`flags`' table for %s gives the key %s more than once.", table, twice[[1L]]
      ), call. = FALSE)
    }
    own <- record_pk_text(input, table, seq_len(nrow(input$data[[table]])))$record_pk
    unknown <- is.na(given) | !given %in% own
    if (any(unknown)) {
      # W175 (D19.42)
      stop(sprintf(
        "`flags`' table for %s gives the key %s, which no record of the table holds.",
        table, if (is.na(given[unknown][[1L]])) "(blank)" else given[unknown][[1L]]
      ), call. = FALSE)
    }
    out[[table]] <- list(
      at = match(own, given, incomparables = NA),
      values = lapply(as.list(element)[flag_columns], value_text)
    )
  }
  out
}

#' The issues checked against the tables: each record-level issue's row in its table, and its
#' PK there unless the PK is positional (D19.14)
#'
#' The PK is written as the run writes record_pk (record_pk_text()), so a number-typed or
#' integer64 key matches its own row. A caller's error names the first issue that doesn't
#' match, so another delivery, or a table sorted or filtered since the run, stops decoding.
#' @noRd
issues_match_tables <- function(issues, input) {
  record <- which(!is.na(issues$row_number))
  for (table in unique(issues$table_name[record])) {
    at <- record[issues$table_name[record] == table]
    n <- if (table %chin% names(input$data)) nrow(input$data[[table]]) else 0L
    row <- issues$row_number[at]
    outside <- at[row < 1L | row > n]
    if (length(outside) > 0L) {
      first <- outside[[1L]]
      # W178
      stop(sprintf(
        paste0(
          "Issue %d is on row %d of table %s, but `tables` gives that table %d rows: decode ",
          "with the tables the issues came from."
        ),
        issues$issue_id[[first]], issues$row_number[[first]], table, n
      ), call. = FALSE)
    }
    keyed <- at[!issues$pk_positional[at] %in% TRUE]
    pk <- record_pk_text(input, table, issues$row_number[keyed])$record_pk
    differ <- keyed[!(pk == issues$record_pk[keyed]) %in% TRUE]
    if (length(differ) > 0L) {
      first <- differ[[1L]]
      held <- pk[[match(first, keyed)]]
      # W179
      stop(sprintf(
        paste0(
          "Issue %d names record %s on row %d of table %s, but `tables` holds %s there: ",
          "decode with the tables the issues came from."
        ),
        issues$issue_id[[first]], issues$record_pk[[first]], issues$row_number[[first]],
        table, if (is.na(held)) "no key" else held
      ), call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Which issues' provider keys another record of their table holds too (9.6; D19.15, D19.76,
#' D20.107)
#'
#' Counted over the table's records of every contributor in `tables`, found by key: only the
#' records that could hold an issue's key are decoded again. They are the records whose ID an
#' issue's record shares, found by duplicated() on the ID column (16.2), and, where an ID of a
#' column held as text holds "=", the records whose ID is spelled as an issue's decoded key or
#' is the values of an issue's delivered key joined by a separator of the table's entries,
#' found by %chin% (D20.107). Each is labelled as the run labelled it: an issue's record by its
#' issue's key_col value, any other record by its key_col value in `tables`. Where `tables`
#' reach key_col for no record of the table (a table or column on the way not given), they
#' can't label a record with no issue: it counts as holding the key of each issue that shares
#' its ID or found it by key, so those keep the pk: form. Returns one logical per issue.
#' @noRd
shared_provider_keys <- function(issues, input, key_col, flags) {
  shared <- logical(nrow(issues))
  keyed <- which(!is.na(issues$provider_key) & !is.na(issues$row_number))
  record <- which(!is.na(issues$row_number))
  spec <- input$spec
  columns <- input$columns
  lineage <- spec$lineage_spec
  for (table in unique(issues$table_name[keyed])) {
    at <- keyed[issues$table_name[keyed] == table]
    attribute <- source_id_attribute(spec, table)
    column <- columns$column[columns$table_name == table & columns$attribute_name %chin% attribute]
    x <- input$data[[table]][[column]]
    # A column stored as integer64 is read as a table made by reference over it: its own
    # duplicated() ignores fromLast, so the first holder of a repeated ID would be missed
    # (D18.10, as record_pk_text() reads a PK). data.table reads an integer64 exactly at any
    # setNumericRounding(), so the reset record_pk_text() makes is left out on purpose (R59).
    ids <- if (inherits(x, "integer64")) setDT(list(x)) else x
    repeats <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
    # A record with another ID can hold an issue's key too (9.6, D20.107): its ID spelled as
    # the issue's decoded key, so raw or no_lineage_spec, or the values of the issue's
    # delivered key joined by a separator of the table's entries, as a decoded key's ID is.
    # Either way an ID holds "=" (a key's, or the delivered key's own), so a column not held
    # as text, or with no ID holding "=", is passed by after one grepl() (16.2).
    spelled <- character()
    spelled_by <- integer()
    found <- integer()
    by_key <- integer()
    text <- stored_text(x)
    if (!is.null(text) && any(grepl("=", text, fixed = TRUE, useBytes = TRUE))) {
      key <- issues$provider_key[at]
      decoded <- issues$provider_key_status[at] %chin% c("decoded", "partial")
      delivered <- which(!decoded & grepl("=", key, fixed = TRUE, useBytes = TRUE))
      # A latin1 key is read as UTF-8; one that still isn't valid UTF-8 no pattern can read.
      readable <- utf8_marked(utf8_converted(key[delivered]))
      valid <- validUTF8(readable)
      delivered <- delivered[valid]
      # The values: the first "<label>=" dropped, and each "; <label>=" after it made the
      # separator, taken as fixed text.
      values <- sub("^[^=]*=", "", readable[valid])
      is_entry <- lineage$spec_type %chin% "id" & lineage$table_name %chin% table &
        lineage$attribute_name %chin% attribute
      separators <- unique(lineage$separator[is_entry])
      separators <- separators[!is.na(separators)]
      joined <- lapply(separators, function(separator) {
        gsub("; [^=]*=", gsub("\\", "\\\\", separator, fixed = TRUE), values)
      })
      # With no separator, only a one-part entry decodes: its ID is the value alone.
      if (length(joined) == 0L) {
        joined <- list(values)
      }
      spelled <- c(key[decoded], unlist(joined, use.names = FALSE))
      spelled_by <- c(which(decoded), rep(delivered, length(joined)))
      found <- which(text %chin% spelled)
      by_key <- spelled_by[spelled %chin% text[found]]
    }
    candidate <- at[repeats[issues$row_number[at]] | seq_along(at) %in% by_key]
    if (length(candidate) == 0L) {
      next
    }
    # Matched on the repeated rows only, so a classed column isn't written as text whole.
    repeated <- which(repeats)
    holders <- repeated[x[repeated] %in% x[issues$row_number[candidate]]]
    # The issues' own records too, as one found by key may share no ID.
    holders <- sort(unique(c(holders, found, issues$row_number[candidate])))
    # A holder that is an issue's record takes that issue's key_col value, as the run gave it
    # (D19.76); the table's other holders are read from `tables`.
    on_table <- record[issues$table_name[record] == table]
    own <- on_table[match(holders, issues$row_number[on_table])]
    key_values <- value_text(issues[[key_col]][own])
    other <- which(is.na(own))
    unlabelled <- logical(length(holders))
    if (length(other) > 0L) {
      reached <- reach_column(input, table, key_col)
      ends <- unique(reached$home[!is.na(reached$home)])
      home <- columns$column[columns$table_name %chin% ends & columns$attribute_name == key_col]
      if (all(is.na(home)) || all(is.na(reached$rows))) {
        unlabelled[other] <- TRUE
      } else {
        other_rows <- holders[other]
        key_values[other] <- value_text(reached_values(
          input, list(home = reached$home[other_rows], rows = reached$rows[other_rows]), key_col
        ))
      }
    }
    # A key "*" or blank is no key here, as in gpq_decode_lineage() (D9.24, D19.69).
    no_key <- is.na(key_values) | key_values %chin% "*" | space_padding(key_values)$blank
    key_values[no_key] <- NA_character_
    labelled <- !unlabelled
    held <- decode_records(
      input, table, holders[labelled], contributor_labels(spec, key_col, key_values[labelled]),
      flags
    )$provider_key
    held <- held[!is.na(held)]
    twice <- unique(held[duplicated(held)])
    # A holder `tables` can't label holds, for all decoding can tell, the key of each issue
    # sharing its ID or finding it by key (D19.76, D20.107).
    rows <- issues$row_number[candidate]
    loose <- holders[unlabelled]
    by_loose <- if (length(spelled) > 0L) at[spelled_by[spelled %chin% text[loose]]]
    shared[candidate] <- issues$provider_key[candidate] %in% twice |
      x[rows] %in% x[loose] | candidate %in% by_loose
  }
  shared
}

#' Decode the provider's keys of a run's issues
#'
#' @description
#' Shows each record-level issue in the provider's own terms: the record's source ID
#' (`src_*_id`), read from `tables` at the issue's row, is split into the parts its
#' contributor's lineage spec expects, and each part is labelled with the provider's table
#' and column. The issues' `provider_key`, `provider_key_status` and `fingerprint` are set by
#' reference.
#'
#' @details
#' A record's contributor comes from the issues' `key_col` value: where `key_col` is the last
#' attribute of the site-ID bands' key chain, the label of that value in the bands' labels
#' sheet, else the value itself. The ID decoded is the record's table's first ID attribute
#' that the lineage spec gives an entry.
#'
#' `provider_key_status`:
#' - `decoded`: the ID split into exactly the parts the entry expects, each written as
#'   `table.column=value`, or for a part the pipeline derives, its attribute name and value,
#'   joined by "; ". An ID of one part isn't split.
#' - `partial`: as `decoded`, but a part is blank or a missing-value code.
#' - `raw`: the ID as delivered: it doesn't split into the expected parts (a value holding the
#'   separator), its entry has labelled alternatives, there is no separator, or one of its
#'   parts or labels holds "; " or "=". The last case keeps every `decoded` or `partial` key
#'   splitting back into its parts exactly.
#' - `no_lineage_spec`: the ID as delivered: the record's contributor has no entry for it.
#' - `fallback_pk`: no key: a column-level issue, a table with no ID to decode, an ID that is
#'   missing, blank or a missing-value code, or an entry that says the contributor has no ID.
#'
#' A part's label can depend on the record: the lineage spec's `when` conditions are tried in
#' order before the entry's own part. A condition reads a column of the record, a parent's
#' column through a chain of foreign keys joined by " > ", or a flag given in `flags` as
#' `flag.<name>`.
#'
#' The fingerprint becomes `rule_id|table_name|column_name|<provider_key>` where a key is
#' given; where another record of the table holds the same key, and where there is no key, it
#' stays the `pk:` form.
#'
#' @param issues The `issues` table of a run's results, from [gpq_check_conformance()]: a
#'   data.table, changed by reference.
#' @param tables The tables the issues came from, as [gpq_check_conformance()] takes them.
#'   Decoding stops if an issue's row isn't in its table or holds another primary key there.
#' @param spec The specification the run used, from [gpq_read_spec()] or [magp_spec()].
#' @param key_col The issues column holding each record's contributor, such as a group column
#'   of the run.
#' @param table_map `NULL`, or, when `tables` is a DBI connection, the database's names for
#'   the specification's tables, as [gpq_check_conformance()] takes it.
#' @param flags `NULL`, or a list named by table of data.frames, each holding the table's
#'   primary-key columns and one or more flag columns, which conditions read as
#'   `flag.<name>`. A record whose key the data.frame doesn't give has no flag there.
#' @return `issues`, invisibly, with `provider_key`, `provider_key_status` and `fingerprint`
#'   set.
#' @seealso [gpq_check_conformance()] for the issues, and [gpq_classify_issues()], which
#'   classes them.
#' @examples
#' # One table of plots whose source ID is the provider's site and plot joined by "_".
#' dictionary <- data.frame(
#'   table_name = "plots",
#'   attribute_name = c("plot_id", "contributor", "src_plot_id", "slope"),
#'   key_type = c("PK", ".", ".", "."),
#'   data_type = c("character", "character", "character", "numeric"),
#'   min_value = c(NA, NA, NA, "0")
#' )
#' spec <- gpq_read_spec(
#'   dictionary,
#'   lineage_spec = data.frame(
#'     contributor_label = "north", table_name = "plots", attribute_name = "src_plot_id",
#'     spec_type = "id", source_text = "survey.site + survey.plot", note = NA,
#'     source_cell = NA
#'   ),
#'   lineage_separator = "_",
#'   id_pattern = "^src_.*_id$",
#'   sentinels = gpq_sentinels(numeric = c(missing = -1), character = c(missing = "X"))
#' )
#' plots <- data.frame(
#'   plot_id = c("p1", "p2", "p3"), contributor = "north",
#'   src_plot_id = c("41_2", "41_X", "41_2_b"), slope = c(-5, -6, -7)
#' )
#' results <- gpq_check_conformance(
#'   list(plots = plots), spec,
#'   group_cols = "contributor", layers = 3
#' )
#' gpq_decode_lineage(results$issues, list(plots = plots), spec, key_col = "contributor")
#' results$issues[, c("record_pk", "provider_key", "provider_key_status")]
#' @export
gpq_decode_lineage <- function(issues, tables, spec, key_col, table_map = NULL, flags = NULL) {
  if (!inherits(spec, "gpq_spec")) {
    stop("`spec` must be a specification from gpq_read_spec().", call. = FALSE)
  }
  validate_gpq_spec(spec)
  needed <- c(
    "issue_id", "rule_id", "table_name", "column_name", "record_pk", "row_number",
    "pk_positional", "provider_key", "provider_key_status", "fingerprint", "fingerprint_pk"
  )
  if (!is.data.table(issues) || !all(needed %in% names(issues))) {
    # W176
    stop(
      "`issues` must be the issues table of a run's results, a data.table, as ",
      "gpq_check_conformance() returns it.",
      call. = FALSE
    )
  }
  key_ok <- is.character(key_col) && length(key_col) == 1L && !is.na(key_col) &&
    key_col %in% names(issues) && key_col %in% spec$attributes$attribute_name
  if (!key_ok) {
    # W177
    stop(
      "`key_col` must be one column of the issues that is an attribute of the specification.",
      call. = FALSE
    )
  }
  read <- read_tables(tables, spec, table_map)
  tables <- read$tables
  validate_tables(tables)
  load_bit64(tables)
  input <- conformance_input(tables, spec, text_tables = read$text_tables)
  issues_match_tables(issues, input)
  given_flags <- flag_values(flags, input)
  # A key_col value "*" or blank is a record that reached no key (D9.24): no label. Layer 4
  # reads such a value as given, so it is made NA here, not in contributor_labels() (D19.69).
  key_values <- issues[[key_col]]
  no_key <- is.na(key_values) | key_values %chin% "*" | space_padding(key_values)$blank
  key_values[no_key] <- NA_character_
  labels <- contributor_labels(spec, key_col, key_values)
  provider_key <- rep(NA_character_, nrow(issues))
  status <- rep("fallback_pk", nrow(issues))
  record <- which(!is.na(issues$row_number))
  for (table in unique(issues$table_name[record])) {
    at <- record[issues$table_name[record] == table]
    decoded <- decode_records(input, table, issues$row_number[at], labels[at], given_flags)
    provider_key[at] <- decoded$provider_key
    status[at] <- decoded$status
  }
  # The provider's text is held as the run holds the caller's other text: valid UTF-8 left
  # unmarked is marked UTF-8, never converted (D16.47, I1).
  set(issues, j = "provider_key", value = utf8_marked(provider_key))
  set(issues, j = "provider_key_status", value = status)
  shared <- shared_provider_keys(issues, input, key_col, given_flags)
  fingerprints <- issue_fingerprint(issues)
  fingerprint <- fingerprints$fingerprint
  fingerprint[shared] <- fingerprints$fingerprint_pk[shared]
  set(issues, j = "fingerprint", value = fingerprint)
  invisible(issues)
}
