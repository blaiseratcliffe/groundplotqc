# The lineage spec (plan 3.6, 10.1; D7.8, D9.14, D12.14, D12.25, D12.31). A2's id rows
# are parsed in its legend's notation; anything that doesn't split cleanly is rejected,
# never guessed. Compiled rows document provenance and aren't parsed.

lineage_part_pattern <- "^[A-Za-z_][A-Za-z0-9_]*([.][A-Za-z_][A-Za-z0-9_]*)?$"

#' One parsed lineage entry: its alternative, parts and id_status
#' @noRd
lineage_row <- function(id_status, alternative = NA_character_, parts = NA_character_) {
  kind <- fifelse(grepl(".", parts, fixed = TRUE), "source", "derived")
  data.table(
    alternative = alternative,
    part_order = if (all(is.na(parts))) NA_integer_ else seq_along(parts),
    part_source = parts,
    part_kind = fifelse(is.na(parts), NA_character_, kind),
    id_status = id_status
  )
}

#' The notation's no-ID and not-applicable tokens, from the character sentinels
#'
#' MAGPlot's X and Z, never literals here (D12.31); without character sentinels there
#' are none.
#' @noRd
lineage_tokens <- function(sentinels) {
  text <- sentinels[sentinels$data_type == "character", ]
  c(
    no_id = text$value[match("missing", text$role)],
    not_applicable = text$value[match("not_applicable", text$role)]
  )
}

#' One lineage entry parsed in A2's legend notation (D12.14)
#'
#' " | " separates labelled alternatives and " + " joins parts, binding tighter; anything
#' that doesn't split cleanly is one unparseable row, never a guess.
#' @noRd
parse_lineage_notation <- function(source_text, tokens = NULL) {
  if (is.na(source_text)) {
    return(lineage_row("blank"))
  }
  text <- trimws(source_text)
  no_id <- if (is.null(tokens)) NA_character_ else tokens[["no_id"]]
  not_applicable <- if (is.null(tokens)) NA_character_ else tokens[["not_applicable"]]
  stand_alone <- c(no_id, not_applicable)
  stand_alone <- stand_alone[!is.na(stand_alone)]
  if (identical(text, no_id)) {
    return(lineage_row("no_id"))
  }
  if (identical(text, not_applicable)) {
    return(lineage_row("not_applicable"))
  }
  fail <- lineage_row("unparseable")
  alternatives <- strsplit(text, " | ", fixed = TRUE)[[1L]]
  n_bars <- lengths(regmatches(text, gregexpr(" | ", text, fixed = TRUE)))
  stray_bar <- grepl("|", gsub(" | ", "", text, fixed = TRUE), fixed = TRUE)
  if (length(alternatives) != n_bars + 1L || stray_bar) {
    return(fail)
  }
  labelled <- grepl("^[^:+]+:", alternatives)
  if (length(alternatives) > 1L && !all(labelled)) {
    return(fail)
  }
  rows <- lapply(seq_along(alternatives), function(k) {
    body <- alternatives[[k]]
    label <- NA_character_
    if (labelled[[k]]) {
      label <- trimws(sub(":.*$", "", body))
      body <- sub("^[^:]*:", "", body)
    }
    body <- trimws(body)
    parts <- strsplit(body, " + ", fixed = TRUE)[[1L]]
    n_plus <- lengths(regmatches(body, gregexpr(" + ", body, fixed = TRUE)))
    split_badly <- !nzchar(body) || length(parts) != n_plus + 1L ||
      any(parts %in% stand_alone) || !all(grepl(lineage_part_pattern, parts))
    if (split_badly) {
      return(NULL)
    }
    lineage_row(if (length(parts) == 1L) "single" else "composite", label, parts)
  })
  if (any(vapply(rows, is.null, logical(1)))) {
    return(fail)
  }
  rbindlist(rows)
}

#' The known cells of one side of a clash, ", "-joined in reading order (D12.33)
#'
#' So they can't be taken for the "; " between a clash's two sides; NA when none is known.
#' @noRd
join_cells <- function(x) {
  known <- x[!is.na(x)]
  if (length(known) == 0L) NA_character_ else paste(known, collapse = ", ")
}

#' The lineage_spec component, its table-placement clashes and its findings
#'
#' Rows whose table and attribute aren't in the dictionary, but whose attribute is in
#' exactly one dictionary table, take that table, the dictionary winning (D1.15b).
#' @noRd
build_lineage_spec <- function(lineage, attributes, file, tokens = NULL,
                               dictionary_file = NA_character_) {
  out <- list(
    component = empty_table(spec_schema()$lineage_spec),
    clashes = empty_table(spec_schema()$clashes),
    findings = empty_table(spec_schema()$read_findings)
  )
  if (is.null(lineage) || nrow(lineage) == 0L) {
    return(out)
  }
  data <- copy(lineage)
  in_dictionary <- paste(data$table_name, data$attribute_name) %chin%
    paste(attributes$table_name, attributes$attribute_name)
  moved <- rep(FALSE, nrow(data))
  # An empty dictionary gives no table to place a row in (D12.26).
  if (nrow(attributes) > 0L) {
    # A home is a table, so an attribute on two rows of one table has one.
    homes <- attributes[, list(n_home = uniqueN(table_name), home = table_name[[1L]]),
      by = attribute_name
    ]
    data[homes, on = "attribute_name", `:=`(n_home = i.n_home, home = i.home)]
    moved <- !in_dictionary & !is.na(data$n_home) & data$n_home == 1L
  }
  if (any(moved)) {
    # One clash per attribute and table named, citing every lineage row behind it and the
    # attribute's row in the dictionary (D12.33).
    placed <- data[moved, list(source_cell_a = join_cells(source_cell)),
      by = list(attribute_name, table_name, home)
    ]
    home_row <- attributes$source_row[match(
      paste(placed$home, placed$attribute_name),
      paste(attributes$table_name, attributes$attribute_name)
    )]
    out$clashes <- data.table(
      rule_id = "spec_clash_resolved", kind = "table_placement", key_col = "attribute_name",
      key_value = placed$attribute_name, column_name = "table_name",
      source_a = "lineage_spec", value_a = placed$table_name,
      source_b = "dictionary", value_b = placed$home,
      winner = "dictionary", basis = "fixed_dictionary_placement",
      source_cell_a = placed$source_cell_a,
      source_cell_b = fifelse(
        is.na(dictionary_file) | is.na(home_row), NA_character_,
        paste0(dictionary_file, ":", home_row)
      )
    )
    data[moved, table_name := home]
  }
  rows <- lapply(seq_len(nrow(data)), function(i) {
    one <- data[i]
    parsed <- switch(one$spec_type,
      id = parse_lineage_notation(one$source_text, tokens),
      compiled = lineage_row(if (is.na(one$source_text)) "blank" else "documented"),
      lineage_row("unparseable")
    )
    if (is.null(parsed)) {
      parsed <- lineage_row("unparseable")
    }
    data.table(
      contributor_label = one$contributor_label, table_name = one$table_name,
      attribute_name = one$attribute_name, spec_type = one$spec_type, parsed,
      source_text = one$source_text, note = one$note, source_cell = one$source_cell
    )
  })
  component <- bind_component("lineage_spec", rows)
  bad <- unique(component[id_status == "unparseable"])
  if (nrow(bad) > 0L) {
    type_ok <- bad$spec_type %chin% c("id", "compiled")
    detail <- fifelse(
      type_ok,
      report_text(
        "preflight_detail_lineage_spec_unparseable_text",
        contributor_label = bad$contributor_label, table_name = bad$table_name,
        attribute_name = bad$attribute_name, source_text = bad$source_text
      ),
      report_text(
        "preflight_detail_lineage_spec_unparseable_type",
        contributor_label = bad$contributor_label, table_name = bad$table_name,
        attribute_name = bad$attribute_name, spec_type = bad$spec_type
      )
    )
    out$findings <- data.table(
      rule_id = "lineage_spec_unparseable", input = "lineage_spec", file = file,
      detail = detail, source_cell = bad$source_cell
    )
  }
  out$component <- component
  out
}
