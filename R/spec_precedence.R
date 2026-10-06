# Clashes between the datasets table and a code-list sheet (plan 2.2, 3.6; D1.4,
# D12.13, D12.14, D12.20, D12.24). Two cells clash when both hold values that differ, or
# when the winning side is blank against a value, its blank_rule naming the winner; a
# blank on the losing side never clashes. Each clash gives both sides' cells, the
# datasets table's from its `where` and the sheet's from its long cells (D12.33). Table
# placement clashes come from the lineage spec (R/spec_lineage.R).

#' Clashes between the datasets table and code-list sheets, settled by precedence
#'
#' Also the datasets_row_missing findings, and spec_clash_unresolved for a precedence
#' sheet or key that doesn't exist (D12.24). `datasets` is NULL when not given; a given
#' table with no rows is still checked (R14). `files$code_lists` is the code-list file of
#' each precedence sheet, named by sheet, so each finding names its sheet's file (D12.28).
#' @noRd
resolve_clashes <- function(datasets, code_lists, precedence, files,
                            datasets_where = list(kind = "memory")) {
  out <- list(
    clashes = empty_table(spec_schema()$clashes),
    findings = empty_table(spec_schema()$read_findings)
  )
  if (is.null(precedence) || is.null(datasets)) {
    return(out)
  }
  clashes <- list()
  findings <- list()
  for (group in split(precedence, by = c("sheet", "key_col"))) {
    sheet_name <- group$sheet[[1L]]
    key <- group$key_col[[1L]]
    list_file <- unname(files$code_lists[match(sheet_name, names(files$code_lists))])
    headers <- code_lists[sheet == sheet_name & source_row == 1L, value]
    if (!key %in% headers || !key %in% names(datasets)) {
      findings[[length(findings) + 1L]] <- data.table(
        rule_id = "spec_clash_unresolved", input = "precedence", file = list_file,
        detail = report_text(
          "preflight_detail_spec_clash_unresolved_setup",
          sheet = sheet_name, key_col = key
        ),
        source_cell = NA_character_
      )
      next
    }
    shared <- intersect(headers[!is.na(headers)], names(datasets))
    body <- code_lists[sheet == sheet_name & source_row > 1L & sheet_column %chin% shared]
    # A sheet with a header and no rows has nothing to compare or to miss (D12.26).
    if (nrow(body) == 0L) {
      next
    }
    # The rows' own numbers go under names no input can share (R15).
    set(body, j = ".gpq_source_row", value = body$source_row)
    sheet_wide <- dcast(
      body, .gpq_source_row ~ sheet_column,
      value.var = "value", fun.aggregate = function(x) x[[1L]], fill = NA_character_
    )
    key_cells <- body[sheet_column == key]
    missing <- key_cells[!is.na(value) & !value %chin% datasets[[key]]]
    if (nrow(missing) > 0L) {
      findings[[length(findings) + 1L]] <- data.table(
        rule_id = "datasets_row_missing", input = "datasets", file = list_file,
        detail = report_text(
          "preflight_detail_datasets_row_missing",
          key_col = key, key_value = missing$value, sheet = sheet_name
        ),
        source_cell = missing$source_cell
      )
    }
    # Each side keeps its row, so a clash can cite both cells (D12.33). A blank key is no
    # key, so its rows match nothing; a key repeated on both sides gives one clash per pair
    # of rows (R13).
    left <- datasets[, shared, with = FALSE]
    set(left, j = ".gpq_datasets_row", value = seq_len(nrow(left)))
    # Each index is made outside `[`, where a column named key, left or sheet_wide
    # would hide the variable of that name.
    keep <- !is.na(left[[key]])
    left <- left[keep]
    keep <- !is.na(sheet_wide[[key]])
    sheet_wide <- sheet_wide[keep]
    joined <- merge(
      left, sheet_wide[, c(".gpq_source_row", shared), with = FALSE],
      by = key, suffixes = c(".a", ".b"), allow.cartesian = TRUE
    )
    for (column in setdiff(shared, key)) {
      a <- joined[[paste0(column, ".a")]]
      b <- joined[[paste0(column, ".b")]]
      differ <- !is.na(a) & !is.na(b) & a != b
      rule <- group[attribute_name == column]
      if (nrow(rule) == 0L) {
        hit <- which(differ)
        winner <- rep(NA_character_, length(hit))
        rule_id <- "spec_clash_unresolved"
        basis <- NA_character_
      } else {
        a_wins <- rule$winner[[1L]] == "datasets"
        winning <- if (a_wins) a else b
        losing <- if (a_wins) b else a
        blank_win <- is.na(winning) & !is.na(losing)
        hit <- which(differ | blank_win)
        other <- if (a_wins) "code_lists" else "datasets"
        yields <- blank_win[hit] & rule$blank_rule[[1L]] == "yields"
        winner <- fifelse(yields, other, rule$winner[[1L]])
        rule_id <- "spec_clash_resolved"
        basis <- "precedence"
      }
      if (length(hit) == 0L) {
        next
      }
      sheet_column_cells <- body[sheet_column == column]
      clashes[[length(clashes) + 1L]] <- data.table(
        rule_id = rule_id, kind = "value", key_col = key, key_value = joined[[key]][hit],
        column_name = column, source_a = "datasets", value_a = a[hit],
        source_b = "code_lists", value_b = b[hit], winner = winner, basis = basis,
        source_cell_a = cell_references(
          datasets_where, joined$.gpq_datasets_row[hit] + 1L, match(column, names(datasets))
        ),
        source_cell_b = sheet_column_cells$source_cell[
          match(joined$.gpq_source_row[hit], sheet_column_cells$source_row)
        ]
      )
    }
  }
  list(
    clashes = bind_component("clashes", clashes),
    findings = bind_component("read_findings", findings)
  )
}
