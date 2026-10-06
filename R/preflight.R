# Pre-flight checks of a specification (plan 4.2, 9.5; D2.11, D8.9, D8.16, D12.15 to
# D12.26). One function per check, held in preflight_check_functions(), returns its
# findings, or the reason it couldn't run; preflight_checks() makes preflight.csv's rows
# from them, in 4.2's order.

#' The M2 pre-flight checks and their outcome on failure, in 4.2's order
#' @noRd
preflight_rules <- function() {
  data.table(
    rule_id = c(
      "dd_duplicate_attribute", "dd_type_unknown", "dd_type_column_ambiguous",
      "dd_pk_missing", "dd_fk_target_missing", "code_list_missing", "code_column_missing",
      "code_list_duplicate_code", "code_list_blank_row", "code_list_unreferenced",
      "code_list_empty_column", "spec_clash_resolved", "spec_clash_unresolved",
      "datasets_row_missing", "spec_encoding_invalid", "spec_csv_malformed",
      "site_id_range_invalid", "lineage_spec_unparseable", "lineage_name_unknown",
      "lineage_spec_row_unflagged", "lineage_id_unflagged", "crosswalk_unreadable"
    ),
    on_failure = c(
      "stop", "stop", "stop", "stop", "stop", "stop", "stop", "warn", "warn", "stop",
      "warn", "warn", "stop", "warn", "stop", "stop", "stop", "stop", "warn", "warn", "warn",
      "stop"
    )
  )
}

#' preflight.csv's columns and their classes
#' @noRd
preflight_columns <- function() {
  c(
    rule_id = "character", outcome = "character", file = "character", detail = "character",
    n_findings = "integer", not_run_reason = "character", source_cell = "character"
  )
}

#' The pre-flight table: a pass row, a not_run row, or one row per finding, per check
#' @noRd
preflight_checks <- function(spec) {
  rules <- preflight_rules()
  checks <- preflight_check_functions()
  rows <- lapply(seq_len(nrow(rules)), function(i) {
    rule <- rules$rule_id[[i]]
    result <- checks[[rule]](spec)
    if (is.character(result)) {
      return(data.table(
        rule_id = rule, outcome = "not_run", file = NA_character_, detail = NA_character_,
        n_findings = NA_integer_, not_run_reason = result, source_cell = NA_character_
      ))
    }
    if (nrow(result) == 0L) {
      return(data.table(
        rule_id = rule, outcome = "pass", file = NA_character_, detail = NA_character_,
        n_findings = 0L, not_run_reason = NA_character_, source_cell = NA_character_
      ))
    }
    data.table(
      rule_id = rule, outcome = rules$on_failure[[i]], file = result$file,
      detail = result$detail, n_findings = nrow(result), not_run_reason = NA_character_,
      source_cell = result$source_cell
    )
  })
  rbindlist(rows)
}

#' A check's empty findings table
#' @noRd
no_findings <- function() {
  data.table(file = character(), detail = character(), source_cell = character())
}

#' A check's findings table, empty when there's no detail
#' @noRd
findings_of <- function(file, detail, source_cell = NA_character_) {
  if (length(detail) == 0L) {
    return(no_findings())
  }
  data.table(
    file = as.character(file), detail = as.character(detail),
    source_cell = as.character(source_cell)
  )
}

#' The reader's findings for one rule, as a check's findings
#' @noRd
read_findings_of <- function(spec, rule) {
  found <- spec$read_findings[rule_id == rule]
  findings_of(found$file, found$detail, found$source_cell)
}

#' Whether an input was given, from the manifest (D12.19)
#' @noRd
given <- function(spec, input) {
  inputs <- spec$manifest$input
  any(inputs == input | startsWith(inputs, paste0(input, ":")))
}

#' The file an input came from, from the manifest
#' @noRd
input_file <- function(spec, input) {
  spec$manifest$file[match(input, spec$manifest$input)]
}

#' Dictionary rows as <file>:<row> (D12.25)
#'
#' NA where the reader doesn't know the dictionary file's rows (D12.33).
#' @noRd
dictionary_cell <- function(spec, rows) {
  file <- input_file(spec, "dictionary")
  known <- !is.na(rows) & !is.na(file) & file != "in memory"
  fifelse(known, paste0(file, ":", rows), NA_character_)
}

#' A clash's two cells for preflight.csv, a then b (D12.33)
#'
#' "<a>; <b>", an unknown side left empty so the position tells the side, NA when
#' neither is known.
#' @noRd
clash_cells <- function(a, b) {
  fifelse(
    is.na(a) & is.na(b), NA_character_,
    paste0(fifelse(is.na(a), "", a), "; ", fifelse(is.na(b), "", b))
  )
}

#' The file each code-list sheet came from: its own CSV, else the workbook
#' @noRd
lists_file <- function(spec, sheets) {
  per_sheet <- input_file(spec, paste0("code_lists:", sheets))
  fifelse(is.na(per_sheet), input_file(spec, "code_lists"), per_sheet)
}

#' The sheet columns that resolved code lists use, non-code sheets left out
#' @noRd
sheet_columns_used <- function(spec) {
  map <- spec$code_list_map
  in_use <- map$source_type == "sheet" & map$status == "resolved" &
    !map$source_name %chin% spec$non_code_sheets$sheet
  unique(map[in_use, list(sheet = source_name, sheet_column = code_column)])
}

#' The dictionary row of each table and attribute
#' @noRd
attribute_rows <- function(spec, tables, attributes) {
  a <- spec$attributes
  a$source_row[match(paste(tables, attributes), paste(a$table_name, a$attribute_name))]
}

#' The 22 checks of 4.2 at M2, one function per rule ID, as a list named by it (D12.26)
#'
#' preflight_checks() takes each from this list, which works in an installed package,
#' where a lookup by name wouldn't search the namespace. Each check returns
#' findings_of(...), or the reason it couldn't run.
#' @noRd
preflight_check_functions <- function() {
  list(
    dd_duplicate_attribute = function(spec) {
      a <- spec$attributes
      dup <- a[, list(n = .N, row = source_row[min(2L, .N)]), by = list(table_name, attribute_name)]
      dup <- dup[n > 1L]
      findings_of(
        input_file(spec, "dictionary"),
        report_text(
          "preflight_detail_dd_duplicate_attribute",
          table_name = dup$table_name, attribute_name = dup$attribute_name, n = dup$n
        ),
        dictionary_cell(spec, dup$row)
      )
    },
    dd_type_unknown = function(spec) {
      a <- spec$attributes
      # A dictionary without a type column is dd_type_column_ambiguous's finding; a type
      # column that is all blank gives one finding per blank type (R9).
      no_column <- all(is.na(a$data_type)) &&
        "dd_type_column_ambiguous" %chin% spec$read_findings$rule_id
      if (no_column) {
        return(no_findings())
      }
      unknown <- a[!data_type %chin% spec$type_map$data_type]
      findings_of(
        input_file(spec, "dictionary"),
        report_text(
          "preflight_detail_dd_type_unknown",
          table_name = unknown$table_name, attribute_name = unknown$attribute_name,
          data_type = unknown$data_type
        ),
        dictionary_cell(spec, unknown$source_row)
      )
    },
    dd_type_column_ambiguous = function(spec) {
      read_findings_of(spec, "dd_type_column_ambiguous")
    },
    dd_pk_missing = function(spec) {
      a <- spec$attributes
      missing <- setdiff(unique(a$table_name), spec$keys[key_type == "PK", table_name])
      findings_of(
        input_file(spec, "dictionary"),
        report_text("preflight_detail_dd_pk_missing", table_name = missing),
        dictionary_cell(spec, a$source_row[match(missing, a$table_name)])
      )
    },
    dd_fk_target_missing = function(spec) {
      tables <- unique(spec$attributes$table_name)
      pk_tables <- unique(spec$keys[key_type == "PK", table_name])
      fk <- spec$keys[key_type == "FK"]
      no_table <- is.na(fk$reference_table) | !fk$reference_table %chin% tables
      no_pk <- !no_table & !fk$reference_table %chin% pk_tables
      hit <- fk[no_table | no_pk]
      values <- list(
        table_name = hit$table_name, attribute_name = hit$attribute_name,
        reference_table = hit$reference_table
      )
      detail <- fifelse(
        no_table[no_table | no_pk],
        do.call(report_text, c("preflight_detail_dd_fk_target_missing_table", values)),
        do.call(report_text, c("preflight_detail_dd_fk_target_missing_pk", values))
      )
      findings_of(
        input_file(spec, "dictionary"), detail,
        dictionary_cell(spec, attribute_rows(spec, hit$table_name, hit$attribute_name))
      )
    },
    code_list_missing = function(spec) {
      m <- spec$code_list_map[status == "no_source"]
      findings_of(
        input_file(spec, "dictionary"),
        report_text(
          "preflight_detail_code_list_missing",
          table_name = m$table_name, attribute_name = m$attribute_name, lookup = m$lookup
        ),
        dictionary_cell(spec, attribute_rows(spec, m$table_name, m$attribute_name))
      )
    },
    code_column_missing = function(spec) {
      m <- spec$code_list_map[status == "no_code_column"]
      file <- fifelse(
        m$source_type == "sheet",
        lists_file(spec, m$source_name),
        input_file(spec, paste0("crosswalks:", m$source_name))
      )
      # A sheet with no header at all has its own detail (D12.29).
      empty <- m$source_type == "sheet" & !m$source_name %chin% spec$code_lists$sheet
      values <- list(
        source_name = m$source_name, table_name = m$table_name, attribute_name = m$attribute_name
      )
      detail <- fifelse(
        empty,
        do.call(report_text, c("preflight_detail_code_column_missing_empty", values)),
        do.call(report_text, c("preflight_detail_code_column_missing", values))
      )
      findings_of(
        file, detail,
        dictionary_cell(spec, attribute_rows(spec, m$table_name, m$attribute_name))
      )
    },
    code_list_duplicate_code = function(spec) {
      if (!given(spec, "code_lists")) {
        return("no_input")
      }
      used <- sheet_columns_used(spec)
      cells <- spec$code_lists[used, on = list(sheet, sheet_column), nomatch = NULL]
      cells <- cells[source_row > 1L & !is.na(value)]
      # A list is one code column of a sheet, so a code repeats within a column (D12.34).
      dups <- cells[, list(
        n = .N, rows = paste(source_row, collapse = ", "), cell = source_cell[min(2L, .N)]
      ), by = list(sheet, sheet_column, value)]
      dups <- dups[n > 1L]
      findings_of(
        lists_file(spec, dups$sheet),
        report_text(
          "preflight_detail_code_list_duplicate_code",
          code = dups$value, n = dups$n, column = dups$sheet_column, sheet = dups$sheet,
          rows = dups$rows
        ),
        dups$cell
      )
    },
    code_list_blank_row = function(spec) {
      if (!given(spec, "code_lists")) {
        return("no_input")
      }
      sheets <- unique(sheet_columns_used(spec)$sheet)
      cells <- spec$code_lists[sheet %chin% sheets & source_row > 1L]
      # No sheet in use, or only header-only ones: no rows to check (D12.26).
      if (nrow(cells) == 0L) {
        return(no_findings())
      }
      rows <- cells[, list(
        blank = all(is.na(value)), cell = source_cell[[1L]]
      ), by = list(sheet, source_row)]
      rows <- rows[blank == TRUE]
      findings_of(
        lists_file(spec, rows$sheet),
        report_text(
          "preflight_detail_code_list_blank_row",
          row = rows$source_row, sheet = rows$sheet
        ),
        rows$cell
      )
    },
    code_list_unreferenced = function(spec) {
      if (!given(spec, "code_lists")) {
        return("no_input")
      }
      # Every sheet, a completely empty one included (D12.29).
      sheets <- spec$code_list_sheets$sheet
      referenced <- spec$code_list_map[source_type == "sheet", source_name]
      unused <- setdiff(sheets, c(referenced, spec$non_code_sheets$sheet))
      findings_of(
        lists_file(spec, unused),
        report_text("preflight_detail_code_list_unreferenced", sheet = unused),
        spec$code_lists$source_cell[match(unused, spec$code_lists$sheet)]
      )
    },
    code_list_empty_column = function(spec) {
      if (!given(spec, "code_lists")) {
        return("no_input")
      }
      sheets <- unique(sheet_columns_used(spec)$sheet)
      cells <- spec$code_lists[sheet %chin% sheets]
      # No sheet in use: no columns to check (D12.26). Each column has its header cell in
      # row 1, so [[1L]] below never meets an empty group.
      if (nrow(cells) == 0L) {
        return(no_findings())
      }
      columns <- cells[, list(
        header = value[source_row == 1L][[1L]],
        filled = any(!is.na(value[source_row > 1L])),
        cell = source_cell[source_row == 1L][[1L]]
      ), by = list(sheet, sheet_column)]
      empty <- columns[!is.na(header) & !filled]
      findings_of(
        lists_file(spec, empty$sheet),
        report_text(
          "preflight_detail_code_list_empty_column",
          column = empty$header, sheet = empty$sheet
        ),
        empty$cell
      )
    },
    spec_clash_resolved = function(spec) {
      by_value <- given(spec, "datasets") && given(spec, "precedence")
      if (!by_value && !given(spec, "lineage_spec")) {
        return("no_input")
      }
      clash <- spec$clashes[rule_id == "spec_clash_resolved"]
      placement <- clash$kind == "table_placement"
      detail <- fifelse(
        placement,
        report_text(
          "preflight_detail_spec_clash_resolved_placement",
          key_value = clash$key_value, value_a = clash$value_a, value_b = clash$value_b
        ),
        report_text(
          "preflight_detail_spec_clash_resolved_value",
          column_name = clash$column_name, key_col = clash$key_col, key_value = clash$key_value,
          source_a = clash$source_a, value_a = clash$value_a,
          source_b = clash$source_b, value_b = clash$value_b, winner = clash$winner
        )
      )
      file <- fifelse(placement, input_file(spec, "lineage_spec"), input_file(spec, "datasets"))
      findings_of(file, detail, clash_cells(clash$source_cell_a, clash$source_cell_b))
    },
    spec_clash_unresolved = function(spec) {
      if (!(given(spec, "datasets") && given(spec, "precedence"))) {
        return("no_input")
      }
      clash <- spec$clashes[rule_id == "spec_clash_unresolved"]
      values <- findings_of(
        input_file(spec, "datasets"),
        report_text(
          "preflight_detail_spec_clash_unresolved",
          column_name = clash$column_name, key_col = clash$key_col, key_value = clash$key_value,
          source_a = clash$source_a, value_a = clash$value_a,
          source_b = clash$source_b, value_b = clash$value_b
        ),
        clash_cells(clash$source_cell_a, clash$source_cell_b)
      )
      rbindlist(list(read_findings_of(spec, "spec_clash_unresolved"), values))
    },
    datasets_row_missing = function(spec) {
      if (!(given(spec, "datasets") && given(spec, "precedence"))) {
        return("no_input")
      }
      read_findings_of(spec, "datasets_row_missing")
    },
    spec_encoding_invalid = function(spec) {
      read_findings_of(spec, "spec_encoding_invalid")
    },
    spec_csv_malformed = function(spec) {
      # The reader makes every finding of this check, from fread()'s warnings (D12.54).
      read_findings_of(spec, "spec_csv_malformed")
    },
    site_id_range_invalid = function(spec) {
      if (!given(spec, "id_bands")) {
        return("no_input")
      }
      # The reader makes every finding of this check, where the bands sheet's file is
      # known (build_id_bands(), D12.28).
      read_findings_of(spec, "site_id_range_invalid")
    },
    lineage_spec_unparseable = function(spec) {
      if (!given(spec, "lineage_spec")) {
        return("no_input")
      }
      parse_findings <- read_findings_of(spec, "lineage_spec_unparseable")
      a <- spec$attributes
      if (all(is.na(a$lineage_flag))) {
        return(parse_findings)
      }
      by_attribute <- c("table_name", "attribute_name")
      by_contributor <- c("contributor_label", by_attribute)
      flagged <- a[lineage_flag == TRUE, list(table_name, attribute_name, source_row)]
      entries <- unique(spec$lineage_spec[, list(
        contributor_label, table_name, attribute_name,
        blank = id_status == "blank", source_cell
      )])
      covered <- unique(entries[, list(table_name, attribute_name)])
      no_row <- flagged[!covered, on = by_attribute]
      row_findings <- findings_of(
        input_file(spec, "dictionary"),
        report_text(
          "preflight_detail_lineage_spec_unparseable_row",
          table_name = no_row$table_name, attribute_name = no_row$attribute_name
        ),
        dictionary_cell(spec, no_row$source_row)
      )
      present <- flagged[covered, on = by_attribute, nomatch = NULL]
      contributors <- unique(entries$contributor_label)
      grid <- present[, list(contributor_label = contributors), by = by_attribute]
      filled <- unique(entries[blank == FALSE, by_contributor, with = FALSE])
      gaps <- grid[!filled, on = by_contributor]
      cells <- entries[blank == TRUE][gaps, on = by_contributor, source_cell, mult = "first"]
      blank_findings <- findings_of(
        input_file(spec, "lineage_spec"),
        report_text(
          "preflight_detail_lineage_spec_unparseable_blank",
          contributor_label = gaps$contributor_label, table_name = gaps$table_name,
          attribute_name = gaps$attribute_name
        ),
        cells
      )
      rbindlist(list(parse_findings, row_findings, blank_findings))
    },
    lineage_name_unknown = function(spec) {
      if (!given(spec, "lineage_spec")) {
        return("no_input")
      }
      lineage <- spec$lineage_spec
      unknown <- lineage$spec_type == "id" & lineage$part_kind == "derived" &
        !lineage$part_source %chin% spec$attributes$attribute_name
      names_used <- unique(lineage[which(unknown), list(
        contributor_label, table_name, attribute_name, part_source, source_cell
      )])
      findings_of(
        input_file(spec, "lineage_spec"),
        report_text(
          "preflight_detail_lineage_name_unknown",
          contributor_label = names_used$contributor_label, table_name = names_used$table_name,
          attribute_name = names_used$attribute_name, name = names_used$part_source
        ),
        names_used$source_cell
      )
    },
    lineage_spec_row_unflagged = function(spec) {
      if (!given(spec, "lineage_spec")) {
        return("no_input")
      }
      a <- spec$attributes
      if (all(is.na(a$lineage_flag))) {
        return("no_dd_column")
      }
      by_attribute <- c("table_name", "attribute_name")
      rows <- unique(spec$lineage_spec[, list(table_name, attribute_name)])
      extra <- rows[!a[lineage_flag == TRUE], on = by_attribute]
      cells <- spec$lineage_spec[extra, on = by_attribute, source_cell, mult = "first"]
      findings_of(
        input_file(spec, "lineage_spec"),
        report_text(
          "preflight_detail_lineage_spec_row_unflagged",
          table_name = extra$table_name, attribute_name = extra$attribute_name
        ),
        cells
      )
    },
    lineage_id_unflagged = function(spec) {
      if (!given(spec, "id_pattern")) {
        return("no_input")
      }
      a <- spec$attributes
      if (all(is.na(a$lineage_flag))) {
        return("no_dd_column")
      }
      hit <- a[id_marked == TRUE & lineage_flag == FALSE]
      findings_of(
        input_file(spec, "dictionary"),
        report_text(
          "preflight_detail_lineage_id_unflagged",
          table_name = hit$table_name, attribute_name = hit$attribute_name
        ),
        dictionary_cell(spec, hit$source_row)
      )
    },
    crosswalk_unreadable = function(spec) {
      if (!given(spec, "crosswalks")) {
        return("no_input")
      }
      read_findings_of(spec, "crosswalk_unreadable")
    }
  )
}
