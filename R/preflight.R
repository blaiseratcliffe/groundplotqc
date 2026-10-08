# Pre-flight checks of a specification, and of the rule set, settings and report text a run
# would use (plan 4.2, 9.5; D2.11, D8.9, D8.16, D12.15 to D12.26, D14.5, D14.6, D14.13). One
# function per check, held in preflight_check_functions(), returns its findings, or the
# reason it couldn't run; preflight_checks() makes preflight.csv's rows from them, in the
# registry's order (R/rules_registry.R). R/preflight_config.R holds the checks of the rule set
# and the text.

#' The pre-flight checks in the registry's order, and the outcome of each on failure
#'
#' A check registered `error` stops and one registered `warning` warns (D14.5).
#' @noRd
preflight_rules <- function() {
  registry <- rule_registry()
  registry <- registry[registry$check_type == "preflight"]
  data.table(
    rule_id = registry$rule_id,
    on_failure = fifelse(registry$default_severity == "error", "stop", "warn")
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
#'
#' `context` holds what the checks read besides the spec (preflight_context()).
#' @noRd
preflight_checks <- function(spec, context = preflight_context()) {
  rules <- preflight_rules()
  checks <- preflight_check_functions()
  rows <- lapply(seq_len(nrow(rules)), function(i) {
    rule <- rules$rule_id[[i]]
    result <- checks[[rule]](spec, context)
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

#' The checks of 4.2, one function per rule ID, as a list named by it (D12.26)
#'
#' M2's 22 here, then the rule set's and the text's from R/preflight_config.R.
#' preflight_checks() takes each from this list, which works in an installed package, where
#' a lookup by name wouldn't search the namespace. Each check takes the spec and the context
#' and returns findings_of(...), or the reason it couldn't run.
#' @noRd
preflight_check_functions <- function() {
  c(list(
    dd_duplicate_attribute = function(spec, context) {
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
    dd_type_unknown = function(spec, context) {
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
    dd_type_column_ambiguous = function(spec, context) {
      read_findings_of(spec, "dd_type_column_ambiguous")
    },
    dd_pk_missing = function(spec, context) {
      a <- spec$attributes
      absent <- setdiff(unique(a$table_name), spec$keys[key_type == "PK", table_name])
      findings_of(
        input_file(spec, "dictionary"),
        report_text("preflight_detail_dd_pk_missing", table_name = absent),
        dictionary_cell(spec, a$source_row[match(absent, a$table_name)])
      )
    },
    dd_fk_target_missing = function(spec, context) {
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
    code_list_missing = function(spec, context) {
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
    code_column_missing = function(spec, context) {
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
    code_list_duplicate_code = function(spec, context) {
      if (!given(spec, "code_lists")) {
        return("no_input")
      }
      used <- sheet_columns_used(spec)
      cells <- spec$code_lists[used, on = list(sheet, sheet_column), nomatch = NULL]
      cells <- cells[source_row > 1L & !is.na(value)]
      # A list is one code column of a sheet, so a code repeats within a column (D12.34).
      # Each cell is placed as its input form counts rows (D12.28, D12.64): a CSV's line or a
      # workbook's row, the number its cell ends in, origins applied; a data.frame's row as
      # R counts it, its cell NA.
      # The position is worked out once for every cell, and only the codes that repeat get
      # their rows joined.
      set(cells, j = "position", value = fifelse(
        is.na(cells$source_cell), as.character(cells$source_row - 1L),
        sub("^.*[^0-9]", "", cells$source_cell)
      ))
      cells[, n := .N, by = list(sheet, sheet_column, value)]
      dups <- cells[n > 1L, list(
        n = .N, rows = paste(position, collapse = ", "), cell = source_cell[2L]
      ), by = list(sheet, sheet_column, value)]
      # A sheet's cells share one form: a workbook's cell is sheet!<letters><row>, which a
      # CSV's file:<line> can't end like. A repeated code always has more than one row.
      form <- fifelse(
        is.na(dups$cell), "memory", fifelse(grepl("![A-Z]+[0-9]+$", dups$cell), "xlsx", "csv")
      )
      where <- character(nrow(dups))
      for (one_form in unique(form)) {
        hit <- form == one_form
        where[hit] <- report_text(paste0("position_", one_form, "_many"), rows = dups$rows[hit])
      }
      findings_of(
        lists_file(spec, dups$sheet),
        report_text(
          "preflight_detail_code_list_duplicate_code",
          code = dups$value, n = dups$n, column = dups$sheet_column, sheet = dups$sheet,
          where = where
        ),
        dups$cell
      )
    },
    code_list_blank_row = function(spec, context) {
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
      # Each blank row, one per finding, placed as its input form counts rows, as for
      # code_list_duplicate_code (D12.28, D12.64).
      position <- fifelse(
        is.na(rows$cell), as.character(rows$source_row - 1L), sub("^.*[^0-9]", "", rows$cell)
      )
      form <- fifelse(
        is.na(rows$cell), "memory", fifelse(grepl("![A-Z]+[0-9]+$", rows$cell), "xlsx", "csv")
      )
      where <- character(nrow(rows))
      for (one_form in unique(form)) {
        hit <- form == one_form
        where[hit] <- report_text(paste0("position_", one_form, "_one"), rows = position[hit])
      }
      findings_of(
        lists_file(spec, rows$sheet),
        report_text("preflight_detail_code_list_blank_row", sheet = rows$sheet, where = where),
        rows$cell
      )
    },
    code_list_unreferenced = function(spec, context) {
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
    code_list_empty_column = function(spec, context) {
      if (!given(spec, "code_lists")) {
        return("no_input")
      }
      # Every referenced sheet, its code column found or not, non-code sheets left out (4.2,
      # D12.65).
      map <- spec$code_list_map
      referenced <- map$source_type %chin% "sheet" &
        !map$source_name %chin% spec$non_code_sheets$sheet
      cells <- spec$code_lists[sheet %chin% unique(map$source_name[referenced])]
      # No sheet referenced: no columns to check (D12.26). A column without a header cell in
      # row 1, which the reader never makes, gets an NA header and is skipped, as a blank
      # header is (D12.64).
      if (nrow(cells) == 0L) {
        return(no_findings())
      }
      columns <- cells[, list(
        header = value[source_row == 1L][1L],
        filled = any(!is.na(value[source_row > 1L])),
        cell = source_cell[source_row == 1L][1L]
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
    spec_clash_resolved = function(spec, context) {
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
    spec_clash_unresolved = function(spec, context) {
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
    datasets_row_missing = function(spec, context) {
      if (!(given(spec, "datasets") && given(spec, "precedence"))) {
        return("no_input")
      }
      read_findings_of(spec, "datasets_row_missing")
    },
    spec_encoding_invalid = function(spec, context) {
      read_findings_of(spec, "spec_encoding_invalid")
    },
    spec_csv_malformed = function(spec, context) {
      # The reader makes every finding of this check: from fread()'s warnings (D12.54), a
      # shortfall of rows against the file's records (D12.58) and fread()'s stop on a quote
      # in a file of one column (D12.59).
      read_findings_of(spec, "spec_csv_malformed")
    },
    site_id_range_invalid = function(spec, context) {
      if (!given(spec, "id_bands")) {
        return("no_input")
      }
      # The reader makes every finding of this check, where the bands sheet's file is
      # known (build_id_bands(), D12.28).
      read_findings_of(spec, "site_id_range_invalid")
    },
    lineage_spec_unparseable = function(spec, context) {
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
    lineage_name_unknown = function(spec, context) {
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
    lineage_spec_row_unflagged = function(spec, context) {
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
    lineage_id_unflagged = function(spec, context) {
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
    crosswalk_unreadable = function(spec, context) {
      if (!given(spec, "crosswalks")) {
        return("no_input")
      }
      read_findings_of(spec, "crosswalk_unreadable")
    }
  ), preflight_rule_set_checks(), preflight_text_checks())
}

#' Pre-flight a specification
#'
#' @description
#' Checks a specification, never the data, and stops before a run that would read it
#' wrongly. Given a rule set, it checks the rule set's rows too, and it checks the
#' settings and report text a run would use. Each check gives one row per finding, or one
#' pass or not-run row.
#'
#' @param spec A specification from [gpq_read_spec()].
#' @param rules `NULL`, or a rule set: a named list of tables with a `rules` component, as
#'   [magp_rules()] returns. Its `rules` rows are checked against the specification and the
#'   registered rules, and its `settings` rows give settings.
#' @param settings A named list of settings, which come before the rule set's, the
#'   options' and the built-in values (see [groundplotqc_options]): `lang`, the run
#'   language, whose text `text_id_fallback` checks, and `severity`, severities by rule ID.
#' @param text `NULL`, or a table of report text with the columns `text_id`, `lang` and
#'   `text`, whose rows stand in for the package's rows of the same `text_id` and `lang`
#'   when pre-flight checks the registered rules' text; pre-flight's own page and table use
#'   the package's English text. A row that uses a \{slot\} the package's row of that text
#'   lacks stops pre-flight (`text_slot_unknown`).
#' @param output_dir `NULL`, or a folder: `metadata/preflight.csv` and
#'   `reports/preflight.html` are written under it, whether or not pre-flight stops.
#' @return The pre-flight table, invisibly: columns `rule_id`, `outcome` (`stop`, `warn`,
#'   `pass`, `not_run`), `file`, `detail`, `n_findings` (the check's total on each of its
#'   rows; 0 on a pass row, `NA` on a `not_run` row), `not_run_reason` and `source_cell`.
#'   A check with findings has one row per finding.
#' @section Conditions:
#' Any stop row signals an error of class `gpq_preflight_error`, whose `preflight`
#' element holds the table. Otherwise any warn row signals one warning of class
#' `gpq_preflight_warning`, with the same element.
#' @section Cells:
#' `source_cell` is where a finding is, when the reader knows: `sheet!B3` in a workbook,
#' `file:line` in a CSV file, `file:row` for a dictionary row. A clash's finding gives
#' both sides as `<a>; <b>`, in the order its detail names them, an unknown side left
#' empty; a side with several cells lists them with ", ", as in
#' `lineage!D11, lineage!F11; dictionary.xlsx:59`. A file or sheet name can itself contain
#' "; " or ", ", so for exact values read the `source_cell_a` and `source_cell_b` columns of
#' the specification's `clashes` component.
#' @section Checks:
#' `dd_duplicate_attribute`, `dd_type_unknown`, `dd_type_column_ambiguous`,
#' `dd_pk_missing`, `dd_fk_target_missing`, `code_list_missing`, `code_column_missing`,
#' `code_list_duplicate_code`, `code_list_blank_row`, `code_list_unreferenced`,
#' `code_list_empty_column`, `spec_clash_resolved`, `spec_clash_unresolved`,
#' `datasets_row_missing`, `spec_encoding_invalid`, `spec_csv_malformed`,
#' `site_id_range_invalid`, `lineage_spec_unparseable`, `lineage_name_unknown`,
#' `lineage_spec_row_unflagged`, `lineage_id_unflagged`, `crosswalk_unreadable`; with a rule
#' set, `rule_set_unknown_column`; with a rule set or a severity setting, `rule_id_unknown`
#' and `rule_set_override_invalid`; and `text_id_missing`, `text_id_fallback` and
#' `text_slot_unknown`, on the report text of the registered rules in English and in the run
#' language and on a caller's text table. No rule set or setting changes what a check stops
#' or warns on.
#' @examples
#' example <- function(file) system.file("extdata", "examples", file, package = "groundplotqc")
#' forest <- gpq_read_spec(
#'   dictionary = example("forest_dictionary.csv"),
#'   code_lists = list(
#'     SPECIES = example("forest_species.csv"), STATUS = example("forest_status.csv")
#'   ),
#'   column_map = gpq_column_map(
#'     table = "TABLE", attribute = "COLUMN", type = "FORMAT", key_type = "KEY",
#'     reference = "REFERS_TO", lookup = "CODE_LIST", description = "DEFINITION"
#'   ),
#'   type_map = gpq_type_map(example("forest_types.csv"))
#' )
#' results <- gpq_preflight(forest)
#' table(results$outcome)
#'
#' # A dictionary row written twice stops pre-flight.
#' twice <- data.frame(
#'   table_name = "t", attribute_name = c("id", "id"), key_type = "PK", data_type = "character"
#' )
#' stopped <- tryCatch(gpq_preflight(gpq_read_spec(twice)), gpq_preflight_error = function(e) e)
#' subset(stopped$preflight, outcome == "stop")
#'
#' # A rule set's row naming a rule no one registered stops pre-flight too.
#' rules <- list(rules = data.frame(
#'   rule_id = "no_such_rule", table_name = "*", attribute_name = "*", severity = NA,
#'   class = NA, enabled = TRUE
#' ))
#' stopped <- tryCatch(gpq_preflight(forest, rules = rules), gpq_preflight_error = function(e) e)
#' subset(stopped$preflight, outcome == "stop", c(rule_id, detail))
#' @export
gpq_preflight <- function(spec, rules = NULL, settings = list(), text = NULL,
                          output_dir = NULL) {
  if (!inherits(spec, "gpq_spec")) {
    stop("`spec` must be a specification from gpq_read_spec().", call. = FALSE)
  }
  # Nothing here changes the caller's tables (plan 16.2), indices included: data.table's
  # automatic indexing is off for every step that sees them, the validator's too, and
  # restored on exit (D12.27).
  auto_index <- options(datatable.auto.index = FALSE)
  on.exit(options(auto_index), add = TRUE)
  # A spec edited since it was read stops here, with the validator's message (D12.54).
  validate_gpq_spec(spec)
  dir_ok <- is.null(output_dir) || is.character(output_dir) && length(output_dir) == 1L &&
    !is.na(output_dir) && !is_blank(output_dir) &&
    !(file.exists(output_dir) && !dir.exists(output_dir))
  if (!dir_ok) {
    stop("`output_dir` must be NULL or one folder, not an existing file.", call. = FALSE)
  }
  # The rule set, settings and text are checked before any check runs: a caller's error in
  # them stops here (D14.6, D14.13).
  results <- preflight_checks(spec, preflight_context(rules, settings, text))
  if (!is.null(output_dir)) {
    write_preflight_files(results, spec, output_dir)
  }
  stops <- results[outcome == "stop"]
  if (nrow(stops) > 0L) {
    message <- paste(c(
      report_text(
        "preflight_stop_message",
        n_findings = nrow(stops), n_checks = length(unique(stops$rule_id))
      ),
      report_text("preflight_stop_line", rule_id = stops$rule_id, detail = stops$detail)
    ), collapse = "\n")
    stop(structure(
      class = c("gpq_preflight_error", "error", "condition"),
      list(message = message, call = NULL, preflight = results)
    ))
  }
  n_warn <- sum(results$outcome == "warn")
  if (n_warn > 0L) {
    warning(structure(
      class = c("gpq_preflight_warning", "warning", "condition"),
      list(
        message = report_text("preflight_warning_message", n_findings = n_warn), call = NULL,
        preflight = results
      )
    ))
  }
  invisible(results)
}
