# Evidence for issues whose class is depends (plan 3.6, 10.3 steps 2 to 5; D4.3, D19.3, D19.19
# to D19.22, D19.58, D19.61): the computed, filled, lineage and crosswalk tables
# gpq_classify_issues() takes, read and checked, and each step's key joins. The first step
# that settles an issue makes it harmonization, its class basis the evidence's name.

#' The evidence tables' own columns (3.6; D19.20, D19.38)
#'
#' `computed` and `crosswalk` also hold one column named as a group column of the issues
#' (MAGPlot: jurisdiction); `filled` has pre-flight's `pipeline_fill` columns, and `lineage`
#' its `lineage` columns.
#' @noRd
evidence_schema <- function() {
  inputs <- run_input_schema()
  list(
    computed = c("table_name", "column_name", "derivation"),
    filled = inputs$pipeline_fill,
    lineage = inputs$lineage,
    crosswalk = c("provider_code", "code", "column_name")
  )
}

#' The evidence read and checked for classification (D19.28 (7), D19.58 (6))
#'
#' `read` holds read_run_inputs()'s reads. Caller's errors: a table that doesn't read cleanly;
#' `filled` or `lineage` without exactly its columns; `computed` or `crosswalk` without its own
#' columns and one more, named as a group column of the issues (`groups`); a `derivation`
#' other than 3.6's six; a cell `filled` or `lineage` lists twice. Returns, for each table
#' given, `data` and `group_col`, the group column `computed` or `crosswalk` holds (NULL for
#' the others).
#' @noRd
read_evidence <- function(read, groups) {
  schema <- evidence_schema()
  shown <- function(x) if (is.na(x)) "(blank)" else mark_invalid_utf8(x)
  out <- list()
  for (input in names(read)) {
    one <- read[[input]]
    stop_if_unclean(one, input)
    data <- one$data
    own <- schema[[input]]
    group_col <- NULL
    if (input %chin% c("computed", "crosswalk")) {
      group_col <- setdiff(names(data), own)
      named <- all(own %chin% names(data)) && length(group_col) == 1L && group_col %chin% groups
      if (!named) {
        # W244
        stop(sprintf(
          paste(
            "`%s` must have the columns %s and one more, named as a group column of the",
            "issues (%s); it has %s."
          ),
          input, paste(own, collapse = ", "),
          if (length(groups) > 0L) paste(groups, collapse = ", ") else "they have none",
          paste(mark_invalid_utf8(names(data)), collapse = ", ")
        ), call. = FALSE)
      }
    } else {
      check_input_columns(data, own, input)
    }
    if (input == "computed") {
      bad <- which(
        !data$derivation %chin% c("id", "class", "formula", "count", "fill", "reference")
      )
      if (length(bad) > 0L) {
        value <- data$derivation[[bad[[1L]]]]
        # W245
        stop(sprintf(
          paste(
            "The derivation at %s is %s; a derivation is id, class, formula, count, fill or",
            "reference."
          ),
          input_place(one, input, bad[[1L]]),
          if (is.na(value)) "(blank)" else paste0("\"", mark_invalid_utf8(value), "\"")
        ), call. = FALSE)
      }
    }
    if (input %chin% c("filled", "lineage")) {
      twice <- which(duplicated(data, by = c("table_name", "record_pk", "column_name")))
      if (length(twice) > 0L) {
        row <- twice[[1L]]
        # W246
        stop(sprintf(
          "`%s` lists the cell %s.%s of record %s twice, again at %s; give each cell once.",
          input, shown(data$table_name[[row]]), shown(data$column_name[[row]]),
          shown(data$record_pk[[row]]), input_place(one, input, row)
        ), call. = FALSE)
      }
    }
    out[[input]] <- list(data = data, group_col = group_col)
  }
  out
}

#' Which issues still depends the evidence settles, step by step (10.3 steps 2 to 5; D19.19 to
#' D19.22, D19.58, D19.61)
#'
#' `open` are the issues whose class is still depends; `evidence` is read_evidence()'s. Each
#' step reads only the issues no earlier step settled, by key joins:
#' - computed: an issue of a table and column `computed` lists, whose group column holds the
#'   row's value, or any value where the row gives "*"; record- and column-level alike;
#' - filled: a record-level issue of a cell `filled` lists whose value is its written_value,
#'   as text;
#' - lineage and crosswalk, both given, for a record-level issue of a code rule
#'   (`code_not_in_list`, D19.19): its raw value is its cell's `lineage` raw_value, else its
#'   value. Where `crosswalk` has rows for its column and its group column's value, a raw value
#'   listed there as a provider_code settles it ("lineage"), and one not listed settles it where
#'   the pipeline changed the cell: `lineage` lists it with a raw value other than its value
#'   ("crosswalk", D19.61, R46); an unchanged cell's code that isn't listed passes on, as does
#'   an issue whose column and value `crosswalk` has no row for.
#' Returns, for each of `open`, the basis that settled it, NA where none did.
#' @noRd
evidence_classes <- function(issues, evidence, open) {
  basis <- rep(NA_character_, length(open))
  computed <- evidence[["computed"]]
  if (!is.null(computed)) {
    rows <- open[is.na(basis)]
    data <- computed$data
    group_col <- computed$group_col
    every_value <- data[[group_col]] %chin% "*"
    one_value <- !every_value
    cell <- list(issues$table_name[rows], issues$column_name[rows])
    by_cell <- c("table_name", "column_name")
    by_value <- c(by_cell, group_col)
    # Each table and column once, and each with its group value once, so an issue meets one
    # row at most however many derivations a table lists for it (16.2), as the crosswalk's are.
    for_every <- unique(data[every_value, by_cell, with = FALSE])
    for_one <- unique(data[one_value, by_value, with = FALSE])
    hits <- c(
      key_matches(cell, rows, for_every, by_cell)$row,
      key_matches(c(cell, list(issues[[group_col]][rows])), rows, for_one, by_value)$row
    )
    basis[match(unique(hits), open)] <- "computed"
  }
  filled <- evidence[["filled"]]
  if (!is.null(filled)) {
    rows <- open[is.na(basis) & issues$level[open] %chin% "record"]
    hits <- key_matches(
      list(
        issues$table_name[rows], issues$record_pk[rows], issues$column_name[rows],
        issues$value[rows]
      ),
      rows, filled$data, c("table_name", "record_pk", "column_name", "written_value")
    )$row
    basis[match(unique(hits), open)] <- "filled"
  }
  lineage <- evidence[["lineage"]]
  crosswalk <- evidence[["crosswalk"]]
  if (!is.null(lineage) && !is.null(crosswalk)) {
    # The rules that judge a value against its code list; a later rule of that kind joins
    # with its milestone (D19.19, D19.58 (3)).
    code_rules <- "code_not_in_list"
    rows <- open[
      is.na(basis) & issues$rule_id[open] %chin% code_rules & issues$level[open] %chin% "record"
    ]
    listed <- key_matches(
      list(issues$table_name[rows], issues$record_pk[rows], issues$column_name[rows]),
      rows, lineage$data, c("table_name", "record_pk", "column_name")
    )
    raw <- issues$value[rows]
    at <- match(listed$row, rows)
    raw[at] <- lineage$data$raw_value[listed$at]
    # Applying D19.61's "the pipeline changed the value": a listed cell whose raw value is its
    # value, compared as text, wasn't changed, so step 5 passes it on as it does an unlisted
    # cell (R46). Step 4 reads the same raw value either way.
    changed <- rep(FALSE, length(rows))
    changed[at] <- !(raw[at] == issues$value[rows[at]]) %in% TRUE
    group_col <- crosswalk$group_col
    # Each column and group value once, and each of their provider codes once, so an issue
    # meets one row at most (16.2). Worked out before the brackets, which take a name of their
    # own for `by`.
    covering_columns <- c("column_name", group_col)
    code_columns <- c(covering_columns, "provider_code")
    covering <- unique(crosswalk$data[, covering_columns, with = FALSE])
    codes <- unique(crosswalk$data[, code_columns, with = FALSE])
    where <- list(issues$column_name[rows], issues[[group_col]][rows])
    covered <- rows %in% key_matches(where, rows, covering, covering_columns)$row
    valid <- rows %in% key_matches(c(where, list(raw)), rows, codes, code_columns)$row
    basis[match(rows[covered & valid], open)] <- "lineage"
    basis[match(rows[covered & !valid & changed], open)] <- "crosswalk"
  }
  basis
}
