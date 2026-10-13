# The clean base and the fixture scaffold (plan 18.3; D5.11, D8.21, D12.38, D15.10): clean
# tables built in code from a spec (MAGPlot's from magp_spec(), the generic ones from the
# fish toy spec), the fixture marker and the guard that refuses tables without it, and the
# answer sheet with its defect blocks, empty until M4 registers the first data rules.
# data-raw/build_magp_example.R sources this file to build magp_example.

# Clean tables for every table of `spec`, in the dictionary's order, each with its
# dictionary columns of the type map's R classes. A table without foreign keys has `n`
# rows; a table with them has `n` rows per row of its largest parent (the first such key
# on a tie), so `n` is not a scale knob: MAGPlot's deepest tables have n^8 rows, and a
# coded key caps its table's rows at the length of its list. Each other foreign key takes a
# parent row that agrees with the record's own ancestors, so two key paths to one ancestor
# reach the same row; where the record and that parent share an ancestor and no parent row
# agrees, the build stops. Keys are unique and point to a parent's row; a coded column holds
# codes of its list, never a sentinel; a date column dates in its format; other columns
# made-up numbers and text with no blanks and no spaces. Single-column primary keys only,
# foreign keys to their parent's PK, and no table that points at itself.
fx_clean_tables <- function(spec, n = 2L) {
  attributes <- spec$attributes
  keys <- spec$keys
  table_names <- unique(attributes$table_name)
  primary_keys <- keys[keys$key_type == "PK"]
  if (anyDuplicated(primary_keys$table_name) || anyDuplicated(attributes, by = c(
    "table_name", "attribute_name"
  ))) {
    stop("fx_clean_tables() builds tables with a single-column PK and unique columns only.")
  }
  linked <- keys$key_type == "FK" & keys$reference_table %chin% table_names &
    !is.na(keys$reference_attribute)
  links <- keys[linked]
  if (any(links$reference_table == links$table_name)) {
    stop("fx_clean_tables() builds no table that points at itself.")
  }
  parent_pks <- primary_keys$attribute_name[match(links$reference_table, primary_keys$table_name)]
  if (!identical(links$reference_attribute, parent_pks)) {
    stop("fx_clean_tables() builds foreign keys that point at their parent's PK only.")
  }
  # Each table after the tables its foreign keys point to, else in the dictionary's order.
  build_order <- character()
  while (length(build_order) < length(table_names)) {
    left <- setdiff(table_names, build_order)
    ready <- left[vapply(left, function(name) {
      all(links$reference_table[links$table_name == name] %chin% build_order)
    }, logical(1))]
    if (length(ready) == 0L) {
      stop("The spec's foreign keys go round in a circle.")
    }
    build_order <- c(build_order, ready[[1L]])
  }
  date_formats <- spec$type_map$date_format[match(attributes$data_type, spec$type_map$data_type)]
  # A column's codes, less every sentinel of the spec, whatever the sentinel's data type.
  pick_codes <- function(table, column) {
    codes <- spec$codes$code[spec$codes$table_name == table & spec$codes$attribute_name == column]
    unique(setdiff(codes, spec$sentinels$value))
  }
  as_class <- function(value, r_class, where) {
    out <- switch(r_class,
      integer = suppressWarnings(as.integer(value)),
      double = suppressWarnings(as.double(value)),
      as.character(value)
    )
    if (anyNA(out)) {
      stop("fx_clean_tables() can't make ", where, " of class ", r_class, ".")
    }
    out
  }
  # The rows of `parent` (lineage `parent_line`) each record takes: one that agrees with the
  # record's ancestors in `line`, the record's place among its siblings choosing among
  # several. Round robin where the record and the parent share no ancestor; where they share
  # one and no parent row agrees, a stop naming the foreign key `where`.
  coherent_rows <- function(parent_line, parent, line, within, where) {
    shared <- intersect(names(parent_line), names(line))
    if (length(shared) == 0L) {
      return(((seq_len(nrow(line)) - 1L) %% nrow(parent_line)) + 1L)
    }
    candidates <- data.table::copy(parent_line[, shared, with = FALSE])
    data.table::set(candidates, j = ".row", value = parent_line[[parent]])
    data.table::setorderv(candidates, c(shared, ".row"))
    # One row per distinct set of shared ancestors, with where its candidates start and how
    # many there are. Grouped by run, not by the names `shared` holds, which a table of that
    # name in the spec would otherwise make columns.
    runs <- data.table::rleidv(candidates, cols = shared)
    leads <- !duplicated(runs)
    groups <- candidates[leads, shared, with = FALSE]
    data.table::set(groups, j = ".start", value = which(leads))
    data.table::set(groups, j = ".count", value = tabulate(runs))
    # The records' shared ancestors and the join columns are worked out before the brackets, as
    # single symbols, so a table named line or shared isn't read as a column of `groups`.
    wanted <- line[, shared, with = FALSE]
    join_on <- shared
    found <- groups[wanted, on = join_on]
    picked <- candidates$.row[found$.start + ((within - 1L) %% found$.count)]
    if (anyNA(picked)) {
      stop(
        "fx_clean_tables() can't make ", where, " agree with the record's ancestors: no row of ",
        parent, " has them."
      )
    }
    picked
  }
  lineage <- list()
  built <- list()
  for (table in build_order) {
    table_links <- links[links$table_name == table]
    chosen <- list()
    if (nrow(table_links) == 0L) {
      rows <- n
      within <- seq_len(n)
      line <- data.table::data.table(seq_len(n))
      data.table::setnames(line, table)
    } else {
      sizes <- vapply(table_links$reference_table, function(p) nrow(lineage[[p]]), integer(1))
      first <- which.max(sizes)
      parent_rows <- rep(seq_len(sizes[[first]]), each = n)
      within <- rep(seq_len(n), times = sizes[[first]])
      rows <- length(parent_rows)
      line <- lineage[[table_links$reference_table[[first]]]][parent_rows]
      data.table::set(line, j = table, value = seq_len(rows))
      chosen[[table_links$attribute_name[[first]]]] <- parent_rows
      for (k in setdiff(seq_len(nrow(table_links)), first)) {
        parent <- table_links$reference_table[[k]]
        key_where <- paste0(table, ".", table_links$attribute_name[[k]])
        picked <- coherent_rows(lineage[[parent]], parent, line, within, key_where)
        chosen[[table_links$attribute_name[[k]]]] <- picked
        for (ancestor in setdiff(names(lineage[[parent]]), names(line))) {
          data.table::set(line, j = ancestor, value = lineage[[parent]][[ancestor]][picked])
        }
      }
    }
    lineage[[table]] <- line
    columns <- which(attributes$table_name == table)
    pk <- primary_keys$attribute_name[primary_keys$table_name == table]
    values <- lapply(columns, function(i) {
      column <- attributes$attribute_name[[i]]
      r_class <- attributes$r_class[[i]]
      where <- paste0(table, ".", column)
      codes <- pick_codes(table, column)
      index <- seq_len(rows)
      if (column %chin% names(chosen)) {
        parent <- table_links$reference_table[table_links$attribute_name == column]
        parent_pk <- primary_keys$attribute_name[primary_keys$table_name == parent]
        return(as_class(built[[parent]][[parent_pk]][chosen[[column]]], r_class, where))
      }
      if (identical(column, pk)) {
        if (length(codes) > 0L) {
          if (length(codes) < rows) {
            stop("fx_clean_tables() needs ", rows, " codes for the key ", where, ".")
          }
          return(as_class(codes[index], r_class, where))
        }
        value <- if (r_class == "character") paste0(table, "_", index) else index
        return(as_class(value, r_class, where))
      }
      value <- if (length(codes) > 0L) {
        codes[((index - 1L) %% length(codes)) + 1L]
      } else if (!is.na(date_formats[[i]])) {
        format(as.Date("2020-06-01") + index - 1L, date_formats[[i]])
      } else if (r_class == "integer") {
        index
      } else if (r_class == "double") {
        index + 0.5
      } else if (isTRUE(attributes$id_marked[[i]])) {
        paste0("s", index)
      } else {
        paste0("t", index)
      }
      as_class(value, r_class, where)
    })
    names(values) <- attributes$attribute_name[columns]
    built[[table]] <- data.table::as.data.table(values)
  }
  fx_mark_fixture(built[table_names])
}

# MAGPlot 2.0's clean base: every DD table and column, built from magp_spec() (21 M3b),
# each dataset in one jurisdiction and one contributor, as its row in the spec's datasets
# sheet has them (D15.32): MAGPlot's grouping columns are the contributor, the jurisdiction
# and the dataset. Nothing else of the generic build changes.
fx_magp_clean <- function() {
  spec <- magp_spec()
  tables <- fx_clean_tables(spec)
  sheet <- spec$datasets
  sites <- tables$magp_sites
  datasets <- tables$magp_datasets
  of_site <- match(sites$magp_dataset_id, sheet$magp_dataset_id)
  of_dataset <- match(datasets$magp_dataset_id, sheet$magp_dataset_id)
  data.table::set(sites, j = "jurisdiction", value = sheet$jurisdiction[of_site])
  data.table::set(
    datasets,
    j = "magp_contributor_id", value = sheet$magp_contributor_id[of_dataset]
  )
  tables
}

# The generic clean fixture: the fish survey toy spec's three tables (18.3).
fx_fish_clean <- function() {
  fx_clean_tables(fx_fish_spec())
}

# The fixture marker (D8.21) set on a list of data.tables and on each table; returns the
# list. The tables are marked by reference but the list only on the copy returned, so use
# the return value. A single data.table or any other input is refused: it would mark each
# column.
fx_mark_fixture <- function(tables) {
  listed <- is.list(tables) && !data.table::is.data.table(tables) &&
    all(vapply(tables, data.table::is.data.table, logical(1)))
  if (!listed) {
    stop("fx_mark_fixture() takes a list of data.tables.")
  }
  for (table in tables) {
    data.table::setattr(table, "gpq_fixture", TRUE)
  }
  attr(tables, "gpq_fixture") <- TRUE
  tables
}

# The guard: test helpers that take tables refuse any input without the marker, so tests
# can't run on delivered data by accident (18.3). Returns `x` invisibly.
fx_require_fixture <- function(x) {
  if (!isTRUE(attr(x, "gpq_fixture", exact = TRUE))) {
    stop(
      "Test helpers take fixtures only, and this input has no gpq_fixture attribute ",
      "(plan 18.3)."
    )
  }
  invisible(x)
}

# The answer sheet (18.3): one row per defect block, each a single edit to the clean base:
# the record it edits, the edit, the rule that must catch it, the cascade rules allowed
# ("; "-joined) and the witness value the rule must report. Empty until M4.
fx_answer_sheet <- function() {
  data.table::data.table(
    defect_id = character(), table_name = character(), column_name = character(),
    record_pk = character(), edit = character(), rule_id = character(),
    cascade_rule_ids = character(), witness = character()
  )
}

# The defect blocks, named by defect ID: each a function of the clean base's tables that
# returns them with its one edit made on a copy (data.table::copy()), so no block changes
# the tables another block or test reads. Empty until M4.
fx_defect_blocks <- function() {
  structure(list(), names = character())
}
