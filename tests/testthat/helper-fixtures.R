# The clean base and the fixture scaffold (plan 18.3; D5.11, D8.21, D12.38, D15.10): clean
# tables built in code from a spec (MAGPlot's from magp_spec(), the generic ones from the
# fish toy spec), the fixture marker and the guard that refuses tables without it, and the
# answer sheet with its defect blocks, one or more for each registered data rule.
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
  # Whole numbers for a number column, inside the dictionary's bounds where it gives them, the
  # inclusive flags read (R14, R16): `index` where it fits, otherwise cycling over the whole
  # numbers that fit, so today's values stay wherever they already did. A double is the whole
  # number plus 0.5, so its bounds are read half a unit in. A bound the column can't fit
  # stops the base.
  within_bounds <- function(index, attribute, r_class) {
    half <- if (r_class == "double") 0.5 else 0
    low <- attribute$min_value
    high <- attribute$max_value
    lower <- if (is.null(low) || is.na(low)) {
      1
    } else if (isFALSE(attribute$min_inclusive)) {
      floor(low - half) + 1
    } else {
      ceiling(low - half)
    }
    upper <- if (is.null(high) || is.na(high)) {
      Inf
    } else if (isFALSE(attribute$max_inclusive)) {
      ceiling(high - half) - 1
    } else {
      floor(high - half)
    }
    if (upper < lower) {
      stop(
        "fx_clean_tables() can't make ", attribute$table_name, ".", attribute$attribute_name,
        " inside its bounds."
      )
    }
    if (is.infinite(upper)) {
      return(index + max(0, lower - 1))
    }
    lower + ((index - lower) %% (upper - lower + 1))
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
      } else if (r_class %in% c("integer", "double")) {
        within_bounds(index, attributes[i], r_class) + if (r_class == "double") 0.5 else 0L
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
# and the dataset. Each site's ID is moved into its contributor's band of site IDs (the
# band's start plus the old ID, in every table holding the ID), and the ec_* columns follow
# their DD patterns (D18.26). Nothing else of the generic build changes.
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
  # The band of each site's contributor: the contributor sheet gives its label.
  lists <- spec$code_lists[spec$code_lists$sheet == "contributor"]
  ids <- lists[lists$sheet_column == "magp_contributor_id"]
  names_at <- lists[lists$sheet_column == "abbreviated_name"]
  contributor <- datasets$magp_contributor_id[
    match(sites$magp_dataset_id, datasets$magp_dataset_id)
  ]
  row_of <- ids$source_row[match(contributor, ids$value)]
  label <- names_at$value[match(row_of, names_at$source_row)]
  start <- spec$id_bands$band_start[match(label, spec$id_bands$label)]
  old <- sites$magp_site_id
  moved <- as.integer(start + old)
  holders <- spec$attributes$table_name[spec$attributes$attribute_name == "magp_site_id"]
  for (table in holders) {
    data.table::set(
      tables[[table]],
      j = "magp_site_id", value = moved[match(tables[[table]]$magp_site_id, old)]
    )
  }
  # The ec_* columns by their DD patterns, a value per site (D18.26).
  i <- seq_len(nrow(sites))
  zone <- (i - 1L) %% 99L + 1L
  data.table::set(sites, j = "ec_zone", value = sprintf("%d", zone))
  data.table::set(sites, j = "ec_province", value = sprintf("%d.1", zone))
  region <- (i - 1L) %% 999L + 1L
  data.table::set(sites, j = "ec_region", value = sprintf("%02d.1.%03d", zone, region))
  data.table::set(
    sites,
    j = "ec_district", value = sprintf("%02d.1.%03d.%04d", zone, region, (i - 1L) %% 9999L + 1L)
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

# One answer-sheet row: the defect, where it is (record "*" for a table or column, NA where
# the record has no key, as pk_missing's), the edit, the rule that must catch it, the other
# rules allowed open issues with it ("; "-joined) and the witness value the rule must report
# (NA where its issue has none).
fx_defect <- function(defect_id, table_name, column_name, record_pk, edit, rule_id,
                      cascade_rule_ids = NA_character_, witness = NA_character_) {
  data.table::data.table(
    defect_id = defect_id, table_name = table_name, column_name = column_name,
    record_pk = record_pk, edit = edit, rule_id = rule_id,
    cascade_rule_ids = cascade_rule_ids, witness = witness
  )
}

# The answer sheet (18.3): one row per defect block, each a single edit to MAGPlot's clean
# base (fx_magp_clean()), caught by gpq_check_conformance() with magp_rules() (fx_caught()).
# A row's record_pk lists every record the block makes fail, "; "-joined: two holders of one
# value where every holder fails (pk_duplicate, src_id_duplicate, the design candidates); the
# witness is the first one's value.
fx_answer_sheet <- function() {
  data.table::rbindlist(list(
    fx_defect(character(), character(), character(), character(), character(), character()),
    # ---- Layer 0 (task 3) ----
    fx_defect(
      "table_missing", "magp_tree_age", "*", "*", "magp_tree_age left out", "table_missing"
    ),
    fx_defect(
      "table_unexpected", "magp_notes", "*", "*", "a table magp_notes added", "table_unexpected"
    ),
    fx_defect(
      "column_missing", "magp_trees", "comments", "*", "magp_trees$comments removed",
      "column_missing"
    ),
    fx_defect(
      "column_unexpected", "magp_plots", "colour", "*", "a column colour added to magp_plots",
      "column_unexpected"
    ),
    fx_defect(
      "column_name_mismatch", "magp_tree_meas", "dbh", "*", "magp_tree_meas$dbh renamed DBH",
      "column_name_mismatch",
      witness = "DBH"
    ),
    # ---- Layer 1 (task 4) ----
    fx_defect(
      "class_mismatch", "magp_tree_meas", "height", "*", "magp_tree_meas$height stored as text",
      "class_mismatch",
      witness = "character"
    ),
    fx_defect(
      "coercion_loss", "magp_tree_meas", "height", "magp_tree_meas_1",
      "magp_tree_meas$height stored as text, row 1 \"tall\"", "coercion_loss",
      cascade_rule_ids = "class_mismatch", witness = "tall"
    ),
    fx_defect(
      "date_unparseable", "magp_site_visits", "meas_date", "magp_site_visits_1",
      "magp_site_visits$meas_date row 1 \"2020-02-30\"", "date_unparseable",
      witness = "2020-02-30"
    ),
    fx_defect(
      "integer_stored_as_double", "magp_tree_meas", "visit_num", "*",
      "magp_tree_meas$visit_num stored as double", "integer_stored_as_double",
      witness = "double"
    ),
    fx_defect(
      "value_not_finite", "magp_tree_meas", "dbh", "magp_tree_meas_1",
      "magp_tree_meas$dbh row 1 Inf", "value_not_finite",
      witness = "Inf"
    ),
    # ---- Layer 2 (task 5) ----
    fx_defect(
      "raw_na", "magp_trees", "tree_dist", "magp_trees_1", "magp_trees$tree_dist row 1 NA",
      "raw_na"
    ),
    fx_defect(
      "blank_string", "magp_trees", "comments", "magp_trees_1", "magp_trees$comments row 1 \"\"",
      "blank_string",
      witness = ""
    ),
    fx_defect(
      "sentinel_wrong_type", "magp_trees", "tree_origin", "magp_trees_1",
      "magp_trees$tree_origin row 1 \"-9\"", "sentinel_wrong_type",
      witness = "-9"
    ),
    fx_defect(
      "sentinel_rogue_code", "magp_trees", "comments", "magp_trees_1",
      "magp_trees$comments row 1 \"N/A\"", "sentinel_rogue_code",
      witness = "N/A"
    ),
    fx_defect(
      "species_unknown_code", "magp_tree_meas", "species", "magp_tree_meas_1",
      "magp_tree_meas$species row 1 \"UNKN.SPP\"", "species_unknown_code",
      witness = "UNKN.SPP"
    ),
    fx_defect(
      "whitespace_padding", "magp_trees", "tree_origin", "magp_trees_1",
      "magp_trees$tree_origin row 1 \"AP \"", "whitespace_padding",
      witness = "AP "
    ),
    fx_defect(
      "na_text_in_id", "magp_trees", "src_tree_id", "magp_trees_1",
      "magp_trees$src_tree_id row 1 \"NA\"", "na_text_in_id",
      witness = "NA"
    ),
    # ---- Layer 3 (M5) ----
    fx_defect(
      "code_not_in_list", "magp_trees", "tree_origin", "magp_trees_1",
      "magp_trees$tree_origin row 1 \"QQ\"", "code_not_in_list",
      witness = "QQ"
    ),
    fx_defect(
      "value_below_min", "magp_tree_meas", "dbh", "magp_tree_meas_1",
      "magp_tree_meas$dbh row 1 0", "value_below_min",
      witness = "0"
    ),
    fx_defect(
      "value_above_max", "magp_tree_meas", "height", "magp_tree_meas_1",
      "magp_tree_meas$height row 1 200", "value_above_max",
      witness = "200"
    ),
    fx_defect(
      "pattern_mismatch", "magp_sites", "ec_zone", "350002",
      "magp_sites$ec_zone row 1 \"1A\"", "pattern_mismatch",
      witness = "1A"
    ),
    fx_defect(
      "site_id_out_of_range", "magp_sites", "magp_site_id", "1",
      "magp_sites$magp_site_id row 1 1, in every table holding it", "site_id_out_of_range",
      witness = "1"
    ),
    fx_defect(
      "undeclared_code_set", "magp_tree_age", "bore_ht", "*",
      "magp_tree_age$bore_ht all \"B\"", "undeclared_code_set",
      witness = "B"
    ),
    # ---- Layer 4 (M5) ----
    fx_defect(
      "pk_duplicate", "magp_tree_cond", "magp_cond_id", "magp_tree_cond_1; magp_tree_cond_1",
      "magp_tree_cond$magp_cond_id row 2 \"magp_tree_cond_1\", row 1's", "pk_duplicate",
      witness = "magp_tree_cond_1"
    ),
    fx_defect(
      "pk_missing", "magp_tree_cond", "magp_cond_id", NA_character_,
      "magp_tree_cond$magp_cond_id row 1 NA", "pk_missing"
    ),
    fx_defect(
      "fk_orphan", "magp_tree_cond", "magp_treemeas_id", "magp_tree_cond_1",
      "magp_tree_cond$magp_treemeas_id row 1 \"magp_tree_meas_0\"", "fk_orphan",
      witness = "magp_tree_meas_0"
    ),
    fx_defect(
      "fk_unassigned", "magp_tree_cond", "magp_treemeas_id", "magp_tree_cond_1",
      "magp_tree_cond$magp_treemeas_id row 1 NA", "fk_unassigned"
    ),
    fx_defect(
      "fk_sentinel_unverified", "magp_site_dist", "magp_site_id", "magp_site_dist_1",
      "magp_site_dist$magp_site_id row 1 -9", "fk_sentinel_unverified",
      witness = "-9"
    )
  ))
}

# A defect block that sets one cell of a copy of the base: `value` at `row` of `column`. The
# cell must read back as `value` (a missing value as a missing value); a value of another
# class than the column's is a change of class, which fx_set_column() makes.
fx_set_cell <- function(table, column, row, value) {
  function(tables) {
    fx_require_fixture(tables)
    tables <- fx_mark_fixture(lapply(tables, data.table::copy))
    data.table::set(tables[[table]], i = row, j = column, value = value)
    stored <- tables[[table]][[column]][row]
    if (!(identical(stored, value) || (all(is.na(value)) && all(is.na(stored))))) {
      stop(
        "fx_set_cell() can't store the value as it is in ", table, "$", column, ", of class ",
        class(tables[[table]][[column]])[[1L]], ": a change of class is fx_set_column()'s."
      )
    }
    tables
  }
}

# A defect block that sets, adds or, when `make()` returns NULL, removes one column of a copy
# of the base: `make(column)` is the new column.
fx_set_column <- function(table, column, make) {
  function(tables) {
    fx_require_fixture(tables)
    tables <- fx_mark_fixture(lapply(tables, data.table::copy))
    data.table::set(tables[[table]], j = column, value = make(tables[[table]][[column]]))
    tables
  }
}

# The defect blocks, named by defect ID: each a function of the clean base's tables that
# returns them with its one edit made on copies (data.table::copy()), so no block changes
# the tables another block or test reads.
fx_defect_blocks <- function() {
  list(
    # ---- Layer 0 (task 3) ----
    table_missing = function(tables) {
      fx_require_fixture(tables)
      fx_mark_fixture(lapply(tables[names(tables) != "magp_tree_age"], data.table::copy))
    },
    table_unexpected = function(tables) {
      fx_require_fixture(tables)
      tables <- lapply(tables, data.table::copy)
      tables$magp_notes <- data.table::data.table(note = "t1")
      fx_mark_fixture(tables)
    },
    column_missing = fx_set_column("magp_trees", "comments", function(x) NULL),
    column_unexpected = fx_set_column("magp_plots", "colour", function(x) "red"),
    column_name_mismatch = function(tables) {
      fx_require_fixture(tables)
      tables <- fx_mark_fixture(lapply(tables, data.table::copy))
      data.table::setnames(tables$magp_tree_meas, "dbh", "DBH")
      tables
    },
    # ---- Layer 1 (task 4) ----
    class_mismatch = fx_set_column("magp_tree_meas", "height", as.character),
    coercion_loss = function(tables) {
      tables <- fx_set_column("magp_tree_meas", "height", as.character)(tables)
      data.table::set(tables$magp_tree_meas, i = 1L, j = "height", value = "tall")
      tables
    },
    date_unparseable = fx_set_cell("magp_site_visits", "meas_date", 1L, "2020-02-30"),
    integer_stored_as_double = fx_set_column("magp_tree_meas", "visit_num", as.double),
    value_not_finite = fx_set_cell("magp_tree_meas", "dbh", 1L, Inf),
    # ---- Layer 2 (task 5) ----
    raw_na = fx_set_cell("magp_trees", "tree_dist", 1L, NA_real_),
    blank_string = fx_set_cell("magp_trees", "comments", 1L, ""),
    sentinel_wrong_type = fx_set_cell("magp_trees", "tree_origin", 1L, "-9"),
    sentinel_rogue_code = fx_set_cell("magp_trees", "comments", 1L, "N/A"),
    species_unknown_code = fx_set_cell("magp_tree_meas", "species", 1L, "UNKN.SPP"),
    whitespace_padding = fx_set_cell("magp_trees", "tree_origin", 1L, "AP "),
    na_text_in_id = fx_set_cell("magp_trees", "src_tree_id", 1L, "NA"),
    # ---- Layer 3 (M5) ----
    code_not_in_list = fx_set_cell("magp_trees", "tree_origin", 1L, "QQ"),
    value_below_min = fx_set_cell("magp_tree_meas", "dbh", 1L, 0),
    value_above_max = fx_set_cell("magp_tree_meas", "height", 1L, 200),
    pattern_mismatch = fx_set_cell("magp_sites", "ec_zone", 1L, "1A"),
    site_id_out_of_range = function(tables) {
      fx_require_fixture(tables)
      tables <- fx_mark_fixture(lapply(tables, data.table::copy))
      # Site 1's ID outside every band, in every table holding it, so its keys still agree.
      old <- tables$magp_sites$magp_site_id[[1L]]
      for (table in names(tables)) {
        if ("magp_site_id" %in% names(tables[[table]])) {
          at <- which(tables[[table]]$magp_site_id == old)
          data.table::set(tables[[table]], i = at, j = "magp_site_id", value = 1L)
        }
      }
      tables
    },
    undeclared_code_set = fx_set_column("magp_tree_age", "bore_ht", function(x) {
      rep("B", length(x))
    }),
    # ---- Layer 4 (M5) ----
    pk_duplicate = fx_set_cell("magp_tree_cond", "magp_cond_id", 2L, "magp_tree_cond_1"),
    pk_missing = fx_set_cell("magp_tree_cond", "magp_cond_id", 1L, NA_character_),
    fk_orphan = fx_set_cell("magp_tree_cond", "magp_treemeas_id", 1L, "magp_tree_meas_0"),
    fk_unassigned = fx_set_cell("magp_tree_cond", "magp_treemeas_id", 1L, NA_character_),
    fk_sentinel_unverified = fx_set_cell("magp_site_dist", "magp_site_id", 1L, -9L)
  )
}

# The results gpq_check_conformance() gives on fixture tables, Layers 0 to 4, with MAGPlot's
# spec, rule set and group columns; pre-flight's warning about the spec is muffled. What a
# planted defect must raise are the open issues: `results$issues[status == "open"]`.
fx_caught <- function(tables) {
  fx_require_fixture(tables)
  withCallingHandlers(
    gpq_check_conformance(
      tables, magp_spec(),
      group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
      problem_unit = list("magp_plots", "magp_plot_id"), rules = magp_rules(), layers = 0:4
    ),
    gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
  )
}
