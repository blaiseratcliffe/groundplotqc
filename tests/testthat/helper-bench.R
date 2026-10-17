# The synthetic benchmark tables (plan 16.3, 18.3, 18.4; D7.14, D8.20, D17.7, D17.8):
# MAGPlot's clean base scaled to the shape of the AB file, every key path to one ancestor
# agreeing, with the fixture marker. bench/generate.R and bench/run.R source this file with
# helper-fixtures.R; the 0.1x smoke test calls fx_magp_scaled() directly. Every value that
# isn't a key is one of fx_magp_clean()'s, so the scaled base is as clean as the clean base.

# MAGPlot's tables at `scale` of the AB file's shape (D17.7): at 1x D1.9a's counts for
# magp_tree_cond (2,012,280), magp_tree_meas (1,615,305) and magp_trees (517,508), the other
# 14 tables sized by children per parent to 124,919 rows, 4,270,012 in all (R6). Each site
# has one plot of four subplots and three or four visits; each visit one plot measurement of
# four subplot measurements; each tree measurement a tree of its subplot, so it agrees with
# its tree along both paths. The contributors, datasets, designs and frames don't scale:
# one contributor with its two datasets, or with `many_groups` every dataset of the
# specification's datasets sheet in its contributor and jurisdiction (R8); two designs per
# contributor and two frames per design. `defects` plants D17.8's defects (see below).
fx_magp_scaled <- function(scale = 1, many_groups = FALSE, defects = FALSE) {
  spec <- magp_spec()
  base <- fx_magp_clean()
  sheet <- spec$datasets
  datasets <- if (many_groups) {
    sheet$magp_dataset_id
  } else {
    sheet$magp_dataset_id[
      sheet$magp_contributor_id == base$magp_contributors$magp_contributor_id[[1L]]
    ]
  }
  of_dataset <- match(datasets, sheet$magp_dataset_id)
  contributors <- unique(sheet$magp_contributor_id[of_dataset])
  scaled <- function(count) max(1L, as.integer(round(count * scale)))
  # Each of `rows` children given one of `parents` in order, as evenly as can be.
  spread <- function(parents, rows) {
    rep(seq_len(parents), times = rows %/% parents + (seq_len(parents) <= rows %% parents))
  }
  n_sites <- scaled(4000)
  n_visits <- scaled(12485)
  n_trees <- scaled(517508)
  n_tree_meas <- scaled(1615305)
  # The parent row of each row, by foreign key column.
  dataset_contributor <- match(sheet$magp_contributor_id[of_dataset], contributors)
  design_contributor <- rep(seq_along(contributors), each = 2L)
  frame_design <- rep(seq_along(design_contributor), each = 2L)
  site_dataset <- spread(length(datasets), n_sites)
  visit_site <- spread(n_sites, n_visits)
  # One plot per site, so a site's plot is the plot of its row; four subplots per plot.
  subplot_plot <- rep(seq_len(n_sites), each = 4L)
  # One plot measurement per visit, by a design of the site's contributor.
  plot_meas_design <- 2L * (dataset_contributor[site_dataset[visit_site]] - 1L) + 1L +
    (seq_len(n_visits) - 1L) %% 2L
  # Four subplot measurements per plot measurement, one per subplot, by a frame of its design.
  subplot_meas_of <- rep(seq_len(n_visits), each = 4L)
  quarter <- rep(1:4, times = n_visits)
  subplot_meas_subplot <- 4L * (visit_site[subplot_meas_of] - 1L) + quarter
  subplot_meas_frame <- 2L * (plot_meas_design[subplot_meas_of] - 1L) + 1L +
    (quarter - 1L) %% 2L
  tree_subplot <- spread(4L * n_sites, n_trees)
  # Each tree measurement takes a tree of its subplot, in turn.
  tree_meas_of <- spread(4L * n_visits, n_tree_meas)
  subplot <- subplot_meas_subplot[tree_meas_of]
  first_tree <- match(seq_len(4L * n_sites), tree_subplot)
  trees_in <- tabulate(tree_subplot, 4L * n_sites)
  if (any(trees_in[subplot] == 0L)) {
    stop("fx_magp_scaled() needs a tree in every subplot: use a larger scale.")
  }
  turn <- data.table::rowid(tree_meas_of) - 1L
  tree_meas_tree <- first_tree[subplot] + turn %% trees_in[subplot]
  tally_of <- spread(4L * n_visits, scaled(8000))
  plan <- list(
    magp_contributors = list(rows = length(contributors), refs = list()),
    magp_datasets = list(
      rows = length(datasets), refs = list(magp_contributor_id = dataset_contributor)
    ),
    magp_sites = list(rows = n_sites, refs = list(magp_dataset_id = site_dataset)),
    magp_site_dist = list(
      rows = n_sites, refs = list(magp_dataset_id = site_dataset, magp_site_id = seq_len(n_sites))
    ),
    magp_site_treat = list(
      rows = n_sites, refs = list(magp_dataset_id = site_dataset, magp_site_id = seq_len(n_sites))
    ),
    magp_site_visits = list(rows = n_visits, refs = list(magp_site_id = visit_site)),
    magp_plots = list(rows = n_sites, refs = list(magp_site_id = seq_len(n_sites))),
    magp_subplots = list(
      rows = 4L * n_sites, refs = list(magp_site_id = subplot_plot, magp_plot_id = subplot_plot)
    ),
    magp_designs = list(
      rows = length(design_contributor), refs = list(magp_contributor_id = design_contributor)
    ),
    magp_design_frames = list(
      rows = length(frame_design), refs = list(magp_design_id = frame_design)
    ),
    magp_plot_meas = list(rows = n_visits, refs = list(
      magp_visit_id = seq_len(n_visits), magp_plot_id = visit_site,
      magp_design_id = plot_meas_design
    )),
    magp_subplot_meas = list(rows = 4L * n_visits, refs = list(
      magp_plotmeas_id = subplot_meas_of, magp_visit_id = subplot_meas_of,
      magp_subplot_id = subplot_meas_subplot, magp_frame_id = subplot_meas_frame
    )),
    magp_trees = list(rows = n_trees, refs = list(
      magp_site_id = subplot_plot[tree_subplot], magp_subplot_id = tree_subplot
    )),
    magp_tree_meas = list(rows = n_tree_meas, refs = list(
      magp_subpmeas_id = tree_meas_of, magp_tree_id = tree_meas_tree,
      magp_frame_id = subplot_meas_frame[tree_meas_of]
    )),
    magp_tree_cond = list(
      rows = scaled(2012280), refs = list(magp_treemeas_id = spread(n_tree_meas, scaled(2012280)))
    ),
    magp_tree_age = list(
      rows = scaled(10000), refs = list(magp_treemeas_id = spread(n_tree_meas, scaled(10000)))
    ),
    magp_tally_trees = list(rows = length(tally_of), refs = list(
      magp_subpmeas_id = tally_of, magp_frame_id = subplot_meas_frame[tally_of]
    ))
  )
  keys <- spec$keys
  foreign <- keys[keys$key_type == "FK" & keys$reference_table %chin% names(base)]
  same_keys <- vapply(names(plan), function(table) {
    setequal(names(plan[[table]]$refs), foreign$attribute_name[foreign$table_name == table])
  }, logical(1))
  if (!identical(sort(names(plan)), sort(names(base))) || !all(same_keys)) {
    stop("fx_magp_scaled() builds the tables and keys magp_spec() had at M4b.")
  }
  # Each site's ID in its contributor's band of site IDs, numbered within it (D18.26); at 1x
  # one contributor's 4,000 sites fit a band of 50,000.
  lists <- spec$code_lists[spec$code_lists$sheet == "contributor"]
  ids <- lists[lists$sheet_column == "magp_contributor_id"]
  names_at <- lists[lists$sheet_column == "abbreviated_name"]
  site_contributor <- sheet$magp_contributor_id[of_dataset][site_dataset]
  row_of <- ids$source_row[match(site_contributor, ids$value)]
  label <- names_at$value[match(row_of, names_at$source_row)]
  site_ids <- as.integer(
    spec$id_bands$band_start[match(label, spec$id_bands$label)] + data.table::rowid(label)
  )
  pk_values <- list()
  tables <- list()
  for (table in names(base)) {
    rows <- plan[[table]]$rows
    refs <- plan[[table]]$refs
    template <- base[[table]]
    pk <- keys$attribute_name[keys$table_name == table & keys$key_type == "PK"]
    pk_values[[table]] <- switch(table,
      magp_contributors = contributors,
      magp_datasets = datasets,
      magp_sites = site_ids,
      paste0(table, "_", seq_len(rows))
    )
    columns <- lapply(names(template), function(column) {
      if (identical(column, pk)) {
        return(pk_values[[table]])
      }
      if (column %chin% names(refs)) {
        parent <- foreign$reference_table[
          foreign$table_name == table & foreign$attribute_name == column
        ]
        return(pk_values[[parent]][refs[[column]]])
      }
      rep_len(template[[column]], rows)
    })
    names(columns) <- names(template)
    tables[[table]] <- data.table::setDT(columns)
  }
  data.table::set(
    tables$magp_sites,
    j = "jurisdiction", value = sheet$jurisdiction[of_dataset][site_dataset]
  )
  if (defects) {
    tables <- fx_scaled_defects(tables)
  }
  fx_mark_fixture(tables)
}

# D17.8's defect run, made in place on fx_magp_scaled()'s own tables: each of the answer
# sheet's 17 defects (fx_answer_sheet()), the record defects in 0.1% of their table's rows
# (every 1000th row, from a different first row where two share a column), the table and
# column defects once, column_missing on magp_tree_cond's comments rather than magp_trees',
# which blank_string and sentinel_rogue_code read (R9); and three carried from M4a: m9's key
# typed as text but stored as numbers (magp_trees' PK and magp_tree_meas' FK to it), m34's
# fully padded key column (magp_tree_meas' magp_frame_id) and m64's column with half its
# values failing coercion (magp_tree_meas' tree_wt as text, every second value "x").
fx_scaled_defects <- function(tables) {
  every <- function(table, first) {
    n <- nrow(tables[[table]])
    rows <- seq.int(first, max(first, n), by = 1000L)
    rows[rows <= n]
  }
  plant <- function(table, column, first, value) {
    data.table::set(tables[[table]], i = every(table, first), j = column, value = value)
  }
  meas <- tables$magp_tree_meas
  trees <- tables$magp_trees
  # m9: the tree key stored as numbers, the FK as its tree's row.
  tree_row <- match(meas$magp_tree_id, trees$magp_tree_id)
  data.table::set(meas, j = "magp_tree_id", value = tree_row)
  data.table::set(trees, j = "magp_tree_id", value = seq_len(nrow(trees)))
  data.table::set(meas, j = "height", value = as.character(meas$height))
  plant("magp_tree_meas", "height", 1L, "tall")
  plant("magp_tree_meas", "dbh", 2L, Inf)
  plant("magp_tree_meas", "species", 3L, "UNKN.SPP")
  data.table::set(meas, j = "visit_num", value = as.double(meas$visit_num))
  data.table::setnames(meas, "dbh", "DBH")
  data.table::set(meas, j = "magp_frame_id", value = paste0(meas$magp_frame_id, "  "))
  tree_wt <- as.character(meas$tree_wt)
  tree_wt[seq.int(1L, length(tree_wt), by = 2L)] <- "x"
  data.table::set(meas, j = "tree_wt", value = tree_wt)
  plant("magp_site_visits", "meas_date", 1L, "2020-02-30")
  plant("magp_trees", "tree_dist", 1L, NA_real_)
  plant("magp_trees", "comments", 1L, "")
  plant("magp_trees", "comments", 2L, "N/A")
  plant("magp_trees", "tree_origin", 1L, "-9")
  plant("magp_trees", "tree_origin", 2L, "AP ")
  plant("magp_trees", "src_tree_id", 1L, "NA")
  data.table::set(tables$magp_tree_cond, j = "comments", value = NULL)
  data.table::set(tables$magp_plots, j = "colour", value = "red")
  tables$magp_notes <- data.table::data.table(note = "t1")
  tables$magp_tree_age <- NULL
  tables
}
