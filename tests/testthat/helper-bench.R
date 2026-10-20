# The synthetic benchmark tables (plan 16.3, 18.3, 18.4; D7.14, D8.20, D17.7, D17.8):
# MAGPlot's clean base scaled to the shape of the AB file, every key path to one ancestor
# agreeing, with the fixture marker. bench/generate.R and bench/run.R source this file with
# helper-fixtures.R; the 0.1x smoke test calls fx_magp_scaled() directly. Every value that
# isn't a key or a src_*_id is one of fx_magp_clean()'s, or for the sites' datum and ec_* made
# by its formulas site by site (D18.20), so the scaled base is as clean as the clean base;
# each src_*_id holds its record's key as text, and magp_datasets' coded src_dataset_id its
# datasets sheet row's, so none repeats (R46).

# MAGPlot's tables at `scale` of the AB file's shape (D17.7): at 1x D1.9a's counts for
# magp_tree_cond (2,012,280), magp_tree_meas (1,615,305) and magp_trees (517,508), the other
# 14 tables sized by children per parent to 124,923 rows, 4,270,016 in all (R6, D18.41).
# Each site has one plot of four subplots and three or four visits; each visit one plot
# measurement of four subplot measurements; each tree measurement a tree of its subplot, so
# it agrees with its tree along both paths. The contributors, datasets, designs and frames
# don't scale: one contributor with its two datasets, or with `many_groups` every dataset
# of the specification's datasets sheet in its contributor and jurisdiction (R8); two
# designs per contributor and four frames per design, a subplot measurement's frame its
# subplot's (D18.41). `defects` plants D17.8's defects (see below).
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
  frame_design <- rep(seq_along(design_contributor), each = 4L)
  site_dataset <- spread(length(datasets), n_sites)
  visit_site <- spread(n_sites, n_visits)
  # One plot per site, so a site's plot is the plot of its row; four subplots per plot.
  subplot_plot <- rep(seq_len(n_sites), each = 4L)
  # One plot measurement per visit, by a design of the site's contributor.
  plot_meas_design <- 2L * (dataset_contributor[site_dataset[visit_site]] - 1L) + 1L +
    (seq_len(n_visits) - 1L) %% 2L
  # Four subplot measurements per plot measurement, one per subplot, each by the frame of its
  # design that matches the subplot (D18.41).
  subplot_meas_of <- rep(seq_len(n_visits), each = 4L)
  quarter <- rep(1:4, times = n_visits)
  subplot_meas_subplot <- 4L * (visit_site[subplot_meas_of] - 1L) + quarter
  subplot_meas_frame <- 4L * (plot_meas_design[subplot_meas_of] - 1L) + quarter
  tree_subplot <- spread(4L * n_sites, n_trees)
  # Each tree measurement takes a tree of its subplot, in turn over all the subplot's
  # measurements, so every tree is measured (record_unreachable, R46).
  tree_meas_of <- spread(4L * n_visits, n_tree_meas)
  subplot <- subplot_meas_subplot[tree_meas_of]
  first_tree <- match(seq_len(4L * n_sites), tree_subplot)
  trees_in <- tabulate(tree_subplot, 4L * n_sites)
  if (any(trees_in[subplot] == 0L)) {
    stop("fx_magp_scaled() needs a tree in every subplot: use a larger scale.")
  }
  turn <- data.table::rowid(subplot) - 1L
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
    # An ID-marked attribute holds its record's key as text (src_id_duplicate, R46): the
    # key's own strings, no new ones at 1x, in a vector of its own (copy()), since
    # as.character() returns a text key itself and fx_scaled_defects() plants keys in place.
    # A coded one keeps the base's codes, which its code list holds; magp_datasets' is set
    # from the datasets sheet below.
    marked <- setdiff(
      spec$attributes$attribute_name[
        spec$attributes$table_name == table & spec$attributes$id_marked %in% TRUE
      ],
      spec$code_list_map$attribute_name[spec$code_list_map$table_name == table]
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
      if (column %chin% marked) {
        return(data.table::copy(as.character(pk_values[[table]])))
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
  # Each dataset its sheet row's own src_dataset_id, as its contributor and jurisdiction are
  # (R8): the base's four codes, cycled, would repeat within a contributor (R46).
  data.table::set(
    tables$magp_datasets,
    j = "src_dataset_id", value = sheet$src_dataset_id[of_dataset]
  )
  # Each site its own datum and ec_* values, by fx_magp_clean()'s formula, so no text column
  # of the scaled sites reads as a code set (undeclared_code_set, D18.20). The other judged
  # columns hold more distinct base values than code_set_max_values, but for magp_datasets'
  # meas_interval, which holds 4: that table doesn't scale, and its rows, the datasets
  # sheet's 24 at most, are too few to judge.
  i <- seq_len(n_sites)
  zone <- (i - 1L) %% 99L + 1L
  region <- (i - 1L) %% 999L + 1L
  data.table::set(tables$magp_sites, j = "datum", value = paste0("t", i))
  data.table::set(tables$magp_sites, j = "ec_zone", value = sprintf("%d", zone))
  data.table::set(tables$magp_sites, j = "ec_province", value = sprintf("%d.1", zone))
  data.table::set(tables$magp_sites, j = "ec_region", value = sprintf("%02d.1.%03d", zone, region))
  data.table::set(
    tables$magp_sites,
    j = "ec_district", value = sprintf("%02d.1.%03d.%04d", zone, region, (i - 1L) %% 9999L + 1L)
  )
  # The frames and subplots labelled alike, and the copies taken from their parents (D18.41).
  tables <- fx_copy_key_pairs(fx_label_frames(fx_mark_fixture(tables), spec), spec)
  if (defects) {
    tables <- fx_scaled_defects(tables)
  }
  fx_mark_fixture(tables)
}

# D17.8's defect run, made in place on fx_magp_scaled()'s own tables: each of the answer
# sheet's defects (fx_answer_sheet()), the record defects in 0.1% of their table's rows
# (every 1000th row, from a different first row where two share a column), but for
# record_unreachable's, slightly fewer, since a tree whose next tree is in another subplot is
# skipped (5 of 6 at 0.01x), the table and column defects once, the three design candidates
# once each (frames and a subplot copied under new keys), column_missing on
# magp_tree_cond's comments rather than magp_trees', which blank_string and sentinel_rogue_code
# read (R9); and three carried from M4a: m9's key typed as text but stored as numbers
# (magp_trees' PK and magp_tree_meas' FK to it), m34's fully padded key column
# (magp_tree_meas' magp_frame_id) and m64's column with half its values failing coercion
# (magp_tree_meas' tree_wt as text, every second value "x").
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
  # record_unreachable: every 1000th tree from row 7 whose next tree is of its subplot gives
  # its measurements to that next tree, so none reaches it. The trees whose next tree is in
  # another subplot are skipped, so the plant falls slightly below D17.8's 0.1%.
  lost <- every("magp_trees", 7L)
  lost <- lost[lost < nrow(trees)]
  lost <- lost[trees$magp_subplot_id[lost] == trees$magp_subplot_id[lost + 1L]]
  moved <- which(meas$magp_tree_id %in% lost)
  data.table::set(meas, i = moved, j = "magp_tree_id", value = meas$magp_tree_id[moved] + 1L)
  # The design candidates, once each: design 2's frames given design 1's frame columns
  # (design_duplicate_signature); then each design's first frame copied under a new key with
  # another plot_area, so both still share one signature (design_geometry_inconsistent); and
  # subplot 1 copied under a new key (subplot_label_ambiguous). A copy's src_* ID is its key.
  # No measurement names the copied subplot, so record_unreachable lists it too (the answer
  # sheet's cascade).
  frames <- tables$magp_design_frames
  signed <- c(
    "frame_type", "subplot", "plot_area", "plot_shape", "min_dbh", "max_dbh", "min_ht",
    "max_ht", "baf"
  )
  for (column in signed) {
    data.table::set(frames, i = 5:8, j = column, value = frames[[column]][1:4])
  }
  spec <- magp_spec()
  copied <- function(table, rows, key) {
    added <- tables[[table]][rows]
    pk <- spec$keys$attribute_name[spec$keys$table_name == table & spec$keys$key_type == "PK"]
    data.table::set(added, j = pk, value = key)
    is_marked <- spec$attributes$table_name == table & spec$attributes$id_marked %in% TRUE
    for (column in spec$attributes$attribute_name[is_marked]) {
      data.table::set(added, j = column, value = key)
    }
    added
  }
  n_frames <- nrow(frames)
  extra <- copied("magp_design_frames", c(1L, 5L), paste0("magp_design_frames_", n_frames + 1:2))
  data.table::set(extra, j = "plot_area", value = 9.5)
  tables$magp_design_frames <- rbind(frames, extra)
  n_subplots <- nrow(tables$magp_subplots)
  tables$magp_subplots <- rbind(
    tables$magp_subplots,
    copied("magp_subplots", 1L, paste0("magp_subplots_", n_subplots + 1L))
  )
  data.table::set(meas, j = "height", value = as.character(meas$height))
  plant("magp_tree_meas", "height", 1L, "tall")
  plant("magp_tree_meas", "dbh", 2L, Inf)
  plant("magp_tree_meas", "species", 3L, "UNKN.SPP")
  # Layer 3 (M5): each from a first row of its own.
  plant("magp_tree_meas", "dbh", 4L, 0)
  plant("magp_tree_meas", "height", 2L, "200")
  plant("magp_sites", "ec_zone", 1L, "1A")
  plant("magp_trees", "tree_origin", 3L, "QQ")
  # site_id_out_of_range: every 1000th site renumbered 1, 2, ..., outside every band, in
  # every table holding its ID.
  moved <- every("magp_sites", 1L)
  old <- tables$magp_sites$magp_site_id[moved]
  for (table in names(tables)) {
    if ("magp_site_id" %in% names(tables[[table]])) {
      at <- match(tables[[table]]$magp_site_id, old)
      hit <- which(!is.na(at))
      data.table::set(tables[[table]], i = hit, j = "magp_site_id", value = at[hit])
    }
  }
  # undeclared_code_set: magp_tree_age is dropped here, so a tally column holds one value.
  data.table::set(tables$magp_tally_trees, j = "ht_class", value = "B")
  # Layer 4 (M5): each from a first row of its own; the repeated key is row 1's.
  plant("magp_tree_cond", "magp_cond_id", 2L, "magp_tree_cond_1")
  plant("magp_tree_cond", "magp_cond_id", 3L, NA_character_)
  plant("magp_tree_cond", "magp_treemeas_id", 4L, "magp_tree_meas_0")
  plant("magp_tree_cond", "magp_treemeas_id", 5L, NA_character_)
  plant("magp_site_dist", "magp_site_id", 1L, -9L)
  plant("magp_tree_meas", "visit_num", 6L, 99)
  # src_id_duplicate in magp_trees: here magp_tree_meas and magp_tree_cond are in report unit
  # "*" (m34's padded frame key, below), where the rule checks nothing; the clean runs check
  # them.
  plant("magp_trees", "src_tree_id", 4L, "dup")
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
