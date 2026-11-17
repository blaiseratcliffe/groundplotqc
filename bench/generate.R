# Writes MAGPlot-shaped synthetic tables, one CSV per table (plan 16.3; D7.14, D17.13):
#
#   Rscript bench/generate.R <scale> [folder]
#
# run from the checkout's root, to bench/data/<scale>/ (git-ignored) or the folder given.
# The tables are fx_magp_scaled()'s clean base at `scale`, built with the package loaded from
# the checkout and tests/testthat/helper-fixtures.R and helper-bench.R sourced, so no
# generator code ships in the package (16.3). Writing the 30x tables chunk by chunk is
# M8a's (16.3). bench/run.R also has write_lineage() write the defect run's lineage table
# (D22.33).

# The fixture builders, sourced into an environment whose parent is the package namespace,
# as data-raw/build_magp_example.R sources them, the package loaded from the checkout.
fixture_env <- function() {
  pkgload::load_all(".", quiet = TRUE)
  env <- new.env(parent = asNamespace("groundplotqc"))
  for (helper in c("helper-fixtures.R", "helper-bench.R")) {
    sys.source(file.path("tests", "testthat", helper), envir = env)
  }
  env
}

# The clean base at `scale` written to `folder`, one <table>.csv each with
# data.table::fwrite(); the paths, named by table, invisibly. `...` goes to fx_magp_scaled():
# bench/run.R writes the many-groups and defect runs' tables with it (D17.21).
write_tables <- function(scale, folder, env = fixture_env(), ...) {
  tables <- env$fx_magp_scaled(scale, ...)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(folder, paste0(names(tables), ".csv"))
  names(paths) <- names(tables)
  for (name in names(tables)) {
    data.table::fwrite(tables[[name]], paths[[name]])
  }
  invisible(paths)
}

# The lineage raw-value table of the defect run's tree measurements (plan 3.5, 10.2; D22.33),
# written to `path` with data.table::fwrite(), for bench/run.R's run_qc_lineage stage: one row
# per record of `tables`' magp_tree_meas (fx_magp_scaled()'s), its species cell, as request 21
# would list every translated species code. The values are synthetic: each raw value is its
# MAGPlot code in lower case, so it differs from the code, and the raw source is one provider
# column.
write_lineage <- function(tables, path) {
  meas <- tables$magp_tree_meas
  data.table::fwrite(data.table::data.table(
    table_name = "magp_tree_meas", record_pk = meas$magp_treemeas_id, column_name = "species",
    raw_value = tolower(meas$species), raw_source = "trees.species"
  ), path)
  invisible(path)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  scale <- suppressWarnings(as.numeric(args[1L]))
  if (length(args) < 1L || is.na(scale) || scale <= 0) {
    stop("Usage: Rscript bench/generate.R <scale> [folder]", call. = FALSE)
  }
  folder <- if (length(args) >= 2L) args[[2L]] else file.path("bench", "data", format(scale))
  paths <- write_tables(scale, folder)
  cat(length(paths), " tables written to ", folder, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
