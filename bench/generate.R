# Writes MAGPlot-shaped synthetic tables, one CSV per table (plan 16.3; D7.14, D17.13):
#
#   Rscript bench/generate.R <scale> [folder]
#
# run from the checkout's root, to bench/data/<scale>/ (git-ignored) or the folder given.
# The tables are fx_magp_scaled()'s clean base at `scale`, built with the package loaded from
# the checkout and tests/testthat/helper-fixtures.R and helper-bench.R sourced, so no
# generator code ships in the package (16.3). Writing the 30x tables chunk by chunk is
# M8a's (16.3).

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

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  scale <- suppressWarnings(as.numeric(args[1L]))
  if (length(args) < 1L || is.na(scale) || scale <= 0) {
    stop("Usage: Rscript bench/generate.R <scale> [folder]", call. = FALSE)
  }
  folder <- if (length(args) >= 2L) args[[2L]] else file.path("bench", "data", format(scale))
  paths <- write_tables(scale, folder)
  cat(length(paths), "tables written to", folder, "\n")
}

if (sys.nframe() == 0L) {
  main()
}
