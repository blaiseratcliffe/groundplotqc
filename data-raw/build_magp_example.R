# Builds data/magp_example.rda, MAGPlot 2.0's synthetic example tables (plan 18.1; D5.11,
# D15.10), with fx_magp_clean() from tests/testthat/helper-fixtures.R, so the example data
# and the tests' clean base come from the same code and the same magp_spec() (21 M3b).
# Writes only data/magp_example.rda.
# Run from the repo root: Rscript data-raw/build_magp_example.R

# The fixture builders, sourced into an environment that sees the package's namespace, as
# testthat sources its helpers.
fixture_builders <- function(helper = file.path("tests", "testthat", "helper-fixtures.R")) {
  env <- new.env(parent = asNamespace("groundplotqc"))
  sys.source(helper, envir = env)
  env
}

main <- function() {
  pkgload::load_all(".", quiet = TRUE)
  magp_example <- fixture_builders()$fx_magp_clean()
  dir.create("data", showWarnings = FALSE)
  save(magp_example, file = file.path("data", "magp_example.rda"), compress = "xz", version = 3L)
  cat("Wrote data/magp_example.rda.\n")
}

if (sys.nframe() == 0L) {
  main()
}
