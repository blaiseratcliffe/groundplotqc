# MAGPlot 2.0's compiled specification (plan 3.6, 4.4; D12.20, D12.36), read from
# inst/extdata/magp/ with the engine's read_compiled_spec() (R/spec_compiled.R).

#' MAGPlot 2.0's compiled specification
#'
#' @description
#' The specification object for MAGPlot 2.0, compiled from the dated specification files
#' named in its manifest.
#'
#' @return An object of class `gpq_spec`, as [gpq_read_spec()] returns.
#' @examples
#' spec <- magp_spec()
#' spec$manifest
#' nrow(spec$attributes)
#' @export
magp_spec <- function() {
  dir <- system.file("extdata", "magp", package = "groundplotqc")
  if (!nzchar(dir)) {
    stop("The package has no compiled MAGPlot specification.", call. = FALSE)
  }
  read_compiled_spec(dir)
}
