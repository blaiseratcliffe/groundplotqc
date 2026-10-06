# Coverage gate for engine files (plan 18.4; D8.21, D11.2, D11.7). Runs the
# package's tests under covr, writes covr's HTML report into <report_dir> when
# any package code ran, prints line coverage per engine file, and fails when
# the engine files' combined line coverage is below 90%.
# Run from the repo root: Rscript .github/scripts/check_coverage.R <report_dir>

engine_families <- c(
  "spec", "preflight", "rules", "results", "check", "fix", "lineage", "report", "utils"
)
coverage_threshold <- 90

is_engine_file <- function(paths) {
  pattern <- paste0("^(", paste(engine_families, collapse = "|"), ")(_[a-z0-9_]+)?[.]R$")
  grepl(pattern, basename(paths))
}

engine_line_coverage <- function(lines) {
  engine <- lines[is_engine_file(lines$filename), c("filename", "value"), drop = FALSE]
  if (nrow(engine) == 0L) {
    return(data.frame(
      filename = character(), covered = integer(), total = integer(), percent = numeric()
    ))
  }
  files <- sort(unique(engine$filename))
  covered <- vapply(files, function(f) sum(engine$value[engine$filename == f] > 0), integer(1))
  total <- vapply(files, function(f) sum(engine$filename == f), integer(1))
  data.frame(
    filename = files, covered = covered, total = total,
    percent = round(100 * covered / total, 1), row.names = NULL
  )
}

coverage_verdict <- function(per_file, threshold = coverage_threshold) {
  if (nrow(per_file) == 0L) {
    return(list(pass = TRUE, percent = NA_real_, message = "no engine files yet"))
  }
  percent <- 100 * sum(per_file$covered) / sum(per_file$total)
  pass <- percent >= threshold
  message <- sprintf(
    "Engine-file line coverage is %.1f%%: %s the %g%% gate.",
    percent, if (pass) "it meets" else "below", threshold
  )
  list(pass = pass, percent = percent, message = message)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) != 1L) {
    stop("Usage: Rscript .github/scripts/check_coverage.R <report_dir>", call. = FALSE)
  }
  coverage <- covr::package_coverage(type = "tests")
  if (length(coverage) > 0L) {
    dir.create(args[[1L]], recursive = TRUE, showWarnings = FALSE)
    covr::report(coverage, file = file.path(args[[1L]], "coverage.html"), browse = FALSE)
  } else {
    cat("No package code ran under the tests, so there is no HTML report.\n")
  }
  per_file <- engine_line_coverage(covr::tally_coverage(coverage, by = "line"))
  if (nrow(per_file) > 0L) {
    print(per_file, row.names = FALSE)
  }
  verdict <- coverage_verdict(per_file)
  cat(verdict$message, "\n", sep = "")
  if (!verdict$pass) {
    quit(save = "no", status = 1L)
  }
}

if (sys.nframe() == 0L) {
  main()
}
