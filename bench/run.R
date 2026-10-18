# Times the conformance run on MAGPlot-shaped synthetic tables, stage by stage, with each
# stage's peak memory (plan 16.1, 16.3; D15.38, D17.7, D17.8, D17.10, D17.21, D17.23):
#
#   Rscript bench/run.R <scale> [variant]
#
# run from the checkout's root. Three runs on fx_magp_scaled()'s tables: the clean base
# (`clean`), the same rows in many groups (`many_groups`) and the defect run (`defects`), each
# in its own R session, so that no run's figures depend on what ran before it (D17.23): run.R
# starts itself once per variant, with a third argument, the file to write that variant's rows
# to, and binds their rows. A variant named runs alone, in this session.
#
# For each variant, first the run on its CSV files, reading included, as MAGPlot runs
# (D17.21): bench/generate.R's write_tables() writes them to bench/data/<scale>/ (the clean
# base), bench/data/<scale>_many_groups/ and bench/data/<scale>_defects/ first when the folder
# holds none (delete a folder to have it written again). Then, on the tables built in memory,
# one stage each:
#   run_csv    gpq_check_conformance() on the CSV paths: the read, the mark scan and the checks;
#   build      fx_magp_scaled(), timed but not counted against 16.1;
#   mark_scan  validate_tables(), the scan of the tables for text marked as bytes (m33);
#   run        gpq_check_conformance() on the tables, which makes the mark scan itself, so
#              `run` includes the time of `mark_scan`, not the build;
#   write      gpq_write_results() on the run's result.
# Each run is Layers 0 to 3. Its call, check() below, repeats fx_caught()'s in
# tests/testthat/helper-fixtures.R (MAGPlot's spec and rule set, the group columns and the
# problem unit, the pre-flight warning muffled), which takes fixtures only, not paths. A
# stage's peak is gc()'s "max used", in Mb, from a reset at its start. One CSV per run goes
# to bench/results/<UTC time>_<scale>x.csv, each variant's CSV run first, its seconds rounded
# to three decimals.

source(file.path("bench", "generate.R"))

# A stage timed: its value, its seconds elapsed and its peak memory in Mb.
time_stage <- function(expr) {
  gc(reset = TRUE)
  seconds <- system.time(value <- expr)[["elapsed"]]
  # gc()'s Mb columns all read "(Mb)", and a memory limit adds a column: the peak is the Mb
  # column right after "max used" (m27).
  used <- gc()
  peak <- used[, match("max used", colnames(used)) + 1L]
  list(value = value, seconds = seconds, peak_mb = sum(peak))
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  variants <- c("clean", "many_groups", "defects")
  scale <- suppressWarnings(as.numeric(args[1L]))
  usable <- length(args) %in% 1:3 && !is.na(scale) && scale > 0 &&
    (length(args) < 2L || args[[2L]] %in% variants)
  if (!usable) {
    stop("Usage: Rscript bench/run.R <scale> [variant]", call. = FALSE)
  }
  commit <- tryCatch(
    system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
    error = function(e) NA_character_
  )
  # One variant's stages in this session: its rows.
  one_variant <- function(variant) {
    env <- fixture_env()
    spec <- magp_spec()
    rules <- magp_rules()
    check <- function(tables) {
      withCallingHandlers(
        gpq_check_conformance(
          tables, spec,
          group_cols = c("magp_contributor_id", "jurisdiction", "magp_dataset_id"),
          problem_unit = list("magp_plots", "magp_plot_id"), rules = rules, layers = 0:3
        ),
        gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
      )
    }
    stage_row <- function(stage, timed, n_rows, results = NULL) {
      data.table::data.table(
        git_commit = commit[1L], scale = scale, variant = variant, stage = stage,
        seconds = round(timed$seconds, 3), peak_mb = timed$peak_mb, n_rows = n_rows,
        n_results = if (is.null(results)) NA_integer_ else nrow(results$results),
        n_issues = if (is.null(results)) NA_integer_ else nrow(results$issues)
      )
    }
    shape <- list(
      clean = list(), many_groups = list(many_groups = TRUE), defects = list(defects = TRUE)
    )[[variant]]
    folder <- file.path("bench", "data", paste0(
      format(scale), if (variant != "clean") paste0("_", variant)
    ))
    paths <- list.files(folder, pattern = "[.]csv$", full.names = TRUE)
    if (length(paths) == 0L) {
      paths <- do.call(write_tables, c(list(scale, folder, env), shape))
    } else {
      names(paths) <- sub("[.]csv$", "", basename(paths))
    }
    invisible(gc())
    from_csv <- time_stage(check(paths))
    csv_row <- stage_row("run_csv", from_csv, NA_integer_, from_csv$value)
    rm(from_csv)
    invisible(gc())
    built <- time_stage(do.call(env$fx_magp_scaled, c(list(scale), shape)))
    tables <- built$value
    n_rows <- sum(vapply(tables, nrow, integer(1)))
    data.table::set(csv_row, j = "n_rows", value = n_rows)
    mark_scan <- time_stage(validate_tables(tables))
    run <- time_stage(check(tables))
    written <- time_stage(gpq_write_results(run$value, tempfile("bench_runs")))
    data.table::rbindlist(list(
      csv_row,
      stage_row("build", built, n_rows),
      stage_row("mark_scan", mark_scan, n_rows),
      stage_row("run", run, n_rows, run$value),
      stage_row("write", written, n_rows)
    ))
  }
  if (length(args) == 3L) {
    data.table::fwrite(one_variant(args[[2L]]), args[[3L]])
    return(invisible())
  }
  if (length(args) == 2L) {
    timings <- one_variant(args[[2L]])
  } else {
    quoted <- function(x) shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")
    files <- vapply(variants, function(variant) {
      file <- tempfile(fileext = ".csv")
      status <- system2(
        file.path(R.home("bin"), "Rscript"),
        quoted(c(file.path("bench", "run.R"), format(scale), variant, file))
      )
      if (!identical(status, 0L) || !file.exists(file)) {
        stop("bench/run.R stopped on the variant ", variant, ".", call. = FALSE)
      }
      file
    }, "")
    timings <- data.table::rbindlist(lapply(
      files, data.table::fread,
      colClasses = list(character = "git_commit")
    ))
  }
  dir.create(file.path("bench", "results"), showWarnings = FALSE, recursive = TRUE)
  file <- file.path(
    "bench", "results",
    paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_", format(scale), "x.csv")
  )
  data.table::fwrite(timings, file)
  print(timings)
  cat("Written to ", file, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
