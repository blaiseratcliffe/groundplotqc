# Times the conformance run on MAGPlot-shaped synthetic tables, stage by stage, with each
# stage's peak memory, and on the defect run MAGPlot's whole run, magp_run_qc(), without and
# with a lineage table (plan 16.1, 16.3; D15.38, D17.7, D17.8, D17.10, D17.21, D17.23, D21.7,
# D21.11, D22.33):
#
#   Rscript bench/run.R <scale> [variant]
#
# run from the checkout's root. Three runs on fx_magp_scaled()'s tables: the clean base
# (`clean`), the same rows in many groups (`many_groups`) and the defect run (`defects`), each
# in its own R session, so that no run's figures depend on what ran before it (D17.23): run.R
# starts itself once per variant, with a third argument, the file to write that variant's rows
# to, and binds their rows. A variant named runs alone, in this session.
#
# For each variant, first the run on its CSV files, reading included, as an engine caller with
# CSV files runs it (D17.21; magp_run_qc() reads an .Rdata, D21.11): bench/generate.R's
# write_tables() writes them to bench/data/<scale>/ (the clean base),
# bench/data/<scale>_many_groups/ and bench/data/<scale>_defects/ first when the folder holds
# none (delete a folder to have it written again). For the defect run, next, MAGPlot's whole
# run from bench/data/<scale>_defects.Rdata, which holds the defect tables, each its own object
# named as its table, then the same run given bench/data/<scale>_defects_lineage.csv, one row
# per tree measurement's species cell, written by bench/generate.R's write_lineage() (D22.33).
# Each of the two files is written first when it is missing, under a temporary name in its
# folder and then renamed, so a run stopped while writing one leaves no file the next run takes
# (m47); delete a file to have it written again. Then, on the tables built in memory, one stage
# each:
#   run_csv    gpq_check_conformance() on the CSV paths: the read, the mark scan and the checks;
#   run_qc     the defect run only: magp_run_qc() given the .Rdata's path, from the load to the
#              reports (D21.7). It runs before `build`, once the CSV run's result is released
#              and any tables built to write the .Rdata are removed, so its peak counts no
#              table but those the run loads itself and drops before classifying (D21.18):
#              16.1 holds it under 8 GB at 1x, and its seconds are read against 15 minutes
#              (Layers 0 to 4). Its row's `report_mb` is the largest provider report's, which
#              isn't the `report` stage's measure of one report: this run's issues are
#              classified with MAGPlot's evidence and that stage's with none, so their
#              sections differ (0.076 against 0.074 at 0.01x);
#   run_qc_lineage
#              the defect run only: the same run given the lineage table's path as `lineage`,
#              after run_qc's result and folder are released, so its peak holds nothing of
#              run_qc's. Its seconds and peak less run_qc's are what the table costs the run:
#              its four reads (the early read, pre-flight, classification and the build;
#              m30), classification's evidence join and the build's match of its rows to each
#              report unit's issues (M7a m46). run_qc also pays the session's first-run costs,
#              which make it the slower of the two at 0.01x;
#   build      fx_magp_scaled(), timed but not counted against 16.1;
#   mark_scan  validate_tables(), the scan of the tables for text marked as bytes (m33);
#   run        gpq_check_conformance() on the tables, which makes the mark scan itself, so
#              `run` includes the time of `mark_scan`, not the build;
#   write      gpq_write_results() on the run's result;
#   report     gpq_build_reports() on the written run, whose issues were classified with no
#              evidence before the write (D20.38, D20.116); its row's `report_mb` is the
#              largest report's size in MB, which 11.9 holds under 10 at 1x on the defect run.
# Each run is Layers 0 to 4. Its call, check() below, repeats fx_caught()'s in
# tests/testthat/helper-fixtures.R (MAGPlot's spec and rule set, the group columns and the
# problem unit, the pre-flight warning muffled), which takes fixtures only, not paths. A
# stage's peak is gc()'s "max used", in Mb, from a reset at its start. One CSV per run goes
# to bench/results/<UTC time>_<scale>x.csv, each variant's CSV run first and the defect run's
# run_qc and run_qc_lineage next, its seconds rounded to three decimals.

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
          problem_unit = list("magp_plots", "magp_plot_id"), rules = rules, layers = 0:4
        ),
        gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
      )
    }
    stage_row <- function(stage, timed, n_rows, results = NULL, report_mb = NA_real_) {
      data.table::data.table(
        git_commit = commit[1L], scale = scale, variant = variant, stage = stage,
        seconds = round(timed$seconds, 3), peak_mb = timed$peak_mb, n_rows = n_rows,
        n_results = if (is.null(results)) NA_integer_ else nrow(results$results),
        n_issues = if (is.null(results)) NA_integer_ else nrow(results$issues),
        report_mb = round(report_mb, 3)
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
    # MAGPlot's whole run, given the defect tables' .Rdata (D21.7, D21.21), before any table is
    # built in this session, then the same run given the lineage table (D22.33): tables built
    # to write the files are removed first, and each run's result and folder once its row is
    # made, so neither the next run nor a later stage holds them.
    qc_rows <- NULL
    if (variant == "defects") {
      rdata <- paste0(folder, ".Rdata")
      lineage_path <- paste0(folder, "_lineage.csv")
      # A file written under a temporary name in its folder, then renamed, so a write cut
      # short leaves no file the next run takes (m47).
      write_whole <- function(path, writer) {
        temp <- tempfile(basename(path), tmpdir = dirname(path))
        on.exit(unlink(temp), add = TRUE)
        writer(temp)
        stopifnot(file.rename(temp, path))
      }
      absent <- !file.exists(c(rdata, lineage_path))
      if (any(absent)) {
        saved <- do.call(env$fx_magp_scaled, c(list(scale), shape))
        if (absent[[1L]]) {
          write_whole(rdata, function(path) {
            save(list = names(saved), envir = list2env(saved), file = path)
          })
        }
        if (absent[[2L]]) {
          write_whole(lineage_path, function(path) write_lineage(saved, path))
        }
        rm(saved)
        invisible(gc())
      }
      # One run timed into a temporary folder, its row made and the run released.
      qc_stage <- function(stage, lineage = NULL) {
        qc_output <- tempfile("bench_runs")
        qc <- time_stage(withCallingHandlers(
          magp_run_qc(rdata, qc_output, lineage = lineage),
          gpq_preflight_warning = function(w) invokeRestart("muffleWarning")
        ))
        qc_pages <- list.files(
          file.path(qc_output, qc$value$metadata$run$run_id, "reports"),
          pattern = "[.]html$", full.names = TRUE
        )
        # The largest provider report: pre-flight's page, which the run writes too, isn't one.
        qc_pages <- qc_pages[basename(qc_pages) != "preflight.html"]
        qc_mb <- if (length(qc_pages) > 0L) max(file.size(qc_pages)) / 2^20 else NA_real_
        qc_row <- stage_row(stage, qc, NA_integer_, qc$value, report_mb = qc_mb)
        rm(qc)
        unlink(qc_output, recursive = TRUE)
        invisible(gc())
        qc_row
      }
      qc_rows <- data.table::rbindlist(list(
        qc_stage("run_qc"),
        qc_stage("run_qc_lineage", lineage_path)
      ))
    }
    built <- time_stage(do.call(env$fx_magp_scaled, c(list(scale), shape)))
    tables <- built$value
    n_rows <- sum(vapply(tables, nrow, integer(1)))
    data.table::set(csv_row, j = "n_rows", value = n_rows)
    if (!is.null(qc_rows)) {
      data.table::set(qc_rows, j = "n_rows", value = n_rows)
    }
    mark_scan <- time_stage(validate_tables(tables))
    run <- time_stage(check(tables))
    # Classified by reference with no evidence before the write, as gpq_build_reports() asks
    # (D20.25, D20.116), and untimed, so no stage changes what it measures (R89).
    gpq_classify_issues(run$value$issues, rules)
    output <- tempfile("bench_runs")
    written <- time_stage(gpq_write_results(run$value, output))
    reported <- time_stage(gpq_build_reports(run$value, output))
    pages <- grep("[.]html$", reported$value, value = TRUE)
    report_mb <- if (length(pages) > 0L) max(file.size(pages)) / 2^20 else NA_real_
    data.table::rbindlist(list(
      csv_row,
      qc_rows,
      stage_row("build", built, n_rows),
      stage_row("mark_scan", mark_scan, n_rows),
      stage_row("run", run, n_rows, run$value),
      stage_row("write", written, n_rows),
      stage_row("report", reported, n_rows, report_mb = report_mb)
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
