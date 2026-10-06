# Manifest check (plan 4.4; D7.10, D9.19, D12.20, D12.33). Compares
# inst/extdata/magp/manifest.csv with the files it names: a bare name is a file in spec/,
# a path such as data-raw/magp/spec_exceptions.csv a file at that repo path; each must
# exist with the same SHA-256, and every file in spec/ must be in the manifest. Rows "in
# memory" are skipped.
# Run from the repo root: Rscript .github/scripts/check_manifest.R

read_manifest <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = "", encoding = "UTF-8")
}

spec_hashes <- function(spec_dir) {
  # Excel's lock file (~$<name>.xlsx) sits beside an open workbook and is never a spec
  # file (D12.34).
  files <- list.files(spec_dir, full.names = TRUE)
  files <- files[!startsWith(basename(files), "~$")]
  stats::setNames(unname(tools::sha256sum(files)), basename(files))
}

repo_hashes <- function(paths, root = ".") {
  found <- paths[file.exists(file.path(root, paths))]
  stats::setNames(unname(tools::sha256sum(file.path(root, found))), found)
}

manifest_problems <- function(manifest, hashes, repo_hashes = character()) {
  rows <- manifest[!is.na(manifest$file) & manifest$file != "in memory", , drop = FALSE]
  at_path <- grepl("/", rows$file, fixed = TRUE)
  in_spec <- rows$file[!at_path]
  in_repo <- rows$file[at_path]
  changed <- function(files, known) {
    both <- intersect(files, names(known))
    both[rows$sha256[match(both, rows$file)] != known[both]]
  }
  c(
    sprintf("in the manifest but not in spec/: %s", setdiff(in_spec, names(hashes))),
    sprintf("in the manifest but not in the repo: %s", setdiff(in_repo, names(repo_hashes))),
    sprintf("in spec/ but not in the manifest: %s", setdiff(names(hashes), in_spec)),
    sprintf("hash differs: %s", c(changed(in_spec, hashes), changed(in_repo, repo_hashes)))
  )
}

main <- function() {
  manifest <- file.path("inst", "extdata", "magp", "manifest.csv")
  if (!file.exists(manifest)) {
    cat("No compiled manifest: run Rscript data-raw/build_magp_config.R.\n")
    quit(save = "no", status = 1L)
  }
  rows <- read_manifest(manifest)
  paths <- rows$file[!is.na(rows$file) & grepl("/", rows$file, fixed = TRUE)]
  problems <- manifest_problems(rows, spec_hashes("spec"), repo_hashes(paths))
  if (length(problems) > 0L) {
    cat(problems, sep = "\n")
    # A hash can differ with no edit at all (D12.34).
    cat(
      "A workbook opened in Excel can change without an edit: if you didn't mean to",
      "change a spec file, restore it with git restore spec/<file>.\n"
    )
    quit(save = "no", status = 1L)
  }
  cat("The manifest matches spec/.\n")
}

if (sys.nframe() == 0L) {
  main()
}
