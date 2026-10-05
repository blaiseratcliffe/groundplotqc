# Agent and skill frontmatter check (D12.43, D12.46). Parses the YAML frontmatter of
# every .claude/agents/*.md and .claude/skills/*/SKILL.md, which Claude Code skips
# without a word when it doesn't parse, and fails on any that doesn't parse or lacks a
# name or a description. Run from the repo root: Rscript .github/scripts/check_agents.R

agent_files <- function(dir = ".") {
  agents <- Sys.glob(file.path(dir, ".claude", "agents", "*.md"))
  skills <- Sys.glob(file.path(dir, ".claude", "skills", "*", "SKILL.md"))
  sort(c(agents, skills), method = "radix")
}

frontmatter_lines <- function(lines) {
  if (length(lines) == 0L || trimws(lines[[1L]]) != "---") {
    return(NULL)
  }
  end <- match("---", trimws(lines[-1L]))
  if (is.na(end)) {
    return(NULL)
  }
  lines[seq_len(end - 1L) + 1L]
}

frontmatter_problems <- function(path, lines) {
  block <- frontmatter_lines(lines)
  if (is.null(block)) {
    return(paste0(
      path, ': no frontmatter: the file must start with a line "---" and close the ',
      "block with another"
    ))
  }
  fields <- tryCatch(
    yaml::yaml.load(paste(block, collapse = "\n")),
    error = function(e) e
  )
  if (inherits(fields, "error")) {
    return(sprintf("%s: frontmatter isn't valid YAML: %s", path, conditionMessage(fields)))
  }
  if (!is.list(fields)) {
    fields <- list()
  }
  single <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  name <- fields[["name"]]
  expected <- if (basename(path) == "SKILL.md") {
    basename(dirname(path))
  } else {
    sub("\\.md$", "", basename(path))
  }
  as.character(c(
    if (!single(name)) sprintf("%s: no name (a single, non-empty string)", path),
    if (!single(fields[["description"]])) {
      sprintf("%s: no description (a single, non-empty string)", path)
    },
    if (single(name) && name != expected) {
      sprintf("%s: name \"%s\" doesn't match the file's name \"%s\"", path, name, expected)
    }
  ))
}

check_agents_files <- function(paths) {
  problems <- lapply(paths, function(path) {
    frontmatter_problems(path, readLines(path, encoding = "UTF-8", warn = FALSE))
  })
  as.character(unlist(problems, use.names = FALSE))
}

main <- function() {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("check_agents.R needs the yaml package (Config/Needs/lint).", call. = FALSE)
  }
  paths <- agent_files()
  if (length(paths) == 0L) {
    stop("no agent or skill files under .claude/; run from the repo root.", call. = FALSE)
  }
  problems <- check_agents_files(paths)
  if (length(problems) > 0L) {
    cat(problems, sep = "\n")
    quit(save = "no", status = 1L)
  }
  cat(sprintf("All %d agent and skill files have valid frontmatter.\n", length(paths)))
}

if (sys.nframe() == 0L) {
  main()
}
