# FILEMAP

Every tracked file in this repo, one table per folder (plan section 20.6). Read this
before searching the tree; search only when it doesn't answer (CLAUDE.md). When you add,
rename or remove a file, update this file in the same commit.

## Folders covered by one row

These folders have one row (path ending in "/", purpose, file-name pattern) instead of a
row per file (D7.10):

- `spec/`: its per-file list is `inst/extdata/magp/manifest.csv`, written from M2 by
  `data-raw/build_magp_config.R`.

## Root

| Path | Purpose | Key functions (exported; internal) | Depends on |
|---|---|---|---|
| .gitignore | Keeps delivered data, the private folders and run outputs out of git; allowlists the spec files and the package's own data (plan 19.2) | none | none |
| .gitattributes | Keeps files in `spec/` byte-exact on every OS, so manifest hashes match on Windows and in CI (D10.13) | none | none |
| .Rbuildignore | Leaves non-package files out of the R package build (plan 19.3) | none | none |
| CLAUDE.md | Rules for every Claude Code session | none | FILEMAP.md, NAMING.md, the plans folder (`GPQ_PLANS_DIR`) |
| FILEMAP.md | This map of tracked files | none | none |
| NAMING.md | Naming rules for functions, arguments, columns, rule IDs, codes, options and files | none | none |

## spec/

| Path | Purpose | Key functions (exported; internal) | Depends on |
|---|---|---|---|
| spec/ | The specification set (plan section 2.1): the data dictionary, the code-list workbook, the datasets lookup and Appendix A2; later the translation tables and the applicability matrix. Files are named `YYYYMMDD_magpv2_<name>.<ext>`. Never edited; new versions come from the user through the `update-spec` skill | none | none |

## .claude/

| Path | Purpose | Key functions (exported; internal) | Depends on |
|---|---|---|---|
| .claude/settings.json | Project permissions (allow, deny, ask), attribution off, and the guard hook's registration (plan 20.2) | none | .claude/hooks/guard-launch.ps1 |

## .claude/hooks/

| Path | Purpose | Key functions (exported; internal) | Depends on |
|---|---|---|---|
| .claude/hooks/guard-launch.ps1 | Entry point settings.json runs: reads the tool call from stdin, runs guard.ps1 on it, and blocks the call if guard.ps1 fails to load (D10.17) | none | .claude/hooks/guard.ps1 |
| .claude/hooks/guard.ps1 | Guard hook run before Bash, Read, Grep, Glob, Edit, Write, NotebookEdit and MCP tool calls: blocks destructive git, changes to `main`, file writes through Bash, changes in `spec/` and to the consent list, recursive `rm`, and data-file reads outside the allowlist without consent; asks before package installs (plan 20.3, D10.14, D10.17) | none exported; Invoke-Guard, Test-BashCommand, Read-ShellCommand, Get-CommandIndex, Test-GitCommand, Test-GitPush, Get-EffectiveBranch, Test-GrepTool, Test-EditTool, Get-ProtectedReason, Assert-DataRead, Get-DataCandidates, Test-DataAllowed, Resolve-GuardPath | the git-ignored consent list `.claude/data_consent.local.txt` in the project folder; environment variables `GPQ_WORKTREE_ROOT`, `GPQ_PLANS_DIR`; git |
| .claude/hooks/test_guard.py | The guard hook's tests: feeds guard-launch.ps1 tool calls as JSON and checks each verdict, every 20.3 row both ways; run with `python .claude/hooks/test_guard.py` after every change to the hook (D10.15, D10.17) | none exported; run(), parse_error_cases(), main() | guard-launch.ps1, guard.ps1; Python 3; environment variables `GPQ_WORKTREE_ROOT`, `GPQ_PLANS_DIR` |

## .claude/agents/

| Path | Purpose | Key functions (exported; internal) | Depends on |
|---|---|---|---|
| .claude/agents/spec-reader.md | Agent: answers questions about the specification set with file, sheet and row citations | none | spec/, inst/extdata/magp/, CLAUDE.md, FILEMAP.md, NAMING.md |
| .claude/agents/implementer.md | Agent: implements one task of a milestone plan, test first, in the milestone's worktree | none | CLAUDE.md, FILEMAP.md, NAMING.md, the milestone's task plan |
| .claude/agents/spec-conformance-reviewer.md | Agent: reviews a change against the spec files, the plan and the decisions log | none | spec/, CLAUDE.md, FILEMAP.md, NAMING.md, the plans folder |
| .claude/agents/datatable-reviewer.md | Agent: reviews R code for data.table idioms, copies, row loops and naming | none | CLAUDE.md, FILEMAP.md, NAMING.md |
| .claude/agents/fixture-builder.md | Agent: designs and writes synthetic test fixtures for a rule | none | spec/, tests/testthat/, CLAUDE.md, FILEMAP.md, NAMING.md |
| .claude/agents/check-runner.md | Agent: runs document, tests, lint, R CMD check, the FILEMAP check and benchmarks, and reports verbatim | none | CLAUDE.md, FILEMAP.md |
| .claude/agents/report-reviewer.md | Agent: reviews HTML reports and provider CSVs built from fixtures | none | tests/testthat/_snaps/, inst/extdata/text/, FILEMAP.md, NAMING.md |
| .claude/agents/filemap-maintainer.md | Agent: keeps FILEMAP.md in step with the file tree | none | FILEMAP.md, .github/scripts/check_filemap.R (from M1) |

## .claude/skills/

| Path | Purpose | Key functions (exported; internal) | Depends on |
|---|---|---|---|
| .claude/skills/add-rule/SKILL.md | Skill: adds or changes a QC rule end to end | none | agents fixture-builder, implementer, spec-conformance-reviewer, datatable-reviewer, check-runner |
| .claude/skills/update-spec/SKILL.md | Skill: brings a new spec file version into the package | none | agents spec-reader, check-runner, filemap-maintainer; data-raw/build_magp_config.R (from M2) |
| .claude/skills/add-report-section/SKILL.md | Skill: adds or changes a report section | none | agents report-reviewer, check-runner, filemap-maintainer |
| .claude/skills/benchmark/SKILL.md | Skill: measures run time and peak memory at 1x, 5x or 30x | none | agent check-runner; bench/ (from M4) |

## External references

Not files of this repo, so no per-file entries (plan section 20.6):

- `spec/`: its files come from the user (CLAUDE.md "Sources of truth"); one row above.
- The MAGPlot pipeline scripts, in the MAGPlot repository: evidence of behaviour only
  (CLAUDE.md "Code"); the user gives their paths when a milestone needs them.
- MAGPlotQC, the package groundplotqc succeeds: evidence of behaviour only.
- The plans folder, named by `GPQ_PLANS_DIR`: the plan, the decisions log, task plans
  (`milestones/<Mx>_tasks.md`) and the matrix working copy
  (`matrix/applicability_working.csv`).
