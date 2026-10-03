# groundplotqc: rules for every Claude Code session

## The rule

Do not make any decisions on your own without my input. If you are unsure, ask me.

A decision is anything that affects scope, design, schemas, dependencies, defaults,
file names or locations, function and argument names, how to interpret or resolve
ambiguity in the specification, or user-facing behaviour. Present the options with
trade-offs and your recommendation, then wait for my answer. Subagents report every
decision and ambiguity back to the orchestrator. The orchestrator relays them to me
unchanged and does not answer them itself.

This file takes precedence over skills. Where a superpowers skill tells you to rule on,
adjudicate or park something yourself (subagent-driven-development's conflict scan,
its fix-round breaker and the final review's residuals), stop and put it to me as a
question.

## Before you start

- The approved plan and decisions log are in the folder named by `GPQ_PLANS_DIR`:
  `groundplotqc_plan.md` and `decisions_log.md`. Read the sections for your milestone.
- Worktrees live in the folder named by `GPQ_WORKTREE_ROOT`.
- If either variable is unset, stop and tell me. Never guess a path.
- Start every session in the project folder, whose `.claude/settings.local.json` sets
  both variables; work in worktrees from there.
- From M1 on, start with `command -v Rscript && Rscript --version`. If it fails, stop
  and tell me so the PATH gets fixed; never hard-code R's path.
- Read FILEMAP.md before searching the tree. Search only when FILEMAP doesn't answer.
- Names follow NAMING.md. When NAMING.md doesn't cover a case, ask me.

## Sources of truth

- `spec/` holds the specification set. Precedence between files is in plan section 2.2.
- Where code, comments or pipeline scripts disagree with the spec files, the files win.
  List each discrepancy for me; don't resolve it.
- Never edit, add or remove spec files. Spec updates go through the `update-spec`
  skill, with my approval.

## Data

- Never open, read, profile or query data files (.rds, .RData, .rda, CSV or TXT data
  extracts, Excel data files, .sqlite, .gpkg, .accdb and similar), and never connect to
  a database, without my explicit consent for that file or database. Exceptions: files
  in `spec/`, the package's own synthetic data and test fixtures. To ask: name the
  file, say why, and wait.
- At most 2 agents at a time may run R on real data.
- Tests, examples and vignettes use synthetic data only.

## Code

- Written from scratch. No code is copied from MAGPlotQC, the MAGPlot processing
  scripts, translation_audit, OSM or anywhere else. Existing code is evidence of
  behaviour and a source of check ideas only.
- data.table and base R. Imports: data.table and utils (D11.5, D11.6), tools from M2
  and stats at its first use, sf from M14 (plan section 15). No new dependency without
  my approval.
- The engine (`gpq_`) never names a MAGPlot table, column or code; MAGPlot specifics
  reach it through the spec object, rule set and settings.
- Thresholds, sentinels, code lists, tolerances and strategies are inputs, never
  hardcoded.
- Performance rules: plan section 16.2. Benchmarks: the `benchmark` skill.

## Files

- Use the Bash tool for commands; the PowerShell tool is disabled in this project.
- Create files with the Write tool and change them with the Edit tool.
- Bash runs commands only (R, tests, git, builds). Never create or change a file through
  Bash: no heredocs, `cat >`, `echo >`, `printf >`, `tee` or `sed -i`. A generated script
  that needs a heredoc quotes the delimiter.
- For a large new file, write a skeleton with Write and fill it in with Edit calls, not
  shell appends.

## Documentation and checks

- Every export: roxygen documentation with a runnable offline example.
- NEWS.md entry for every user-visible change.
- Before committing, style with `styler::style_pkg()`, then run `roxygen2::roxygenise()`
  and commit the `man/` and NAMESPACE files it regenerates with the change.
- Before a PR: document (any change to `man/` or NAMESPACE fails), test, lint, check
  style with `styler::style_pkg(dry = "fail")`, check the site's index with
  `pkgdown::check_pkgdown()`, and R CMD check with 0 errors, 0 warnings and 0 notes,
  except environment notes I have approved (use the check-runner agent).
- Add, rename or remove a file: update FILEMAP.md in the same commit.

## Git, branches and worktrees

- Create worktrees with `git worktree add "$GPQ_WORKTREE_ROOT/<branch>" -b <branch>`,
  never with the native worktree tool.
- One feature branch per milestone, plus one for the follow-up PR after M11 (D8.1).
  Never push to `main`. I merge every PR.
- Commit messages and PR descriptions carry no `Co-Authored-By` or "Generated with
  Claude Code" line; a `Claude-Session:` line is fine.
- The guard hook blocks destructive git, commits and pushes to `main`, file writes
  through Bash, edits in `spec/` and to the consent list, and reads of data files named
  in a tool call. It can't see reads made inside R scripts or through a database
  connection, or commands run through `eval`, `$(...)`, `xargs` or `git --git-dir`;
  "Data" and "Files" still cover those. Don't work around it; ask me.
- Superpowers specs and task plans go in the folder named by `GPQ_PLANS_DIR` (task plans
  as `milestones/<Mx>_tasks.md`), not in the repo; don't commit them.

## Evidence statuses

Tag every claim in reports and reviews: VERIFIED (checked this session, cite it),
SUPPORTED (consistent evidence, not checked directly), UNVERIFIED, CONTRADICTED,
SUPERSEDED, DECIDED (cite the decision ID from the decisions log).

## Agents and skills

| Use | When |
|---|---|
| spec-reader | you need what the spec says about a table, attribute, code, key or dataset |
| implementer | executing one task of a milestone plan |
| spec-conformance-reviewer | after any task that touches rules, spec handling, the applicability matrix or bundled configuration, and before a milestone closes |
| datatable-reviewer | after any task that changes files in R/, and before a milestone closes |
| fixture-builder | a rule is added or changed |
| check-runner | document, test, lint, check, benchmark |
| report-reviewer | report builders, sections or report text change |
| filemap-maintainer | files were added, renamed or removed |
| skill add-rule | adding or changing a rule |
| skill update-spec | a new specification file version |
| skill add-report-section | a new report section |
| skill benchmark | measuring time and memory |

Where a superpowers template dispatches a general-purpose subagent, dispatch the
matching agent above instead (plan section 20.7).

## Context awareness

Each prompt includes the current context window usage. Above 70%, mention it briefly
before starting any large multi-file task. Above 85%, suggest running /compact or
writing a hand-off note to `handoff.md` in the folder named by `GPQ_PLANS_DIR` before
continuing.
