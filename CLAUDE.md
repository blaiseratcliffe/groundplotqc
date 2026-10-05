# groundplotqc: rules for every Claude Code session

## The rule

Every decision falls in one of three tiers. If you are unsure which, it is Tier 1.

- **Tier 1: ask me first.** Anything that affects scope, design, schemas,
  dependencies, defaults, file names or locations, function and argument names, how to
  interpret or resolve ambiguity in the specification, or user-facing behaviour, and
  anything that would change what the plan or the decisions log says the package does.
  A change to what report text means is Tier 1, and so is a fix that changes an
  exported function's documented behaviour or its signature. Present the options with
  trade-offs and your recommendation, then wait for my answer. Only the work that
  depends on the answer stops.
- **Tier 2: decide, then log.** A fix that makes code do what the plan, the decisions
  log and its documentation already say, its signature, names and documented
  behaviour unchanged: a bug in code the plan wrote, or a verified bug in an exported
  function. Typos, grammar and my writing rules in report text and documentation.
  Choices inside one task that touch none of the Tier 1 items, such as how a test is
  laid out. Record each in the milestone's ledger as
  `Ruling (T2): <what you decided>; why: <reason>; cost if wrong: <cost>`. The session
  copies them into the rulings digest, `milestones/<Mx>_rulings.md` in the folder named
  by `GPQ_PLANS_DIR`, which I read at each hand-off.
- **Tier 3: just do it.** Work with one right answer: applying a decision already
  taken, making a test, lint or style check pass without changing behaviour, FILEMAP
  rows that describe files as they are, and Minor review findings (see "Review
  findings"). The commit is the record.

Subagents tag every decision and finding they report T1, T2 or T3. They act on Tiers 2
and 3 and list what they did; they report Tier 1 items without acting on them. The
orchestrator relays every Tier 1 item to me unchanged and does not answer it itself.

This file takes precedence over skills. Where a superpowers skill tells you to rule on,
adjudicate or park something yourself (subagent-driven-development's conflict scan,
its fix-round breaker and the final review's residuals), do so only for Tier 2 and
Tier 3 items, each ruling a `Ruling (T2)` line; a Tier 1 item stops the work it affects
and comes to me as a question.

## Review findings

Reviewers grade each finding Critical, Important or Minor and tag its tier.

- A Tier 1 Critical or Important finding stops its task: it comes to me as a question,
  and the task waits for my answer.
- Every other Critical or Important finding enters the fix loop as Tier 2.
- Minor findings are Tier 3. The implementer applies them in the fix round when the
  finding states the fix and the fix stays in the task's own files; the session lists
  the rest in the milestone's minors list, `milestones/<Mx>_minors.md` in the folder
  named by `GPQ_PLANS_DIR`, which the milestone's PR description carries for me. A
  Minor finding whose fix would need a Tier 1 choice goes on the list, never applied.
- Every reviewer writes its full report to the file the dispatch names and returns only
  its verdict, its counts by tier and severity, and its Tier 1 questions verbatim.
- Stray edits. Before dispatching a task's reviewers, the session checks that
  `git status --porcelain` in the worktree prints nothing; if it prints anything, it
  stops and asks me, since those changes may be mine. It then records the time and
  copies the milestone's `.superpowers/sdd/<Mx>_tasks/` folder, its task plan
  (`milestones/<Mx>_tasks.md`), `decisions_log.md` and the rulings digest to its
  scratchpad folder; until the sweep, it writes nothing under the worktree or the
  folder named by `GPQ_PLANS_DIR`. After the reviewers return, it lists every file
  under the worktree (ignored files included, `.git` excluded) and under
  `GPQ_PLANS_DIR` whose last-write time is later than the recorded time. Any file other
  than those reviewers' own report files is a stray edit, and it may be mine. The
  session shows me each one with its diff and restores it only with my OK: a tracked
  file with `git restore --source=HEAD --staged --worktree -- <file>`, a copied file
  from its copy, a new file by deleting it; a file with neither a copy nor a version in
  `HEAD` is left for me. The copies, the listing and a restore from a copy run from one
  R script the session writes in its scratchpad with Write, an exception to "Files" for
  this check only.

## Before you start

- The approved plan and decisions log are in the folder named by `GPQ_PLANS_DIR`:
  `groundplotqc_plan.md` and `decisions_log.md`. Read the sections for your milestone.
- Resuming a milestone, read in this order: `handoff.md` in the folder named by
  `GPQ_PLANS_DIR`, this file, the milestone's ledger `progress.md` and its
  `global-constraints.md` (in the worktree's `.superpowers/sdd/<Mx>_tasks/`), and the
  brief of the task in hand. Read the milestone's task plan in full only once, for
  subagent-driven-development's conflict scan; after that, read only the parts a brief
  or the ledger points to.
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
  in `spec/`, the hand-kept configuration in `data-raw/magp/`, the package's own
  synthetic data and test fixtures. To ask: name the file, say why, and wait.
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
  `pkgdown::check_pkgdown()`, check the agent and skill files with
  `Rscript .github/scripts/check_agents.R`, and R CMD check with 0 errors, 0 warnings
  and 0 notes, except environment notes I have approved (use the check-runner agent).
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
