---
name: benchmark
description: Measure groundplotqc run time and peak memory on synthetic data at 1x, 5x or 30x scale and compare with targets and the last saved result. Use at milestone gates and after performance-sensitive changes.
---

1. Generate synthetic data with `bench/generate.R` at the requested scale. Never use delivered data unless the user has consented for that run.
2. Dispatch check-runner to run `bench/run.R`; results go to the git-ignored `bench/results/` folder.
3. Delete `bench/data/` whenever `tests/testthat/helper-bench.R` changes or a run is interrupted: `bench/run.R` reuses any folder there that holds a CSV.
4. Compare each stage with plan section 16.1 targets and the previous result. On the 1x defect run, the report stage's `report_mb`, the largest report's size in MB, must stay under 10: plan 11.9's gate.
5. Over budget: profile the stage with `Rprof(memory.profiling = TRUE)` and report the top costs.
6. Report a table of stage, time, peak memory, target, previous, change. Note in the report that the scaled tables repeat the clean base's few values in each column, so string memory and per-value time may read lower than a real delivery's.

## Rules you always follow

- Decisions follow CLAUDE.md's three tiers; if you are unsure of a tier, it is Tier 1.
  Tier 1: don't act on it; report it to the orchestrator with the options, trade-offs
  and your recommendation, and the orchestrator relays it to the user unchanged.
  Tier 2: decide within your task and report each as a `Ruling (T2)` line (what, why,
  cost if wrong). Tier 3: just do it. Tag every decision and finding you report T1, T2
  or T3.
- Never open data files or connect to a database without consent the orchestrator has
  relayed for that file or database (see CLAUDE.md "Data"). If you need data, stop and
  report why.
- No copied code (CLAUDE.md "Code").
- Read FILEMAP.md before searching. Names follow NAMING.md.
- Create files with Write, change them with Edit; Bash runs commands only.
- Tag claims with evidence statuses (CLAUDE.md).
- Keep your final report to at most 300 words plus tables, in the report format below
  (a skill without one reports what it did and the decisions it raised). Questions for
  the user and items needing the user's decision are never cut to fit the cap; if they
  don't fit in 300 words, put them in a table.
- In a skill, which runs in the main session, you are the orchestrator: put Tier 1
  decisions and consent requests to the user directly, and add Tier 2 rulings to the
  milestone's rulings digest.
- For a finding or Tier 1 item about how the package handles some input, name that
  input first, and probe only with inputs inside CLAUDE.md's "Supported inputs". If it
  is outside that list, list the finding only under "Outside supported inputs" (one
  line: what fails, on what input), with no tier, severity or question. On a
  re-review, follow CLAUDE.md's re-review rule.
