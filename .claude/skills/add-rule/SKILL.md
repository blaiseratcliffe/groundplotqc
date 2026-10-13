---
name: add-rule
description: Add or change a groundplotqc QC rule end to end (registry, rule set, text, check code, fixtures, docs). Use whenever a rule is created, renamed, split or its definition changes.
---

1. Find the decision that approves the rule (decision ID or plan section). None: stop and ask the user.
2. Choose the rule ID per NAMING.md; for a rename or split, add a `rule_id_map.csv` row.
3. Add the registry row in `R/rules_registry.R` (stage, layer, check type, default severity, default class, inputs, strategy, message_id, rule_version; plan 9.7). When a rule changes, raise its `rule_version` whenever its check behaves differently: the check-hash test in `test-rules_registry.R` fails on any change to a check function or a package function it calls (those of `R/report_text.R`, `R/report_html.R` and `R/rules_registry.R` aside), and its message gives the rows to record in `tests/testthat/fixtures/check_body_hashes.csv`. A change that leaves behaviour as it was records the new hashes only (D15.6, D15.13). The test follows package constants as well as functions, and a new `check_<family>()` fails it as new until its first row is recorded the same way.
4. Add rule-set `rules` rows for the MAGPlot layer to `data-raw/magp/rules_rules.csv` (table, attribute, severity or class override only where it differs from the registry, enabled; plan 3.5, D14.2), then rerun the build, and report text rows (English) in `report_text_engine.csv`, with MAGPlot's wording, where it differs, as a row of `report_text_magp.csv` with the same text_id (D14.8).
5. Dispatch fixture-builder for defect blocks; confirm they fail before the code exists.
6. Dispatch implementer for the check code (vectorised; plan 16.2), which calls package functions by name, never by string (`do.call("x")`, `match.fun("x")`, `get()`), so the check-hash test can follow it (D15.13).
7. Update the help page's list of rule IDs, NEWS.md and FILEMAP.md.
8. Dispatch spec-conformance-reviewer and datatable-reviewer, then check-runner.
9. Report decisions raised along the way to the user unchanged.

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
