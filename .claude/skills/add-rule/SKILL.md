---
name: add-rule
description: Add or change a groundplotqc QC rule end to end (registry, rule set, text, check code, fixtures, docs). Use whenever a rule is created, renamed, split or its definition changes.
---

1. Find the decision that approves the rule (decision ID or plan section). None: stop and ask the user.
2. Choose the rule ID per NAMING.md; for a rename or split, add a `rule_id_map.csv` row.
3. Add the registry row in `R/rules_registry.R` (stage, layer, check type, default severity, default class, inputs, strategy, message_id; plan 9.7).
4. Add rule-set `rules` rows for the MAGPlot layer (table, attribute, severity or class override only where it differs from the registry, enabled; plan 3.5) and report text rows (English).
5. Dispatch fixture-builder for defect blocks; confirm they fail before the code exists.
6. Dispatch implementer for the check code (vectorised; plan 16.2).
7. Update the help page's list of rule IDs, NEWS.md and FILEMAP.md.
8. Dispatch spec-conformance-reviewer and datatable-reviewer, then check-runner.
9. Report decisions raised along the way to the user unchanged.

## Rules you always follow

- Do not make any decisions on your own without my input. If you are unsure, ask me.
  Report every decision and ambiguity to the orchestrator in your final report; never
  resolve one yourself. The orchestrator relays it to the user unchanged.
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
- In a skill, which runs in the main session, you are the orchestrator: put decisions
  and consent requests to the user directly.
