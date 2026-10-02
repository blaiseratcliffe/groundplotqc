---
name: add-report-section
description: Add or change a section in groundplotqc's provider report or national summary with the base-R builders. Use for any new report content.
---

1. Find the decision or plan section that defines the section. None: stop and ask.
2. Add text rows (English) for every string; no literal text in R code.
3. Build with `html_section`, `html_table`, `html_bar`; every value through `html_escape`; caps from settings.
4. Filter to the report unit before building (confidentiality, plan section 13).
5. Tests: escaping, caps, two-contributor confidentiality, snapshot.
6. Dispatch report-reviewer, then check-runner and filemap-maintainer.

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
