---
name: update-spec
description: Bring a new version of a groundplotqc specification file into the package (spec/, compiled configuration, manifest, matrix template, rule-set drafts). Use when the user supplies a new dated spec file.
---

1. Ask the user to put the new dated file into `spec/` and remove the file it replaces (only the user brings files in or takes them out, D7.35; D9.30), and confirm which file it replaced. A committed spec file is never opened in Excel inside a checkout, since Excel can change a workbook's bytes without an edit: open a copy elsewhere, and undo a change with `git restore spec/<file>` (D12.34).
2. Dispatch spec-reader to diff old and new: tables, attributes, types, keys, codes, datasets, A2 rows.
3. List clashes, discrepancies and anything the precedence rule resolves for the user. Stop for answers.
4. Add matrix rows for new attributes with status "proposed" (in the working copy in `GPQ_PLANS_DIR`, D7.26; after M9, refresh it from `spec/` first, and ask the user to bring the result into `spec/` before the build, D7.35), so the matrix checks see every DD attribute (D9.30).
5. Run `data-raw/build_magp_config.R` (pre-flight runs inside it); fix nothing in spec files.
6. Draft cross-field rows from new DD text with review_status "draft" for the user's review (skipped at M10a, whose `build_crossfield_draft.R` drafts every row, D9.30).
7. Bump the package version; NEWS.md names the spec files that changed.
8. Dispatch check-runner (the manifest test must pass) and filemap-maintainer.

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
