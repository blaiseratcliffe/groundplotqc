"""Test suite for the groundplotqc guard hook (plan section 20.3; D10.15, D10.17).

Feeds each case to .claude/hooks/guard-launch.ps1 as JSON on stdin, the way Claude Code
does, and compares the verdict (allow, block, ask) with the expected one. Paths come
from this file's location and from GPQ_WORKTREE_ROOT and GPQ_PLANS_DIR, which
.claude/settings.local.json sets. No case reads a real data file: every path is judged,
not opened.

Run from the project folder or a worktree, after every change to the hook:

    python .claude/hooks/test_guard.py
"""
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HOOKS = Path(__file__).resolve().parent
LAUNCHER = HOOKS / "guard-launch.ps1"
ROOT = str(HOOKS.parents[1])                  # the checkout holding this file
WT_ROOT = os.environ.get("GPQ_WORKTREE_ROOT")
PLANS = os.environ.get("GPQ_PLANS_DIR")
if not WT_ROOT or not PLANS:
    sys.exit("GPQ_WORKTREE_ROOT and GPQ_PLANS_DIR must be set (see CLAUDE.md).")
P = ROOT
W = os.path.join(WT_ROOT, "hook-test")         # a worktree path; it needn't exist
ENV = dict(os.environ)
ENV_NO_WT = {k: v for k, v in os.environ.items() if k != "GPQ_WORKTREE_ROOT"}


def j(*parts):
    return os.path.join(*parts)


def bash(cmd, cwd=P):
    return {"tool_name": "Bash", "tool_input": {"command": cmd}, "cwd": cwd}


def read(path, cwd=P):
    return {"tool_name": "Read", "tool_input": {"file_path": path}, "cwd": cwd}


def grep(path=None, glob=None, type_=None, cwd=P):
    ti = {"pattern": "x"}
    for key, value in (("path", path), ("glob", glob), ("type", type_)):
        if value:
            ti[key] = value
    return {"tool_name": "Grep", "tool_input": ti, "cwd": cwd}


def edit(path, tool="Edit"):
    ti = {"file_path": path}
    ti.update({"old_string": "a", "new_string": "b"} if tool == "Edit" else {"content": "x"})
    return {"tool_name": tool, "tool_input": ti, "cwd": P}


def tool(name, ti):
    return {"tool_name": name, "tool_input": ti, "cwd": P}


def current_branch():
    out = subprocess.run(["git", "-C", P, "symbolic-ref", "--quiet", "--short", "HEAD"],
                         capture_output=True, text=True)
    return out.stdout.strip()


ON_MAIN = current_branch() == "main"

# (row, expected, description, call[, env])
CASES = [
    # Row 1: destructive git, pushes to main, force pushes, rebases of main
    ("1 push", "block", "git push origin main", bash("git push origin main")),
    ("1 push", "block", "git push origin HEAD:main", bash("git push origin HEAD:main")),
    ("1 push", "block", "push to refs/heads/main", bash("git push origin x:refs/heads/main")),
    ("1 push", "block", "git push --dry-run origin HEAD:main", bash("git push --dry-run origin HEAD:main")),
    ("1 push", "block", "git push (no refspec) after switching to main", bash("git switch main && git push")),
    ("1 push", "block", "git push --force", bash("git push --force origin feature-x")),
    ("1 push", "block", "git push -f", bash("git push -f origin feature-x")),
    ("1 push", "block", "--force-with-lease", bash("git push origin feature-x --force-with-lease")),
    ("1 push", "block", "+ refspec (D10.14)", bash("git push origin +feature-x")),
    ("1 push", "block", "--all (D10.14)", bash("git push --all origin")),
    ("1 push", "block", "--mirror (D10.14)", bash("git push --mirror origin")),
    ("1 push", "allow", "git push -u origin feature-x", bash("git push -u origin feature-x")),
    ("1 push", "allow", "git push (no refspec) on a feature branch", bash("git switch feature-x && git push")),
    ("1 push", "allow", "--force-if-includes (D10.14: left out)", bash("git push --force-if-includes origin feature-x")),
    ("1 reset", "block", "git reset --hard", bash("git reset --hard")),
    ("1 reset", "block", "git reset --hard HEAD~1", bash("git reset --hard HEAD~1")),
    ("1 reset", "allow", "git reset --soft HEAD~1", bash("git reset --soft HEAD~1")),
    ("1 clean", "block", "git clean -fd", bash("git clean -fd")),
    ("1 clean", "block", "git clean --force", bash("git clean --force")),
    ("1 clean", "allow", "git clean -n", bash("git clean -n")),
    ("1 branch", "block", "git branch -D", bash("git branch -D old")),
    ("1 branch", "block", "git branch --delete --force", bash("git branch --delete --force old")),
    ("1 branch", "allow", "git branch -a", bash("git branch -a")),
    ("1 checkout", "block", "git checkout -- .", bash("git checkout -- .")),
    ("1 checkout", "block", "git checkout . (D10.14)", bash("git checkout .")),
    ("1 checkout", "block", "git restore . (D10.14)", bash("git restore .")),
    ("1 checkout", "allow", "git restore --staged .", bash("git restore --staged .")),
    ("1 checkout", "allow", "git checkout -- R/x.R", bash("git checkout -- R/x.R")),
    ("1 checkout", "allow", "git checkout feature-x", bash("git checkout feature-x")),
    ("1 rebase", "block", "rebase while on main", bash("git switch main && git rebase origin/main")),
    ("1 rebase", "block", "git rebase x main (rebases main)", bash("git rebase feature-x main")),
    ("1 rebase", "allow", "rebase a feature branch onto main", bash("git switch feature-x && git rebase main")),
    # Row 2: commits on main
    ("2 commit", "block", "commit after switching to main", bash('git switch main && git commit -m "x"')),
    ("2 commit", "block", "commit --dry-run after checkout main", bash("git checkout main && git commit --dry-run")),
    ("2 commit", "allow", "commit after creating a branch", bash('git checkout -b feature-y && git commit -m "x"')),
    ("2 commit", "block" if ON_MAIN else "allow", "commit on the current branch (%s)" % current_branch(), bash('git commit -m "x"')),
    ("2 commit", "allow", "message naming a .csv (D10.14)", bash('git switch feature-x && git commit -m "Update thresholds_count.csv"')),
    ("2 commit", "allow", "combined -am message (D10.17)", bash('git switch feature-x && git commit -am "Update x.csv"')),
    ("2 commit", "allow", "attached -m\"...\" message (D10.17)", bash('git switch feature-x && git commit -m"Update x.csv"')),
    ("2 commit", "allow", "gh pr create title/body naming data (D10.14)", bash('gh pr create --title "x.csv" --body "mentions a.rds"')),
    # Row 3: data reads (Read, Grep, Bash), allowlist per checkout
    ("3 read", "block", "Read .Rdata at the root", read(j(P, "4_magpv2_final_tables.Rdata"))),
    ("3 read", "allow", "Read spec/*.xlsx", read(j(P, "spec", "20260925_magpv2_DD.xlsx"))),
    ("3 read", "allow", "Read data/magp_example.rda", read(j(P, "data", "magp_example.rda"))),
    ("3 read", "block", "Read data/other.rda", read(j(P, "data", "other.rda"))),
    ("3 read", "allow", "Read inst/extdata/magp/manifest.csv", read(j(P, "inst", "extdata", "magp", "manifest.csv"))),
    ("3 read", "allow", "Read tests/testthat/fx.csv", read(j(P, "tests", "testthat", "fx.csv"))),
    ("3 read", "allow", "Read bench/results/1x.csv", read(j(P, "bench", "results", "1x.csv"))),
    ("3 read", "allow", "Read the build tarball at the root", read(j(P, "groundplotqc_0.1.0.tar.gz"))),
    ("3 read", "block", "Read a tarball below the root", read(j(P, "sub", "groundplotqc_0.1.0.tar.gz"))),
    ("3 read", "allow", "Read the matrix working copy", read(j(PLANS, "matrix", "applicability_working.csv"))),
    ("3 read", "block", "Read another CSV in the plans folder", read(j(PLANS, "audit_validation_verdicts.csv"))),
    ("3 read", "block", "Read notes.txt (*.txt is data, D10.3)", read(j(P, "notes.txt"))),
    ("3 read", "block", "Read a data file outside any checkout", read(r"D:\deliveries\AB.rds")),
    ("3 read", "allow", "Read a non-data file", read(j(P, "CLAUDE.md"))),
    ("3 read", "allow", "Read a relative spec path", read("spec/20260925_magpv2_datasets.csv")),
    ("3 read", "block", "trailing dot (D10.17)", read(j(P, "4_magpv2_final_tables.Rdata."))),
    ("3 read", "block", "alternate data stream (D10.17)", read(j(P, "4_magpv2_final_tables.Rdata::$DATA"))),
    ("3 worktree", "allow", "Read spec/ in a worktree", read(j(W, "spec", "20260925_magpv2_DD.xlsx"), cwd=W)),
    ("3 worktree", "block", "Read a CSV at a worktree's root", read(j(W, "hook_test_other.csv"), cwd=W)),
    ("3 worktree", "block", "Read a file in the worktree root itself", read(j(WT_ROOT, "stray.csv"))),
    ("3 grep", "block", "Grep the root filtered to *.rds (D10.14)", grep(P, glob="*.rds")),
    ("3 grep", "block", "Grep the root, type csv (D10.14)", grep(P, type_="csv")),
    ("3 grep", "block", "Grep the .Rdata file", grep(j(P, "4_magpv2_final_tables.Rdata"))),
    ("3 grep", "allow", "Grep spec/ filtered to *.csv", grep(j(P, "spec"), glob="*.csv")),
    ("3 grep", "allow", "Grep tests/, type csv", grep(j(P, "tests"), type_="csv")),
    ("3 grep", "allow", "Grep the root filtered to *.R", grep(P, glob="*.R")),
    ("3 grep", "allow", "Grep the root excluding *.csv (D10.17)", grep(P, glob="!*.csv")),
    ("3 bash", "block", "cat the .Rdata", bash("cat 4_magpv2_final_tables.Rdata")),
    ("3 bash", "block", "Rscript reading a plans CSV", bash("Rscript -e \"x <- read.csv('plans/audit_validation_verdicts.csv')\"")),
    ("3 bash", "block", "python opening a root CSV", bash("python -c \"open('delivery.csv')\"")),
    ("3 bash", "block", "input redirect from a plans CSV", bash("wc -l < plans/audit_validation_verdicts.csv")),
    ("3 bash", "block", "a quoted path with spaces", bash('cat "temp pipeline scripts for development/x.csv"')),
    ("3 bash", "block", "message exempt, a real read still caught", bash('git commit -m "x" && cat thresholds_count.csv')),
    ("3 bash", "block", "a path holding a variable (D10.17)", bash("cat spec/$X/x.csv")),
    ("3 bash", "block", "a wildcard path, even in spec/ (D10.17)", bash("git add spec/*.xlsx")),
    ("3 bash", "block", "git show of a revision's file outside the allowlist", bash("git show HEAD:plans/x.csv")),
    ("3 bash", "allow", "git show of a revision's spec file (D10.17)", bash("git show abb74b6:spec/20260925_magpv2_datasets.csv")),
    ("3 bash", "allow", "Rscript reading a spec CSV", bash("Rscript -e \"x <- read.csv('spec/20260925_magpv2_datasets.csv')\"")),
    ("3 bash", "allow", "read.csv as a function name", bash("Rscript -e \"y <- lapply(files, read.csv)\"")),
    ("3 bash", "allow", "utils::write.csv call", bash("Rscript -e \"utils::write.csv(x, f)\"")),
    ("3 bash", "allow", "read.csv as a whole word (D10.17)", bash("grep -c read.csv R/x.R")),
    ("3 bash", "allow", "an extension alone, as in a regex (D10.17)", bash("grep -n '\\.csv' R/x.R")),
    ("3 bash", "allow", "a URL ending .gz", bash("curl -s https://cran.r-project.org/src/contrib/PACKAGES.gz -o /dev/null")),
    ("3 bash", "allow", "a 50,000-character word, quickly (D10.17)", bash("echo " + "a" * 50000)),
    # Row 4: package installs ask
    ("4 install", "ask", "install.packages", bash("Rscript -e \"install.packages('data.table')\"")),
    ("4 install", "ask", "remotes::install_github", bash("Rscript -e \"remotes::install_github('a/b')\"")),
    ("4 install", "ask", "pak::pkg_install", bash("Rscript -e \"pak::pkg_install('a')\"")),
    ("4 install", "allow", "library(data.table)", bash("Rscript -e \"library(data.table)\"")),
    # Row 5: Bash writing files (D6.1, D8.22)
    ("5 write", "block", "echo > file", bash("echo hi > out.R")),
    ("5 write", "block", "echo >> file", bash("echo hi >> notes.md")),
    ("5 write", "block", "git log > file", bash("git log > log.md")),
    ("5 write", "block", "&> file", bash("Rscript x.R &> run.log")),
    ("5 write", "block", "tee", bash("ls | tee files.md")),
    ("5 write", "block", "sed -i", bash("sed -i 's/a/b/' R/x.R")),
    ("5 write", "block", "heredoc, unquoted delimiter", bash("cat <<EOF\nx\nEOF")),
    ("5 write", "block", "heredoc, unquoted delimiter, <<-", bash("cat <<-END\n\tx\n\tEND")),
    ("5 write", "allow", "2>&1", bash("Rscript -e \"x <- 1\" 2>&1")),
    ("5 write", "allow", "> /dev/null", bash("git status > /dev/null")),
    ("5 write", "allow", "2>/dev/null", bash("git status 2>/dev/null")),
    ("5 write", "allow", "> NUL", bash("git status > NUL")),
    ("5 write", "allow", "> /dev/tty (D10.17)", bash("ls > /dev/tty")),
    ("5 write", "allow", "quoted R comparison", bash("Rscript -e \"if (1 > 0) print(1)\"")),
    ("5 write", "allow", "> inside quotes", bash("echo \"a > b\"")),
    ("5 write", "allow", "[[ a > b ]] (D10.17)", bash("[[ \"$a\" > \"$b\" ]] && echo y")),
    ("5 write", "allow", "$((1<<3)) (D10.17)", bash("echo $((1<<3))")),
    ("5 write", "allow", "(( 2 > 1 )) (D10.17)", bash("(( 2 > 1 )) && echo y")),
    ("5 write", "allow", "quoted heredoc with > in body", bash("cat <<'EOF'\nx > y\nEOF")),
    ("5 write", "allow", "double-quoted heredoc with >> in body", bash("python - <<\"EOF\"\nprint(1 >> 0)\nEOF")),
    ("5 write", "allow", "sed -n", bash("sed -n '1,5p' R/x.R")),
    # Row 6: Glob
    ("6 glob", "allow", "Glob **/*.rds", tool("Glob", {"pattern": "**/*.rds"})),
    # Row 7: the consent list
    ("7 consent", "block", "Edit the consent list", edit(j(P, ".claude", "data_consent.local.txt"))),
    ("7 consent", "block", "Write the consent list", edit(j(P, ".claude", "data_consent.local.txt"), tool="Write")),
    ("7 consent", "block", "Write the consent list with a trailing dot (D10.17)", edit(j(P, ".claude", "data_consent.local.txt."), tool="Write")),
    # Row 8: spec/
    ("8 spec", "block", "Edit a spec file", edit(j(P, "spec", "20260925_magpv2_DD.xlsx"))),
    ("8 spec", "block", "Write a new spec file", edit(j(P, "spec", "20261001_magpv2_new.csv"), tool="Write")),
    ("8 spec", "block", "Write spec/ in a worktree", edit(j(W, "spec", "x.csv"), tool="Write")),
    ("8 spec", "block", "spec. folder (D10.17)", edit(j(P, "spec.", "x.csv"), tool="Write")),
    ("8 spec", "block", "worktree spec/ without GPQ_WORKTREE_ROOT (D10.17)", edit(j(W, "spec", "x.csv"), tool="Write"), ENV_NO_WT),
    ("8 spec", "allow", "Write R/x.R", edit(j(P, "R", "x.R"), tool="Write")),
    ("8 spec", "allow", "Edit FILEMAP.md", edit(j(P, "FILEMAP.md"))),
    # Row 9: NotebookEdit as Edit, MCP tools as Read
    ("9 notebook", "block", "NotebookEdit in spec/", tool("NotebookEdit", {"notebook_path": j(P, "spec", "x.ipynb"), "new_source": "x"})),
    ("9 notebook", "allow", "NotebookEdit elsewhere", tool("NotebookEdit", {"notebook_path": j(P, "vignettes", "x.ipynb"), "new_source": "x"})),
    ("9 mcp", "block", "MCP tool with a data path outside the allowlist", tool("mcp__files__read", {"path": r"D:\deliveries\AB.rds"})),
    ("9 mcp", "allow", "MCP tool with a spec path", tool("mcp__files__read", {"path": j(P, "spec", "20260925_magpv2_DD.xlsx")})),
    ("9 mcp", "allow", "MCP tool without paths", tool("mcp__search__query", {"query": "hello"})),
    # D10.17: bypasses through shell syntax and wrappers
    ("bypass", "block", "if ...; then git push origin main", bash("if true; then git push origin main; fi")),
    ("bypass", "block", "for ...; do git branch -D", bash("for b in a; do git branch -D $b; done")),
    ("bypass", "block", "{ git reset --hard; }", bash("{ git reset --hard; }")),
    ("bypass", "block", "! git push --force", bash("! git push --force origin x")),
    ("bypass", "block", "bash -c with a push to main", bash("bash -c \"git push origin main\"")),
    ("bypass", "block", "sh -c with git reset --hard", bash("sh -c 'git reset --hard'")),
    ("bypass", "block", "bash -lc with a file write", bash("bash -lc \"echo hi > out.R\"")),
    ("bypass", "block", "timeout 30 git push origin main", bash("timeout 30 git push origin main")),
    ("bypass", "block", "timeout -k 5 30 git push origin main", bash("timeout -k 5 30 git push origin main")),
    ("bypass", "block", "env -i FOO=1 git push origin main", bash("env -i FOO=1 git push origin main")),
    ("bypass", "block", "nice -n 5 git reset --hard", bash("nice -n 5 git reset --hard")),
    ("bypass", "allow", "bash -c git status", bash("bash -c \"git status\"")),
    ("bypass", "allow", "if git diff --quiet; then echo same; fi", bash("if git diff --quiet; then echo same; fi")),
    ("limit", "allow", "eval is left to CLAUDE.md (20.3 limits)", bash("eval \"git push origin main\"")),
    ("limit", "allow", "xargs is left to CLAUDE.md (20.3 limits)", bash("echo main | xargs git push origin")),
    # D10.17: new rows
    ("new rows", "block", "git merge on main", bash("git switch main && git merge feature-x")),
    ("new rows", "block", "git cherry-pick on main", bash("git switch main && git cherry-pick abc123")),
    ("new rows", "block", "git revert on main", bash("git switch main && git revert abc123")),
    ("new rows", "block", "git am on main", bash("git switch main && git am fix.patch")),
    ("new rows", "block", "git pull on main", bash("git switch main && git pull")),
    ("new rows", "allow", "git pull --ff-only on main", bash("git switch main && git pull --ff-only")),
    ("new rows", "allow", "git merge main into a feature branch", bash("git switch feature-x && git merge main")),
    ("new rows", "block", "git branch -f main", bash("git branch -f main HEAD~1")),
    ("new rows", "block", "git branch -m main", bash("git branch -m main old")),
    ("new rows", "block", "git update-ref refs/heads/main", bash("git update-ref refs/heads/main HEAD")),
    ("new rows", "block", "cp into spec/", bash("cp x.xlsx spec/")),
    ("new rows", "block", "mv out of spec/", bash("mv spec/20260925_magpv2_DD.xlsx old.xlsx")),
    ("new rows", "block", "rm a spec file", bash("rm spec/20260925_magpv2_DD.xlsx")),
    ("new rows", "block", "touch a new spec file", bash("touch spec/new.csv")),
    ("new rows", "block", "git rm a spec file", bash("git rm spec/20260925_magpv2_DD.xlsx")),
    ("new rows", "block", "git mv within spec/", bash("git mv spec/a.csv spec/b.csv")),
    ("new rows", "block", "cp onto the consent list", bash("cp a.md .claude/data_consent.local.txt")),
    ("new rows", "block", "cp from spec/ to R/ (D10.18)", bash("cp spec/20260925_magpv2_DD.xlsx R/")),
    ("new rows", "block", "touch R/x.R (D10.18)", bash("touch R/x.R")),
    ("new rows", "block", "cp R/a.R R/b.R (D10.18)", bash("cp R/a.R R/b.R")),
    ("new rows", "block", "install -m 644 (D10.18)", bash("install -m 644 a.R R/a.R")),
    ("new rows", "block", "rsync (D10.18)", bash("rsync -a a/ b/")),
    ("new rows", "block", "truncate (D10.18)", bash("truncate -s 0 R/x.R")),
    ("new rows", "block", "ln -s (D10.18)", bash("ln -s a.R b.R")),
    ("new rows", "allow", "mv R/a.R R/b.R (D10.18)", bash("mv R/a.R R/b.R")),
    ("new rows", "allow", "git mv R/a.R R/b.R (D10.18)", bash("git mv R/a.R R/b.R")),
    ("new rows", "allow", "git rm R/a.R (D10.18)", bash("git rm R/a.R")),
    ("new rows", "block", "rm -r", bash("rm -r tmpdir")),
    ("new rows", "block", "rm -rf", bash("rm -rf tmpdir")),
    ("new rows", "block", "rm -f -r", bash("rm -f -r tmpdir")),
    ("new rows", "block", "rm -R", bash("rm -R tmpdir")),
    ("new rows", "block", "rm --recursive", bash("rm --recursive tmpdir")),
    ("new rows", "allow", "rm a single file", bash("rm x.R")),
    ("new rows", "block", "gh api merge endpoint", bash("gh api -X PUT repos/o/r/pulls/1/merge")),
    ("new rows", "allow", "gh api reading a PR", bash("gh api repos/o/r/pulls/1")),
    # Internal errors (D10.14, D10.17)
    ("err", "block", "malformed input blocks", '{"tool_name":"Bash","tool_input":{"command":"git status"'),
    ("err", "block", "empty input blocks (D10.17)", ""),
    ("err", "block", "input without tool_name blocks (D10.17)", '{"tool_input":{"command":"git status"}}'),
    ("err", "allow", "malformed input, Edit on guard.ps1, allowed",
     '{"tool_name":"Edit","tool_input":{"file_path":"' + j(P, ".claude", "hooks", "guard.ps1").replace("\\", "\\\\") + '","old_string":'),
]


def run(call, env=None, launcher=LAUNCHER):
    raw = call if isinstance(call, str) else json.dumps(call)
    start = time.time()
    p = subprocess.run(
        ["powershell.exe", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", str(launcher)],
        input=raw.encode("utf-8"), capture_output=True, env=env or ENV, timeout=60)
    seconds = time.time() - start
    out = p.stdout.decode("utf-8", "replace").strip()
    err = p.stderr.decode("utf-8", "replace").strip()
    if p.returncode == 2:
        return "block", err, seconds
    if p.returncode == 0 and '"permissionDecision":"ask"' in out:
        return "ask", out, seconds
    if p.returncode == 0:
        return "allow", out or err, seconds
    return "exit%d" % p.returncode, err or out, seconds


def parse_error_cases():
    """The launcher blocks when guard.ps1 doesn't parse, except an edit of the guard."""
    folder = Path(tempfile.mkdtemp(prefix="gpq_guard_"))
    try:
        shutil.copy(LAUNCHER, folder / LAUNCHER.name)
        (folder / "guard.ps1").write_text("param([string]$RawInput)\nif (\n", encoding="ascii")
        launcher = folder / LAUNCHER.name
        broken = run(bash("git status"), launcher=launcher)
        self_edit = run(edit(j(P, ".claude", "hooks", "guard.ps1")), launcher=launcher)
    finally:
        shutil.rmtree(folder, ignore_errors=True)
    return [("err", "block", "guard.ps1 that doesn't parse blocks (D10.17)", broken),
            ("err", "allow", "guard.ps1 that doesn't parse, its own edit allowed", self_edit)]


def main():
    with ThreadPoolExecutor(max_workers=8) as pool:
        results = list(pool.map(lambda c: run(c[3], c[4] if len(c) > 4 else None), CASES))
    rows = [(c[0], c[1], c[2], r) for c, r in zip(CASES, results)] + parse_error_cases()
    failed = 0
    for row, expected, desc, (verdict, message, seconds) in rows:
        ok = verdict == expected and seconds < 10
        failed += not ok
        note = "" if ok and verdict == "allow" else "  | " + message.replace("groundplotqc guard: ", "")[:100]
        slow = "" if seconds < 10 else "  [%.1f s]" % seconds
        print("%s  %-10s %-6s %s%s%s" % ("PASS" if ok else "FAIL", row, verdict, desc, slow, note))
    print("\n%d cases, %d passed, %d failed" % (len(rows), len(rows) - failed, failed))
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
