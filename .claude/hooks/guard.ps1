# groundplotqc guard hook (plan section 20.3; D5.13, D8.22, D9.4, D9.6, D9.7, D10.2,
# D10.12, D10.14).
#
# Claude Code runs this before Bash, Read, Grep, Glob, Edit, Write, NotebookEdit and MCP
# tool calls, passing the call as JSON on stdin. Exit 0 allows the call; exit 2 blocks
# it and returns the message on stderr to Claude; an "ask" decision is printed as JSON
# on stdout with exit 0. Windows PowerShell 5.1; keep this file ASCII.
#
# What it can't see (plan 20.3, D9.4): reads inside R scripts, database connections
# opened from R, a Grep over a folder with no data-extension filter, and provider data
# in files without a data extension. CLAUDE.md "Data" covers those.

$ErrorActionPreference = 'Stop'

# The project folder: this file is <project>\.claude\hooks\guard.ps1.
$ProjectDir = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$ConsentFile = Join-Path $ProjectDir '.claude\data_consent.local.txt'

# Data extensions: the .gitignore list (plan 19.2; *.txt kept, D10.3).
$DataExtPattern = 'rdata|rda|rds|csv|tsv|txt|xlsx|xls|sqlite|gpkg|accdb|mdb|shp|shx|dbf|prj|cpg|tif|tiff|zip|gz|parquet|feather|fst|qs'

# A path with a data extension inside a longer word, such as R code; not a function
# call such as read.csv( (plan 20.3 row 3).
$PathInText = "(?i)[^\s'""(),;=<>|&{}\[\]]*\.(?:$DataExtPattern)(?![A-Za-z0-9_(])"

# Folders allowlisted for data files, relative to a checkout root (plan 20.3 row 3).
$AllowedPrefixes = @('spec/', 'inst/extdata/', 'tests/', 'bench/')

$script:Consents = $null
$RawInput = ''

function Stop-Call([string]$Message) {
    [Console]::Error.WriteLine("groundplotqc guard: $Message")
    exit 2
}

function Request-Approval([string]$Reason) {
    $text = $Reason.Replace('\', '\\').Replace('"', '\"')
    [Console]::Out.Write('{"hookSpecificOutput":{"hookEventName":"PreToolUse",' +
        '"permissionDecision":"ask","permissionDecisionReason":"' + $text + '"}}')
    exit 0
}

function Read-HookInput {
    $stdin = [Console]::OpenStandardInput()
    $buffer = New-Object System.IO.MemoryStream
    $stdin.CopyTo($buffer)
    return [System.Text.Encoding]::UTF8.GetString($buffer.ToArray()).TrimStart([char]0xFEFF)
}

function ConvertFrom-HookJson([string]$Text) {
    Add-Type -AssemblyName System.Web.Extensions
    $serializer = New-Object System.Web.Script.Serialization.JavaScriptSerializer
    $serializer.MaxJsonLength = [int]::MaxValue
    return $serializer.DeserializeObject($Text)
}

function Get-Key($Map, [string]$Key) {
    if ($Map -is [System.Collections.IDictionary] -and $Map.ContainsKey($Key)) {
        return $Map[$Key]
    }
    return $null
}

# ---------------------------------------------------------------------------------
# Paths

# Full Windows path for a path as written in a tool call: quotes stripped, Git Bash
# drive form (/d/...) and ~ converted, relative paths taken from Base, . and .. folded.
function Resolve-GuardPath([string]$Path, [string]$Base) {
    $p = $Path.Trim()
    if ($p.Length -ge 2 -and (($p[0] -eq '"' -and $p[-1] -eq '"') -or
            ($p[0] -eq "'" -and $p[-1] -eq "'"))) {
        $p = $p.Substring(1, $p.Length - 2)
    }
    if ($p -match '^~(?=[\\/]|$)') { $p = $env:USERPROFILE + $p.Substring(1) }
    if ($p -match '^/([A-Za-z])(?=/|$)') { $p = $Matches[1] + ':' + $p.Substring(2) }
    $p = $p.Replace('/', '\')
    if ($p -match '^[A-Za-z]:$') { $p += '\' }
    if ($p -notmatch '^[A-Za-z]:\\' -and -not $p.StartsWith('\\')) {
        if ($p.StartsWith('\')) {
            $p = $Base.Substring(0, 2) + $p
        } else {
            $p = $Base.TrimEnd('\') + '\' + $p
        }
    }
    $prefix = ''
    $rest = $p
    if ($p -match '^([A-Za-z]:)\\(.*)$') {
        $prefix = $Matches[1]
        $rest = $Matches[2]
    } elseif ($p -match '^(\\\\[^\\]+\\[^\\]+)(.*)$') {
        $prefix = $Matches[1]
        $rest = $Matches[2]
    }
    $parts = New-Object System.Collections.Generic.List[string]
    foreach ($segment in $rest.Split('\')) {
        if ($segment -eq '' -or $segment -eq '.') { continue }
        if ($segment -eq '..') {
            if ($parts.Count -gt 0) { $parts.RemoveAt($parts.Count - 1) }
            continue
        }
        $parts.Add($segment)
    }
    return $prefix + '\' + ($parts -join '\')
}

function Test-Under([string]$Path, [string]$Dir) {
    $d = $Dir.TrimEnd('\')
    return ($Path.Equals($d, [StringComparison]::OrdinalIgnoreCase) -or
        $Path.StartsWith($d + '\', [StringComparison]::OrdinalIgnoreCase))
}

# The checkout a path is in: the project folder or a worktree under GPQ_WORKTREE_ROOT
# (D9.7). Paths anywhere else are in no checkout.
function Get-CheckoutRoot([string]$FullPath) {
    if (Test-Under $FullPath $ProjectDir) { return $ProjectDir }
    if ($env:GPQ_WORKTREE_ROOT) {
        $worktrees = (Resolve-GuardPath $env:GPQ_WORKTREE_ROOT $ProjectDir).TrimEnd('\')
        if (Test-Under $FullPath $worktrees) {
            $first = $FullPath.Substring($worktrees.Length).TrimStart('\').Split('\')[0]
            if ($first) { return $worktrees + '\' + $first }
        }
    }
    return $null
}

# Path inside its checkout, lower case with forward slashes, such as spec/x.xlsx.
function Get-CheckoutPath([string]$FullPath, [string]$Root) {
    $inside = $FullPath.Substring($Root.TrimEnd('\').Length).TrimStart('\')
    return $inside.Replace('\', '/').ToLowerInvariant()
}

function Test-DataExtension([string]$Path) {
    return ($Path -match "(?i)\.($DataExtPattern)$")
}

# ---------------------------------------------------------------------------------
# Data reads (plan 20.3 row 3; consent list format D10.12)

# Consent list: one line per file, "YYYY-MM-DD <full path>"; # starts a comment.
# The date records the consent and isn't checked.
function Get-Consents {
    $list = New-Object System.Collections.Generic.List[string]
    if (Test-Path -LiteralPath $ConsentFile) {
        foreach ($line in [System.IO.File]::ReadAllLines($ConsentFile)) {
            $text = $line.Trim()
            if ($text -eq '' -or $text.StartsWith('#')) { continue }
            if ($text -match '^\d{4}-\d{2}-\d{2}\s+(.+)$') {
                $list.Add((Resolve-GuardPath $Matches[1] $ProjectDir))
            }
        }
    }
    return , $list
}

function Test-DataAllowed([string]$FullPath) {
    $root = Get-CheckoutRoot $FullPath
    if ($root) {
        $inside = Get-CheckoutPath $FullPath $root
        foreach ($prefix in $AllowedPrefixes) {
            if ($inside.StartsWith($prefix)) { return $true }
        }
        if ($inside -eq 'data/magp_example.rda') { return $true }
        if ($inside -match '^groundplotqc_[^/]+\.tar\.gz$') { return $true }
    }
    if ($env:GPQ_PLANS_DIR) {
        $matrix = Resolve-GuardPath ($env:GPQ_PLANS_DIR.TrimEnd('\', '/') +
            '\matrix\applicability_working.csv') $ProjectDir
        if ($FullPath.Equals($matrix, [StringComparison]::OrdinalIgnoreCase)) { return $true }
    }
    if ($null -eq $script:Consents) { $script:Consents = Get-Consents }
    foreach ($consent in $script:Consents) {
        if ($FullPath.Equals($consent, [StringComparison]::OrdinalIgnoreCase)) { return $true }
    }
    return $false
}

function Assert-DataRead([string]$Candidate, [string]$Base) {
    if (-not $Candidate) { return }
    if (-not (Test-DataExtension $Candidate)) { return }
    $full = Resolve-GuardPath $Candidate $Base
    if (Test-DataAllowed $full) { return }
    Stop-Call ("reading '$Candidate' needs the user's consent: it has a data extension " +
        'and is outside spec/, data/magp_example.rda, inst/extdata/, tests/, bench/, the ' +
        'build tarball and the matrix working copy. Ask the user, naming the file and why ' +
        '(CLAUDE.md "Data"); with consent they add it to .claude/data_consent.local.txt.')
}

# Paths with a data extension in one word of a command or one string of a tool input.
function Get-DataCandidates([string]$Word) {
    $found = New-Object System.Collections.Generic.List[string]
    if (-not $Word -or $Word.Contains('://')) { return , $found }
    $text = $Word
    if ($text -match '^--?[A-Za-z][A-Za-z0-9-]*=(.*)$') { $text = $Matches[1] }
    if ((Test-DataExtension $text) -and $text -notmatch "[()'"",;]") {
        $found.Add($text)
        return , $found
    }
    foreach ($m in [regex]::Matches($text, $PathInText)) {
        $leaf = ($m.Value -split '[\\/]')[-1]
        if ($leaf -match '^\.[A-Za-z]+$') { continue }
        $found.Add($m.Value)
    }
    return , $found
}

# ---------------------------------------------------------------------------------
# Bash: a small shell lexer (quotes, escapes, comments, operators, redirections and
# heredocs), enough to find file writes, git commands and file names.

function Read-ShellCommand([string]$Command) {
    $result = @{
        Tokens          = New-Object System.Collections.Generic.List[object]
        Writes          = New-Object System.Collections.Generic.List[string]
        UnquotedHeredoc = $false
        HeredocWord     = ''
    }
    $tokens = $result.Tokens
    $word = New-Object System.Text.StringBuilder
    $state = @{ InWord = $false; Expect = '' }
    $heredocs = New-Object System.Collections.Generic.List[object]
    $n = $Command.Length
    $i = 0

    # End the current word. A word after > or >> is a write target (allowed only for
    # /dev/null, NUL and the standard streams); every other word is kept.
    $flush = {
        if ($state.InWord) {
            $text = $word.ToString()
            if ($state.Expect -eq 'write') {
                if ($text -notmatch '^(?i)(/dev/null|nul|/dev/stdout|/dev/stderr)$') {
                    $result.Writes.Add($text)
                }
            } else {
                $tokens.Add(@{ Kind = 'word'; Text = $text })
            }
            $state.Expect = ''
            $null = $word.Clear()
            $state.InWord = $false
        }
    }
    $addOp = {
        param([string]$Op)
        & $flush
        $state.Expect = ''
        $tokens.Add(@{ Kind = 'op'; Text = $Op })
    }
    $stops = " `t`r`n;|&<>()"

    while ($i -lt $n) {
        $c = $Command[$i]

        if ($c -eq "`n") {
            & $addOp ';'
            $i++
            # Skip the bodies of heredocs opened on the line just ended.
            foreach ($doc in $heredocs) {
                while ($i -lt $n) {
                    $end = $Command.IndexOf("`n", $i)
                    if ($end -lt 0) { $end = $n }
                    $line = $Command.Substring($i, $end - $i).TrimEnd("`r")
                    $i = [Math]::Min($end + 1, $n)
                    if ($doc.StripTabs) { $line = $line.TrimStart("`t") }
                    if ($line -eq $doc.Delimiter) { break }
                }
            }
            $heredocs.Clear()
            continue
        }
        if ($c -eq ' ' -or $c -eq "`t" -or $c -eq "`r") {
            & $flush
            $i++
            continue
        }
        if ($c -eq "'") {
            $close = $Command.IndexOf("'", $i + 1)
            if ($close -lt 0) { $close = $n }
            $null = $word.Append($Command.Substring($i + 1, $close - $i - 1))
            $state.InWord = $true
            $i = $close + 1
            continue
        }
        if ($c -eq '"') {
            $i++
            while ($i -lt $n -and $Command[$i] -ne '"') {
                if ($Command[$i] -eq '\' -and $i + 1 -lt $n -and
                    '"\$`'.Contains([string]$Command[$i + 1])) {
                    $null = $word.Append($Command[$i + 1])
                    $i += 2
                    continue
                }
                $null = $word.Append($Command[$i])
                $i++
            }
            $i++
            $state.InWord = $true
            continue
        }
        if ($c -eq '\') {
            if ($i + 1 -lt $n) {
                if ($Command[$i + 1] -ne "`n") {
                    $null = $word.Append($Command[$i + 1])
                    $state.InWord = $true
                }
                $i += 2
            } else {
                $i++
            }
            continue
        }
        if ($c -eq '#' -and -not $state.InWord) {
            $end = $Command.IndexOf("`n", $i)
            if ($end -lt 0) { $end = $n }
            $i = $end
            continue
        }
        if ($c -eq ';' -or $c -eq '(' -or $c -eq ')') {
            & $addOp ([string]$c)
            $i++
            continue
        }
        if ($c -eq '|') {
            & $addOp '|'
            if ($i + 1 -lt $n -and ($Command[$i + 1] -eq '|' -or $Command[$i + 1] -eq '&')) {
                $i += 2
            } else {
                $i++
            }
            continue
        }
        if ($c -eq '&') {
            if ($i + 1 -lt $n -and $Command[$i + 1] -eq '>') {
                # &> and &>>: stdout and stderr to a file.
                & $flush
                $i += 2
                if ($i -lt $n -and $Command[$i] -eq '>') { $i++ }
                $state.Expect = 'write'
                continue
            }
            & $addOp '&'
            if ($i + 1 -lt $n -and $Command[$i + 1] -eq '&') { $i += 2 } else { $i++ }
            continue
        }
        if ($c -eq '<' -or $c -eq '>') {
            # A word of digits right before the operator is a file descriptor (2>&1).
            if ($state.InWord -and $word.ToString() -match '^\d+$') {
                $null = $word.Clear()
                $state.InWord = $false
            } else {
                & $flush
            }
            $next = [char]0
            if ($i + 1 -lt $n) { $next = $Command[$i + 1] }
            if ($next -eq '(') {
                # Process substitution <( ) or >( ): a command, not a file.
                & $addOp '('
                $i += 2
                continue
            }
            if ($c -eq '<') {
                if ($next -eq '<') {
                    if ($i + 2 -lt $n -and $Command[$i + 2] -eq '<') {
                        # Here-string <<<: its text is kept as a word.
                        $state.Expect = 'herestring'
                        $i += 3
                        continue
                    }
                    # Heredoc << or <<-: read the delimiter word; quoted or escaped
                    # means the body is literal (D6.1, D8.22).
                    $i += 2
                    $strip = $false
                    if ($i -lt $n -and $Command[$i] -eq '-') {
                        $strip = $true
                        $i++
                    }
                    while ($i -lt $n -and ($Command[$i] -eq ' ' -or $Command[$i] -eq "`t")) { $i++ }
                    $delimiter = New-Object System.Text.StringBuilder
                    $quotedDelimiter = $false
                    while ($i -lt $n -and -not $stops.Contains([string]$Command[$i])) {
                        $d = $Command[$i]
                        if ($d -eq "'" -or $d -eq '"') {
                            $quotedDelimiter = $true
                            $close = $Command.IndexOf([string]$d, $i + 1)
                            if ($close -lt 0) { $close = $n }
                            $null = $delimiter.Append($Command.Substring($i + 1, $close - $i - 1))
                            $i = $close + 1
                            continue
                        }
                        if ($d -eq '\') {
                            $quotedDelimiter = $true
                            $i++
                            if ($i -lt $n) {
                                $null = $delimiter.Append($Command[$i])
                                $i++
                            }
                            continue
                        }
                        $null = $delimiter.Append($d)
                        $i++
                    }
                    if (-not $quotedDelimiter) {
                        $result.UnquotedHeredoc = $true
                        $result.HeredocWord = $delimiter.ToString()
                    }
                    $heredocs.Add(@{ Delimiter = $delimiter.ToString(); StripTabs = $strip })
                    continue
                }
                if ($next -eq '>') {
                    # <> opens a file for reading and writing.
                    $state.Expect = 'write'
                    $i += 2
                    continue
                }
                if ($next -eq '&') {
                    $i += 2
                    while ($i -lt $n -and ([string]$Command[$i]) -match '[0-9-]') { $i++ }
                    continue
                }
                # < file: the file is read; the word is kept and scanned for data.
                $i++
                continue
            }
            # >, >>, >|, and >&n (a descriptor copy, not a file).
            $j = $i + 1
            if ($j -lt $n -and ($Command[$j] -eq '>' -or $Command[$j] -eq '|')) { $j++ }
            if ($j -lt $n -and $Command[$j] -eq '&') {
                $k = $j + 1
                if ($k -lt $n -and ([string]$Command[$k]) -match '[0-9-]') {
                    while ($k -lt $n -and ([string]$Command[$k]) -match '[0-9-]') { $k++ }
                    $i = $k
                    continue
                }
                $j++
            }
            $state.Expect = 'write'
            $i = $j
            continue
        }
        $null = $word.Append($c)
        $state.InWord = $true
        $i++
    }
    & $flush
    return $result
}

# Words of each simple command, split at ; | & && || ( ) and newlines.
function Get-Segments($Tokens) {
    $segments = New-Object System.Collections.Generic.List[object]
    $current = New-Object System.Collections.Generic.List[string]
    foreach ($token in $Tokens) {
        if ($token.Kind -eq 'op') {
            if ($current.Count -gt 0) { $segments.Add($current) }
            $current = New-Object System.Collections.Generic.List[string]
        } else {
            $current.Add($token.Text)
        }
    }
    if ($current.Count -gt 0) { $segments.Add($current) }
    return , $segments
}

# Positions of message text, which isn't read as file names (D10.14): the word after
# -m or --message (git) and after -t, --title, -b or --body (gh), and --flag=text.
function Get-MessageIndexes([string]$Name, $Words) {
    $skip = New-Object System.Collections.Generic.List[int]
    $flags = @()
    if ($Name -eq 'git') { $flags = @('-m', '--message') }
    if ($Name -eq 'gh') { $flags = @('-t', '--title', '-b', '--body') }
    for ($w = 0; $w -lt $Words.Count; $w++) {
        if ($flags -ccontains $Words[$w]) { $skip.Add($w + 1) }
        foreach ($flag in $flags) {
            if ($flag.StartsWith('--') -and $Words[$w].StartsWith($flag + '=')) { $skip.Add($w) }
        }
    }
    return , $skip
}

function Get-CurrentBranch([string]$Dir) {
    $previous = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    try {
        $branch = & git -C $Dir symbolic-ref --quiet --short HEAD 2>$null
        $code = $LASTEXITCODE
    } finally {
        $ErrorActionPreference = $previous
    }
    if ($code -ne 0 -or -not $branch) { return '' }
    return ([string]$branch).Trim()
}

function Get-WordsAfter($Words, [int]$Index) {
    if ($Index + 1 -ge $Words.Count) { return , @() }
    return , @($Words[($Index + 1)..($Words.Count - 1)])
}

# git push (plan 20.3 rows 1 and 2; D10.14): force pushes, + refspecs, --all and
# --mirror, and any push whose destination is main.
function Test-GitPush($Words, [string]$Dir) {
    $positional = New-Object System.Collections.Generic.List[string]
    for ($p = 0; $p -lt $Words.Count; $p++) {
        $w = $Words[$p]
        if ($w -eq '--force' -or $w -like '--force-with-lease*' -or
            $w -cmatch '^-[A-Za-z]*f[A-Za-z]*$') {
            Stop-Call 'force pushes are blocked (D5.13). Ask the user.'
        }
        if ($w -eq '--all' -or $w -eq '--mirror') {
            Stop-Call "git push $w is blocked: it pushes main too (D5.13, D10.14)."
        }
        if (@('-o', '--push-option', '--repo', '--receive-pack', '--exec') -contains $w) {
            $p++
            continue
        }
        if ($w.StartsWith('-')) { continue }
        $positional.Add($w)
    }
    $toMain = 'pushing to main is blocked: main changes only through a PR the user ' +
        'merges (D5.13). Push the milestone branch instead.'
    if ($positional.Count -le 1) {
        if ((Get-CurrentBranch $Dir) -eq 'main') { Stop-Call $toMain }
        return
    }
    for ($r = 1; $r -lt $positional.Count; $r++) {
        $spec = $positional[$r]
        if ($spec.StartsWith('+')) {
            Stop-Call 'a push with a + refspec is a force push and is blocked (D5.13, D10.14).'
        }
        $destination = $spec
        if ($spec.Contains(':')) { $destination = $spec.Substring($spec.LastIndexOf(':') + 1) }
        if ($destination -eq 'HEAD' -or $destination -eq '@') {
            $destination = Get-CurrentBranch $Dir
        }
        if ($destination -eq 'main' -or $destination -eq 'refs/heads/main') { Stop-Call $toMain }
    }
}

function Test-GitCommand($Words, [string]$Dir) {
    $j = 0
    while ($j -lt $Words.Count) {
        $w = $Words[$j]
        if (($w -ceq '-C' -or $w -ceq '-c') -and $j + 1 -lt $Words.Count) {
            if ($w -ceq '-C') { $Dir = Resolve-GuardPath $Words[$j + 1] $Dir }
            $j += 2
            continue
        }
        if ($w.StartsWith('-')) {
            $j++
            continue
        }
        break
    }
    if ($j -ge $Words.Count) { return }
    $sub = $Words[$j]
    $rest = Get-WordsAfter $Words $j

    if ($sub -eq 'push') {
        Test-GitPush $rest $Dir
    } elseif ($sub -eq 'commit') {
        if ((Get-CurrentBranch $Dir) -eq 'main') {
            Stop-Call ('committing on main is blocked: commit on the milestone branch; main ' +
                'changes only through a PR the user merges (D5.13, D7.24).')
        }
    } elseif ($sub -eq 'reset') {
        if ($rest -ccontains '--hard') {
            Stop-Call 'git reset --hard is blocked: it discards work. Ask the user.'
        }
    } elseif ($sub -eq 'clean') {
        $force = @($rest | Where-Object { $_ -cmatch '^-[A-Za-z]*f' -or $_ -eq '--force' })
        if ($force.Count -gt 0) {
            Stop-Call 'git clean -f is blocked: it deletes untracked files. Ask the user.'
        }
    } elseif ($sub -eq 'branch') {
        $forceDelete = @($rest | Where-Object { $_ -cmatch '^-[A-Za-z]*D' })
        $delete = @($rest | Where-Object { $_ -ceq '-d' -or $_ -eq '--delete' })
        $force = @($rest | Where-Object { $_ -ceq '-f' -or $_ -eq '--force' })
        if ($forceDelete.Count -gt 0 -or ($delete.Count -gt 0 -and $force.Count -gt 0)) {
            Stop-Call 'git branch -D is blocked: it deletes a branch with unmerged work. Ask the user.'
        }
    } elseif ($sub -eq 'checkout') {
        if ($rest -contains '.') {
            Stop-Call ('git checkout . (or -- .) is blocked: it throws away every uncommitted ' +
                'change (D10.14). Ask the user.')
        }
    } elseif ($sub -eq 'restore') {
        $staged = ($rest -contains '--staged') -or ($rest -ccontains '-S')
        $worktree = ($rest -contains '--worktree') -or ($rest -ccontains '-W')
        if (($rest -contains '.') -and (-not $staged -or $worktree)) {
            Stop-Call ('git restore . is blocked: it throws away every uncommitted change ' +
                '(D10.14). Ask the user.')
        }
    } elseif ($sub -eq 'rebase') {
        $positional = New-Object System.Collections.Generic.List[string]
        for ($p = 0; $p -lt $rest.Count; $p++) {
            if (@('--onto', '--strategy', '--exec') -contains $rest[$p] -or
                @('-s', '-X', '-x') -ccontains $rest[$p]) {
                $p++
                continue
            }
            if ($rest[$p].StartsWith('-')) { continue }
            $positional.Add($rest[$p])
        }
        if ((Get-CurrentBranch $Dir) -eq 'main' -or
            ($positional.Count -ge 2 -and $positional[1] -eq 'main')) {
            Stop-Call ('a git rebase that rewrites main is blocked (D9.6). Rebase the milestone ' +
                'branch, or merge main into it.')
        }
    }
}

function Test-BashCommand([string]$Command, [string]$Cwd) {
    if (-not $Command) { return }
    $parsed = Read-ShellCommand $Command
    if ($parsed.Writes.Count -gt 0) {
        Stop-Call ("Bash can't create or change files (CLAUDE.md ""Files""): this command " +
            "redirects output to '" + $parsed.Writes[0] + "'. Use the Write or Edit tool. " +
            '2>&1, > /dev/null and a > inside quotes or a quoted heredoc are fine.')
    }
    if ($parsed.UnquotedHeredoc) {
        Stop-Call ("a heredoc with an unquoted delimiter (<<" + $parsed.HeredocWord + ") is " +
            "blocked (CLAUDE.md ""Files""): quote the delimiter, as in <<'EOF'.")
    }
    $dir = $Cwd
    foreach ($words in (Get-Segments $parsed.Tokens)) {
        $k = 0
        while ($k -lt $words.Count -and ($words[$k] -match '^[A-Za-z_][A-Za-z0-9_]*=' -or
                @('command', 'time', 'nohup', 'exec', 'env', 'builtin') -contains $words[$k])) {
            $k++
        }
        if ($k -ge $words.Count) { continue }
        $name = (($words[$k] -split '[\\/]')[-1]).ToLowerInvariant() -replace '\.exe$', ''
        $rest = Get-WordsAfter $words $k

        if ($name -eq 'cd' -or $name -eq 'pushd') {
            if ($rest.Count -gt 0) { $dir = Resolve-GuardPath $rest[0] $dir }
        } elseif ($name -eq 'tee') {
            Stop-Call ("tee writes files; Bash can't create or change files (CLAUDE.md " +
                '"Files"). Use the Write or Edit tool.')
        } elseif ($name -eq 'sed') {
            $inPlace = @($rest | Where-Object { $_ -cmatch '^(-[A-Za-z]*i|--in-place)' })
            if ($inPlace.Count -gt 0) {
                Stop-Call 'sed -i changes files in place; use the Edit tool (CLAUDE.md "Files").'
            }
        } elseif ($name -eq 'git') {
            Test-GitCommand $rest $dir
        }

        $exempt = Get-MessageIndexes $name $rest
        for ($w = 0; $w -lt $rest.Count; $w++) {
            if ($exempt -contains $w) { continue }
            foreach ($candidate in (Get-DataCandidates $rest[$w])) {
                Assert-DataRead $candidate $dir
            }
        }
    }
    if ($Command -match 'install\.packages|remotes::|pak::') {
        Request-Approval ("installing R packages needs the user's OK (plan 20.3): " +
            $Matches[0])
    }
}

# ---------------------------------------------------------------------------------
# Other tools

function Test-GrepTool($ToolInput, [string]$Cwd) {
    $target = $Cwd
    $path = [string](Get-Key $ToolInput 'path')
    if ($path) { $target = Resolve-GuardPath $path $Cwd }
    if (Test-DataExtension $target) {
        Assert-DataRead $target $Cwd
        return
    }
    # A file filter naming a data extension is judged by the folder searched (D10.14).
    $extensions = New-Object System.Collections.Generic.List[string]
    $glob = [string](Get-Key $ToolInput 'glob')
    if ($glob) {
        $pattern = "(?i)(?<=[.{,])($DataExtPattern)(?=$|[},])"
        foreach ($m in [regex]::Matches($glob, $pattern)) { $extensions.Add($m.Value) }
    }
    $type = [string](Get-Key $ToolInput 'type')
    if ($type -and $type -match "^(?i)($DataExtPattern)$") { $extensions.Add($type) }
    foreach ($ext in $extensions) {
        $probe = $target.TrimEnd('\') + '\_gpq_probe.' + $ext
        if (-not (Test-DataAllowed $probe)) {
            Stop-Call ("a Grep over '" + $target + "' filtered to ." + $ext + ' reads data ' +
                'files outside the allowlisted folders (plan 20.3, D10.14). Ask the user, ' +
                'or search inside spec/, inst/extdata/, tests/ or bench/.')
        }
    }
}

# Edit, Write and NotebookEdit (plan 20.3 rows 7 and 8).
function Test-EditTool([string]$Path, [string]$Cwd) {
    if (-not $Path) { return }
    $full = Resolve-GuardPath $Path $Cwd
    $pieces = $full.Split('\')
    if ($pieces.Count -ge 2 -and $pieces[-1] -eq 'data_consent.local.txt' -and
        $pieces[-2] -eq '.claude') {
        Stop-Call ('only the user edits the consent list, .claude/data_consent.local.txt ' +
            '(plan 20.3, D8.22).')
    }
    $root = Get-CheckoutRoot $full
    if ($root -and (Get-CheckoutPath $full $root).StartsWith('spec/')) {
        Stop-Call ('files in spec/ are never edited (CLAUDE.md "Sources of truth"); new ' +
            'versions come from the user through the update-spec skill.')
    }
}

function Get-StringValues($Value) {
    $out = New-Object System.Collections.Generic.List[string]
    if ($null -eq $Value) { return , $out }
    if ($Value -is [string]) {
        $out.Add($Value)
        return , $out
    }
    $items = $Value
    if ($Value -is [System.Collections.IDictionary]) { $items = $Value.Values }
    if ($items -is [System.Collections.IEnumerable]) {
        foreach ($item in $items) {
            foreach ($s in (Get-StringValues $item)) { $out.Add($s) }
        }
    }
    return , $out
}

function Invoke-Guard([string]$Raw) {
    $call = ConvertFrom-HookJson $Raw
    $tool = [string](Get-Key $call 'tool_name')
    $toolInput = Get-Key $call 'tool_input'
    $cwd = [string](Get-Key $call 'cwd')
    if (-not $cwd) { $cwd = $ProjectDir }
    $cwd = Resolve-GuardPath $cwd $ProjectDir

    if ($tool -eq 'Bash') {
        Test-BashCommand ([string](Get-Key $toolInput 'command')) $cwd
    } elseif ($tool -eq 'Read') {
        Assert-DataRead ([string](Get-Key $toolInput 'file_path')) $cwd
    } elseif ($tool -eq 'Grep') {
        Test-GrepTool $toolInput $cwd
    } elseif ($tool -eq 'Edit' -or $tool -eq 'Write' -or $tool -eq 'MultiEdit') {
        Test-EditTool ([string](Get-Key $toolInput 'file_path')) $cwd
    } elseif ($tool -eq 'NotebookEdit') {
        # NotebookEdit as Edit (D9.6).
        Test-EditTool ([string](Get-Key $toolInput 'notebook_path')) $cwd
    } elseif ($tool -like 'mcp__*') {
        # An MCP tool as Read, on any path in its input (D9.6).
        foreach ($s in (Get-StringValues $toolInput)) {
            foreach ($candidate in (Get-DataCandidates $s)) { Assert-DataRead $candidate $cwd }
        }
    }
    # Glob, and any other tool, is allowed: it lists names only (plan 20.3 row 6).
}

# An Edit or Write on this file, recognised from the raw input so a broken hook can be
# repaired (D10.14); settings.json still asks the user first.
function Test-SelfEdit([string]$Raw) {
    return ($Raw -match '"tool_name"\s*:\s*"(Edit|Write|MultiEdit)"' -and
        $Raw -match '"file_path"\s*:\s*"[^"]*[\\/]\.claude[\\/]+hooks[\\/]+guard\.ps1"')
}

try {
    $RawInput = Read-HookInput
    Invoke-Guard $RawInput
} catch {
    if (Test-SelfEdit $RawInput) { exit 0 }
    Stop-Call ('internal error (' + $_.Exception.Message + '), so the call is blocked ' +
        '(D10.14). Fix .claude/hooks/guard.ps1, or turn the hook off in /hooks.')
}
exit 0
