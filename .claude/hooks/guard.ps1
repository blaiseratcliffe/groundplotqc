# groundplotqc guard hook (plan section 20.3; D5.13, D8.22, D9.4, D9.6, D9.7, D10.2,
# D10.12, D10.14).
#
# Claude Code runs this before Bash, Read, Grep, Glob, Edit, Write, NotebookEdit and MCP
# tool calls, passing the call as JSON on stdin. Exit 0 allows the call; exit 2 blocks
# it and returns the message on stderr to Claude; an "ask" decision is printed as JSON
# on stdout with exit 0. Windows PowerShell 5.1; keep this file ASCII.
#
# What it can't see (plan 20.3, D9.4, D10.17): reads inside R scripts, database
# connections opened from R, a Grep over a folder with no data-extension filter,
# provider data in files without a data extension, and commands run through eval,
# $(...), xargs or git --git-dir. CLAUDE.md covers those.
#
# settings.json runs guard-launch.ps1, which passes the call in as -RawInput and
# blocks the call if this file fails to load (D10.17).

param([string]$RawInput)

$ErrorActionPreference = 'Stop'

# The project folder: this file is <project>\.claude\hooks\guard.ps1.
$ProjectDir = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$ConsentFile = Join-Path $ProjectDir '.claude\data_consent.local.txt'

# Data extensions: the .gitignore list (plan 19.2; *.txt kept, D10.3).
$DataExtPattern = 'rdata|rda|rds|csv|tsv|txt|xlsx|xls|sqlite|gpkg|accdb|mdb|shp|shx|dbf|prj|cpg|tif|tiff|zip|gz|parquet|feather|fst|qs'

# Path-like tokens inside a longer word, such as R code. A plain character class, so
# the scan is linear on long words (D10.17).
$TokenPattern = "[^\s'""(),;=<>|&{}\[\]]+"

# Folders allowlisted for data files, relative to a checkout root (plan 20.3 row 3).
$AllowedPrefixes = @('spec/', 'inst/extdata/', 'tests/', 'bench/')

# Words that start a command without being one: shell keywords and wrappers whose
# command follows (D10.17). eval, xargs and $(...) are not followed (20.3's limits).
$ShellKeywords = @('if', 'then', 'else', 'elif', 'fi', 'do', 'done', 'while', 'until',
    'for', 'case', 'esac', '{', '}', '!', 'time', 'command', 'nohup', 'exec', 'builtin')

# git subcommands that name paths without reading what is in them, each with the
# options it may carry while its path arguments stay unread (D12.46, D12.52).
$NonReadingGit = @{
    'check-ignore' = @('-q', '--quiet', '-v', '--verbose', '-n', '--non-matching', '--no-index', '-z')
    'check-attr'   = @('-a', '--all', '--cached', '-z')
    'ls-files'     = @('-c', '--cached', '-d', '--deleted', '-m', '--modified', '-o', '--others',
        '-i', '--ignored', '-s', '--stage', '-u', '--unmerged', '-k', '--killed', '-t', '-v', '-f',
        '-z', '--directory', '--no-empty-directory', '--exclude-standard', '--full-name',
        '--deduplicate', '--error-unmatch', '--eol', '--sparse')
}

$script:Consents = $null
# Branch a directory is on after a same-line git switch or checkout (D10.17).
$script:AssumedBranch = @{}

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
        # Windows ignores an alternate data stream (name:stream) and trailing dots
        # and spaces, so x.Rdata::$DATA and x.Rdata. name x.Rdata (D10.17).
        $segment = ($segment -replace ':.*$', '').TrimEnd('.', ' ')
        if ($segment -eq '') { continue }
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
                # An inline comment after the path is dropped (D10.17).
                $path = ($Matches[1] -replace '\s+#.*$', '').Trim()
                if ($path) { $list.Add((Resolve-GuardPath $path $ProjectDir)) }
            }
        }
    }
    return , $list
}

function Test-DataAllowed([string]$FullPath) {
    # A path holding a variable or a wildcard can expand outside the allowlist, and
    # can't match a consent line (D10.17).
    if ($FullPath -match '[$*?]') { return $false }
    # The consent list itself may be read; only the user edits it (D12.46).
    if ($FullPath.Equals($ConsentFile, [StringComparison]::OrdinalIgnoreCase)) { return $true }
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
    $full = Resolve-GuardPath $Candidate $Base
    if (-not (Test-DataExtension $full)) { return }
    if (Test-DataAllowed $full) { return }
    Stop-Call ("reading '$Candidate' needs the user's consent: it has a data extension " +
        'and is outside spec/, data/magp_example.rda, inst/extdata/, tests/, bench/, the ' +
        'build tarball and the matrix working copy. Ask the user, naming the file and why ' +
        '(CLAUDE.md "Data"); with consent they add it to .claude/data_consent.local.txt.')
}

# Names that look like data files but aren't: an extension alone (".csv", as in a
# regex), and an R reader or writer named without a call, as in
# lapply(files, read.csv) (D10.17).
function Test-NotAFile([string]$Value) {
    $leaf = ($Value -split '[\\/]')[-1]
    if ($leaf -match '^\.[A-Za-z0-9]+$') { return $true }
    return ($Value -notmatch '[\\/]' -and
        ($leaf -replace '^.*::', '') -match '^(read|write)\.(csv|xlsx|xls|dbf)$')
}

# Paths with a data extension in one word of a command or one string of a tool input.
function Get-DataCandidates([string]$Word) {
    $found = New-Object System.Collections.Generic.List[string]
    if (-not $Word -or $Word.Contains('://')) { return , $found }
    $text = $Word
    if ($text -match '^--?[A-Za-z][A-Za-z0-9-]*=(.*)$') { $text = $Matches[1] }
    # A git revision and path, such as HEAD:spec/x.csv or :spec/x.csv: the path part
    # (D10.17). A drive letter (D:) and R's pkg::fun don't match.
    if ($text -match '^([A-Za-z0-9_.@^~/{}-]{2,})?:(?![\\/:])(.+)$') { $text = $Matches[2] }
    $whole = $text.TrimEnd('.', ' ')
    if ((Test-DataExtension $whole) -and $text -notmatch "[()'"",;]") {
        if (-not (Test-NotAFile $whole)) { $found.Add($whole) }
        return , $found
    }
    foreach ($m in [regex]::Matches($text, $TokenPattern)) {
        $token = $m.Value.TrimEnd('.', ' ')
        if (-not (Test-DataExtension $token)) { continue }
        # A function call such as read.csv( is not a file.
        $after = $m.Index + $m.Length
        if ($after -lt $text.Length -and $text[$after] -eq '(') { continue }
        if (Test-NotAFile $token) { continue }
        $found.Add($token)
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
    $state = @{ InWord = $false; Expect = ''; InTest = $false }
    $heredocs = New-Object System.Collections.Generic.List[object]
    $n = $Command.Length
    $i = 0

    # End the current word. A word after > or >> is a write target (allowed only for
    # /dev/null, NUL, the terminal and the standard streams); every other word is
    # kept, a word after < marked as redirected in (D12.52). [[ and ]] open and close
    # a test, where < and > compare (D10.17).
    $flush = {
        if ($state.InWord) {
            $text = $word.ToString()
            if ($state.Expect -eq 'write') {
                if ($text -notmatch '^(?i)(/dev/null|nul|/dev/stdout|/dev/stderr|/dev/tty|/dev/fd/\d+)$') {
                    $result.Writes.Add($text)
                }
            } else {
                $tokens.Add(@{ Kind = 'word'; Text = $text; Redirected = ($state.Expect -eq 'read') })
                if ($text -eq '[[') { $state.InTest = $true }
                if ($text -eq ']]') { $state.InTest = $false }
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

    # Index just past the )) that closes an arithmetic (( opened at Start.
    $arithmeticEnd = {
        param([int]$Start)
        $depth = 0
        for ($p = $Start; $p -lt $n; $p++) {
            if ($Command[$p] -eq '(') { $depth++ }
            elseif ($Command[$p] -eq ')') {
                $depth--
                if ($depth -eq 0) { return $p + 1 }
            }
        }
        return $n
    }

    while ($i -lt $n) {
        $c = $Command[$i]

        # Arithmetic $(( )) and (( )): < and > there are operators, not redirects.
        if ($c -eq '$' -and $i + 2 -lt $n -and $Command[$i + 1] -eq '(' -and
            $Command[$i + 2] -eq '(') {
            $end = & $arithmeticEnd ($i + 1)
            $null = $word.Append($Command.Substring($i, $end - $i))
            $state.InWord = $true
            $i = $end
            continue
        }
        if ($c -eq '(' -and -not $state.InWord -and $i + 1 -lt $n -and $Command[$i + 1] -eq '(') {
            $i = & $arithmeticEnd $i
            continue
        }

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
        if (($c -eq '<' -or $c -eq '>') -and $state.InTest) {
            # Inside [[ ]], < and > compare strings.
            $null = $word.Append($c)
            $state.InWord = $true
            $i++
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
                # < file: the file is read; the word is kept, marked and scanned for data.
                $state.Expect = 'read'
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

# Words of each simple command, split at ; | & && || ( ) and newlines, and whether a
# file is redirected into it with < (D12.52).
function Get-Segments($Tokens) {
    $segments = New-Object System.Collections.Generic.List[object]
    $current = @{ Words = New-Object System.Collections.Generic.List[string]; Redirected = $false }
    foreach ($token in $Tokens) {
        if ($token.Kind -eq 'op') {
            if ($current.Words.Count -gt 0) { $segments.Add($current) }
            $current = @{ Words = New-Object System.Collections.Generic.List[string]; Redirected = $false }
        } else {
            $current.Words.Add($token.Text)
            if ($token.Redirected) { $current.Redirected = $true }
        }
    }
    if ($current.Words.Count -gt 0) { $segments.Add($current) }
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
        # git's combined short flags: -am "text" and -m"text" (D10.17).
        if ($Name -eq 'git') {
            if ($Words[$w] -cmatch '^-[A-Za-z]*m$') { $skip.Add($w + 1) }
            elseif ($Words[$w] -cmatch '^-[A-Za-z]*m.') { $skip.Add($w) }
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

# The branch Dir is on when this part of the command runs: the target of an earlier
# git switch or checkout in the same command, else the current branch (D10.17).
function Get-EffectiveBranch([string]$Dir) {
    $key = $Dir.ToLowerInvariant()
    if ($script:AssumedBranch.ContainsKey($key)) { return $script:AssumedBranch[$key] }
    return Get-CurrentBranch $Dir
}

# Record the branch a git switch or checkout moves Dir to, when the words name one.
function Set-AssumedBranch([string]$Sub, $Words, [string]$Dir) {
    if ($Words -contains '--') { return }    # checkout -- paths: not a branch switch
    $target = $null
    for ($p = 0; $p -lt $Words.Count; $p++) {
        $w = $Words[$p]
        if (@('-b', '-B', '-c', '-C', '--orphan', '--create', '--force-create') -ccontains $w) {
            if ($p + 1 -lt $Words.Count) { $target = $Words[$p + 1] }
            break
        }
        if ($w -eq '--detach' -or $w -eq '-d') {
            $target = ''
            break
        }
        if ($w.StartsWith('-')) { continue }
        if ($null -eq $target) { $target = $w }
    }
    if ($null -ne $target -and $target -ne '-') {
        $script:AssumedBranch[$Dir.ToLowerInvariant()] = $target
    }
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
        if ((Get-EffectiveBranch $Dir) -eq 'main') { Stop-Call $toMain }
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
            $destination = Get-EffectiveBranch $Dir
        }
        if ($destination -eq 'main' -or $destination -eq 'refs/heads/main') { Stop-Call $toMain }
    }
}

# Positions, in a git command's words, of the path arguments that check-ignore,
# check-attr or ls-files only name. None when a file is redirected in, when a global
# option other than -C comes first, or when an option is off the subcommand's read-free
# list: then every word is judged as a possible read (D12.46, D12.52).
function Get-PathOnlyIndexes($Words, [bool]$Redirected) {
    $none = New-Object System.Collections.Generic.List[int]
    if ($Redirected) { return , $none }
    $j = 0
    while ($j -lt $Words.Count -and $Words[$j].StartsWith('-')) {
        if ($Words[$j] -cne '-C' -or $j + 1 -ge $Words.Count) { return , $none }
        $j += 2
    }
    if ($j -ge $Words.Count -or -not $NonReadingGit.ContainsKey($Words[$j])) { return , $none }
    $flags = $NonReadingGit[$Words[$j]]
    $paths = New-Object System.Collections.Generic.List[int]
    $afterDashes = $false
    for ($p = $j + 1; $p -lt $Words.Count; $p++) {
        $w = $Words[$p]
        if (-not $afterDashes -and $w -eq '--') {
            $afterDashes = $true
            continue
        }
        if (-not $afterDashes -and $w.StartsWith('-')) {
            if ($flags -cnotcontains $w) { return , $none }
            continue
        }
        $paths.Add($p)
    }
    return , $paths
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
        if ((Get-EffectiveBranch $Dir) -eq 'main') {
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
        # Moving, renaming or copying onto main (D10.17).
        $moves = @($rest | Where-Object { @('-f', '--force', '-m', '-M', '--move', '-c', '-C', '--copy') -ccontains $_ })
        if ($moves.Count -gt 0 -and $rest -contains 'main') {
            Stop-Call 'git branch -f, -m or -c involving main is blocked: main moves only through a PR the user merges (D10.17).'
        }
    } elseif ($sub -eq 'checkout') {
        if ($rest -contains '.') {
            Stop-Call ('git checkout . (or -- .) is blocked: it throws away every uncommitted ' +
                'change (D10.14). Ask the user.')
        }
        Set-AssumedBranch $sub $rest $Dir
    } elseif ($sub -eq 'switch') {
        Set-AssumedBranch $sub $rest $Dir
    } elseif (@('merge', 'cherry-pick', 'revert', 'am') -contains $sub) {
        if ((Get-EffectiveBranch $Dir) -eq 'main') {
            Stop-Call ("git $sub on main is blocked: main changes only through a PR the user " +
                'merges (D10.17).')
        }
    } elseif ($sub -eq 'pull') {
        if (-not ($rest -contains '--ff-only') -and (Get-EffectiveBranch $Dir) -eq 'main') {
            Stop-Call ('git pull on main is blocked unless it is --ff-only: a merge or rebase ' +
                'would change main locally (D10.17). Use git pull --ff-only.')
        }
    } elseif ($sub -eq 'update-ref') {
        if ($rest -contains 'main' -or $rest -contains 'refs/heads/main') {
            Stop-Call 'git update-ref on main is blocked (D10.17).'
        }
    } elseif ($sub -eq 'rm' -or $sub -eq 'mv') {
        foreach ($w in $rest) {
            if (-not $w.StartsWith('-')) { Assert-NotProtected $w $Dir "git $sub" }
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
        if ((Get-EffectiveBranch $Dir) -eq 'main' -or
            ($positional.Count -ge 2 -and $positional[1] -eq 'main')) {
            Stop-Call ('a git rebase that rewrites main is blocked (D9.6). Rebase the milestone ' +
                'branch, or merge main into it.')
        }
    }
}

# Index of the word that names the command: past assignments, shell keywords and the
# wrappers env, nice and timeout with their options (D10.17).
function Get-CommandIndex($Words) {
    $k = 0
    while ($k -lt $Words.Count) {
        $w = $Words[$k]
        if ($w -match '^[A-Za-z_][A-Za-z0-9_]*=' -or $ShellKeywords -contains $w) {
            $k++
            continue
        }
        $base = (($w -split '[\\/]')[-1]).ToLowerInvariant() -replace '\.exe$', ''
        if ($base -eq 'env' -or $base -eq 'nice' -or $base -eq 'timeout') {
            $k++
            while ($k -lt $Words.Count -and $Words[$k].StartsWith('-')) {
                if (@('-u', '-C', '-n', '-k', '-s') -ccontains $Words[$k]) { $k++ }
                $k++
            }
            if ($base -eq 'timeout') { $k++ }    # the duration
            continue
        }
        break
    }
    return $k
}

# Why a path may not be changed by Edit, Write or a Bash command, or '' if it may
# (plan 20.3 rows 7 and 8; Bash writers D10.17).
function Get-ProtectedReason([string]$FullPath) {
    $pieces = $FullPath.Split('\')
    if ($pieces.Count -ge 2 -and $pieces[-1] -eq 'data_consent.local.txt' -and
        $pieces[-2] -eq '.claude') {
        return ('only the user edits the consent list, .claude/data_consent.local.txt ' +
            '(plan 20.3, D8.22).')
    }
    $inSpec = $false
    $root = Get-CheckoutRoot $FullPath
    if ($root) {
        $inside = Get-CheckoutPath $FullPath $root
        $inSpec = ($inside -eq 'spec' -or $inside.StartsWith('spec/'))
    }
    # Without GPQ_WORKTREE_ROOT a worktree isn't recognised: any spec folder counts.
    if (-not $env:GPQ_WORKTREE_ROOT -and $pieces -contains 'spec') { $inSpec = $true }
    if ($inSpec) {
        return ('files in spec/ are never edited (CLAUDE.md "Sources of truth"); new ' +
            'versions come from the user through the update-spec skill.')
    }
    return ''
}

function Assert-NotProtected([string]$Path, [string]$Base, [string]$What) {
    $reason = Get-ProtectedReason (Resolve-GuardPath $Path $Base)
    if ($reason) { Stop-Call ("$What on '$Path' is blocked: " + $reason) }
}

function Test-BashCommand([string]$Command, [string]$Cwd, [int]$Depth = 0) {
    if (-not $Command) { return }
    if ($Depth -gt 4) { Stop-Call 'commands nested more than four deep are blocked (D10.17).' }
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
    foreach ($segment in (Get-Segments $parsed.Tokens)) {
        $words = $segment.Words
        $k = Get-CommandIndex $words
        if ($k -ge $words.Count) { continue }
        $name = (($words[$k] -split '[\\/]')[-1]).ToLowerInvariant() -replace '\.exe$', ''
        $rest = Get-WordsAfter $words $k

        if ($name -eq 'cd' -or $name -eq 'pushd') {
            if ($rest.Count -gt 0) { $dir = Resolve-GuardPath $rest[0] $dir }
        } elseif (@('bash', 'sh', 'dash', 'zsh', 'ksh') -contains $name) {
            # bash -c 'commands': the string is a command line of its own (D10.17).
            for ($w = 0; $w -lt $rest.Count - 1; $w++) {
                if ($rest[$w] -cmatch '^-[A-Za-z]*c[A-Za-z]*$') {
                    Test-BashCommand $rest[$w + 1] $dir ($Depth + 1)
                    break
                }
            }
        } elseif ($name -eq 'rm') {
            $recursive = @($rest | Where-Object { $_ -cmatch '^-[A-Za-z]*[rR]' -or $_ -eq '--recursive' })
            if ($recursive.Count -gt 0) {
                Stop-Call ('rm with a recursive flag is blocked (D10.17). Remove a folder from R ' +
                    'with unlink(path, recursive = TRUE), or a worktree with git worktree ' +
                    'remove (plan 20.2).')
            }
            foreach ($w in $rest) { if (-not $w.StartsWith('-')) { Assert-NotProtected $w $dir 'rm' } }
        } elseif (@('mv', 'rmdir', 'unlink') -contains $name) {
            foreach ($w in $rest) { if (-not $w.StartsWith('-')) { Assert-NotProtected $w $dir $name } }
        } elseif (@('cp', 'touch', 'install', 'rsync', 'truncate', 'ln') -contains $name) {
            # These create or overwrite files, which is the Write tool's job; renames and
            # deletions (mv, rm, git mv, git rm) have no tool equivalent (D10.18).
            Stop-Call ("$name creates or overwrites files; Bash can't create or change files " +
                '(CLAUDE.md "Files"). Use the Write or Edit tool. Renames and deletions (mv, ' +
                'rm, git mv, git rm) are allowed outside spec/ and the consent list (D10.18).')
        } elseif ($name -eq 'gh' -and $rest.Count -gt 0 -and $rest[0] -eq 'api') {
            if (@($rest | Where-Object { $_ -match 'pulls/\d+/merge' }).Count -gt 0) {
                Stop-Call 'merging a PR through gh api is blocked: the user merges every PR (D9.5, D10.17).'
            }
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
        if ($name -eq 'git') {
            foreach ($p in (Get-PathOnlyIndexes $rest $segment.Redirected)) { $exempt.Add($p) }
        }
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
    # A negated glob (!*.csv) excludes those files rather than reading them (D10.17).
    if ($glob -and -not $glob.StartsWith('!')) {
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
    $reason = Get-ProtectedReason (Resolve-GuardPath $Path $Cwd)
    if ($reason) { Stop-Call $reason }
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
    if (-not $tool) { throw 'the input names no tool (empty or malformed)' }
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

# An Edit or Write on the guard's own files, recognised from the raw input so a broken
# hook can be repaired (D10.14); settings.json still asks the user first.
function Test-SelfEdit([string]$Raw) {
    return ($Raw -match '"tool_name"\s*:\s*"(Edit|Write|MultiEdit)"' -and
        $Raw -match '"file_path"\s*:\s*"[^"]*[\\/]\.claude[\\/]+hooks[\\/]+guard(-launch)?\.ps1"')
}

try {
    if (-not $PSBoundParameters.ContainsKey('RawInput')) { $RawInput = Read-HookInput }
    Invoke-Guard $RawInput
} catch {
    if (Test-SelfEdit $RawInput) { exit 0 }
    Stop-Call ('internal error (' + $_.Exception.Message + '), so the call is blocked ' +
        '(D10.14). Fix .claude/hooks/guard.ps1, or turn the hook off in /hooks.')
}
exit 0
