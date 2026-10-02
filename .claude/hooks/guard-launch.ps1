# groundplotqc guard launcher (plan section 20.3; D10.14, D10.17).
#
# settings.json runs this file. It reads the tool call from stdin and runs guard.ps1 on
# it. If guard.ps1 can't be loaded or run (a syntax error, a missing file), the call is
# blocked, except an Edit or Write on the guard's own files, so a broken guard can be
# repaired; settings.json's ask under .claude/ still puts that edit to the user.
# Keep this file small and ASCII: a syntax error here can't be caught.

$ErrorActionPreference = 'Stop'
$raw = ''
try {
    $stdin = [Console]::OpenStandardInput()
    $buffer = New-Object System.IO.MemoryStream
    $stdin.CopyTo($buffer)
    $raw = [System.Text.Encoding]::UTF8.GetString($buffer.ToArray()).TrimStart([char]0xFEFF)
    & (Join-Path $PSScriptRoot 'guard.ps1') -RawInput $raw
    exit $LASTEXITCODE
} catch {
    $selfEdit = ($raw -match '"tool_name"\s*:\s*"(Edit|Write|MultiEdit)"' -and
        $raw -match '"file_path"\s*:\s*"[^"]*[\\/]\.claude[\\/]+hooks[\\/]+guard(-launch)?\.ps1"')
    if ($selfEdit) { exit 0 }
    [Console]::Error.WriteLine('groundplotqc guard: guard.ps1 failed to load or run (' +
        $_.Exception.Message + '), so the call is blocked (D10.14, D10.17). Fix ' +
        '.claude/hooks/guard.ps1, or turn the hook off in /hooks.')
    exit 2
}
