#requires -Version 2 -Modules posh-git


function Write-Theme {
    param(
        [bool]
        $lastCommandFailed,
        [string]
        $with
    )

    try{
        User-Prompt
    }catch{
        $sl.ErrorCount++
    }

    $lastColor = $sl.Colors.PromptBackgroundColor
    $prompt = Write-Prompt -Object $sl.PromptSymbols.StartSymbol -ForegroundColor $sl.Colors.PromptForegroundColor -BackgroundColor $sl.Colors.SessionInfoBackgroundColor

    #check the last command state and indicate if failed
    If ($lastCommandFailed) {
        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.FailedCommandSymbol) " -ForegroundColor $sl.Colors.CommandFailedIconForegroundColor -BackgroundColor $sl.Colors.SessionInfoBackgroundColor
    }

    #check for elevated prompt
    If (Test-Administrator) {
        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.ElevatedSymbol) " -ForegroundColor $sl.Colors.AdminIconForegroundColor -BackgroundColor $sl.Colors.SessionInfoBackgroundColor
    }

    If (Test-Wakatime) {
        $wakaBgColor = $sl.Colors.GitDefaultColor
        $wakaFgColor = $sl.Colors.GitForegroundColor

        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.SegmentForwardSymbol) " `
        -ForegroundColor $sl.Colors.SessionInfoBackgroundColor `
        -BackgroundColor $wakaBgColor

        $prompt +=Write-Prompt ("W") `
        -ForegroundColor $wakaFgColor `
        -BackgroundColor $wakaBgColor

        if( (Get-GitDirectory) -eq $null){
        } else {
            $prompt +=Write-Prompt ("P") `
            -ForegroundColor $sl.Colors.DarkYellow `
            -BackgroundColor $wakaBgColor
        }
        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.SegmentForwardSymbol) " `
        -ForegroundColor $wakaBgColor `
        -BackgroundColor $sl.Colors.SessionInfoBackgroundColor
    }

    $user = [System.Environment]::UserName
    $computer = [System.Environment]::MachineName
    $path = Get-FullPath -dir $pwd
    if (Test-NotDefaultUser($user)) {
        $prompt += Write-Prompt -Object "$user@$computer " -ForegroundColor $sl.Colors.SessionInfoForegroundColor -BackgroundColor $sl.Colors.SessionInfoBackgroundColor
    }

    if (Test-VirtualEnv) {
        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.SegmentForwardSymbol) " -ForegroundColor $sl.Colors.SessionInfoBackgroundColor -BackgroundColor $sl.Colors.VirtualEnvBackgroundColor
        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.VirtualEnvSymbol) $(Get-VirtualEnvName) " -ForegroundColor $sl.Colors.VirtualEnvForegroundColor -BackgroundColor $sl.Colors.VirtualEnvBackgroundColor
        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.SegmentForwardSymbol) " -ForegroundColor $sl.Colors.VirtualEnvBackgroundColor -BackgroundColor $sl.Colors.PromptBackgroundColor
    }
    else {
        $prompt += Write-Prompt -Object "$($sl.PromptSymbols.SegmentForwardSymbol) " -ForegroundColor $sl.Colors.SessionInfoBackgroundColor -BackgroundColor $sl.Colors.PromptBackgroundColor
    }

    # Writes the drive portion
    $prompt += Write-Prompt -Object "$path " -ForegroundColor $sl.Colors.PromptForegroundColor -BackgroundColor $sl.Colors.PromptBackgroundColor

    $status = Get-VCSStatus
    if ($status) {
        $themeInfo = Get-VcsInfo -status ($status)
        $lastColor = $themeInfo.BackgroundColor
        $prompt += Write-Prompt -Object $($sl.PromptSymbols.SegmentForwardSymbol) -ForegroundColor $sl.Colors.PromptBackgroundColor -BackgroundColor $lastColor
        $prompt += Write-Prompt -Object " $($themeInfo.VcInfo) " -BackgroundColor $lastColor -ForegroundColor $sl.Colors.GitForegroundColor
    }

    # Writes the postfix to the prompt
    $prompt += Write-Prompt -Object $sl.PromptSymbols.SegmentForwardSymbol -ForegroundColor $lastColor

    $timeStamp = Get-Date -UFormat %R
    $timestamp = " $timeStamp "
    $battery = (Get-BatteryChargeStatus).Utilization
    $batteryText = "["
    $batteryText += (Get-batterySymbol)
    $batteryText += " "
    $batteryText += "$battery%"
    $batteryText += "]"

    $prompt += Set-CursorForRightBlockWrite -textLength ($timestamp.Length + 6 + $batteryText.Length)
    $prompt += Write-Prompt -Object $($sl.PromptSymbols.SegmentBackwardSymbol) `
                            -BackgroundColor $sl.Colors.GitForegroundColor `
                            -ForegroundColor $sl.Colors.PromptBackgroundColor
    $prompt += Write-Prompt $batteryText `
                            -ForegroundColor $sl.Colors.PromptForegroundColor `
                            -BackgroundColor $sl.Colors.PromptBackgroundColor
    $prompt += Write-Prompt $timeStamp `
                            -ForegroundColor $sl.Colors.PromptForegroundColor `
                            -BackgroundColor $sl.Colors.PromptBackgroundColor
    $prompt += Write-Prompt -Object $($sl.PromptSymbols.SegmentBackwardSymbol) `
                            -BackgroundColor $sl.Colors.PromptBackgroundColor `
                            -ForegroundColor $sl.Colors.GitForegroundColor


    $prompt += Set-Newline

    if ($with) {
        $prompt += Write-Prompt -Object "$($with.ToUpper()) " -BackgroundColor $sl.Colors.WithBackgroundColor -ForegroundColor $sl.Colors.WithForegroundColor
    }
    $prompt += Write-Prompt -Object ($sl.PromptSymbols.PromptIndicator) -ForegroundColor $sl.Colors.PromptBackgroundColor
    $prompt += ' '
    $prompt
}

$sl = $global:ThemeSettings #local settings
$sl.PromptSymbols.StartSymbol = ''
$sl.PromptSymbols.PromptIndicator = [char]::ConvertFromUtf32(0x276F)
$sl.PromptSymbols.SegmentForwardSymbol = [char]::ConvertFromUtf32(0xe0c0)
$sl.PromptSymbols.SegmentBackwardSymbol = [char]::ConvertFromUtf32(0xe0c2)
$sl.Colors.PromptForegroundColor = [ConsoleColor]::White
$sl.Colors.PromptSymbolColor = [ConsoleColor]::White
$sl.Colors.PromptHighlightColor = [ConsoleColor]::DarkBlue
$sl.Colors.GitForegroundColor = [ConsoleColor]::Black
$sl.Colors.WithForegroundColor = [ConsoleColor]::DarkRed
$sl.Colors.WithBackgroundColor = [ConsoleColor]::Magenta
$sl.Colors.VirtualEnvBackgroundColor = [System.ConsoleColor]::Red
$sl.Colors.VirtualEnvForegroundColor = [System.ConsoleColor]::White
