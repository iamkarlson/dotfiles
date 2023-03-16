(Get-Host).UI.RawUI.CursorSize = 100

############ Loading scripts from autorun folder ###############################
# directory where my scripts are stored
$ScriptFolder="$home\dev\dotfiles\powershell\autorun"
# Source all .ps1 files in PowerShell profile folder
Get-ChildItem $ScriptFolder -name -include '*.ps1' -Recurse |?{$_ -notmatch "._"}| foreach { Write-Host "loading script: " ("$ScriptFolder\$_") " ...."; %{. ($ScriptFolder+"\"+$_)}};


############ Loading scripts from Dropbox ######################################
$ScriptFolder="$dropbox\powershell\autorun"
Get-ChildItem $ScriptFolder -name -include '*.ps1' -Recurse | foreach { Write-Host "loading script: " ("$ScriptFolder\$_") " ...."; %{. ($ScriptFolder+"\"+$_)}};


#PSReadLine settings
Set-PSReadlineKeyHandler -Key Tab -Function Complete
Set-PSReadlineOption -EditMode Emacs

############################# Wakatime #########################################
function Test-Wakatime{

    $wakatime = $(get-command wakatime);

    if($wakatime) {
        return $True;
    } else {
        return $False;
    }
}

$env:wakaDebug = $False

function Send-Wakatime(){
    if(!(Test-Wakatime)) {
        return;
    }
    $command = "";
    try {
        $historyItem = (Get-History |select -Last 1)
        $commandObj = ($historyItem|select -Property CommandLine).CommandLine
        $commandText = ([regex]::split($commandObj,"[ |;:]")[0])
        $command = $commandText.Replace("(","")
    } catch [Exception] {
        if($command -eq "") {
            $command = "error"
        }
    }
    $gitFolder = (Get-GitDirectory);
    Get-Job -State Completed|?{$_.Name.Contains("WakaJob")}|Remove-Job
    $job = Start-Job -Name "WakaJob" -ScriptBlock {
        param($command, $gitFolder)

        Write-Host "Sending wakatime"
        Write-Host "Waka command: $command"
        if($command -eq "") {
            return;
        }

        $PLUGIN_VERSION = "0.2";

        $wakaCommand = 'wakatime --write'
        $wakaCommand =$wakaCommand + " --plugin `"powershell-wakatime-iamkarlson-plugin/$PLUGIN_VERSION`""
        $wakaCommand =$wakaCommand + ' --entity-type app '
        $wakaCommand =$wakaCommand + ' --entity "'
        $wakaCommand =$wakaCommand +  $command
        $wakaCommand =$wakaCommand + '" '
        $wakaCommand =$wakaCommand + ' --language "PowerShell"'

        if($gitFolder -eq $null){
        } else {
            $gitFolder = (get-item ($gitFolder).Replace(".git",""))
            $wakaCommand =$wakaCommand + ' --project "'
            $wakaCommand =$wakaCommand + $gitFolder.Name
            $wakaCommand =$wakaCommand + '"'
        }
        $envwakaDebug=$env:wakaDebug
        Write-Host "wakaDebug: $envwakaDebug"
        $wakaCommand
        if($envwakaDebug){
            $wakaCommand |out-file ~/.wakapwsh.log -Append
        }
        iex $wakaCommand
    } -ArgumentList $command, $gitFolder
}
#$env:wakaDebug = $True

############################## import modules ##################################

function User-Prompt{
    Send-Wakatime
    Update-NavigationHistory $pwd.Path
    Save-HistoryIncremental
}
Import-Module PersistentHistory
Import-Module posh-git
Import-Module oh-my-posh
Import-Module Watch
#Import-Module PoShFuck


$ThemeSettings.MyThemesLocation= "~\Documents\PowerShell\PoshThemes"

Set-Theme Paradox
$ThemeSettings.GitSymbols.BranchSymbol = [char]::ConvertFromUtf32(0xE0A0)


Import-Module z

# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
    Import-Module "$ChocolateyProfile"
}

########################### Cool greeting ######################################
Get-Date |Write-Host

Write-Host "Wake up Neo"
Write-Host ""
Write-Host "The Matrix has you..."
Write-Host ""
Write-Host "Follow the white rabbit..."
Write-Host ""
Write-Host "Knock knock Neo."

