(Get-Host).UI.RawUI.CursorSize = 100

############ Loading scripts from autorun folder #########################################
# directory where my scripts are stored
$ScriptFolder="$home\dotfiles\powershell\autorun"
# Source all .ps1 files in PowerShell profile folder
Get-ChildItem $ScriptFolder -name -include '*.ps1' -Recurse | foreach { Write-Host "loading script: " ("$ScriptFolder\$_") " ...."; %{. ($ScriptFolder+"\"+$_)}};


############ Loading scripts from Dropbox ################################################
$ScriptFolder="c:\dropbox\apps\powershell\autorun"
Get-ChildItem $ScriptFolder -name -include '*.ps1' -Recurse | foreach { Write-Host "loading script: " ("$ScriptFolder\$_") " ...."; %{. ($ScriptFolder+"\"+$_)}};


#PSReadLine settings
Set-PSReadlineKeyHandler -Key Tab -Function Complete
Set-PSReadlineOption -EditMode Emacs


$wakatime = $(where.exe wakatime);

#Clear-Host

############ Override standart PS line start with git status ##############################
function Write-WakaStatus{
    Write-Host("{") -nonewline  -ForegroundColor Black -BackgroundColor Green
    Write-Host("W") -nonewline  -ForegroundColor Black -BackgroundColor Green

    if( (Get-GitDirectory) -eq $null){
    } else {
        Write-Host("P") -nonewline -ForegroundColor Red -BackgroundColor Green
    }
    Write-Host("}") -nonewline -ForegroundColor Black -BackgroundColor Green

}


function global:prompt {
    $realLASTEXITCODE = $LASTEXITCODE

# z plugin requrements
    Update-NavigationHistory $pwd.Path

    $new_pwd = $pwd.ProviderPath.Replace("$home","~");

    Write-Host "" -nonewline  -ForegroundColor Black -BackgroundColor Green


    Write-Host($new_pwd.ToLower()) -nonewline  -ForegroundColor Black -BackgroundColor Green


    if($wakatime) {

        Write-WakaStatus
        Get-Job -State Completed|?{$_.Name.Contains("WakaJob")}|Remove-Job
        $job = Start-Job -Name "WakaJob" -ScriptBlock {
            $gitFolder = (Get-GitDirectory);

            $command = "";
            try{
                
                $command = (Get-History -Count 1|select -Property CommandLine).CommandLine.Split(" ")[0].Replace("(","")
            } catch{
                if($command -eq "") {
                    $command = "error"
                }
            }

            $wakaCommand = 'wakatime --write'
            $wakaCommand =$wakaCommand + ' --plugin "powershell-wakatime-iamkarlson-plugin/$PLUGIN_VERSION"'
            $wakaCommand =$wakaCommand + ' --entity-type app '
            $wakaCommand =$wakaCommand + ' --entity "'
            $wakaCommand =$wakaCommand +  $command
            $wakaCommand =$wakaCommand + '" '
            $wakaCommand =$wakaCommand + ' --language PowerShell'

            if($gitFolder -eq $null){
            } else {
                $gitFolder = (get-item ($gitFolder).Replace(".git",""))
                $wakaCommand =$wakaCommand + ' --project $gitFolder.Name'
            }
            $env:wakaDebug
            if($env:wakaDebug){
                $wakaCommand |out-file ~/.wakapwsh.log
            }
            & $wakaCommand
        }
    }

    $global:LASTEXITCODE = $realLASTEXITCODE
# This is needed because posh-git has a bug
    if( (Get-GitDirectory) -eq $null){
        Write-Host ""  -nonewline -ForegroundColor Green -BackgroundColor Black

    } else {
        Write-VcsStatus
        Write-Host ""  -nonewline -ForegroundColor Green -BackgroundColor Black
    }
    return " "

}

############ import modules ###############################################################
Get-ChildItem "$PSScriptroot\Modules" -File -Filter "*.psm1" |%{Import-Module $_.FullName -Force}

foreach ($module in Get-childItem "$PSScriptroot\Modules" -File -Filter "*.psm1" -Recurse){
    if($module.FullName -match "TestModules"){
        continue;
    }
    Write-Host "Loading module $module";
    Import-Module $module.FullName -Force
}


$GitPromptSettings.BeforeText =" "
$GitPromptSettings.AfterText ="]"

Start-SshAgent -Quiet

Write-Host "PowerShell Environment Loaded"
Write-Host ""

Get-Date |Write-Host

############ Cool greeting ################################################################
Write-Host "Wake up Neo"
Write-Host ""
Write-Host "The Matrix has you..."
Write-Host ""
Write-Host "Follow the white rabbit..."
Write-Host ""
Write-Host "Knock knock Neo."

# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
    Import-Module "$ChocolateyProfile"
}
