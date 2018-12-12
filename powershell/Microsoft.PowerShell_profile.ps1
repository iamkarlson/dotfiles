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


############ import modules ###############################################################
Get-ChildItem "$PSScriptroot\Modules" -File -Filter "*.psm1" |%{Import-Module $_.FullName -Force}

foreach ($folder in Get-childItem "$PSScriptroot\Modules" -Directory ){
    Write-Host "loading modules from $folder`n"
    foreach($module in Get-ChildItem $folder.FullName -File -Filter "*.psm1"){
        Write-Host "Loading module $module";
        Import-Module $module.FullName -Force
    }
}

$wakatime = $(where.exe wakatime);

#Clear-Host

############ Override standart PS line start with git status ##############################
function Write-WakaStatus{
    Write-Host("{") -nonewline  -ForegroundColor DarkGray
    Write-Host("W") -nonewline  -ForegroundColor DarkGray

    if( (Get-GitDirectory) -eq $null){
    } else {
        Write-Host("P") -nonewline -ForegroundColor Green
    }
    Write-Host("}") -nonewline -ForegroundColor DarkGray
}

function global:prompt {
    $realLASTEXITCODE = $LASTEXITCODE

    $new_pwd = $pwd.ProviderPath.Replace("$home","~");

    Write-Host($new_pwd) -nonewline


    Write-VcsStatus

    if($wakatime) {

        Write-WakaStatus
        Get-Job -State Completed|?{$_.Name.Contains("WakaJob")}|Remove-Job
        $job = Start-Job -Name "WakaJob" -ScriptBlock {
            $gitFolder = (Get-GitDirectory);

            $command = "";
            try{
                $command = (Get-History -Count 1|select -Property CommandLine).CommandLine.Split(" ")[0].Replace("(","")
            } catch{
                $command = "error"
            }

            if($gitFolder -eq $null){
                wakatime --write `
                --plugin "powershell-wakatime-iamkarlson-plugin/$PLUGIN_VERSION" `
                --entity-type app `
                --entity "$command" `
                --language PowerShell;
            } else {
                $gitFolder = (get-item ($gitFolder).Replace(".git",""))
                wakatime --write `
                --plugin "powershell-wakatime-iamkarlson-plugin/$PLUGIN_VERSION" `
                --entity-type app `
                --entity "$command" `
                --language PowerShell `
                --project $gitFolder.Name;
            }
        }
    }

    $global:LASTEXITCODE = $realLASTEXITCODE
    return "=] "
}

Import-Module posh-git
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
