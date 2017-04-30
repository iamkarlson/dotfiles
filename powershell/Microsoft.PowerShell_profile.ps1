(Get-Host).UI.RawUI.CursorSize = 100

############ Loading scripts from autorun folder #########################################
# directory where my scripts are stored
$ScriptFolder="$home\dotfiles\powershell\autorun"
# Source all .ps1 files in PowerShell profile folder
Get-ChildItem $ScriptFolder -name -include '*.ps1' -Recurse | foreach { Write-Host "loading script: " ("$ScriptFolder\$_") " ...."; %{. ($ScriptFolder+"\"+$_)}};


############ Loading scripts from Dropbox ################################################
$ScriptFolder="n:\dropbox\apps\powershell\autorun"
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

#Clear-Host

############ Override standart PS line start with git status ##############################

function global:prompt {
    $realLASTEXITCODE = $LASTEXITCODE

    Write-Host($pwd.ProviderPath) -nonewline

    Write-VcsStatus

    $global:LASTEXITCODE = $realLASTEXITCODE
    return "> "
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
