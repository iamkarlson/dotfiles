$scriptRoot = Split-Path $MyInvocation.MyCommand.Path

$profileFileInfo = New-Object System.IO.FileInfo $PROFILE

$profileExist = Test-Path $profile
$modulesFolder = [System.IO.Path]::Combine($profileFileInfo.Directory,"Modules")

if(-not ((Test-Path $profile) -AND (Test-Path $modulesFolder)))
{
    Write-Host "Creating directories"
    New-Item -ItemType directory -path $profileFileInfo.Directory -ErrorAction SilentlyContinue
    New-Item -ItemType directory -path $modulesFolder
}



Copy-Item -Path $scriptRoot\Microsoft.PowerShell_profile.ps1 -Destination $profile

Write-host $modulesFolder
#Get-ChildItem -Path Modules | Copy-Item -Destination $modulesFolder -Recurse -Container -Force
xcopy.exe Modules\* "$modulesFolder" /Y /S

Write-Host "Profile was updated"
