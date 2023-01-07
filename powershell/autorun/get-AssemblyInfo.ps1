Function global:Get-AssemblyInfo()
{
    param([switch]$Recurse)
    if($Recurse){
            get-childitem * -Recurse -include *.dll,*.exe | foreach-object { "{0}\{1}`t{2}" -f $_.Directory,  $_.Name, [System.Diagnostics.FileVersionInfo]::GetVersionInfo($_).FileVersion }
    } else{
        get-childitem * -include *.dll,*.exe | foreach-object { "{0}`t{1}" -f $_.Name, [System.Diagnostics.FileVersionInfo]::GetVersionInfo($_).FileVersion }
    }
}