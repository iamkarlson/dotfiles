function Find-Alias{

    param
    (
        [string] $aliasName
    )
    return    Get-Alias |?{$_.Definition -match ".*$aliasName.*"}
}
