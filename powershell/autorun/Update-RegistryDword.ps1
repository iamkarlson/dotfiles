function Update-RegistryDword
{
    param
    (
        [string] $RegistryPath,
        [string] $Name,
        [string] $Value
    )

IF(!(Test-Path $registryPath))
{
    New-Item -Path $registryPath -Force | Out-Null
}

    New-ItemProperty -Path $registryPath -Name $name -Value $value  -PropertyType DWORD -Force | Out-Null
}