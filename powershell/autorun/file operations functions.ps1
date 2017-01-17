function mkfile{

    [Cmdletbinding(SupportsShouldProcess=$TRUE)]
    param
    (
        [parameter(Mandatory=$True, ValueFromPipeline=$True, Position=0)]
        [String[]]$Creatingfile
    )

    If (Test-Path $Creatingfile)
    {
        Write-Host 'File already exist';
        Return;
    }

    New-Item -Type File $Creatingfile
    if(-not (Test-Path $Creatingfile)){
        return "File not create";
    }
    Return "File created";
}