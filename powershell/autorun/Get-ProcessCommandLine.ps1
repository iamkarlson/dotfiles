function Get-ProcessCommandline () {
    param
    (
        [string] $searchWord
    )

    if((Get-Process $searchWord).Length -gt 0){
        $procName = (Get-Process $searchWord)[0].Name
        $tempFile = [System.IO.Path]::GetTempFileName()
        WMIC /OUTPUT:$tempFile PROCESS get Caption,Commandline,Processid
        cat $tempFile|sls $procName|%{$_ -replace " +"," "}
        rm $tempFile
    }
}
