function Get-LaunchSettings(){
    ls -Recurse -Filter launchSettings.json|%{
        $fileName = $_.Directory.Parent; gc $_.FullName|ConvertFrom-Json|%{
            $url=$_.profiles."$fileName".applicationUrl; 
            Write-Host "$fileName : $url";
        }
    }
}
