
function Pandoc-OutHtml{
    param(
            [string] $InputFile,
            [string] $Output,
            [Parameter(Mandatory=$false)]
            [Switch]$ReplaceTitle
         )

    if([string]::IsNullOrWhiteSpace($InputFile)){
        $InputFile = (Resolve-Path "README.md").Path
    }

    if([string]::IsNullOrWhiteSpace($Output)){
        $filePath = (Resolve-Path $InputFile).Path
        $file = [system.io.fileinfo]  $filePath
        $Output = [system.io.path]::Combine($file.Directory, $file.Name.Substring(0,$file.Name.Length - $file.Extension.Length)+".html")
    }

    $cssHeaderTemplateFile = [io.path]::combine($Dropbox,"pandoc","header.html.template")
    Write-Host $cssHeaderTemplateFile 

    Write-Host $InputFile 
    Write-Host $Output

    $title = ($InputFile -replace ".md","")
    if($ReplaceTitle -eq $true) {
        $command = "pandoc.exe -s --toc -H $cssHeaderTemplateFile $InputFile -o $Output --number-sections --from=markdown_strict --metadata title=$title"
        Write-Host $command
        pandoc.exe -s --toc -H $cssHeaderTemplateFile $InputFile -o $Output --number-sections --from=markdown_strict --metadata title=$title
    } else {
        $command = "pandoc.exe -s --toc -H $cssHeaderTemplateFile $InputFile -o $Output --number-sections --from=markdown_strict"
        Write-Host $command
        pandoc.exe -s --toc -H $cssHeaderTemplateFile $InputFile -o $Output --number-sections --from=markdown_strict
    }
}
