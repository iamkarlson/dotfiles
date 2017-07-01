function ExtractTo-Folder{
    param(
            [Parameter(Mandatory=$True)]
            [string]$filePath
         )
    $file = get-item $filePath
    Write-Host "Extracting "$file.FullName
    Write-Host "Extension "$file.Extension
    $exts = ".zip",".7z",".rar" 
    if(!($exts -contains $file.Extension)){
        Write-Host "extract operation can be performed only for archive files"
        return
    }

    $fileName = [io.path]::GetFileNameWithoutExtension($file.FullName)
#mkdir $fileName
    $output = [system.io.path]::combine($file.Directory.FullName,$fileName)
    Write-Host "Extracting to output $output"
    7z x -o"$output" $file.FullName
}
