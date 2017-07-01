function Group-ByDate(){
    [Cmdletbinding(SupportsShouldProcess=$TRUE)]
    param
    (
     [parameter(Mandatory=$True, ValueFromPipeline=$True, Position=0)]
     [String[]]$Folder
    )
    Write-Output "Arranging files in $folder"

    $exist=Test-Path $folder;

    if($exist){
        ls -Path $folder | % {
            write-output "Perform move operation to "$_.FullName
#            if($_.LastWriteTime -gt (Get-Date).AddDays(-2)){
#                return;
#            }

            $file = $_.FullName
            $date = Get-Date ($_.LastWriteTime)
            $month = $date.ToString('MM')
            $year = $date.ToString('yyyy')

            $newFolder = [system.io.path]::Combine($folder,"$year","$month")
            if(!(test-path $newFolder)){
                Write-output "Creating "$newFolder
                new-item -type Directory -path $newFolder
            }
            Write-Output "Moving "$file" to"$newFolder
            move-item $file -Destination $newFolder
        }
    }

}
