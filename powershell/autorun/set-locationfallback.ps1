function Set-LocationWithFallback{
    param([string] $path)

    $exist=Test-Path $path;

    if($exist){
        Set-Location $path;
    }else{
        Set-Location 'c:\'
    }
}
