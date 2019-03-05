ag "$search" --no-heading|%{($_ -split ":")[0]}
|%{$filePath = $_;

$search = '<PackageReference Include="Microsoft.AspNetCore.All" Version="2.2.2" />';
$replacement = '<PackageReference Include="Microsoft.AspNetCore.All" />';
Get-ChildItem -recurse | Select-String -pattern "$search" | group path | select name |%{$filePath = $_.Name; (Get-Content $filePath).Replace("$search","$replacement")|Set-Content -Path $filePath};
