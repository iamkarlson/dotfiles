While(1) {
    cls;
    Get-Process |`
    Sort-Object -Descending cpu |`
    select -f 15 | ft -a;
    sleep 1;
} 
