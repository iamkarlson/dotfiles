function Start-Sqlserver{
    
    gsv 'MSSQL$*'|sasv
    gsv '*SQL*'|sasv
}

function Stop-Sqlserver{
    gsv '*SQL*'|spsv
    gsv 'MSSQL$*'|spsv
}
