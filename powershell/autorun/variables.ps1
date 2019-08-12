
Set-Variable -Name Projects -Value "s:\work\" -Scope Global

$envDrop = $env:Dropbox;
if(Test-Path($envDrop)){
    Set-Variable -Name Dropbox -Value "$envDrop\Apps" -Scope Global;
} else {
    Set-Variable -Name Dropbox -Value "\\Mac\Dropbox\Apps" -Scope Global;
}


Set-Variable -Name ntw -Value "$Projects\NT.Wallet" -Scope Global

Set-Variable -Name kiosk -Value "$Projects\kiosk" -Scope Global

Set-Variable -Name TauResFolder -Value $env:AppData"\Taunigma\Resources" -Scope Global
