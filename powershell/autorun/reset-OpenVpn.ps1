function Reset-OpenVpn {
    Get-NetAdapter | Where-Object {$_.InterfaceDescription -match "TAP-Windows*"} | Disable-NetAdapter -Confirm:$false
    Get-NetAdapter | Where-Object {$_.InterfaceDescription -match "TAP-Windows*"} | ForEach-Object{ Write-Host $_.InterfaceDescription "disabled"}
    Get-NetAdapter | Where-Object {$_.InterfaceDescription -match "TAP-Windows*"} | Enable-NetAdapter
    Get-NetAdapter | Where-Object {$_.InterfaceDescription -match "TAP-Windows*"} | ForEach-Object{ Write-Host $_.InterfaceDescription "enabled"}
    Get-Service "OpenVpn*" | Restart-Service
    Get-Service "OpenVpn*" | ForEach-Object {Write-Host $_.Name "restarted"}
    Write-Host "waiting for establishing connection 10 seconds....."
    Start-Sleep -Milliseconds 10000
    Write-Host "check connection to vpn server"
    Test-Connection 10.1.0.1
    Test-Connection 10.0.0.1
}