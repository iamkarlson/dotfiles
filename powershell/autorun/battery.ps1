function global:Get-BatteryChargeStatus
{
    <#
    .Synopsis
    Get-BatteryChargeStatus
    .DESCRIPTION
    Get-BatteryChargeStatus shows the Battery Charging status,
    the remaining Battery capacity in percent and if the system
    is running on Battery.

    The Battery Status can have one of the following values:
    Charging, Discharging, Idle or NotPresent
    .PARAMETER Detail
    Displays additional Battery Information

    .EXAMPLE
    Get-BatteryChargeStatus

    Status Utilization PowerOnline
    ------ ----------- -----------
    Charging        99        True


    .EXAMPLE
    Get-BatteryChargeStatus -Detail

    ChargeRateInMilliwatts             : 3052
    DesignCapacityInMilliwattHours     : 68902
    FullChargeCapacityInMilliwattHours : 70222
    RemainingCapacityInMilliwattHours  : 69689
    Status                             : Charging
    Utilization                        : 99
    PowerOnline                        : True

    .NOTES
    30/07/2017, Initial version, Alex Verboon
    01/08/2017, added class init to check that Windows.Device class is available. 

    For more information see: 
    https://docs.microsoft.com/en-us/uwp/api/windows.devices.power.batteryreport

#>


    [CmdletBinding()]
    Param
    (
     [switch]$Detail
    )

    Begin
    {
        Try{
# First ensure Windows.Devices class is available            
            $BattAssembly = [Windows.Devices.Power.Battery,Windows.Devices.Power.Battery,ContentType=WindowsRuntime] 
#[Windows.Devices.Power.Battery].Assembly
        }
        Catch
        {
            Write-Error "Unable to load the Windows.Devices.Power.Battery class"
        }

        Try{
            $Report = [Windows.Devices.Power.Battery]::AggregateBattery.GetReport() 
        }
        Catch{
            Write-Error "Unable to retrieve Battery Report information"
            Break
        }

        If ($Report.Status -ne "NotPresent")
        {
            $pbmax = [convert]::ToDouble($Report.FullChargeCapacityInMilliwattHours)
            $pbvalue = [convert]::ToDouble($Report.RemainingCapacityInMilliwattHours)
            $Utilization = [int][math]::Round( (($pbvalue / $pbmax) *100))
            $PowerOnlineStatus = (Get-CimInstance -ClassName batterystatus -Namespace root/WMI).PowerOnline

# Check if at least one battery reports running on power
            If ($PowerOnlineStatus -contains "True")
            {
                $PowerOnline = $true
            }
            Else
            {
                $PowerOnline = $false
            }
        }
        Else
        {
            [int]$Utilization = 0
            $PowerOnline = ""
        }
    }


    Process
    {
        If ($Detail -eq $true)
        {
            $Properties = [ordered] @{
                ChargeRateInMilliwatts = $Report.ChargeRateInMilliwatts
                DesignCapacityInMilliwattHours = $report.DesignCapacityInMilliwattHours
                FullChargeCapacityInMilliwattHours = $Report.FullChargeCapacityInMilliwattHours
                RemainingCapacityInMilliwattHours = $Report.RemainingCapacityInMilliwattHours
                Status = $Report.Status
                Utilization = $Utilization
                PowerOnline = $PowerOnline
            }
            $BatteryChargeStatus = (New-Object -TypeName PSObject -Property $Properties)
        }
        Elseif ($Detail -eq $false)
        {
            $Properties = [ordered] @{
                Status = $Report.Status
                Utilization = $Utilization
                PowerOnline = $PowerOnline
            }
            $BatteryChargeStatus = (New-Object -TypeName PSObject -Property $Properties)
        }
    }
    End
    {
        $BatteryChargeStatus
    }
}

function global:Get-BatterySymbol{
    $battery = (Get-BatteryChargeStatus);
    if($battery.PowerOnline){
        if($battery.Utilization -lt 30) {
            <#nf-mdi-battery_charging_20#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf585); 
        }
        elseif($battery.Utilization -lt 40) {
            <#nf-mdi-battery_charging_30#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf586); 
        }
        elseif($battery.Utilization -lt 60) {
            <#nf-mdi-battery_charging_40#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf587); 
        }
        elseif($battery.Utilization -lt 80){
            <#nf-mdi-battery_charging_60#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf588); 
        }
        elseif($battery.Utilization -lt 90){
            <#nf-mdi-battery_charging_80#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf589); 
        }
        elseif($battery.Utilization -lt 100){
            <#nf-mdi-battery_charging_90#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf58a); 
        }
        else {
            <#nf-mdi-battery_charging_100#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf584); 
        }
    }
    else{
        if($battery.Utilization -lt 10){
            <#nf-mdi-battery_alert#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf582);
        }
        elseif($battery.Utilization -lt 20){
            <#nf-mdi-battery_10#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf579); 
        }
        elseif($battery.Utilization -lt 30){
            <#nf-mdi-battery_20#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf57a); 
        }
        elseif($battery.Utilization -lt 40){
            <#nf-mdi-battery_30#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf57b); 
        }
        elseif($battery.Utilization -lt 50){
            <#nf-mdi-battery_40#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf57c); 
        }
        elseif($battery.Utilization -lt 60){
            <#nf-mdi-battery_50#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf57d); 
        }
        elseif($battery.Utilization -lt 70){
            <#nf-mdi-battery_60#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf57e); 
        }
        elseif($battery.Utilization -lt 80){
            <#nf-mdi-battery_70#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf57f); 
        }
        elseif($battery.Utilization -lt 90){
            <#nf-mdi-battery_80#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf580); 
        }
        elseif($battery.Utilization -lt 100){
            <#nf-mdi-battery_90#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf581); 
        } else{
            <#nf-mdi-battery#>
            $batterySymbol =[char]::ConvertFromUtf32(0xf578);
        }
    }
    return $batterySymbol;
}
