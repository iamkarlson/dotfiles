function Start-RemoteProcess{
    param
    (
        [string] $app,
        [string] $arguments
    )

    $pwd = (pwd).path
    $runCommand = "
cd /D `"$pwd`"
 $app `"$arguments`"
    "

    Set-Content -Path "~/remote_run.bat" -Value $runCommand
    Get-ScheduledTask  "run remote UI app"|Start-ScheduledTask
}
