function Start-RemoteProcess{
    param
    (
        [Parameter(Mandatory=$True)]
        [string] $app,
        [string] $arguments
    )

    $pwd = (pwd).path
    $runCommand = "pushd `"$pwd`";
    Invoke-Command {" +
    " $app";
    if($arguments){
        $runCommand = $runCommand+" `"$arguments`""
    }
    $runCommand = $runCommand+"}";

    Set-Content -Path "~/remote_run.ps1" -Value $runCommand
    Get-ScheduledTask  "run remote UI app"|Start-ScheduledTask
}

Set-Alias rr Start-RemoteProcess

