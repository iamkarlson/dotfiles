function alokhomora
{
    param
    (
        [string] $pass
    )

    Unlock-BitLocker  -MountPoint "S:" -Password (ConvertTo-SecureString "$pass" -AsPlainText -Force)
}