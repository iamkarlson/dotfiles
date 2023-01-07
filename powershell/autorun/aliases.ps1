Set-Alias gh Get-Help
Set-Alias ping Test-Connection
Set-Alias install Install-Package
Set-Alias vs -Value "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\devenv.exe"

function ex{exit}



set-Alias less more


Set-Alias quit ex
Set-Alias :q ex

Set-Alias g git

function pwd-clip {($pwd).Path |clip.exe}
Set-Alias k kubectl

Set-Alias z Search-NavigationHistory
Set-Alias n nvim-qt.exe


# for editing your PowerShell profile
Function Edit-Profile
{
    nvim-qt.exe $profile
}

