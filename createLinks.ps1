push-location $HOME
$repoPath = 'c:\src\dotfiles'
cmd /c mklink .vimrc $repoPath\vim\.vimrc 
cmd /c mklink .ideavimrc $repoPath\vim\.ideavimrc 
cmd /c mklink .gvimrc $repoPath\vim\.gvimrc 
cmd /c mklink .vsvimrc $repoPath\vim\.vsvimrc 
cmd /c mklink /d .vim $repoPath\vim\ 
mkdir .config -ErrorAction Ignore

push-location .config
mkdir nvim -ErrorAction Ignore

push-location nvim
cmd /c mklink init.vim $repoPath\vim\init.vim
cmd /c mklink ginit.vim $repoPath\vim\ginit.vim
pop-location

pop-location

push-location Documents
mkdir Powershell -ErrorAction Ignore

push-location PowerShell
cmd /c mklink Microsoft.PowerShell_profile.ps1 $repoPath\powershell\Microsoft.PowerShell_profile.ps1
cmd /c mklink /d modules $repoPath\powershell\Modules

pop-location

pop-location