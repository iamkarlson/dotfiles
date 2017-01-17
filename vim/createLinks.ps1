push-location $HOME
$repoPath = 's:\sideprojects\dotfiles'
cmd /c mklink .vimrc $repoPath\vim\.vimrc 
cmd /c mklink .gvimrc $repoPath\vim\.gvimrc 
cmd /c mklink .vsvimrc $repoPath\vim\.vsvimrc 
cmd /c mklink /d .vim $repoPath\vim\ 
push-location .config
push-location nvim
cmd /c mklink init.vim $repoPath\vim\init.vim
cmd /c mklink ginit.vim $repoPath\vim\ginit.vim

pop-location
pop-location
pop-location
