$repoPath = 's:\sideprojects\dotfiles'
cmd /c mklink .vimrc $repoPath\vim\.vimrc 
cmd /c mklink .vsvimrc $repoPath\vim\.vsvimrc 
cmd /c mklink /d .vim $repoPath\vim\ 
