# for editing your Vim settings
Function Edit-Vimrc
{
    vim $home\.vimrc
}


function gvim-new-tab(){
#$vimexe = "C:\Program Files (x86)\vim\vim80\gvim.exe"
    $vimexe = "C:\tools\neovim\Neovim\bin\nvim-qt.exe"
    if($args.Length -eq 0){
        & $vimexe
    } else{
#& $vimexe -p --remote-tab-silent $args
        & $vimexe $args
    }
}

Set-Alias gvim gvim-new-tab
