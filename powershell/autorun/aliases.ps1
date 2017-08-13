Set-Alias gh Get-Help
Set-Alias ping Test-Connection
Set-Alias install Install-Package
Set-Alias vs -Value "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\devenv.exe"

function ex{exit}

Set-Alias g git

function gign(){ git ls-files -v | sls "^h" -CaseSensitive }

function gd(){ git diff }
function g-files { git diff --cached --name-only}

function g-stagedFiles { git diff --cached --name-only}
set-Alias less more

function tgit-log(){ TortoiseGitProc /command:log /path:.}


Set-Alias quit ex
Set-Alias :q ex


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


function pwd-clip {($pwd).Path |clip.exe}
