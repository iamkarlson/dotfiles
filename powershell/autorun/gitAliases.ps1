
Set-Alias g git
set-Alias less more

function tgit-log(){ TortoiseGitProc /command:log /path:.}


function global:Git-CommitAndPush(){
    param([string]$Message)
    git add .;
    git commit -m "$Message";
    git push;
}
