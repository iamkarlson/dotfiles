
Set-Alias g git
set-Alias less more

function git-push{
    $git_out = (cmd /c git push 2`>`&1);
    cmd /c $git_out[3]
}

function tgit-log(){ TortoiseGitProc /command:log /path:.}


function global:Git-CommitAndPush(){
    param([string]$Message)
    git add .;
    git commit -m "$Message";
    git push;
}
