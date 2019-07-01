
Set-Alias g git


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


function Git-MergeInMaster {
    $git_branch_name = (git rev-parse --abbrev-ref HEAD);
    $develop = (git config --local --get gitflow.branch.develop)
    g st;
    Write-Host "[[[stashed]]]"
    g co $develop;
    g merge $git_branch_name;
    g push;
    g co $git_branch_name;
    g stash pop;
}
function Git-MergeMaster {
    $git_branch_name = (git rev-parse --abbrev-ref HEAD);
    $develop = (git config --local --get gitflow.branch.develop)
    g st;
    Write-Host "[[[stashed]]]"
    g co $develop;
    g pl;
    g co $git_branch_name;
    g merge $develop;
    g stash pop;
}


function gign(){ git ls-files -v | sls "^h" -CaseSensitive }
