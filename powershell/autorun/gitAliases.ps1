
function gd(){ git diff }
function g-files { git diff --cached --name-only}

function g-stagedFiles { git diff --cached --name-only}


function git-push{
    $git_out = (cmd /c git push 2`>`&1);
    cmd /c $git_out[3]
}

function tgit-log(){ TortoiseGitProc /command:log /path:.}




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

function fuck-it(){
    git reset --hard; 
    g clean -fd;
    $develop = (git config --local --get gitflow.branch.develop)
    g co $develop;
    g pl;
}
