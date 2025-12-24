
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases

alias vim=nvim

alias e="~/.config/hypr/scripts/apps/emacs.sh"

alias k="kubectl"
alias g="git"
alias gs="git status"

alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

alias kopy="kitty +kitten clipboard"

alias rm-no-trash='/usr/bin/rm'
alias rm='rmtrash'
alias rmdir='rmdirtrash'

# why??
#alias sudo='sudo '

# arch distribution avoids a conflict with another tool
alias task='go-task'


alias poetry_activate='source "$( poetry env info --path )/bin/activate"'

