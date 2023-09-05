
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias vim=nvim
#alias cat=batcat
alias zshconfig="vim ~/.zshrc"
alias vimconfig="vim ~/src/dotfiles/vim/.vimrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#

alias k="kubectl"
alias g="git"

alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

alias kopy="kitty +kitten clipboard"

alias rm-no-trash='/usr/bin/rm'
alias rm='rmtrash'
alias rmdir='rmdirtrash'

alias sudo='sudo '


alias poetry_activate='source "$( poetry env info --path )/bin/activate"'

timer_countdown() {
  local total_seconds=300

  if [[ $1 =~ ([0-9]+)h ]]; then
    total_seconds=$((total_seconds + ${match[1]} * 3600))
  fi

  if [[ $1 =~ ([0-9]+)m ]]; then
    total_seconds=$((total_seconds + ${match[1]} * 60))
  fi

  if [[ $1 =~ ([0-9]+)s ]]; then
    total_seconds=$((total_seconds + ${match[1]}))
  fi
  {sleep $total_seconds && notify-send "Timer countdown $1" "Time's up!" }&
}
