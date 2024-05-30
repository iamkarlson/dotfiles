
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases

alias vim=nvim

alias e=emacsclient -r -n

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

# Function to create a notification with the notify-send command
# parameter $TIME is the time to wait before sending the notification
ring_the_bell(){
  local TIME=$1
  notify-send \
    --urgency=critical \
    --expire-time=120 \
    --icon=appointment-soon-symbolic \
  "Timer countdown $TIME" "Time's up!"
  echo "timer rang at $(date +'%H:%M:%S')"
}

timer_countdown() {
  # validate input
  #
  #validate that the time parameter is passed. Otherwise, echo an error message and return 1.

  if [[ -z $1 ]]; then
    echo "missing time parameter"
    echo "usage: timer_countdown 2h3m4s"
    return 1
  fi

  if [[ ! $1 =~ ^[0-9]+[hms]?$ ]]; then
    echo "wrong format"
    return 1
  fi

  local total_seconds=0

  if [[ $1 =~ ([0-9]+)h ]]; then
    echo "parsed ${match[1]} hours"
    total_seconds=$((total_seconds + ${match[1]} * 3600))
  fi

  if [[ $1 =~ ([0-9]+)m ]]; then
    echo "parsed ${match[1]} minutes"
    total_seconds=$((total_seconds + ${match[1]} * 60))
  fi

  if [[ $1 =~ ([0-9]+)s ]]; then
    echo "parsed ${match[1]} seconds"
    total_seconds=$((total_seconds + ${match[1]}))
  fi

  if [[ $1 =~ ^([0-9]+)$ ]]; then
    echo "parsed ${match[1]} seconds from number argument"
    total_seconds=$((total_seconds + ${match[1]}))
  fi

  if [[ $total_seconds -eq 0 ]]; then
    echo "no seconds parsed"
    return 1
  fi

  echo "total seconds: $total_seconds"
  {sleep $total_seconds && ring_the_bell $1 }&
}
