# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch notify
bindkey -v

# Path to your oh-my-zsh installation.
export ZSH=/home/iamkarlson/.oh-my-zsh

# Antigen
ANTIGEN_LOG=/tmp/antigen.log
POWERLEVEL9K_MODE='awesome-fontconfig'

source ~/.zsh/antigen.zsh

source ~/.shellrc

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="false"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=13

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

HIST_STAMPS="yyyy-mm-dd"

zstyle ':completion:*:paths' accept-exact '[^.]' '^(*/.)'

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias zshconfig="vim ~/.zshrc"
alias vimconfig="vim ~/.vimrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#


POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir rbenv virtualenv pyenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs history time battery)

POWERLEVEL9K_PROMPT_ON_NEWLINE=true

