export TERM="xterm-256color"
setopt NO_BEEP
unsetopt BG_NICE

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch notify
bindkey -v

# Path to your oh-my-zsh installation.
export ZSH=/home/iamkarlson/.oh-my-zsh

# you have to run it once
# source ~/.fonts/*.sh

# Antigen
ANTIGEN_LOG=/tmp/antigen.log
POWERLEVEL9K_MODE='awesome-fontconfig'

source ~/.zsh/antigen.zsh

#source ~/.shellrc
source ~/.zsh/aliases.zshrc

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
alias vim=nvim
alias cat=batcat
alias zshconfig="vim ~/.zshrc"
alias vimconfig="vim ~/.vimrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#


POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir rbenv virtualenv pyenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs history time battery)

POWERLEVEL9K_PROMPT_ON_NEWLINE=true


test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export PATH="/usr/local/sbin:$PATH"

export ANT_HOME=/usr/local/share/ant
export MAVEN_HOME=/usr/local/share/maven
export GRADLE_HOME=/usr/local/share/gradle


export ANDROID_HOME=$HOME/Library/Android/sdk
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export ANDROID_AVD_HOME=$HOME/.android/avd

export PATH=$PATH:$ANDROID_SDK_ROOT/tools/bin
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools
export PATH=$PATH:$ANDROID_SDK_ROOT/emulator

export PATH=$ANT_HOME/bin:$PATH
export PATH=$MAVEN_HOME/bin:$PATH
export PATH=$GRADLE_HOME/bin:$PATH
export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH
export PATH=$ANDROID_HOME/build-tools/19.1.0:$PATH

eval $(thefuck --alias)

eval "$(rbenv init -)"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
ZSH_DISABLE_COMPFIX=true

eval "$(pyenv init -)"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
export ZSH_WAKATIME_BIN=/usr/local/bin/wakatime
