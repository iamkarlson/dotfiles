export TERM="xterm-256color"
setopt NO_BEEP
unsetopt BG_NICE
export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY

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
source ~/.zsh/private.zshrc

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
    
    
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir rbenv virtualenv pyenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs history time battery)

POWERLEVEL9K_PROMPT_ON_NEWLINE=true


#autoenv settings
AUTOENV_FILE_ENTER=.autoenv.zsh
AUTOENV_HANDLE_LEAVE=1
AUTOENV_FILE_LEAVE=.autoenv.zsh


test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export PATH="/usr/local/sbin:$PATH"

fpath+=$HOME/.local/share/zsh/site-functions
fpath+=~/.zsh/completions

PATH=$HOME/.local/bin:$PATH
PATH=$HOME/bin:$PATH
MANPATH=$HOME/.local/share/man:$MANPATH
INFOPATH=$HOME/.local/share/info:$INFOPATH

#cargo bins
source $HOME/.cargo/env

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

export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
ZSH_DISABLE_COMPFIX=true

eval "$(pyenv init -)"

eval "$(pyenv virtualenv-init -)"

export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"


if (( $+commands[rbenv] ))
then
    eval "$(rbenv init -)"
fi

if (( $+commands[jenv] ))
then
    export PATH="$HOME/.jenv/bin:$PATH"
    eval "$(jenv init -)"
fi

export ZSH_WAKATIME_BIN=/usr/local/bin/wakatime

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

# hotfix for minikube completions 
source <(minikube completion zsh | sed --expression='s/aliashash\["\([a-z]*\)"\]/aliashash[\1]/g')

export PATH="$HOME/.poetry/bin:$PATH"

# zsh tab title settings
ZSH_TAB_TITLE_ADDITIONAL_TERMS='alacritty|kitty|foot'
