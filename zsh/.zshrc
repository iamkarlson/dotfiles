# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export TERM="xterm-256color"
unsetopt beep
setopt NO_BEEP
unsetopt BG_NICE
export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

HISTFILE=~/.histfile
export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY

setopt appendhistory autocd nomatch notify

# Work related and private stuff with sensetive data
source ~/.zsh/private.zshrc

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"
    


#
# Plugins settings
#


# zsh tab title settings
ZSH_TAB_TITLE_ADDITIONAL_TERMS='alacritty|kitty|foot'

# you have to run it once
# source ~/.fonts/*.sh

POWERLEVEL9K_MODE='awesome-fontconfig'

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir rbenv virtualenv pyenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs history time battery)

POWERLEVEL9K_PROMPT_ON_NEWLINE=true


# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="false"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

HIST_STAMPS="yyyy-mm-dd"

zstyle ':completion:*:paths' accept-exact '[^.]' '^(*/.)'


export PATH="/usr/local/sbin:$PATH"
PATH=$HOME/.local/bin:$PATH
PATH=$HOME/bin:$PATH
PATH="$HOME/.poetry/bin:$PATH"

fpath+=$HOME/.local/share/zsh/site-functions
fpath+=~/.zsh/completions

MANPATH=$HOME/.local/share/man:$MANPATH
INFOPATH=$HOME/.local/share/info:$INFOPATH



ZSH_DISABLE_COMPFIX=true

################################################################################
# Binaries, program settings, and other stuff that really goes unsorted
################################################################################

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"


eval $(thefuck --alias)

export ZSH_WAKATIME_BIN=/usr/bin/wakatime

# nvm - node.js version manager
source /usr/share/nvm/init-nvm.sh

# setting up pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# google shit for using their gcloud auth pluging instead of native k8s one when calling `get-credentials`
export USE_GKE_GCLOUD_AUTH_PLUGIN=True

################################################################################
# Setting up completion system. 
# It must go before plugins
################################################################################
# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:]}={[:upper:]}' '+r:|[._-]=** r:|=**' '+l:|=* r:|=*'
zstyle :compinstall filename '/home/iamkarlson/.zshrc'

# https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/docker
zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes

autoload -Uz compinit
compinit
# End of lines added by compinstall


################################################################################
# Setting up plugin manager
################################################################################


source '/usr/share/zsh-antidote/antidote.zsh'
antidote load


# Plugins are loaded from cached antidote bundle
# antidote bundle <~/.zsh/plugins.txt >~/.zsh_plugins.zsh

source ~/.zsh_plugins.zsh
source "$(antidote home)/https-COLON--SLASH--SLASH-github.com-SLASH-larkery-SLASH-zsh-histdb/sqlite-history.zsh"


################################################################################
# Setting up prompt, look&feel
# It must go after plugins
################################################################################
autoload -Uz promptinit && promptinit && prompt powerlevel10k


# set emacs key bindings
bindkey -e

source ~/.zsh/aliases.zshrc

# autoenv settings
# needs to go after plugins, because it overrides `cd`
#export AUTOENV_ENV_FILENAME=.autoenv
export AUTOENV_FILE_ENTER=.autoenv.zsh
export AUTOENV_AUTH_FILE=.autoenv
source /usr/share/nvm/init-nvm.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

neofetch | lolcat -
