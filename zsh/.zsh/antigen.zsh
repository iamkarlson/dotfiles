
case ${TERM} in
    xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
        ANTIGEN_CACHE=$HOME/.antigen/init-xterm.zsh
    ;;
    *)
        ANTIGEN_CACHE=$HOME/.antigen/init-other.zsh
    ;;
esac

source ~/dev/sources/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle pip
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle Tarrasch/zsh-autoenv
antigen bundle zsh-users/zsh-completions


POWERLEVEL9K_INSTALLATION_PATH=/home/iamkarlson/.antigen/bundles/bhilburn/powerlevel9k/powerlevel9k.zsh-theme

antigen theme bhilburn/powerlevel9k powerlevel9k 
# Tell Antigen that you're done.
antigen apply 
