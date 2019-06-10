
case ${TERM} in
    xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
        ANTIGEN_CACHE=~/.antigen/init-xterm.zsh
    ;;
    *)
        ANTIGEN_CACHE=~/.antigen/init-other.zsh
    ;;
esac

source ~/antigen/antigen.zsh|| source /usr/local/share/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle pip
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle Tarrasch/zsh-autoenv
antigen bundle zsh-users/zsh-completions
antigen bundle rupa/z


POWERLEVEL9K_INSTALLATION_PATH=~/.antigen/bundles/bhilburn/powerlevel9k/powerlevel9k.zsh-theme

antigen theme bhilburn/powerlevel9k powerlevel9k 
# Tell Antigen that you're done.
antigen apply 
