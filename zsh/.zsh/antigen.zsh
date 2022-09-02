
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
antigen bundle sobolevn/wakatime-zsh-plugin
antigen bundle pyenv
antigen bundle trystan2k/zsh-tab-title

antigen bundle bobthecow/git-flow-completion

#POWERLEVEL9K_INSTALLATION_PATH=~/.antigen/bundles/bhilburn/powerlevel9k/powerlevel9k.zsh-theme
POWERLEVEL9K_INSTALLATION_PATH=~/.antigen/bundles/romkatv/powerlevel10k/powerlevel9k.zsh-theme

#antigen theme bhilburn/powerlevel9k powerlevel9k 
antigen theme romkatv/powerlevel10k
# Tell Antigen that you're done.
antigen apply 
