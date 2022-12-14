#!/bin/zsh

src="$HOME/src/"
dotfiles="$src/dotfiles"



mkdir ~/.config

echo "creating git config at ~/git"
(ls ~/git >> /dev/null 2>&1 && echo "~/git exists") || ln -ds "$dotfiles/git" ~/git

ln -s ~$dotfiles/git/system.gitconfig ~/.gitconfig


# install onedrive syncer from https://github.com/abraunegg/onedrive
#
mkdir -p ~/.config/onedrive

ln -s ~$dotfiles/onedrive.config ~/.config/onedrive/config
ln -s ~$dotfiles/sync_list.config ~/.config/onedrive/sync_list



echo "creating espanso config at ~/.config/espanso"
(ls ~/.config/espanso >> /dev/null 2>&1 && echo "espanso config exists") || ln -ds ~$dotfiles/espanso ~/.config/espanso

(ls ~/.doom.d >> /dev/null 2>&1 && echo "doom emacs config exists") || ln -ds ~$dotfiles/doomemacs ~/.doom.d




ln -ds ~$dotfiles/zsh/.zsh ~/.zsh
ln -s ~$dotfiles/zsh/.shellrc ~/.shellrc
ln -s ~$dotfiles/zsh/.zshrc ~/.zshrc


ln -s ~$dotfiles/.tmux.conf ~/.tmux.conf


mkdir -p ~/.config/alacritty
ln -s ~$dotfiles/alacritty.yml ~/.config/alacritty/alacritty.yml

ln -s ~$dotfiles/vim/.vsvimrc ~/.vsvimrc
ln -s ~$dotfiles/vim/.ideavimrc ~/.ideavimrc
ln -s ~$dotfiles/vim/.vimrc ~/.vimrc

(ls ~/.gvimrc >> /dev/null 2>&1 && echo "~/.gvimrc exists") || ln -s ~$dotfiles/vim/.gvimrc ~/.gvimrc

mkdir -p ~/.config/nvim

ln -s ~$dotfiles/vim/init.vim ~/.config/nvim/init.vim

mkdir ~/.vim
mkdir ~/.vim/backup_files
mkdir ~/.vim/swap_files
mkdir ~/.vim/undo_files

ln -s ~$dotfiles/vim/autocmd.vimrc ~/.vim/autocmd.vimrc
ln -s ~$dotfiles/vim/plugin-settings.vimrc ~/.vim/plugin-settings.vimrc
ln -s ~$dotfiles/vim/ide.vimrc ~/.vim/ide.vimrc
ln -s ~$dotfiles/vim/simple-mapping.vimrc ~/.vim/simple-mapping.vimrc
ln -s ~$dotfiles/vim/mapping.vimrc ~/.vim/mapping.vimrc
ln -ds ~$dotfiles/vim/plugins ~/.vim/plugins


# generating shell completions for tools
kubectl completion zsh > ~/.zsh/completions/_kubectl
helm completion zsh > ~/.zsh/completions/_helm
k3d completion zsh > ~/.zsh/completions/_k3d
minikube completion zsh > ~/.zsh/completions/_minikube


#(ls ~/.autoenv >> /dev/null 2>&1 && echo "autoenv already installed") || git clone git@github.com:hyperupcall/autoenv.git ~/.autoenv
