#!/usr/bin/env bash
#
# Purpose of this file is to create symlinks to configuration files I have in my repo.

src="$HOME/src"
dotfiles="$src/dotfiles"
clear
echo "setting up links from $dotfiles"

# 0. links creation functions
function ln_file() {
	echo "linking file from $1 to $2"
	(ls $2 >> /dev/null 2>&1 && echo "$2 exists") || ln -s $1 $2
	ln -s $1 $2

	echo "rm $2">>remove_links.sh
}

function ln_directory(){
	echo "linking directory from $1 to $2"
	(ls $2 >> /dev/null 2>&1 && echo "$2 exists") || ln -ds $1 $2
	echo "rm $2">>remove_links.sh
}


# Create user home .config 

mkdir -p ~/.config

# Git configuration
echo "creating git config at ~/git"
ln_directory "$dotfiles/git" ~/git


ln_file "~$dotfiles/git/system.gitconfig" "~/.gitconfig"

#
# Onedrive syncer 
# URL: https://github.com/abraunegg/onedrive
#

mkdir -p ~/.config/onedrive

ln -s ~$dotfiles/onedrive.config ~/.config/onedrive/config
ln -s ~$dotfiles/sync_list.config ~/.config/onedrive/sync_list


# Zsh settings

ln_directory "~$dotfiles/zsh/.zsh" "~/.zsh"

(ls ~/.zsh/private.zshrc >> /dev/null 2>&1 && echo "private zshrc exists") || touch ~/.zsh/private.zshrc

ln_file "~$dotfiles/zsh/.shellrc" "~/.shellrc"
ln_file "~$dotfiles/zsh/.zshrc" "~/.zshrc"


#
# Tmux
#

ln -s ~$dotfiles/.tmux.conf ~/.tmux.conf

#
# Espanso text expander
# URL: https://espanso.org
#

ln_directory "~$dotfiles/espanso" "~/.config/espanso"


ln_directory "~$dotfiles/doomemacs" "~/.doom.d"


function alacritty(){
	mkdir -p ~/.config/alacritty
	ln -s ~$dotfiles/alacritty.yml ~/.config/alacritty/alacritty.yml
}

function vim() {
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
}

function autocomplete(){
	# generating shell completions for tools
	kubectl completion zsh > ~/.zsh/completions/_kubectl
	helm completion zsh > ~/.zsh/completions/_helm
	k3d completion zsh > ~/.zsh/completions/_k3d
	minikube completion zsh > ~/.zsh/completions/_minikube
}


#(ls ~/.autoenv >> /dev/null 2>&1 && echo "autoenv already installed") || git clone git@github.com:hyperupcall/autoenv.git ~/.autoenv
