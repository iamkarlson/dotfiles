#!/usr/bin/env bash
#
# Purpose of this file is to create symlinks to configuration files I have in my repo.

target="$1"
src="$target/src"
dotfiles="$src/dotfiles"
clear
echo "setting up links from $dotfiles"
echo "targeting into $target"
read -p "Are you sure? " -n 1 -r
echo    # (optional) move to a new line
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

# 0. links creation functions

function ln_file() {
	echo "linking file from $1 to $2"
	(ls $2 >> /dev/null 2>&1 && echo "$2 exists") || ln -s $1 $2
	echo "# Removing file -------------------------------------- $2">>remove_links.sh
	echo "delete_link \"$2\"">>remove_links.sh
}

function ln_directory(){
	echo "linking directory from $1 to $2"
	(ls $2 >> /dev/null 2>&1 && echo "$2 exists") || ln -ds $1 $2
	echo "# Removing directory ********************************* $2">>remove_links.sh
	echo "delete_link \"$2\"">>remove_links.sh
}
function delete_link(){
	[-h $1] && rm -i $1 
}

# Create user home .config 

mkdir -p $target/.config

# Git configuration
echo "creating git config at $target/git"
ln_directory "$dotfiles/git/git" $target/git


ln_file "$dotfiles/git/system.gitconfig" "$target/.gitconfig"

#
# Onedrive syncer 
# URL: https://github.com/abraunegg/onedrive
#

mkdir -p $target/.config/onedrive

ln_file $dotfiles/onedrive.config $target/.config/onedrive/config
ln_file $dotfiles/sync_list.config $target/.config/onedrive/sync_list


# Zsh settings

ln_directory "$dotfiles/zsh/scripts" "$target/.zsh"

(ls $target/.zsh/private.zshrc >> /dev/null 2>&1 && echo "private zshrc exists") || touch $target/.zsh/private.zshrc

ln_file "$dotfiles/zsh/.shellrc" "$target/.shellrc"
ln_file "$dotfiles/zsh/.zshrc" "$target/.zshrc"


#
# Tmux
#

ln_file $dotfiles/.tmux.conf $target/.tmux.conf

#
# Espanso text expander
# URL: https://espanso.org
#

ln_directory "$dotfiles/espanso" "$target/.config/espanso"


ln_directory "$dotfiles/doomemacs" "$target/.doom.d"


function alacritty(){
	mkdir -p $target/.config/alacritty
	ln_file $dotfiles/alacritty.yml $target/.config/alacritty/alacritty.yml
}

function vim() {
	ln_file $dotfiles/vim/.vsvimrc $target/.vsvimrc
	ln_file $dotfiles/vim/.ideavimrc $target/.ideavimrc
	ln_file $dotfiles/vim/.vimrc $target/.vimrc

	ln_file $dotfiles/vim/.gvimrc $target/.gvimrc

	mkdir -p $target/.config/nvim

	ln_file $dotfiles/vim/init.vim $target/.config/nvim/init.vim

	mkdir $target/.vim
	mkdir $target/.vim/backup_files
	mkdir $target/.vim/swap_files
	mkdir $target/.vim/undo_files

	ln_file $dotfiles/vim/autocmd.vimrc $target/.vim/autocmd.vimrc
	ln_file $dotfiles/vim/plugin-settings.vimrc $target/.vim/plugin-settings.vimrc
	ln_file $dotfiles/vim/ide.vimrc $target/.vim/ide.vimrc
	ln_file $dotfiles/vim/simple-mapping.vimrc $target/.vim/simple-mapping.vimrc
	ln_file $dotfiles/vim/mapping.vimrc $target/.vim/mapping.vimrc
	ln -ds $dotfiles/vim/plugins $target/.vim/plugins
}

function autocomplete(){
	# generating shell completions for tools
	kubectl completion zsh > $target/.zsh/completions/_kubectl
	helm completion zsh > $target/.zsh/completions/_helm
	k3d completion zsh > $target/.zsh/completions/_k3d
	minikube completion zsh > $target/.zsh/completions/_minikube
}


#(ls ~/.autoenv >> /dev/null 2>&1 && echo "autoenv already installed") || git clone git@github.com:hyperupcall/autoenv.git ~/.autoenv