#!/bin/zsh
mkdir ~/.config

(ls ~/git >> /dev/null 2>&1 && echo "~/git exists") || ln -ds ~/src/dotfiles/git/git ~/git


(ls ~/.config/espanso >> /dev/null 2>&1 && echo "espanso config exists") || ln -ds ~/src/dotfiles/espanso ~/.config/espanso
ln -s ~/src/dotfiles/git/system.gitconfig ~/.gitconfig

ln -ds ~/src/dotfiles/zsh/.zsh ~/.zsh
ln -s ~/src/dotfiles/zsh/.shellrc ~/.shellrc
ln -s ~/src/dotfiles/zsh/.zshrc ~/.zshrc

ln -s ~/src/dotfiles/.tmux.conf ~/.tmux.conf

mkdir -p ~/.config/alacritty
ln -s ~/src/dotfiles/alacritty.yml ~/.config/alacritty/alacritty.yml

ln -s ~/src/dotfiles/vim/.vsvimrc ~/.vsvimrc
ln -s ~/src/dotfiles/vim/.ideavimrc ~/.ideavimrc
ln -s ~/src/dotfiles/vim/.vimrc ~/.vimrc

(ls ~/.gvimrc >> /dev/null 2>&1 && echo "~/.gvimrc exists") || ln -s ~/src/dotfiles/vim/.gvimrc ~/.gvimrc

mkdir -p ~/.config/nvim

ln -s ~/src/dotfiles/vim/init.vim ~/.config/nvim/init.vim

mkdir ~/.vim
mkdir ~/.vim/backup_files
mkdir ~/.vim/swap_files
mkdir ~/.vim/undo_files

ln -s ~/src/dotfiles/vim/autocmd.vimrc ~/.vim/autocmd.vimrc
ln -s ~/src/dotfiles/vim/plugin-settings.vimrc ~/.vim/plugin-settings.vimrc
ln -s ~/src/dotfiles/vim/ide.vimrc ~/.vim/ide.vimrc
ln -s ~/src/dotfiles/vim/simple-mapping.vimrc ~/.vim/simple-mapping.vimrc
ln -s ~/src/dotfiles/vim/mapping.vimrc ~/.vim/mapping.vimrc
ln -ds ~/src/dotfiles/vim/plugins ~/.vim/plugins


(ls ~/.doom.d >> /dev/null 2>&1 && echo "doom emacs config exists") || ln -ds ~/src/dotfiles/doomemacs ~/.doom.d

mkdir -p ~/.config/onedrive

ln -s ~/src/dotfiles/onedrive.config ~/.config/onedrive/config
ln -s ~/src/dotfiles/sync_list.config ~/.config/onedrive/sync_list

# generating shell completions for tools
kubectl completion zsh > ~/.zsh/completions/_kubectl
helm completion zsh > ~/.zsh/completions/_helm
k3d completion zsh > ~/.zsh/completions/_k3d
minikube completion zsh > ~/.zsh/completions/_minikube


#(ls ~/.autoenv >> /dev/null 2>&1 && echo "autoenv already installed") || git clone git@github.com:hyperupcall/autoenv.git ~/.autoenv
