#!/bin/zsh
ln -ds ~/dev/dotfiles/git/git ~/git
ln -ds ~/dev/dotfiles/zsh/.zsh ~/.zsh

ln -s ~/dev/dotfiles/git/system.gitconfig ~/.gitconfig
ln -s ~/dev/dotfiles/vim/.vsvimrc ~/.vsvimrc
ln -s ~/dev/dotfiles/vim/.vimrc ~/.vimrc
ln -s ~/dev/dotfiles/zsh/.shellrc ~/.shellrc
ln -s ~/dev/dotfiles/zsh/.zshrc ~/.zshrc

ln -s ~/dev/dotfiles/.tmux.conf ~/.tmux.conf
mkdir ~/.config

ln -s ~/dev/dotfiles/vim/init.vim ~/.config/nvim/init.vim
mkdir ~/.vim
ln -s ~/dev/dotfiles/vim/autocmd.vimrc ~/.vim/autocmd.vimrc
ln -s ~/dev/dotfiles/vim/plugin-settings.vimrc ~/.vim/plugin-settings.vimrc
ln -s ~/dev/dotfiles/vim/ide.vimrc ~/.vim/ide.vimrc
ln -s ~/dev/dotfiles/vim/simple-mapping.vimrc ~/.vim/simple-mapping.vimrc
ln -s ~/dev/dotfiles/vim/mapping.vimrc ~/.vim/mapping.vimrc
