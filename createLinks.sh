#!/bin/zsh
ln -ds ~/src/dotfiles/git/git ~/git
ln -ds ~/src/dotfiles/zsh/.zsh ~/.zsh

ln -s ~/src/dotfiles/git/system.gitconfig ~/.gitconfig
ln -s ~/src/dotfiles/vim/.vsvimrc ~/.vsvimrc
ln -s ~/src/dotfiles/vim/.vimrc ~/.vimrc
ln -s ~/src/dotfiles/zsh/.shellrc ~/.shellrc
ln -s ~/src/dotfiles/zsh/.zshrc ~/.zshrc

ln -s ~/src/dotfiles/.tmux.conf ~/.tmux.conf
mkdir -p ~/.config/nvim

ln -s ~/src/dotfiles/vim/init.vim ~/.config/nvim/init.vim
mkdir ~/.vim
ln -s ~/src/dotfiles/vim/autocmd.vimrc ~/.vim/autocmd.vimrc
ln -s ~/src/dotfiles/vim/plugin-settings.vimrc ~/.vim/plugin-settings.vimrc
ln -s ~/src/dotfiles/vim/ide.vimrc ~/.vim/ide.vimrc
ln -s ~/src/dotfiles/vim/simple-mapping.vimrc ~/.vim/simple-mapping.vimrc
ln -s ~/src/dotfiles/vim/mapping.vimrc ~/.vim/mapping.vimrc
