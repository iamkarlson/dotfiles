cd ~

ln /home/iamkarlson/dev/prj/dotfiles/vim/.vimrc .vimrc
ln -ds /home/iamkarlson/dev/prj/dotfiles/vim/ .vim

mkdir /home/iamkarlson/.vim/bundle
mkdir /home/iamkarlson/.vim/swap_files
mkdir /home/iamkarlson/.vim/backup_files

git clone https://github.com/VundleVim/Vundle.vim.git /home/iamkarlson/.vim/bundle/Vundle.vim

