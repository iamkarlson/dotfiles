" Identify platform {
    silent function! OSX()
        return has('macunix')
    endfunction
    silent function! LINUX()
        return has('unix') && !has('macunix') && !has('win32unix')
    endfunction
    silent function! WINDOWS()
        return  (has('win32') || has('win64'))
    endfunction
" }

if has('nvim')
    let s:editor_root=expand("~/.config/nvim")
else
    let s:editor_root=expand("~/.vim")
endif


"add my folders as bundle sources
if WINDOWS()
    :source ~\.vsvimrc
    set rtp+=~\.vim
    set rtp+=~\.vim\bundle\vim-colors-solarized
    set rtp+=~\.vim\bundle\molokai
    set rtp+=~\.vim\bundle\Vundle.vim

    set backupdir=~\.vim\backup_files
    set directory=~\.vim\swap_files
    set undodir=~\.vim\undo_files
else
    :source ~/.vsvimrc
    set rtp+=~/.vim/bundle/vim-colors-solarized
    set rtp+=~/.vim/bundle/molokai
    set rtp+=~/.vim/bundle/Vundle.vim

    "swap files directory
    "to create directory mkdir -p ~/.vim/{backup_files,swap_files,undo_files}
    set backupdir=~/.vim/backup_files/
    set directory=~/.vim/swap_files/
    set undodir=~/.vim/undo_files/
endif


set linebreak
set spell
set spelllang=en_us

"always show line numbers
set number 
set relativenumber 
"enable syntax highlighting
syntax on

set autoread

"always show status line
set laststatus=2

set encoding=utf-8

highlight LineNr ctermfg=grey

"Vundle settings

set nocompatible
filetype off


call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
"default dependcy, WTF
Plugin 'L9'
Plugin 'godlygeek/tabular'

"syntax
Plugin 'w0rp/ale'
Plugin 'plasticboy/vim-markdown'
Plugin 'iamkarlson/vim-log-syntax'
Plugin 'iamkarlson/vim-theme-test'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'leafgarland/typescript-vim'

"zen coding
Plugin 'mattn/emmet-vim'
"Plugin 'Valloric/YouCompleteMe'
Plugin 'Shougo/deoplete.nvim'

Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
"Plugin 'mhinz/vim-signify'
Plugin 'simeji/winresizer'
Plugin 'Asheq/close-buffers.vim'

"Plugin 'Chiel92/vim-autoformat'
Plugin 'sbdchd/neoformat'
Plugin 'luochen1990/rainbow'
Plugin 's3rvac/AutoFenc'
Plugin 'tpope/vim-surround'

Plugin 'tomtom/tcomment_vim'

"fs utils
Plugin 'scrooloose/nerdtree'
Plugin 'haya14busa/incsearch.vim'
"Plugin 'mileszs/ack.vim'
Plugin 'rking/ag.vim'
Plugin 'Chun-Yang/vim-action-ag'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'kshenoy/vim-signature'

"view last files
Plugin 'yegappan/mru'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-session'

Plugin 'jlanzarotta/bufexplorer'
Plugin 'easymotion/vim-easymotion'
Plugin 'KabbAmine/vCoolor.vim'
Plugin 'reedes/vim-pencil'

"color themes
Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'
Plugin 'sjl/badwolf'
Plugin 'iamkarlson/breezy'
Plugin 'broduo/broduo-color-scheme'
"
"GUI
Plugin 'vim-airline/vim-airline'
Plugin 'severin-lemaignan/vim-minimap'


if WINDOWS()
    Plugin 'vim-scripts/Windows-PowerShell-Syntax-Plugin'
    set shq=
endif

call vundle#end()

runtime! ftdetect\*.vim
syntax on
filetype plugin indent on


set spelllang=ru,en
set nospell
set ff=unix

if WINDOWS()
    :source ~\.vim\windows.vimrc
    :source ~\.vim\autocmd.vimrc
    :source ~\.vim\ide.vimrc
else
    :source ~/.vim/autocmd.vimrc
    :source ~/.vim/ide.vimrc
endif


set showcmd             " show command in bottom bar
set cursorline          " highlight current line
set wildmenu            " visual autocomplete for command menu
"set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]

set synmaxcol=256


"show spaces,eol and tabs
set list
"\u00b7 working bad
set listchars=space:·,tab:▸\ ,eol:↲
hi SpecialKey ctermfg=7 guifg=gray guibg=NONE
hi NonText ctermfg=7 guifg=gray guibg=NONE

"open new windows more natural 
"new horizontal window appears at the bottom
set splitbelow
"new vertical window appears at the right
set splitright

set hidden

if WINDOWS()
    :source ~\.vim\plugin-settings.vimrc
    :source ~\.vim\simple-mapping.vimrc
    :source ~\.vim\mapping.vimrc
else
    :source ~/.vim/plugin-settings.vimrc
    :source ~/.vim/simple-mapping.vimrc
    :source ~/.vim/mapping.vimrc
endif


