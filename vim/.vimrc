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
    set rtp+=~\.vim\
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

"Vundle settings

set nocompatible
filetype off


call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
"default dependcy, WTF
Plugin 'L9'
Plugin 'godlygeek/tabular'

"syntax
Plugin 'plasticboy/vim-markdown'
Plugin 'iamkarlson/vim-log-syntax'
Plugin 'iamkarlson/vim-theme-test'
Plugin 'w0rp/ale'

"zen coding
Plugin 'mattn/emmet-vim'
"Plugin 'Valloric/YouCompleteMe'

Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'simeji/winresizer'

"Plugin 'Chiel92/vim-autoformat'
Plugin 'sbdchd/neoformat'
Plugin 'luochen1990/rainbow'
Plugin 's3rvac/AutoFenc'

Plugin 'tomtom/tcomment_vim'

"fs utils
Plugin 'scrooloose/nerdtree'
Plugin 'haya14busa/incsearch.vim'
Plugin 'mileszs/ack.vim'

"view last files
Plugin 'yegappan/mru'

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


if WINDOWS()
    Plugin 'vim-scripts/Windows-PowerShell-Syntax-Plugin'
    set shq=
endif

call vundle#end()

runtime! ftdetect\*.vim
syntax on
filetype plugin indent on


"add encoding to status line
"if has("statusline")
	"set statusline=%{fugitive#statusline()}%<%f\ %h%m%r%=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%k\ %-14.(%l,%c%V%)\ %P
"endif


set spelllang=ru,en
set nospell
set ff=unix

"map russian keys to english equalent 

if WINDOWS()
    :source ~\.vim\windows.vimrc
    :source ~\.vim\autocmd.vimrc
else
    :source ~/.vim/autocmd.vimrc
endif


set showcmd             " show command in bottom bar
set cursorline          " highlight current line
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]



"show spaces,eol and tabs
set list
"\u00b7 working bad
set listchars=space:·,tab:▸\ ,eol:↲
hi SpecialKey ctermfg=7 guifg=gray guibg=NONE
hi NonText ctermfg=7 guifg=gray guibg=NONE

"open new windows more natral 
"new horizontal window appears at the bottom
set splitbelow
"new vertical window appears at the right
set splitright

set hidden

let NERDTreeShowHidden=1

"disable auto fold 
let g:vim_markdown_folding_disabled = 1

let g:rainbow_active = 1 

"bufexplorer settings
let g:bufExplorerDefaultHelp=1       " Show default help.
let g:bufExplorerShowNoName=1        " Show No Name buffers.
let g:bufExplorerShowUnlisted=1      " Show unlisted buffers.

"neoformat settings
"let g:neoformat_try_formatprg = 1

if WINDOWS()
    :source ~\.vim\mapping.vimrc
    :source ~\.vim\powershellVundleFix.vimrc
else
    :source ~/.vim/mapping.vimrc
endif

