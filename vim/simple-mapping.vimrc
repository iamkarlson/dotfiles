"HOTKEYS
let mapleader = "\<Space>"

"Esc match
imap jj <Esc>

nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

"Remove annoying middle mouse paste
map <MiddleMouse> <Nop>
imap <MiddleMouse> <Nop>
map <2-MiddleMouse> <Nop>
imap <2-MiddleMouse> <Nop>
map <3-MiddleMouse> <Nop>
imap <3-MiddleMouse> <Nop>
map <4-MiddleMouse> <Nop>
imap <4-MiddleMouse> <Nop>

noremap ;; :%s///g<Left><Left><Left>
noremap ;' :%s///cg<Left><Left><Left><Left>

" ctrl+c to toggle highlight.
let hlstate=0
nnoremap <c-c> :if (hlstate%2 == 0) \| nohlsearch \| else \| set hlsearch \| endif \| let hlstate=hlstate+1<cr>


vnoremap <C-f> y<ESC>/<c-r>"<CR>   

" make tab in v mode ident code
vmap <tab> >gv
vmap <s-tab> <gv


command! WQ wq
command! Wq wq
command! W w
command! Q q

nmap <C-s> :s<CR>
nnoremap <Leader>w :w<CR>

vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

nmap <Leader><Leader> V



"HARDCORE!
map <UP> <NOP>
map <DOWN> <NOP>
map <LEFT> <NOP>
map <RIGHT> <NOP>
map <HOME> <NOP>
map <END> <NOP>
map <PageUp> <NOP>
map <PageDown> <NOP>
map <Del> <NOP>

