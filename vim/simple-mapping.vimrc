"HOTKEYS
let mapleader = "\<Space>"

"Esc match
imap jj <Esc>

"Remove annoying middle mouse paste
"map <MiddleMouse> <Nop>
"imap <MiddleMouse> <Nop>
"map <2-MiddleMouse> <Nop>
"imap <2-MiddleMouse> <Nop>
"map <3-MiddleMouse> <Nop>
"imap <3-MiddleMouse> <Nop>
"map <4-MiddleMouse> <Nop>
"imap <4-MiddleMouse> <Nop>

noremap ;; :%s///g<Left><Left><Left>
noremap ;' :%s///cg<Left><Left><Left><Left>

nmap <C-c> :set nohls<CR>

vnoremap <C-f> y<ESC>/<c-r>"<CR>

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

