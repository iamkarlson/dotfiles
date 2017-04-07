"aditional mapping for incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" make tab in normal mode ident code
nmap <tab> I<tab><esc>
nmap <s-tab> ^i<bs><esc>

"map autocomplete to ctrl+space
inoremap <C-Space> <C-n>

map <C-tab> :bn<CR>
map <C-s-tab> :bN<CR>
nmap zq :bd<CR>

nmap <leader>f :set ft=
map <F3> :Neoformat<CR>

nmap <F8> :sort u<CR>


if WINDOWS()
    nmap <leader>v :tabedit $HOME\.vim\.vimrc<CR>
else
    nmap <leader>v :tabedit $HOME/.vim/.vimrc<CR>
endif

vnoremap <leader><C-f> y<ESC>:Ack <C-r>"<CR>

"current file only
nnoremap <Leader>af :Ack  %<Left><Left>


nnoremap <F5> :silent !%:p<CR>

nnoremap <F11> :silent !start "c:\Program Files\ConEmu\ConEmu64.exe" -single -dir "%:p:h"<CR>


if WINDOWS()
    nnoremap <C-F10> :silent !explorer .<CR>
else
    nnoremap <C-F10> :silent !nautilus .<CR>
endif
nnoremap <F10> :NERDTree<CR>

nnoremap <leader>d :windo diffthis<CR>

nmap <leader>o :only<CR>





" execute a command and show it's output in a split window
command! -nargs=* -complete=shellcmd Rsplit execute "new | r! <args>"

" execute a command and show it's output in a new tab
command! -nargs=* -complete=shellcmd Rtab execute "tabnew | r! <args>"

"resizing windows
let g:winresizer_start_key = '<leader>t'


