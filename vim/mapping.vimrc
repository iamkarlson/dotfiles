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


if WINDOWS()
    nmap <leader>v :tabedit $HOME\.vim\.vimrc<CR>
else
    nmap <leader>v :tabedit $HOME/.vim/.vimrc<CR>
endif

nmap <C-s> :s<CR>
vnoremap <C-f> y<ESC>:Ack <C-r>"<CR>
vnoremap <C-A-f> y<ESC>/<c-r>"<CR>   

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

nnoremap <F6> :windo diffthis<CR>

nmap <C-O> :only<CR>



function! DiffStart()
    set cursorline
    map ] ]c
    map [ [c
    map <leader>1 :diffget //2<CR>]:diffupdate<CR>2<C-e>
    map <leader>2 :diffget //3<CR>]:diffupdate<CR>2<C-e>

    nnoremap <Esc><Esc> :qa<CR>

    hi DiffAdd    ctermfg=233 ctermbg=LightGreen guifg=#003300 guibg=#DDFFDD gui=none cterm=none
    hi DiffText   ctermfg=233  ctermbg=yellow  guifg=#000033 guibg=#DDDDFF gui=none cterm=none
endfunction

function! DiffStop()
    silent! unmap ]
    silent! unmap [
    silent! unmap <leader>1
    silent! unmap <leader>2
endfunction

function! Get3WayLayout()
    :exe "normal \<C-W>J"
endfunction

if &diff
    :call DiffStart()
endif

au BufEnter * if &diff | :call DiffStart()

au BufLeave * if &diff | :call DiffStop()


" execute a command and show it's output in a split window
command! -nargs=* -complete=shellcmd Rsplit execute "new | r! <args>"

" execute a command and show it's output in a new tab
command! -nargs=* -complete=shellcmd Rtab execute "tabnew | r! <args>"

"resizing windows
let g:winresizer_start_key = '<leader>t'
