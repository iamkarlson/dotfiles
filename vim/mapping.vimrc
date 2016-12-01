"aditional mapping for incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
" make tab in normal mode ident code
nmap <tab> I<tab><esc>
nmap <s-tab> ^i<bs><esc>


map <C-tab> :bn<CR>
map <C-s-tab> :bN<CR>

noremap <F3> :Autoformat<CR>

"resizing and navigation windows hotkeys

nnoremap <C-A-up> :exe "resize " . (winheight(0) * 2/3)<CR>
nnoremap <C-A-down> :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <C-A-left> :exe "vertical resize " . (winwidth(0) * 3/2)<CR>
nnoremap <C-A-right> :exe "vertical resize " . (winwidth(0) * 2/3)<CR>

if WINDOWS()
    nmap <leader>v :tabedit $HOME\.vim\.vimrc<CR>
else
    nmap <leader>v :tabedit $HOME/.vim/.vimrc<CR>
endif

nmap <C-s> :s<CR>
vnoremap <C-f> y<ESC>:Ack "<CR>

nnoremap <F5> :silent !%:p<CR>

nnoremap <F11> :silent !console.exe -reuse -d .<CR>


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

