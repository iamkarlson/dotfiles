function! DiffStart()
    set cursorline
    map ]] ]c
    map [[ [c
    map <leader>1 :diffget //2<CR>]:diffupdate<CR>2<C-e>
    map <leader>2 :diffget //3<CR>]:diffupdate<CR>2<C-e>

    nmap <Esc><Esc> :qa<CR>

    hi DiffAdd    ctermfg=233 ctermbg=LightGreen guifg=#003300 guibg=#DDFFDD gui=none cterm=none
    hi DiffText   ctermfg=233  ctermbg=yellow  guifg=#000033 guibg=#DDDDFF gui=none cterm=none
endfunction

function! DiffStop()
    silent! unmap ]
    silent! unmap [
    silent! unmap <leader>1
    silent! unmap <leader>2

    silent! nunmap <Esc><Esc> :qa<CR>
endfunction

function! Get3WayLayout()
    :exe "normal \<C-W>J"
endfunction



if &diff
    call DiffStart()
endif


function! MakeDiff()
    :Gvdiff
    call DiffStart()
    call Get3WayLayout()
endfunction

