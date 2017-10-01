
let NERDTreeShowHidden=1


let g:autofenc_enable=1
let g:autofenc_autodetect_bom=1

"disable auto fold 
let g:vim_markdown_folding_disabled = 1

let g:rainbow_active = 1 

"bufexplorer settings
let g:bufExplorerDefaultHelp=1       " Show default help.
let g:bufExplorerShowNoName=1        " Show No Name buffers.
let g:bufExplorerShowUnlisted=1      " Show unlisted buffers.

"neoformat settings
let g:neoformat_try_formatprg = 1

" Use deoplete.
let g:deoplete#enable_at_startup = 1

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


if WINDOWS()
    set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows
    let g:ctrlp_user_command = 'dir %s /-n /b /s /a-d'  " Windows
else
    let g:ctrlp_user_command = 'find %s -type f'        " MacOSX/Linux
    set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " Linux/MacOSX
endif

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v[\/]\.(git|hg|svn)$',
	\ 'file': '\v\.(exe|so|dll)$',
	\ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
	\ }

let g:ctrlp_use_caching = 0
set grepprg=ag\ --nogroup\ --nocolor

let g:ctrlp_cmd = 'CtrlPMRU'
