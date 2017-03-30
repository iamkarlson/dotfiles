
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
"let g:neoformat_try_formatprg = 1


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

let g:ctrlp_cmd = 'CtrlPMixed'
