
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

" Use deoplete.
let g:deoplete#enable_at_startup = 1


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

let g:session_autosave = 'yes'

"if WINDOWS()
"    let g:session_lock_directory='~\.vim\sessions\locks\'
"else
"    let g:session_lock_directory='~/.vim/sessions/locks/'
"endif

if WINDOWS()
    let g:wakatime_PythonBinary = 'C:\Python37\python'  " (Default: 'python')
endif

"denite

" For Pt(the platinum searcher)
" NOTE: It also supports windows.
call denite#custom#var('file/rec', 'command', ['pt', '--follow', '--nocolor', '--nogroup', (has('win32') ? '-g:' : '-g='), ''])

" Ag command on grep source
call denite#custom#var('grep', 'command', ['ag'])
call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', [])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])

noremap   <leader>p         :<C-u>Denite file_rec<CR>
noremap   <leader>m         :<C-u>Denite file_mru<CR>
nnoremap  <leader>s         :<C-u>Denite buffer<CR>
nnoremap  <leader><Space>s  :<C-u>DeniteBufferDir buffer<CR>
nnoremap  <leader>8         :<C-u>DeniteCursorWord grep:. -mode=normal<CR>
nnoremap  <leader>/         :<C-u>Denite grep:. -mode=normal<CR>
nnoremap  <leader><Space>/  :<C-u>DeniteBufferDir grep:. -mode=normal<CR>
nnoremap  <leader>d         :<C-u>DeniteBufferDir file_rec<CR>
nnoremap  <leader>r         :<C-u>Denite -resume -cursor-pos=+1<CR>
nnoremap  <leader>lr        :<C-u>Denite references -mode=normal<CR>

