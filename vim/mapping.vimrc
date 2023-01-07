"aditional mapping for incsearch
"map /  <Plug>(incsearch-forward)
"map ?  <Plug>(incsearch-backward)
"map g/ <Plug>(incsearch-stay)

" You can use other keymappings like <C-l> instead of <CR> if you want to
" use these mappings as default search and sometimes want to move cursor with
" EasyMotion.
function! s:incsearch_config(...) abort
  return incsearch#util#deepextend(deepcopy({
  \   'modules': [incsearch#config#easymotion#module({'overwin': 1})],
  \   'keymap': {
  \     "\<CR>": '<Over>(easymotion)'
  \   },
  \   'is_expr': 0
  \ }), get(a:, 1, {}))
endfunction

noremap <silent><expr> /  incsearch#go(<SID>incsearch_config())
noremap <silent><expr> ?  incsearch#go(<SID>incsearch_config({'command': '?'}))
noremap <silent><expr> g/ incsearch#go(<SID>incsearch_config({'is_stay': 1}))


" <Leader>f{char} to move to {char}
 map  <Leader>f <Plug>(easymotion-bd-f)
 nmap <Leader>f <Plug>(easymotion-overwin-f)

 " s{char}{char} to move to {char}{char}
 nmap s <Plug>(easymotion-overwin-f2)

 " Move to line
 map <Leader>L <Plug>(easymotion-bd-jk)
 nmap <Leader>L <Plug>(easymotion-overwin-line)

 " Move to word
 map  <Leader>w <Plug>(easymotion-bd-w)
 nmap <Leader>w <Plug>(easymotion-overwin-w)

let g:EasyMotion_smartcase = 1


" ctrl+c to toggle highlight.
let hlstate=0
nnoremap <c-c> :if (hlstate%2 == 0) \| nohlsearch \| else \| set hlsearch \| endif \| let hlstate=hlstate+1<cr>

" make tab in normal mode ident code
nmap <tab> I<tab><esc>
nmap <s-tab> ^i<bs><esc>

"map autocomplete to ctrl+space
inoremap <C-Space> <C-n>

" nnoremap <c-tab> :bn<CR>
" nnoremap <c-s-tab> :bN<CR>
nnoremap zq :bd<CR>

nmap j gj
nmap k gk

nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz


nmap <leader>l :set ft=
nmap <leader>3 :Neoformat<CR>
nmap <leader>b :BufExplorer<CR>

nmap <F8> :sort u<CR>


if WINDOWS()
    nmap <leader>e :tabedit $HOME\.vim\.vimrc<CR>:cd $HOME\.vim\<CR>

    nnoremap <C-F10> :silent !explorer .<CR>
    nnoremap <F11> :silent !start-process -FilePath 'c:\Program Files\ConEmu\ConEmu64.exe' -ArgumentList '-single -dir %:p:h'<CR>
else
    nmap <leader>e :tabedit $HOME/src/dotfiles/vim/.vimrc<CR>
    nnoremap <C-F10> :silent !nautilus .<CR>
endif

vnoremap <leader><C-f> y<ESC>:Ack <C-r>"<CR>

"current file only
nnoremap <Leader>af :Ack  %<Left><Left>


nnoremap <F5> :silent !%:p<CR>

nnoremap <F10> :NERDTree<CR>

nnoremap <leader>d :windo diffthims<CR>

nmap <leader>o :only<CR>


" execute a command and show it's output in a split window
command! -nargs=* -complete=shellcmd Rsplit execute "new | r! <args>"

" execute a command and show it's output in a new tab
command! -nargs=* -complete=shellcmd Rtab execute "tabnew | r! <args>"

"resizing windows
let g:winresizer_start_key = '<leader>t'


if has('nvim')
    map! <S-Insert> <C-R>+
endif
