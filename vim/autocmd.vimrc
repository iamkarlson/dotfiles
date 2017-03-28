"execute auto commands region
"syncronize current directory with current buffer directory
autocmd BufEnter * if expand('%:p') !~ '://' && expand('%:p') !~ '\Temp\'  | lcd %:p:h | endif


" Source the vimrc file after saving it
autocmd bufwritepost .vimrc source $MYVIMRC 

autocmd BufEnter *.xaml set filetype=xml
autocmd BufEnter Web*.config set filetype=xml
autocmd BufEnter App*.config set filetype=xml

au BufEnter *.log setlocal nospell

"au FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -

"au FileType json setlocal equalprg=python\ -m\ json.tool


au BufEnter taulink.cfg set filetype=xml

au BufEnter gitconfig set filetype=config

au BufEnter *.xmllog set ft=xml

au BufEnter *.log set autoread
au BufEnter *.log set ft=log


"au BufEnter * if &diff | :call DiffStart()

au BufLeave * if &diff | :call DiffStop()
