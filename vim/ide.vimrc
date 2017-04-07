""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"     Specific settings for achieve integrated development environment
"     for particular languages
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"     Python
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RunPythonIDE()
    nmap <f6> :!python3.5 -m pudb %:p<CR>
endfunction

" Settings up debugging hotkeys

autocmd BufEnter *.py :call RunPythonIDE()
