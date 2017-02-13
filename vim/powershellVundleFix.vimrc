function! VundlePluginInstall(...)
    let shell = &shell
    let shellcmdflag = &shellcmdflag
    let shellquote = &shellquote
    let shellxquote = &shellxquote

    set shell=C:\Windows\system32\cmd.exe
    set shellcmdflag=/c
    set shellquote=
    set shellxquote=(

    call call('vundle#installer#new', a:000)

    :source ~\.vim\windows.vimrc
endfunction

command! -nargs=* -bang -complete=custom,vundle#scripts#complete PluginInstall
\ call VundlePluginInstall('!' == '<bang>', <f-args>)
