function! AutonameBuffer()
if @% == '' 
    "echo 'buffer name is empty' 

    if WINDOWS()
        let tmp_file =  tempname()
        echo tmp_file
    else
        let tmp_file =  /tmp/working_copy.$USER
        echo tmp_file
    endif

    execute "w! ".tmp_file
    execute "e! ".tmp_file
else 
    "echo 'here''s a buffer name: ' . @% 
endif
endfunction


au BufEnter * :call AutonameBuffer()
