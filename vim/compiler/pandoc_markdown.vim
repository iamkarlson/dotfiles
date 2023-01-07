
if exists("current_compiler")
    finish
endif

let current_compiler = "pandoc_markdown"
setlocal makeprg=pandoc\ -s\ -S\ --tco\ %\ -o\ %.html
setlocal errorformat=%f:%l:\ %m`
