"neovim config file
"eventualy it works with clipboard

let $VIMRUNTIME='C:/tools/neovim/Neovim/share/nvim/runtime'

let &runtimepath = 'C:/tools/neovim/Neovim/share/nvim/runtime'.&runtimepath
let g:python3_host_prog = 'C:\Program Files\Python36\python.exe'

source ~\.vimrc

if has('nvim')
  let $VISUAL = 'nvr -cc split --remote-wait'
endif
