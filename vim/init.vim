"neovim config file
"eventualy it works with clipboard

let $VIMRUNTIME='C:/tools/neovim/Neovim/share/nvim/runtime'


let $NVIM_PYTHON_LOG_FILE = 'c:/tools/nvim.log'

let &runtimepath = 'C:/tools/neovim/Neovim/share/nvim/runtime'.&runtimepath
let g:python3_host_prog = 'C:\Python37\python.exe'

source ~\.vimrc

if has('nvim')
  let $VISUAL = 'nvr -cc split --remote-wait'
endif


if exists('g:gui_oni')
    source ~\.gvimrc
    " Statements here
endif
