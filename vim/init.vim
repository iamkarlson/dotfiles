"neovim config file
"eventualy it works with clipboard

let $VIMRUNTIME='C:/tools/neovim/Neovim/share/nvim/runtime'

let &runtimepath = 'C:/tools/neovim/Neovim/share/nvim/runtime'.&runtimepath

source ~\.vimrc

if has('nvim')
  let $VISUAL = 'nvr -cc split --remote-wait'
endif
