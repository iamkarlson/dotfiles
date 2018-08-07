"neovim config file
"eventualy it works with clipboard


source ~\.vimrc

if has('nvim')
  let $VISUAL = 'nvr -cc split --remote-wait'
endif
