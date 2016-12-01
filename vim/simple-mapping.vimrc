"HOTKEYS
let mapleader = ","

"Esc match
imap jj <Esc>

"Remove annoying middle mouse paste
:map <MiddleMouse> <Nop>
:imap <MiddleMouse> <Nop>
:map <2-MiddleMouse> <Nop>
:imap <2-MiddleMouse> <Nop>
:map <3-MiddleMouse> <Nop>
:imap <3-MiddleMouse> <Nop>
:map <4-MiddleMouse> <Nop>
:imap <4-MiddleMouse> <Nop>

noremap ;; :%s///g<Left><Left><Left>
noremap ;' :%s///cg<Left><Left><Left><Left>

"map autocomplete to ctrl+space
inoremap <C-Space> <C-n>


" make tab in v mode ident code
vmap <tab> >gv
vmap <s-tab> <gv


:command! WQ wq
:command! Wq wq
:command! W w
:command! Q q


"HARDCORE!
:map <UP> <NOP>
:map <DOWN> <NOP>
:map <LEFT> <NOP>
:map <RIGHT> <NOP>
:map <HOME> <NOP>
:map <END> <NOP>
:map <PageUp> <NOP>
:map <PageDown> <NOP>
:map <Del> <NOP>

