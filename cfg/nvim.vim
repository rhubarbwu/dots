set expandtab
set shiftwidth=4
set tabstop=4
set clipboard=unnamedplus

filetype plugin on
syntax on

"set spell
"set spelllang=en_gb

augroup numbertoggle
set number
autocmd!
autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
augroup END

set background=dark

call plug#begin()
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'Yggdroot/indentLine'

" might learn these
Plug 'tpope/vim-surround'
call plug#end()
