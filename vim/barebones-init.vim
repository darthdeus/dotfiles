" set runtimepath^=~/.dotfiles/test-plugs/semshi/
"
" set packpath=~/.dotfiles/test-plugs/
" packloadall!

" syntax on
" filetype plugin indent on

" set runtimepath^=~/.dotfiles/test-plugs/semshi/
set nocompatible
" set runtimepath^=~/.dotfiles/debug-vim-runtime
source ~/.dotfiles/third-party/vim-plug/plug.vim

call plug#begin("~/.dotfiles/test-plugs")

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-jedi'
Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
Plug 'davidhalter/jedi-vim'

" let g:deoplete#enable_at_startup = 1

call plug#end()
