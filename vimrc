set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin('~/.vim/plugged')

Plug 'itchyny/lightline.vim'
Plug 'ap/vim-css-color'
Plug 'ryanoasis/vim-devicons'
Plug 'vimwiki/vimwiki'

" Development plugins
Plug 'prettier/vim-prettier'
Plug 'styled-components/vim-styled-components'
" JS plugins
Plug 'maxmellon/vim-jsx-pretty'

" Theme
Plugin 'dracula/vim', { 'name': 'dracula' }

call plug#end()

syntax on
set tabstop=4
set softtabstop=4
set expandtab
set number
set termguicolors

"packadd! dracula
"syntax enable
colorscheme dracula
