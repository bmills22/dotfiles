set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin('~/.nvim/autoload/plugged')

Plug 'itchyny/lightline.vim'
Plug 'ap/vim-css-color'
Plug 'ryanoasis/vim-devicons'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-unimpaired'

" Development plugins
Plug 'prettier/vim-prettier'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'preservim/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'rstacruz/vim-closer'

" HTML / CSS plugins
Plug 'mattn/emmet-vim'
Plug 'cakebaker/scss-syntax.vim'

" JS plugins
Plug 'maxmellon/vim-jsx-pretty'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'jparise/vim-graphql'
Plug 'heavenshell/vim-jsdoc', { 
  \ 'for': ['javascript', 'javascript.jsx','typescript'], 
  \ 'do': 'make install'
\}

" Theme
Plug 'dracula/vim', { 'as': 'dracula' }

call plug#end()

syntax on
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab
set expandtab
set number
set termguicolors

colorscheme dracula

" Powerline theme
let g:lightline = {
      \ 'colorscheme': 'darcula',
      \ }

" Coc global extensions
let g:coc_global_extensions = [
  \ 'coc-tsserver'
  \ ]

" Always show statusline
set laststatus=2

" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256

" NERDTree - (ctrl+n)
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeIgnore = ['^node_modules$']
" Highlight currently open buffer in NERDTree
autocmd BufEnter * call SyncTree()

" NERDCommenter - (ctrl+/)
nmap <C-_>   <Plug>NERDCommenterToggle
vmap <C-_>   <Plug>NERDCommenterToggle<CR>gv

" Javascript syntax highlighting
autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear

" Prettier
command! -nargs=0 Prettier :CocCommand prettier.formatFile

" ctrlp
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" Activate eslint & prettier only if they exist in node_modules
if isdirectory('./node_modules') && isdirectory('./node_modules/prettier')
  let g:coc_global_extensions += ['coc-prettier']
endif

if isdirectory('./node_modules') && isdirectory('./node_modules/eslint')
  let g:coc_global_extensions += ['coc-eslint']
endif

" autostart nerdtree when no file specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" sync open file with NERDTree
" " Check if NERDTree is open or active
function! IsNERDTreeOpen()        
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

" Call NERDTreeFind iff NERDTree is active, current window contains a modifiable
" file, and we're not in vimdiff
function! SyncTree()
  if &modifiable && IsNERDTreeOpen() && strlen(expand('%')) > 0 && !&diff
    NERDTreeFind
    wincmd p
  endif
endfunction

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>

" remap jk to <esc> key
imap jk <Esc>
imap kj <Esc>

" Enable emmet for only html/css/js
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
