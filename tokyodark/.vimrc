set nocompatible
set encoding=utf8
syntax enable

call plug#begin('~/.config/nvim/plugged') 
Plug 'tiagovla/tokyodark.nvim'
Plug 'ghifarit53/tokyonight-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'zchee/deoplete-jedi'
Plug 'rust-lang/rust.vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'othree/html5.vim'
Plug 'vim-scripts/c.vim'
Plug 'vim-scripts/bash-support.vim'
Plug 'lervag/vimtex'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'ptzz/lf.vim'
Plug 'voldikss/vim-floaterm'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-fugitive'
call plug#end()

let g:tokyonight_style = 'storm' " available: night, storm
let g:tokyonight_enable_italic = 1

colorscheme tokyonight
let g:airline_theme = "tokyonight"


set relativenumber

