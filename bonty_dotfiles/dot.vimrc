set nocompatible

" display
" -----------------------------
set number
set ruler
set cmdheight=2
set laststatus=2
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P
set title
set linespace=0
set wildmenu
set showcmd

" syntax color
" -----------------------------
syntax on
colorscheme ron
highlight LineNr ctermfg=white

" search
" -----------------------------
set incsearch
set ignorecase
set smartcase
set wrapscan
set hlsearch

" edit
" -----------------------------
set autoindent
set cindent
set showmatch
set backspace=indent,eol,start
set clipboard=unnamed
set pastetoggle=<F12>
set guioptions+=a

" tab
" -----------------------------
set tabstop=4
set expandtab
set smarttab
set shiftwidth=4
set shiftround
set nowrap

" keymap
" -----------------------------
" bracket
inoremap { {}<Left>
inoremap ( ()<Left>

" emacs like
inoremap <C-b> <Left>
inoremap <C-f> <Right>
inoremap <C-n> <Down>
inoremap <C-p> <Up>
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-d> <Del>

" backup
" -----------------------------
set backup
set backupdir=~/.vim_bak
set noswapfile

