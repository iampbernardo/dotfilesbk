" Hack for fun and productivity !!! :)


" {{{ Basic configurations ====================================

language en_US.UTF-8    " Soluciona problemas con algunos plugins
set nocompatible        " No a la compatibilidad completa con vi.
filetype plugin indent on
syntax on 			    " Activar coloreado de sintaxis
set tabstop=4			" Number of visual spaces per tab
set softtabstop=4		" Number of spaces when editing
set expandtab			" Tabs are spaces
let mapleader=","       " leader is comma
set colorcolumn=80      " Show a marker in line 80
set laststatus=2        " Enable statusline
set background=dark 	" Utilizar la versión oscura de los temas

" Visual configurations

set number			    " Show line numbers
set showcmd			    " Show command in bottom bar
set cursorline		    " Show current cursor line
filetype indent on      " load filetype-specific indent files
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to
set showmatch           " highlight matching [{()}]

" Searching

set incsearch           " search as characters are entered
set hlsearch            " highlight matches
" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

" Folding

set foldenable          " enable folding
set foldlevelstart=10   " open most folds by default
set foldnestmax=10      " 10 nested fold max
set foldmethod=marker   " fold based on indent level
" space open/closes folds
nnoremap <space> za



" }}}

" NEOBUNDLE {{{ ==============================================================


" Autoinstalando NeoBundle
let iCanHazNeoBundle=1
let neobundle_readme=expand($HOME.'/.vim/bundle/neobundle.vim/README.md')
if !filereadable(neobundle_readme)
    echo "Installing NeoBundle.."
    echo ""
    silent !mkdir -p $HOME/.vim/bundle
    silent !git clone https://github.com/Shougo/neobundle.vim $HOME/.vim/bundle/neobundle.vim
    let iCanHazNeoBundle=0
endif

" Llama a NeoBundle
if has('vim_starting')
    set rtp+=$HOME/.vim/bundle/neobundle.vim/
endif
call neobundle#rc(expand($HOME.'/.vim/bundle/'))

" dejemos que NeoBundle administre a NeoBundle (necesario!)
NeoBundle 'Shougo/neobundle.vim'

" Status line
NeoBundle 'itchyny/lightline.vim'
" More config available

" Syntax checker
NeoBundle 'scrooloose/syntastic'

" NerdTREE explorer
NeoBundle 'scrooloose/nerdtree'
map <leader>n :NERDTreeToggle<CR>

" GoYo distraction free mode
NeoBundle 'junegunn/goyo.vim'

" Language syntax -----------------------

" JavaScript
NeoBundle 'pangloss/vim-javascript'
" CoffeScript
NeoBundle 'kchmck/vim-coffee-script'
" CSS3
NeoBundle 'hail2u/vim-css3-syntax'
" Markdown
NeoBundle 'gabrielelana/vim-markdown'

" ---------------------------------------

" }}}

" GUI configs {{{ ===========================================================

" Colorschemes :
NeoBundle 'noahfrederick/vim-hemisu'
NeoBundle 'flazz/vim-colorschemes'
colorscheme molokai     " Default colorscheme

set guifont=Droid\ Sans\ Mono\ 12 " Defualt font and font size
set guioptions-=m       " Remove menu
set guioptions-=T       " Remove toolbar
set guioptions-=L       " Remove left scroll
set guioptions-=r       " Remove right scroll
" }}}

" Custom mappings {{{ =======================================================

" Vertical split
noremap <leader>vs :vsplit<CR>

" Horizontal split
noremap <leader>hs :split<CR>

" Save
noremap <C-S> :w<CR>

" Solve syntax on problem
noremap <leader>so :syntax on<CR>

" }}}
=======
