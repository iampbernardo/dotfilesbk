" Hack for fun and productivity !!! :)


" NEOBUNDLE {{{ ==============================================================

language en_US.UTF-8    " Soluciona problemas con algunos plugins
set nocompatible        " No a la compatibilidad completa con vi.

" Autoinstalando NeoBundle
let iCanHazNeoBundle=1
let neobundle_readme=expand($HOME.'/.vim/bundle/neobundle.vim/README.md')
if !filereadable(neobundle_readme)
    echo "Installing NeoBundle..."
    echo ""
    silent !mkdir -p $HOME/.vim/bundle
    silent !git clone https://github.com/Shougo/neobundle.vim $HOME/.vim/bundle/neobundle.vim
    let iCanHazNeoBundle=0
endif

" Llama a NeoBundle
if has('vim_starting')
    set rtp+=$HOME/.vim/bundle/neobundle.vim/
endif
call neobundle#begin(expand($HOME.'/.vim/bundle/'))
call neobundle#end()

" dejemos que NeoBundle administre a NeoBundle (necesario!)
NeoBundle 'Shougo/neobundle.vim'

" Status line
NeoBundle 'itchyny/lightline.vim'
" More config available

" Syntax checker
NeoBundle 'scrooloose/syntastic'

" NerdTREE explorer
NeoBundle 'scrooloose/nerdtree'

" NerdTREE Tabs
NeoBundle 'jistr/vim-nerdtree-tabs'

" Editor config
NeoBundle 'editorconfig/editorconfig-vim'

" Ctrlp (Fuzzy search)
NeoBundle 'kien/ctrlp.vim'


" GoYo distraction free mode
NeoBundle 'junegunn/goyo.vim'

" Emmet for Zen Coding
NeoBundle 'mattn/emmet-vim'

" Vim tags
NeoBundle 'szw/vim-tags'

" Tagbar (see tags in file)
NeoBundle 'majutsushi/tagbar'

" Autocomplete
NeoBundle 'Valloric/YouCompleteMe'

" Emmet rainbow parenthesis
NeoBundle 'kien/rainbow_parentheses.vim'

" Haskell
NeoBundle 'wlangstroth/vim-haskell'

" Snippets
NeoBundle 'SirVer/ultisnips'
NeoBundle 'honza/vim-snippets'

" Fugitive (git wrapper)
NeoBundle 'tpope/vim-fugitive'


" Language syntax -----------------------

" JavaScript
NeoBundle 'pangloss/vim-javascript'
" CoffeScript
NeoBundle 'kchmck/vim-coffee-script'
" CSS3
NeoBundle 'hail2u/vim-css3-syntax'
" Markdown
NeoBundle 'gabrielelana/vim-markdown'
" Stylus
NeoBundle 'wavded/vim-stylus'
" Handlebars
NeoBundle 'mustache/vim-mustache-handlebars'
" Less
NeoBundle 'groenewege/vim-less'

" ---------------------------------------

" Colorschemes :
NeoBundle 'noahfrederick/vim-hemisu'
NeoBundle 'flazz/vim-colorschemes'

" }}}
" {{{ Basic configurations ====================================

filetype plugin indent on
syntax on 			    " Activar coloreado de sintaxis
set tabstop=2			" Number of visual spaces per tab
set softtabstop=2		" Number of spaces when editing
set expandtab			" Tabs are spaces
let mapleader=","       " leader is comma
set colorcolumn=80      " Show a marker in line 80
set laststatus=2        " Enable statusline
set t_Co=256            " Habilita 256 colores en modo consola.
set list                " Show  spaces, tabs, eol ...
" Selected chars  (eol, tab, trail, extends, precedes)
set listchars=tab:>-,trail:~,extends:>,precedes:<

if has('gui_running') " Habilita el tema molokai para gvim y vim.
    colorscheme molokai
else
    colorscheme molokai
endif

" Visual configurations

set number			    " Show line numbers
set showcmd			    " Show command in bottom bar
set cursorline		    " Show current cursor line
filetype indent on      " load filetype-specific indent files
set wildmenu            " visual autocompletefor command menu
set lazyredraw          " redraw only when we need to
set showmatch           " highlight matching [{()}]

" No swap files
set noswapfile

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


" moving around
nmap <C-Left> <C-w>h
nmap <C-Right> <C-w>l
nmap <C-Up> <C-w>k
nmap <C-Down> <C-w>j

nmap <Leader>t :tabnew<CR>
nmap <C-l> :tabnext<CR>
nmap <C-h> :tabprevious<CR>



" }}}


" GUI configs {{{ ===========================================================


" Default font and font size
set guifont=Monospace\ 13
set guioptions-=m       " Remove menu
set guioptions-=T       " Remove toolbar
set guioptions-=L       " Remove left scroll
set guioptions-=r       " Remove right scroll
set guioptions=i        " VIM icon
set guioptions-=e       " Hide Tab page

" }}}

colorscheme molokai     " Default colorscheme
set background=dark 	" Utilizar la versión oscura de los temas

"set showtabline=2
" Custom mappings {{{ =======================================================

" Vertical split
noremap <leader>vs :vsplit<CR>

" Horizontal split
noremap <leader>hs :split<CR>

" Solve syntax on problem
noremap <leader>so :syntax on<CR>

" Config NERDTree
let NERDTreeShowHidden=1
"autocmd VimEnter * NERDTree
"autocmd BufEnter * NERDTreeMirror
"autocmd VimEnter * wincmd w
noremap <leader>n :NERDTreeTabsToggle<CR>
let g:nerdtree_tabs_open_on_console_startup=1

" }}}
 

" Plugins config {{{ =======================================================

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" UtilSnips

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Rainbow partenthesis
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Ctrl P
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" Vim - tags
let g:vim_tags_auto_generate = 1


" Tagbar
nmap <F8> :TagbarToggle<CR>

" YouCompleteMe
let g:ycm_server_keep_logfiles = 1
let g:ycm_server_log_level = 'debug'
let g:ycm_autotrigger = 1
let g:ycm_min_num_of_chars_for_completion = 2



" }}}

" Charge a machine custom config
if filereadable('~/.custom_vim')
    source ~/.custom_vim
endif


" Language configs  {{{ ====================================================

" PHP Symfony
autocmd BufRead, BufNewFile *.php set ts=2 sts=2 et
autocmd BufRead, BufNewFile *.js set ts=2 sts=2 et

" }}}
