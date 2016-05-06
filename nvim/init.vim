"
" neovim configuration
" @author Pablo Bernardo @voylinux
"

set number
set expandtab                 " spaces instead of tabs
set tabstop=2                 " a tab = four spaces
set shiftwidth=2              " number of spaces for auto-indent
set softtabstop=2             " a soft-tab of four spaces
set autoindent                " set on the auto-indent
set nowrap                    " no wrap lines
  
" Tabs are spaces
let mapleader=","
" Set column maker at 80 lines
set colorcolumn=80 

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬
set t_Co=256


" Plugins config {{{ =======================================================

call plug#begin('~/.vim/plugged')
Plug 'junegunn/vim-plug'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'

" Enviroment
Plug 'sheerun/vim-polyglot'
" let g:polyglot_disabled = ['php']

" Layout
Plug 'Yggdroot/indentLine'
let g:indentLine_enable = 1

" Git help
Plug 'tpope/vim-fugitive'

" Snippets
" Track the engine.
Plug 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets'

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" Web development
Plug 'mattn/emmet-vim'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'sickill/vim-pasta'
" Plug 'wookiehangover/jshint.vim'
" Plug 'skammer/vim-css-color'

Plug 'Shougo/deoplete.nvim'
" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#disable_auto_complete = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" omnifuncs
augroup omnifuncs
  autocmd!
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup end

" tern
if exists('g:plugs["tern_for_vim"]')
  let g:tern_show_argument_hints = 'on_hold'
  let g:tern_show_signature_in_pum = 1
  autocmd FileType javascript setlocal omnifunc=tern#Complete
endif

" deoplete tab-complete
" inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : deoplete#mappings#manual_complete()

" ,<Tab> for regular tab
" inoremap <Leader><Tab> <Space><Space>


" tern
autocmd FileType javascript nnoremap <silent> <buffer> gb :TernDef<CR>

" Colorschemes
Plug 'junegunn/seoul256.vim'
Plug 'tomasr/molokai'
Plug 'altercation/vim-colors-solarized'
let g:solarized_termcolors=256

" Editor configuration
Plug 'editorconfig/editorconfig-vim'
Plug 'mhinz/vim-startify'

" Navigation
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle'}

Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle'}

let NERDTreeShowBookmarks=1
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "✹",
    \ "Staged"    : "✚",
    \ "Untracked" : "✭",
    \ "Renamed"   : "➜",
    \ "Unmerged"  : "═",
    \ "Deleted"   : "✖",
    \ "Dirty"     : "✗",
    \ "Clean"     : "✔︎",
    \ "Unknown"   : "?"
    \ }

Plug 'ctrlpvim/ctrlp.vim'

" Setup some default ignores
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn|node_modules|bower_components)|\_site)$',
  \ 'file': '\v\.(exe|so|dll|.atom|png|jpg|jpeg)$',
\}

nmap <leader>p :CtrlP<cr>
" Use the nearest .git directory as the cwd
" This makes a lot of sense if you are working on a project that is in version
" control. It also supports works with .svn, .hg, .bzr.
let g:ctrlp_working_path_mode = 'rw'

" Use a leader instead of the actual named binding
let g:ctrlp_extensions = ['buffertag']
let g:ctrlp_match_window = 'top,order:btt,min:1,max:5,results:20'
let g:ctrlp_show_hidden = 1

" Use git to search when inside a repository
let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files'],
    \ 2: ['.hg', 'hg --cwd %s locate -I .'],
    \ },
  \ 'fallback': 'find %s -type f'
  \ }

" Ctags
Plug 'szw/vim-tags'
let g:vim_tags_auto_generate = 1

" Tagbar
Plug 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>

" Nyaovim pluggins
Plug 'rhysd/nyaovim-markdown-preview'
g:markdown_preview_auto


" Autocomplete

" Easy bindings for its various modes
nmap <leader>bb :CtrlPBuffer<cr>
nmap <leader>bm :CtrlPMixed<cr>
nmap <leader>bs :CtrlPMRU<cr>

call plug#end()

" }}}

set number          " Show line numbers
set showcmd         " Show command in bottom bar
set cursorline      " Show current cursor line
set wildmenu        " visual autocompletefor command menu
set lazyredraw      " redraw only when we need to
set showmatch       " highlight matching [{()}]

set noswapfile

" Searching

set incsearch           " search as characters are entered
set hlsearch            " highlight matches
set smartcase           " vim will detect your searching of case sentivie or not
" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

" Folding

set foldenable          " enable folding
set foldlevelstart=10   " open most folds by default
set foldnestmax=10      " 10 nested fold max
set foldmethod=marker   " fold based on indent level
" space open/closes folds
nnoremap <space> za

" GUI configs {{{ ============================================================

" Default font and font size
set guifont=Monospace\ 13
set guioptions-=m       " Remove menu
set guioptions-=T       " Remove toolbar
set guioptions-=L       " Remove left scroll
set guioptions-=r       " Remove right scroll
set guioptions=i        " VIM icon
set guioptions-=e       " Hide Tab page

" }}}

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
noremap <leader>n :NERDTreeToggle<CR>
let g:nerdtree_tabs_open_on_console_startup=1

" }}}

" Folding {{{ ================================================================

set foldenable          " enable folding
set foldlevelstart=10   " open most folds by default
set foldnestmax=10      " 10 nested fold max
set foldmethod=marker   " fold based on indent level
" space open/closes folds
nnoremap <space> za
 
" }}}

" File extensions {{{ ========================================================

au BufNewFile,BufRead *.js.php set filetype=javascript

" }}}

" Buffers navigation {{{ =====================================================

nnoremap <C-l> :bnext <cr>
nnoremap <C-h> :bprevious <cr>

" }}}
"
" Enable airline
set laststatus=2
" Enable powerline fonts
let g:airline_powerline_fonts = 1
" Enable extensions
let g:airline#extensions#tabline#enabled = 1

let g:airline_theme='gruvbox'
set background=dark



color gruvbox

