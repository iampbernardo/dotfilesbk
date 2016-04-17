"
" neovim configuration
" @author Pablo Bernardo @voylinux
"
"

set number
set expandtab                  " spaces instead of tabs
set tabstop=2                 " a tab = four spaces
set shiftwidth=2               " number of spaces for auto-indent
set softtabstop=2              " a soft-tab of four spaces
set autoindent                 " set on the auto-indent
  
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


" Music
Plug 'takac/vim-spotifysearch'
let g:spotify_country_code = 'ES'
let g:spotify_prev_key = "<F9>"
let g:spotify_playpause_key = "<F10>"
let g:spotify_next_key = "<F11>"

" Layout
Plug 'Yggdroot/indentLine'
let g:indentLine_enable = 1
	

" Git help
Plug 'tpope/vim-git'
Plug 'tpope/vim-fugitive'

"Web development
Plug 'mattn/emmet-vim'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
let g:deoplete#enable_at_startup = 1
if !exists('g:deoplete#omni#input_patterns')
  let g:deoplete#omni#input_patterns = {}
endif
" let g:deoplete#disable_auto_complete = 1
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
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : deoplete#mappings#manual_complete()
" " ,<Tab> for regular tab
inoremap <Leader><Tab> <Space><Space>
" tern
autocmd FileType javascript nnoremap <silent> <buffer> gb :TernDef<CR>

" Colorschemes
Plug 'goatslacker/mango.vim'
Plug 'junegunn/seoul256.vim'
Plug 'tomasr/molokai'

" Navigation
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle'}
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle'}

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
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
\}

" Use the nearest .git directory as the cwd
" This makes a lot of sense if you are working on a project that is in version
" control. It also supports works with .svn, .hg, .bzr.
let g:ctrlp_working_path_mode = 'r'

" Use a leader instead of the actual named binding
nmap <leader>p :CtrlP<cr>

" Nyaovim pluggins
Plug 'rhysd/nyaovim-markdown-preview'
g:markdown_preview_auto


" Autocomplete

Plug 'Shougo/deoplete.nvim'
" Use deoplete.
let g:deoplete#enable_at_startup = 1

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


" Enable airline
set laststatus=2
" Enable powerline fonts
let g:airline_powerline_fonts = 1
" Enable extensions
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='powerlineish'
set background=dark


color molokai

