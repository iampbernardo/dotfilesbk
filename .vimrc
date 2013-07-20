" Yeah this is MYVIMRC !!!
" Created and mantained by Pablo Bernardo
" Web: http://elkarmadelteclado.com
" Twitter( @voylinux )

" I usually use PHP, HTML5 and JS
" Feel free to share and modify this file ( please mention if you like )
" Free as in Freedom !!!

" ---------------------------------------------------------------------------
" GENERAL ENVIROMENT
" ---------------------------------------------------------------------------

" Remove vi syntax compatibility
set nocompatible
" Pathogen enable
"call pathogen#incubate()
"call pathogen#helptags()

" Markdown gets auto textwidth
au Bufread,BufNewFile *.md set textwidth=79 filetype=markdown
au Bufread,BufNewFile *.markdown set textwidth=79 filetype=markdown

if $COLORTERM == 'gnome-terminal'
  set t_Co=256
endif

set rtp+=~/.vim/bundle/vundle/
 call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles

Bundle "daylerees/colour-schemes", { "rtp": "vim-themes/" }
" Sass syntax
Bundle 'cakebaker/scss-syntax.vim'


" Enable line numbers
set number
" Enable syntax
if has("syntax")
    syntax on
    filetype on
    filetype plugin on
endif

" Use , as leader key
let mapleader = ","
" Set bottom ruler
set ruler
" Set cursor line visualization
set cursorline
" Set column alert
set cc=80
" Set NO visualbells ( Obvious right? )
set novisualbell
" You can be fast, dude !!!
set timeoutlen=600
" ---------------------------------------------------------------------------
" GUI OPTIONS
" ---------------------------------------------------------------------------

" Disable some GUI options
set guioptions-=T
" And add some other ones
set guioptions=aAc
" Colours
colorscheme TronLegacy
set background=dark
" Font
set guifont=DejaVu\ Sans\ Mono\ 12
"set guifont=Inconsolata\ Medium\ 15

"Window Minimun width
set winwidth=80

" More line spacing
set linespace=5
" No wraping
set nowrap

" ---------------------------------------------------------------------------
" FILES
" ---------------------------------------------------------------------------
" Don't save security copy file
set noswapfile


"" Function for whitespaces
function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

autocmd BufWritePre *.php,*.css,*.js :call <SID>StripTrailingWhitespaces()


" ---------------------------------------------------------------------------
" INDENTATIONS
" ---------------------------------------------------------------------------
set smartindent
set autoindent
set backspace=indent,eol,start
set ts=4 sts=4 sw=4
set expandtab

" ---------------------------------------------------------------------------
" SEARCH
" ---------------------------------------------------------------------------
" Highlight search
set hlsearch
" Incremental search
set incsearch


" ---------------------------------------------------------------------------
" REMAPS
" ---------------------------------------------------------------------------

if exists(":Tabularize")
  nmap <Leader>a= :Tabularize /=<CR>
  vmap <Leader>a= :Tabularize /=<CR>
  nmap <Leader>a: :Tabularize /:\zs<CR>
  vmap <Leader>a: :Tabularize /:\zs<CR>
endif


noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>


" /* NORMAL MODE */
" Save with Ctrl + s
map <C-s> <Esc>:w<CR>
" Edit $MYVIMRC
nnoremap <leader>ev :vs $MYVIMRC<CR>
" Source $MYVIMRC
nnoremap <leader>sv :source $MYVIMRC<CR>
" Paragraphs Hand-wraping
nnoremap <leader>q gqip
" Tags folding
nnoremap <leader>ft Vatzf
" See Open Buffers
nnoremap <leader>bu :buffers<CR>

"/* INSERT MODE */
" Save with Ctrl + S
inoremap <C-s> <Esc>:w<CR>
inoremap <Tab><Space> <C-x><C-o>

" Completion based on context
inoremap <C-Space> <C-x><C-o>


"/* VISUAL MODE */


" Avoid temptation of 'Bad old Habits'
" Arrow Keys are prohibited
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>



" ---------------------------------------------------------------------------
" SPECIFIC FOR PLUGINS
" ---------------------------------------------------------------------------

" ---------------------------------------------------------------------------
" EXTRA
" ---------------------------------------------------------------------------
"Autoload $MYVIMRC file on save
autocmd bufwritepost .vimrc source $MYVIMRC
autocmd bufread * source $MYVIMRC

"PHP Syntax Autocomplete
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
" Wordpress highlight
autocmd BufEnter *.php :set syn=wordpress


"Color Syntax inspector

" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
