" 
" VIM Configuration file
"
" Author: Pablo Bernardo <voylinux@gmail.com>
" Twitter: @voylinux
" Blog: http://elkarmadelteclado.com
"

" Establecer lenguaje {{{ ====================================================

language en_US.UTF-8           " Soluciona problemas con algunos plugins

" }}}

" NEOBUNDLE {{{ ==============================================================

set nocompatible               " No a la compatibilidad completa con vi.

" Autoinstalacion y configuracion de NeoBundle {{{

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
" }}}

" BUNDLES (Plugins gestionados a través de NeoBundle) {{{

" YouCompleteMe {{{

NeoBundle 'Valloric/YouCompleteMe'

" }}}

" Shougo's way {{{

" Vimproc para ejecutar comandos de forma asincrona (NeoBundle, Unite)
"
NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }


" }}}

" Esquemas de color {{{

" Temas Oscuros
" Versión de molokai mejorada para terminal, casi identica a la de GUI
NeoBundle 'joedicastro/vim-molokai256'

NeoBundle 'daylerees/colour-schemes', { "rtp": "vim-themes/"}

NeoBundle 'tomasr/molokai'

NeoBundle 'sjl/badwolf'

NeoBundle 'nielsmadan/harlequin'

NeoBundle 'joedicastro/vim-github256'

NeoBundle 'noahfrederick/vim-hemisu'

" }}}

" Python {{{

" }}}

" Snippets de código {{{

" Gestor de snippets avanzado y potente
NeoBundle 'SirVer/ultisnips'

let g:UltiSnipsSnippetDirectories=["UltiSnips", "ultisnips_my", "vim-snippets"]

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" }}}

" HTML/CSS {{{

" }}}

" JS {{{


" }}}

" Tmux {{{

" Permite interactuar con Tmux desde vim
NeoBundle 'benmills/vimux'
" Sintaxis del archivo de configuración de tmux
NeoBundleLazy 'vimez/vim-tmux', { 'autoload' : { 'filetypes' : 'conf'}}

" }}}

" NERDTree {{{

NeoBundle 'scrooloose/nerdtree'



" }}}

" FIN BUNDLES }}}

" Instalar los plugins automaticamente {{{

" Instalando los plugins por primera vez
if iCanHazNeoBundle == 0
    echo "Installing Bundles, please ignore key map error messages"
    echo ""
    :NeoBundleInstall
endif

" Comprueba que todos los plugins estén instalados, si alguno no lo esta, nos
" preguntara si queremos instalarlo (util para cuando añadimos uno nuevo al
" .vimrc)
NeoBundleCheck

" }}}

filetype plugin indent on      " Sangrado y plugins por tipo de archivo.

" FIN NEOBUNDLE }}}

" CONFIGURACIÓN DE VIM {{{ ===================================================

" Opciones básicas {{{ -------------------------------------------------------

scriptencoding utf-8
set encoding=utf-8              " Establece la codificación a UTF-8.
set ls=2                        " Linea de estado siempre visible.
set go-=T                       " Elimina la barra de herramientas.
set go-=m                       " Elimina el menú.
set go+=rRlLbh                  " Activa todas las barras de desplazamiento.
set go-=rRlLbh                  " Desactiva todas las barras de desplazamiento.
set novisualbell                  " Activa la alarma visual.
set cursorline                  " Resalta la linea bajo el cursor.
set fillchars+=vert:│           " Mejora aspecto de la división de ventanas.
set ttyfast                     " Mejora el redibujado de la pantalla.
set title                       " Añade el nombre del archivo al terminal.
set showcmd                     " Muestra comandos incompletos.
set hidden                      " Oculta los bufferes cerrados.
set ruler                       " Establece una regla permanente.
set lazyredraw                  " Redibuja solo cuando es necesario.
set autoread                    " Actualiza cambios realizados fuera de vim.
set ttimeoutlen=10               " Conmuta instantaneamente entre modos
set backspace=indent,eol,start  " Definir el comportamiento de tecla Backspace
set number

" }}}

" Título automático de la ventana de VIM {{{ ---------------------

set title
let &titlestring = hostname() . "[vim(" . expand("%:t") . ")]"
if &term == "screen"
    set t_ts=^[k
    set t_fs=^[\
endif
if &term == "screen" || &term == "xterm"
    set title
endif


" }}}

" Búsqueda {{{ ---------------------------------------------------------------

set incsearch                   " Búsqueda incremental.
set showmatch                   " Muestra la pareja de paréntesis.
set hlsearch                    " Resultados de búsquela resaltados.
set smartcase                   " Distinción mayúsculas/minúsculas inteligente.
set ignorecase                  " No distingue entre mayúsculas y minúsculas.

" }}}

" Historia y niveles de deshacer persistentes {{{ ----------------------------

set history=1000
set undofile
set undoreload=1000

" }}}

" Copias de seguridad {{{ ----------------------------------------------------

set backup
set noswapfile
set backupdir=$HOME/.vim/tmp/backup/
set undodir=$HOME/.vim/tmp/undo/
set directory=$HOME/.vim/tmp/swap/
set viminfo+=n$HOME/.vim/tmp/viminfo

" Crear estos directorios si no existen previamente
silent! call MakeDirIfNoExists(&undodir)
silent! call MakeDirIfNoExists(&backupdir)
silent! call MakeDirIfNoExists(&directory)

" }}}

" Wildmenu {{{ ---------------------------------------------------------------

set wildmenu                        " Autocompletado de la linea de comandos.
set wildmode=list:longest,full      " Muestra una lista de todas las opciones.

" set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.bak,*.?~,*.??~,*.???~,*.~      " Backup files
set wildignore+=*.luac                           " Lua byte code
set wildignore+=*.jar                            " java archives
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.stats                          " Pylint stats

" }}}

" Tabulado, espaciado y wrapping {{{ -----------------------------------------

set expandtab                  " Emplea espacios en vez de tabulados.
set tabstop=4                  " Un Tabulado son cuatro espacios.
set shiftwidth=4               " Numero de espacios para autoindentado.
set softtabstop=4              " Un Tabulado de cuatro espacios.
set autoindent                 " Establece el autoindentado.

" set wrap
set textwidth=0
" set formatoptions=qrn1
set colorcolumn=+1
" set column
set colorcolumn=80,120
" }}}

" Hagamos las cosas bien, fuera teclas de desplazamiento {{{ -----------------

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" }}}

" Esquema de color {{{ -------------------------------------------------------
"Set enable the route for Powerline
set rtp+=/usr/local/lib/python2.7/dist-packages/powerline/bindings/vim/
" Allways show statusline
set laststatus=2

syntax on                      " Habilita el resaltado de sintaxis.
set background=dark            " Establece el fondo oscuro.
set t_Co=256                   " Habilita 256 colores en modo consola.
if has('gui_running')          " Habilita el tema molokai para gvim y vim.
    colorscheme molokai
else
    colorscheme molokai256
endif

" }}}

" Fuente monoespaciada para gui {{{ ------------------------------------------

set guifont=Inconsolata\ medium\ 14      " La fuente de texto para gvim.

" }}}

" Reescalar las divisiones cuando se cambia el tamaño de la ventana {{{ ------

au VimResized * exe "normal! \<c-w>="

" }}}

" Mapeo de la teclas <Leader> y <LocalLeader> {{{ -----------------------------

let mapleader=','
let maplocalleader= ' '

" }}}

" Nuevas ventanas {{{ -------------------------------------------------

nnoremap <Leader>v <C-w>v
nnoremap <Leader>h <C-w>s

" }}}

" Cambio rápido entre ventanas {{{ -------------------------------------------

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" }}}

" Conmutar números de linea {{{ ----------------------------------------------

nnoremap <silent><Leader>l :call ToggleRelativeAbsoluteNumber()<CR>
function! ToggleRelativeAbsoluteNumber()
  if !&number && !&relativenumber
      set number
      set norelativenumber
  elseif &number && !&relativenumber
      set nonumber
      set relativenumber
  elseif !&number && &relativenumber
      set number
      set relativenumber
  elseif &number && &relativenumber
      set nonumber
      set norelativenumber
  endif
endfunction

" }}}

" Mostrar caracteres no imprimibles {{{

nmap <Leader>eh :set list!<CR>
set listchars=tab:→\ ,eol:↵,trail:·,extends:↷,precedes:↶

" }}}

" Plegado {{{ ----------------------------------------------------------------

set foldmethod=marker

" Conmutar el plegado

nnoremap \ za
vnoremap \ za

" }}}

" Copiar/Pegar {{{

" copiar/pegar desde el portapapeles
map <Leader>y "*y
map <Leader>p "*p

" conmutar el paste mode
map <Leader>P :set invpaste<CR>

" }}}

" Autocargar configuración cuando se cambia $MYVIMRC {{{

" Tambien volvemos a cargar el esquema de color de Powerline para evitar
" artefactos
autocmd! BufWritePost vimrc source % | call Pl#Load()

" }}}

" Guardar como root {{{

cmap w!! w !sudo tee % >/dev/null<CR>:e!<CR><CR>

" }}}

" Guardado rápido {{{

nmap <silent> <Leader>w :update<CR>

" }}}

" Mantener el cursor centrado vericalmente {{{

set scrolloff=999
noremap <Leader>zz :let &scrolloff=999-&scrolloff<CR>

" }}}

" NERDTree  {{{

noremap <Leader>n :NERDTreeToggle<CR>


" }}}


" FIN CONFIGURACIÓN VIM }}}

" CONFIGURACIÓN DE PLUGINS {{{ ===============================================

" Fugitive {{{

nnoremap <Leader>gn :Unite output:echo\ system("git\ init")<CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>go :Gread<CR>
nnoremap <Leader>gR :Gremove<CR>
nnoremap <Leader>gm :Gmove<Space>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gB :Gbrowse<CR>
nnoremap <Leader>gp :Git! push<CR>
nnoremap <Leader>gP :Git! pull<CR>
nnoremap <Leader>gi :Git!<Space>
nnoremap <Leader>ge :Gedit<CR>
nnoremap <Leader>gE :Gedit<Space>
nnoremap <Leader>gl :exe "silent Glog <Bar> Unite -no-quit
            \ quickfix"<CR>:redraw!<CR>
nnoremap <Leader>gL :exe "silent Glog -- <Bar> Unite -no-quit
            \ quickfix"<CR>:redraw!<CR>
nnoremap <Leader>gt :!tig<CR>:redraw!<CR>
nnoremap <Leader>gg :exe 'silent Ggrep -i '.input("Pattern: ")<Bar>Unite
            \ quickfix -no-quit<CR>
nnoremap <Leader>ggm :exe 'silent Glog --grep='.input("Pattern: ").' <Bar>
            \Unite -no-quit quickfix'<CR>
nnoremap <Leader>ggt :exe 'silent Glog -S='.input("Pattern: ").' <Bar>
            \Unite -no-quit quickfix'<CR>

nnoremap <Leader>ggc :silent! Ggrep -i<Space>

" para el diffmode
noremap <Leader>du :diffupdate<CR>

if !exists(":Gdiffoff")
    command Gdiffoff diffoff | q | Gedit
endif
noremap <Leader>dq :Gdiffoff<CR>
" }}}

" winresizer {{{

let g:winresizer_start_key = '<C-C><C-W>'
" cancelar pulsando ESC
" let g:winresizer_finish_with_escape = 1
let g:winresizer_keycode_finish = 27

" }}}

" NERDTree {{{

let NERDTreeShowHidden=1


" }}}

" FIN CONFIGURACION PLUGINS }}}

" TIPOS DE ARCHIVO  {{{ ======================================================




" FIN TIPOS DE ARCHIVO }}}

" FUNCIONES PROPIAS {{{ ==============================================================


" }}}

" vim:foldmethod=marker
