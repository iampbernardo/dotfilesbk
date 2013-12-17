"
"                       __   _(_)_ __ ___  _ __ ___
"                       \ \ / / | '_ ` _ \| '__/ __|
"                        \ V /| | | | | | | | | (__
"                       (_)_/ |_|_| |_| |_|_|  \___|
"
" Author: joe di castro <joe@joedicastro.com>
" Source: http://github.com/joedicastro/dotfiles/tree/master/vim
"
" Este archivo esta sujeto a cambios frecuentes. Conviene seguir los commits en
" el control de versiones.

" Establecer lenguaje {{{ ====================================================

" language en_US.UTF-8           " Soluciona problemas con algunos plugins

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

" Shougo's way {{{

" Vimproc para ejecutar comandos de forma asincrona (NeoBundle, Unite)
NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

" Unite. La interfaz para casi todo
NeoBundle 'Shougo/unite.vim'

" Unite sources
NeoBundleLazy 'Shougo/unite-outline', {'autoload':{'unite_sources':'outline'}}
NeoBundleLazy 'Shougo/unite-session', {'autoload':{'unite_sources':'session',
            \ 'commands' : ['UniteSessionSave', 'UniteSessionLoad']}}
NeoBundleLazy 'tsukkee/unite-help', {'autoload':{'unite_sources':'help'}}
NeoBundleLazy 'ujihisa/unite-colorscheme', {'autoload':{'unite_sources':
            \ 'colorscheme'}}
NeoBundleLazy 'ujihisa/unite-locate', {'autoload':{'unite_sources':'locate'}}
NeoBundleLazy 'thinca/vim-unite-history', { 'autoload' : { 'unite_sources' :
            \ ['history/command', 'history/search']}}
NeoBundleLazy 'osyo-manga/unite-filetype', { 'autoload' : {'unite_sources' :
            \ 'filetype', }}
NeoBundleLazy 'osyo-manga/unite-quickfix', {'autoload':{'unite_sources':
            \ ['quickfix', 'location_list']}}
NeoBundleLazy 'osyo-manga/unite-fold', {'autoload':{'unite_sources':'fold'}}
NeoBundleLazy 'tacroe/unite-mark', {'autoload':{'unite_sources':'mark'}}

" Explorador de archivos (para cuando no tenga disponible ranger)
NeoBundleLazy 'Shougo/vimfiler', {'autoload' : { 'commands' : ['VimFiler']}}

" Archivos desechables
NeoBundleLazy 'Shougo/junkfile.vim', {'autoload':{'commands':'JunkfileOpen',
            \ 'unite_sources':['junkfile','junkfile/new']}}

" }}}

" Esquemas de color {{{

" Temas Oscuros
" Versión de molokai mejorada para terminal, casi identica a la de GUI
NeoBundle 'joedicastro/vim-molokai256'

NeoBundle 'tomasr/molokai'
NeoBundleLazy 'sjl/badwolf', { 'autoload' :
        \ { 'unite_sources' : 'colorscheme', }}
NeoBundleLazy 'nielsmadan/harlequin', { 'autoload' :
        \ { 'unite_sources' : 'colorscheme', }}


" Temas claros
NeoBundleLazy 'vim-scripts/summerfruit256.vim', { 'autoload' :
        \ { 'unite_sources' : 'colorscheme', }}
NeoBundleLazy 'joedicastro/vim-github256', { 'autoload' :
        \ { 'unite_sources' : 'colorscheme', }}

" Crear temas para terminal a partir de temas para GUI
NeoBundleLazy 'godlygeek/csapprox', { 'autoload' :
        \ { 'commands' : ['CSApprox', 'CSApproxSnapshot']}}

" }}}

" DCVS {{{
"
" Para gestionar Git
NeoBundle 'tpope/vim-fugitive'
" Para señalar los cambios del archivo con respecto al repo Git
NeoBundle 'airblade/vim-gitgutter'
" Visor de Git
NeoBundleLazy 'gregsexton/gitv', {'depends':['tpope/vim-fugitive'],
            \ 'autoload':{'commands':'Gitv'}}

" }}}

" Markdown {{{

" Sintaxis de Markdown
NeoBundleLazy 'joedicastro/vim-markdown'
" Previsualizacion de Markdown en el navegador, con soporte de Markdown Extra
NeoBundleLazy 'joedicastro/vim-markdown-extra-preview'

" }}}

" Utilidades de Linux {{{

" Permite realizar un diff entre directorios
NeoBundleLazy 'joedicastro/DirDiff.vim', { 'autoload': { 'commands' : 'DirDiff'}}
" Edición hexadecimal
NeoBundleLazy 'vim-scripts/hexman.vim', { 'autoload' :
            \ { 'mappings' : [['ni', '<Plug>HexManager']]}}

" }}}

" Python {{{

" Autocompletado
NeoBundle 'Shougo/neocomplete.vim'
" Gran plugin para programar en Python
NeoBundleLazy 'klen/python-mode', {'autoload': {'filetypes': ['python']}}
" Manejar virtualenvs
NeoBundleLazy 'jmcantrell/vim-virtualenv', {'autoload': {'filetypes': ['python']}}
" Visualizar lineas de identado
NeoBundleLazy 'Yggdroot/indentLine', {'autoload': {'filetypes': ['python']}}
" Visualizar informes de coverage.py
NeoBundleLazy 'alfredodeza/coveragepy.vim', {'autoload': {'filetypes': ['python']}}

" }}}

" Snippets de código {{{

" Gestor de snippets avanzado y potente
NeoBundle 'SirVer/ultisnips'

" }}}

" Sintaxis {{{

NeoBundleLazy 'vim-scripts/JSON.vim', {'autoload': {'filetypes': ['json']}}
NeoBundleLazy 'vim-scripts/po.vim--gray', {'autoload': {'filetypes': ['po']}}
NeoBundleLazy 'joedicastro/vim-pentadactyl', {
            \ 'autoload': {'filetypes': ['pentadactyl']}}
NeoBundleLazy 'vim-scripts/crontab.vim', {'autoload': {'filetypes': ['crontab']}}
NeoBundle 'scrooloose/syntastic'

" }}}

" Abrir enlaces externos {{{

" Abrir una url en el navegador u otro tipo de archivos con una app externa
NeoBundle 'vim-scripts/utl.vim'

" }}}

" Edición de texto {{{

" Autocompletado de (, [, {, ', ", ...
NeoBundle 'kana/vim-smartinput'
" NeoBundle 'delimitMate.vim'
" Cambio de fechas rápido e inteligente
NeoBundle 'tpope/vim-speeddating'
" genial plugin para insertar/cambiar/eliminar pares de caracteres en torno a un
" objeto
NeoBundle 'tpope/vim-surround'
" permite repetir ciertas operaciones con la tecla 'dot'
NeoBundle 'tpope/vim-repeat'
" conmutar entre lineas de comentario
NeoBundle 'tpope/vim-commentary'
" inserción inteligente de digrafos
NeoBundle 'Rykka/easydigraph.vim'
" para navegar por el arbol de niveles de deshacer de vim
NeoBundleLazy 'sjl/gundo.vim', { 'autoload' : {'commands': 'GundoToggle'}}
" para insertar bloques de lorem ipsum
NeoBundleLazy 'vim-scripts/loremipsum', { 'autoload' :
            \ { 'commands' : 'Loremipsum'}}
" para conocer toda la informacion sobre un caracter, incluido Unicode
NeoBundle 'tpope/vim-characterize'
" transponer lines y bloques de texto (intercambiar columnas y lineas)
NeoBundleLazy 'salsifis/vim-transpose', { 'autoload' :
            \ { 'commands' : 'Transpose'}}
" gestion de marcadores
NeoBundle 'kshenoy/vim-signature'
" text-objects
NeoBundle 'kana/vim-textobj-entire' " ae, ie
NeoBundle 'kana/vim-textobj-indent' " ai, ii, aI, iI
NeoBundle 'kana/vim-textobj-lastpat' " a/, i/, a?, i?
NeoBundle 'kana/vim-textobj-line' " al, il
NeoBundle 'kana/vim-textobj-underscore' " a_, i_
NeoBundle 'kana/vim-textobj-user'
" multiples cursores
NeoBundle 'terryma/vim-multiple-cursors'

" }}}

" HTML/CSS {{{

" Permite seleccionar colores mediante una paleta y previsualizarlos en los
" archivos css/html (los atajos de ColorV solo funcionaran cuando se cargue
" este, a traves de un comando o Unite. Lo prefiero asi, no lo uso tan a menudo)
NeoBundleLazy 'Rykka/colorv.vim', {'autoload' : {
            \ 'commands' : [
                             \ 'ColorV', 'ColorVView', 'ColorVPreview',
                             \ 'ColorVPicker', 'ColorVEdit', 'ColorVEditAll',
                             \ 'ColorVInsert', 'ColorVList', 'ColorVName',
                             \ 'ColorVScheme', 'ColorVSchemeFav',
                             \ 'ColorVSchemeNew', 'ColorVTurn2'],
            \ }}
NeoBundleLazy 'othree/html5.vim', {'autoload':
            \ {'filetypes': ['html', 'xhttml', 'css']}}
NeoBundleLazy 'joedicastro/vim-sparkup', {'autoload':
            \ {'filetypes': ['html', 'xhttml', 'css']}}

" }}}

" GUI {{{

" Barra de estado mas elegante y efectiva
NeoBundle 'joedicastro/vim-powerline'
" permite des/hacer un zoom a ventana completa de uno de las ventanas
NeoBundleLazy 'vim-scripts/zoomwintab.vim', {'autoload' :
            \{'commands' : 'ZoomWinTabToggle'}}
" redimensionar ventanas facilmente
NeoBundle 'jimsei/winresizer'

" }}}

" Tmux {{{

" Permite interactuar con Tmux desde vim
NeoBundle 'benmills/vimux'
" Sintaxis del archivo de configuración de tmux
NeoBundleLazy 'vimez/vim-tmux', { 'autoload' : { 'filetypes' : 'conf'}}

" }}}

" API Web {{{

NeoBundle 'mattn/webapi-vim'

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
set visualbell                  " Activa la alarma visual.
set cursorline                  " Resalta la linea bajo el cursor.
set fillchars+=vert:│           " Mejora aspecto de la división de ventanas.
set ttyfast                     " Mejora el redibujado de la pantalla.
set title                       " Añade el nombre del archivo al terminal.
set showcmd                     " Muestra comandos incompletos.
set hidden                      " Oculta los bufferes cerrados.
set ruler                       " Establece una regla permanente.
set lazyredraw                  " Redibuja solo cuando es necesario.
set autoread                    " Actualiza cambios realizados fuera de vim.
set ttimeoutlen=0               " Conmuta instantaneamente entre modos
set backspace=indent,eol,start  " Definir el comportamiento de tecla Backspace

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

" Crear directorio si no existe previamente {{{

function! MakeDirIfNoExists(path)
    if !isdirectory(expand(a:path))
        call mkdir(expand(a:path), "p")
    endif
endfunction

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
set textwidth=80
" set formatoptions=qrn1
set colorcolumn=+1

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

set guifont=Dejavu\ Sans\ Mono\ for\ Powerline\ 11      " La fuente de texto para gvim.

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

" Rotar ventanas (dentro de la misma columna o fila) {{{

nnoremap <Tab>j <C-W>R
nnoremap <Tab>k <C-W>r

" }}}

" Cierre rápido de ventana y buffer {{{

nnoremap <Leader>k <C-w>c
nnoremap <silent><Leader>B :bd<CR>

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

" Revisión de ortografía {{{ ---------------------------------------------------

" Revisar ortografía en español
nmap <Leader>ss :setlocal spell spelllang=es<CR>
" Revisar ortografía en inglés
nmap <Leader>se :setlocal spell spelllang=en<CR>
" Desactivar revisión ortográfica
nmap <Leader>so :setlocal nospell <CR>
" Ir a la siguiente palabra mal escrita
nmap <Leader>sn ]s
" Sugerir palabras
nmap <Leader>sp z=
" Ir a la siguiente palabra y sugerir correcion
nmap <Leader>sc ]sz=
" Añadir palabra al diccionario
nmap <Leader>sa zg
" }}}

" Guardar como root {{{

cmap w!! w !sudo tee % >/dev/null<CR>:e!<CR><CR>

" }}}

" Guardado rápido {{{

nmap <silent> <Leader>w :update<CR>

" }}}

" Eliminar espacios al final de la línea {{{

nmap <silent><Leader>et :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" }}}

" Emplear Ranger como navegador de archivos {{{

fun! RangerChooser()
    exec "silent !ranger --choosefile=/tmp/chosenfile " . expand("%:p:h")
    if filereadable('/tmp/chosenfile')
        exec 'edit ' . system('cat /tmp/chosenfile')
        call system('rm /tmp/chosenfile')
    endif
    redraw!
endfun
map <Leader>x :call RangerChooser()<CR>
" }}}

" Mostrar/Ocultar la ventana Quickfix {{{

function! s:QuickfixToggle()
    for i in range(1, winnr('$'))
        let bnum = winbufnr(i)
        if getbufvar(bnum, '&buftype') == 'quickfix'
            cclose
            lclose
            return
        endif
    endfor
    copen
endfunction
command! ToggleQuickfix call <SID>QuickfixToggle()

nnoremap <silent> <Leader>q :ToggleQuickfix<CR>

" }}}

" Estadisticas de texto {{{

" obtener el numero de lineas, palabras, caracteres y bytes (totales y de la
" posicion actual)
map <Leader>es g<C-G>

" obtener la frecuencia con la que aparece cada palabra para un texto dado
function! WordFrequency() range
  let all = split(join(getline(a:firstline, a:lastline)), '\A\+')
  let frequencies = {}
  for word in all
    let frequencies[word] = get(frequencies, word, 0) + 1
  endfor
  let lst = []
  for [key,value] in items(frequencies)
    call add(lst, value."\t".key."\n")
  endfor
  call sort(lst)
  echo join(lst)
endfunction
command! -range=% WordFrequency <line1>,<line2>call WordFrequency()
map <Leader>ew :Unite output:WordFrequency<CR>

" }}}

" Numero de lineas de codigo {{{

" obtener el numero de lineas de codigo
" map <silent> <Leader>sc :cgete system('cloc --quiet '. bufname("%"))<Bar>copen<CR>
function! LinesOfCode()
    echo system('cloc --quiet '.bufname("%"))
endfunction
"}}}

" Activar/Desactivar el resaltado de búsqueda {{{

map <silent><Leader>eq :set invhlsearch<CR>

" }}}

" Desplazarse entre ventanas de Vim y Tmux {{{

if exists('$TMUX')
  function! TmuxOrSplitSwitch(wincmd, tmuxdir)
    let previous_winnr = winnr()
    execute "wincmd " . a:wincmd
    if previous_winnr == winnr()
      " The sleep and & gives time to get back to vim so tmux's focus tracking
      " can kick in and send us our ^[[O
      execute "silent !sh -c 'sleep 0.01; tmux select-pane -" . a:tmuxdir . "' &"
      redraw!
    endif
  endfunction
  let previous_title = substitute(system("tmux display-message -p '#{pane_title}'"), '\n', '', '')
  let &t_ti = "\<Esc>]2;vim\<Esc>\\" . &t_ti
  let &t_te = "\<Esc>]2;". previous_title . "\<Esc>\\" . &t_te
  nnoremap <silent> <C-h> :call TmuxOrSplitSwitch('h', 'L')<CR>
  nnoremap <silent> <C-j> :call TmuxOrSplitSwitch('j', 'D')<CR>
  nnoremap <silent> <C-k> :call TmuxOrSplitSwitch('k', 'U')<CR>
  nnoremap <silent> <C-l> :call TmuxOrSplitSwitch('l', 'R')<CR>
else
  map <C-h> <C-w>h
  map <C-j> <C-w>j
  map <C-k> <C-w>k
  map <C-l> <C-w>l
endif

" }}}

" Salir sin guardar nada {{{

nnoremap <Leader>`` :qa!<CR>

" }}}

" Permisos de ejecución automáticos a los archivos con shebang (#!) {{{

augroup shebang_chmod
  autocmd!
  autocmd BufNewFile  * let b:brand_new_file = 1
  autocmd BufWritePost * unlet! b:brand_new_file
  autocmd BufWritePre *
        \ if exists('b:brand_new_file') |
        \   if getline(1) =~ '^#!' |
        \     let b:chmod_post = '+x' |
        \   endif |
        \ endif
  autocmd BufWritePost,FileWritePost *
        \ if exists('b:chmod_post') && executable('chmod') |
        \   silent! execute '!chmod '.b:chmod_post.' "<afile>"' |
        \   unlet b:chmod_post |
        \ endif
augroup END

" }}}

" Cargar matchit por defecto {{{

if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

" }}}

" Convertir comportamiento de Y en similar a D y C {{{

nnoremap Y y$

" }}}

" FIN CONFIGURACIÓN VIM }}}

" CONFIGURACIÓN DE PLUGINS {{{ ===============================================

" ColorV {{{

let g:colorv_cache_file=$HOME.'/.vim/tmp/vim_colorv_cache'
let g:colorv_cache_fav=$HOME.'/.vim/tmp/vim_colorv_cache_fav'

" }}}

" Commentary {{{ -------------------------------------------------------------

nmap <Leader>c <Plug>CommentaryLine
xmap <Leader>c <Plug>Commentary

augroup plugin_commentary
    au!
    au FileType python setlocal commentstring=#%s
    au FileType htmldjango setlocal commentstring={#\ %s\ #}
    au FileType puppet setlocal commentstring=#\ %s
augroup END

" }}}

" easydigraph {{{

let g:EasyDigraph_nmap = '<Leader>dd'

" }}}

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

" Gitv {{{

nnoremap <silent> <leader>gv :Gitv --all<CR>
nnoremap <silent> <leader>gV :Gitv! --all<CR>
vnoremap <silent> <leader>gV :Gitv! --all<CR>

let g:Gitv_OpenHorizontal = 'auto'
let g:Gitv_WipeAllOnClose = 1
let g:Gitv_DoNotMapCtrlKey = 1
" let g:Gitv_WrapLines = 1

autocmd FileType git set nofoldenable

" }}}

" Gundo {{{ ------------------------------------------------------------------

nnoremap <Leader>u :GundoToggle<CR>

let g:gundo_preview_bottom = 1

" }}}

" HexManager {{{

map <F6> <Plug>HexManager<CR>

" }}}

" indentLine {{{

map <silent> <Leader>L :IndentLinesToggle<CR>
let g:indentLine_enabled = 0
let g:indentLine_char = '┊'
let g:indentLine_color_term = 239

" }}}

" Neocomplete {{{

let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_auto_select = 1
let g:neocomplete#enable_refresh_always = 1
let g:neocomplete#max_list = 30
let g:neocomplete#min_keyword_length = 1
let g:neocomplete#sources#syntax#min_keyword_length = 1
let g:neocomplete#data_directory = $HOME.'/.vim/tmp/neocomplete'

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" }}}

" Po.vim {{{ -----------------------------------------------------------------

let g:po_translator = "joe di castro <joe@joedicastro.com>"

" }}}

" Powerline {{{ --------------------------------------------------------------

let g:Powerline_symbols = 'fancy'
let g:Powerline_cache_file = $HOME.'/.vim/tmp/Powerline.cache'

set noshowmode

call Pl#Theme#InsertSegment('ws_marker', 'after', 'lineinfo')

" }}}

" PythonMode {{{ -------------------------------------------------------------

let g:pymode_breakpoint_key = '<Leader>B'

let g:pymode_lint_checker = 'pylint,pep8,mccabe,pep257'
let g:pymode_lint_ignore = ''
let g:pymode_lint_config = $HOME.'/dotfiles/pylint/pylint.rc'

let g:pymode_rope = 1
let g:pymode_rope_goto_def_newwin = 'new'
let g:pymode_rope_guess_project = 0
let g:pymode_rope_vim_completion = 1
let g:pymode_rope_always_show_complete_menu = 1

" }}}

" Syntastic {{{

let g:syntastic_python_pylint_exe = "pylint2"

let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_style_error_symbol  = '⚡'
let g:syntastic_style_warning_symbol  = '⚡'

" }}}

" Unite {{{

" archivos
nnoremap <silent><Leader>o :Unite -silent -start-insert file<CR>
nnoremap <silent><Leader>m :Unite -silent file_mru<CR>
" buffers
nnoremap <silent><Leader>b :Unite -silent buffer<CR>
" busquedas buffer
nnoremap <silent><Leader>f :Unite -silent -no-split -start-insert -auto-preview
            \ line<CR>
nnoremap <silent>[menu]w :UniteWithCursorWord -silent -no-split -auto-preview
            \ line<CR>
" yankring
nnoremap <silent><Leader>i :Unite -silent history/yank<CR>
" grep
nnoremap <silent><Leader>a :Unite -silent -no-quit grep<CR>
" ayuda
nnoremap <silent> g<C-h> :UniteWithCursorWord -silent help<CR>
" tareas
nnoremap <silent><Leader>; :Unite -silent -toggle
            \ grep:%::FIXME\|TODO\|NOTE\|XXX\|COMBAK\|@todo<CR>
" encabezados (tambien ctags)
nnoremap <silent><Leader>t :Unite -silent -vertical -winwidth=40
            \ -direction=topleft -toggle outline<CR>
" archivos desechables
  nnoremap <silent><Leader>d :Unite -silent junkfile/new junkfile<CR>

" menus {{{
let g:unite_source_menu_menus = {}

" tecla prefijo para menus (se usa para todos los menus de Unite) {{{
nnoremap [menu] <Nop>
nmap <LocalLeader> [menu]
" }}}

" menu menus
nnoremap <silent>[menu]u :Unite -silent -winheight=20 menu<CR>

" menu archivos y directorios {{{
let g:unite_source_menu_menus.archivos = {
    \ 'description' : '       archivos y directorios
        \                                ⌘ [espacio]o',
    \}
let g:unite_source_menu_menus.archivos.command_candidates = [
    \['▷ abrir archivo                                              ⌘ ,o',
        \'Unite -start-insert file'],
    \['▷ abrir archivo reciente                                     ⌘ ,m',
        \'Unite file_mru'],
    \['▷ abrir archivo con busqueda recursiva',
        \'Unite -start-insert file_rec/async'],
    \['▷ crear nuevo archivo',
        \'Unite file/new'],
    \['▷ buscar directorio',
        \'Unite directory'],
    \['▷ buscar directorio reciente',
        \'Unite directory_mru'],
    \['▷ buscar directorio con busqueda recursiva',
        \'Unite directory_rec/async'],
    \['▷ crear nuevo directorio',
        \'Unite directory/new'],
    \['▷ cambiar directorio de trabajo',
        \'Unite -default-action=lcd directory'],
    \['▷ conocer directorio de trabajo',
        \'Unite output:pwd'],
    \['▷ archivos desechables                                       ⌘ ,d',
        \'Unite junkfile/new junkfile'],
    \['▷ guardar como root                                          ⌘ :w!!',
        \'exe "write !sudo tee % >/dev/null"'],
    \['▷ guardado rapido                                            ⌘ ,w',
        \'normal ,w'],
    \['▷ abrir ranger                                               ⌘ ,x',
        \'call RangerChooser()'],
    \['▷ abrir vimfiler                                             ⌘ ,X',
        \'VimFiler'],
    \]
nnoremap <silent>[menu]o :Unite -silent -winheight=17 -start-insert
            \ menu:archivos<CR>
" }}}

" menu busqueda de archivos {{{
let g:unite_source_menu_menus.grep = {
    \ 'description' : '           buscar archivos
        \                                       ⌘ [espacio]a',
    \}
let g:unite_source_menu_menus.grep.command_candidates = [
    \['▷ grep (ag → ack → grep)                                     ⌘ ,a',
        \'Unite -no-quit grep'],
    \['▷ find',
        \'Unite find'],
    \['▷ locate',
        \'Unite -start-insert locate'],
    \['▷ vimgrep (muy lento)',
        \'Unite vimgrep'],
    \]
nnoremap <silent>[menu]a :Unite -silent menu:grep<CR>
" }}}

" menu buffers, ventanas y pestañas {{{
let g:unite_source_menu_menus.navegacion = {
    \ 'description' : '     navegar por buffers, pestañas y ventanas
        \              ⌘ [espacio]b',
    \}
let g:unite_source_menu_menus.navegacion.command_candidates = [
    \['▷ buffers                                                    ⌘ ,b',
        \'Unite buffer'],
    \['▷ pestañas',
        \'Unite tab'],
    \['▷ ventanas',
        \'Unite window'],
    \['▷ location list',
        \'Unite location_list'],
    \['▷ quickfix',
        \'Unite quickfix'],
    \['▷ redimensionar ventanas                                     ⌘ C-C C-W',
        \'WinResizerStartResize'],
    \['▷ crear nueva ventana vertical                               ⌘ ,v',
        \'vsplit'],
    \['▷ crear nueva ventana horizontal                             ⌘ ,h',
        \'split'],
    \['▷ cerrar ventana actual                                      ⌘ ,k',
        \'close'],
    \['▷ cerrar/abrir ventana quickfix                              ⌘ ,q',
        \'normal ,q'],
    \['▷ zoom                                                       ⌘ ,z',
        \'ZoomWinTabToggle'],
    \['▷ eliminar buffer                                            ⌘ ,B',
        \'bd'],
    \]
nnoremap <silent>[menu]b :Unite -silent menu:navegacion<CR>
" }}}

" menu busquedas dentro del buffer {{{
let g:unite_source_menu_menus.busquedas = {
    \ 'description' : '      busquedas dentro del buffer activo
        \                    ⌘ [espacio]f',
    \}
let g:unite_source_menu_menus.busquedas.command_candidates = [
    \['▷ buscar linea                                               ⌘ ,f',
        \'Unite -auto-preview -start-insert line'],
    \['▷ buscar palabra bajo el cursor                              ⌘ [espacio]w',
        \'UniteWithCursorWord -no-split -auto-preview line'],
    \['▷ buscar encabezados & etiquetas (ctags)                     ⌘ ,t',
        \'Unite -vertical -winwidth=40 -direction=topleft -toggle outline'],
    \['▷ buscar marcas',
        \'Unite -auto-preview mark'],
    \['▷ buscar pliegues',
        \'Unite -vertical -winwidth=30 -auto-highlight fold'],
    \['▷ buscar cambios',
        \'Unite change'],
    \['▷ buscar saltos',
        \'Unite jump'],
    \['▷ buscar undos',
        \'Unite undo'],
    \['▷ buscar tareas                                              ⌘ ,;',
        \'Unite -toggle grep:%::FIXME|TODO|NOTE|XXX|COMBAK|@todo'],
    \]
nnoremap <silent>[menu]f :Unite -silent menu:busquedas<CR>
" }}}

" menu yanks, registros e historia {{{
let g:unite_source_menu_menus.registros = {
    \ 'description' : '      yanks, registros e historia
        \                           ⌘ [espacio]i',
    \}
let g:unite_source_menu_menus.registros.command_candidates = [
    \['▷ yanks                                                      ⌘ ,i',
        \'Unite history/yank'],
    \['▷ comandos       (historia)                                  ⌘ q:',
        \'Unite history/command'],
    \['▷ busquedas      (historia)                                  ⌘ q/',
        \'Unite history/search'],
    \['▷ registros',
        \'Unite register'],
    \['▷ mensajes',
        \'Unite output:messages'],
    \['▷ deshacer       (gundo)                                     ⌘ ,u',
        \'GundoToggle'],
    \]
nnoremap <silent>[menu]i :Unite -silent menu:registros<CR>
" }}}

" menu edicion de ortografia {{{
let g:unite_source_menu_menus.ortografia = {
    \ 'description' : '     correcion ortografica
        \                                 ⌘ [espacio]s',
    \}
let g:unite_source_menu_menus.ortografia.command_candidates = [
    \['▷ activa corrector en español                                ⌘ ,ss',
        \'setlocal spell spelllang=es'],
    \['▷ activa corrector en ingles                                 ⌘ ,se',
        \'setlocal spell spelllang=en'],
    \['▷ desactiva corrector ortografico                            ⌘ ,so',
        \'setlocal nospell'],
    \['▷ ir a siguiente palabra y sugerir correcion                 ⌘ ,sc',
        \'normal ,sc'],
    \['▷ siguiente palabra mal escrita                              ⌘ ,sn',
        \'normal ,sn'],
    \['▷ sugerencias                                                ⌘ ,sp',
        \'normal ,sp'],
    \['▷ añadir palabra al diccionario                              ⌘ ,sa',
        \'normal ,sa'],
    \]
nnoremap <silent>[menu]s :Unite -silent menu:ortografia<CR>
" }}}

" menu edicion de texto {{{
let g:unite_source_menu_menus.texto = {
    \ 'description' : '          edicion de texto
        \                                      ⌘ [espacio]e',
    \}
let g:unite_source_menu_menus.texto.command_candidates = [
    \['▷ activa/desactiva resaltado de busqueda                     ⌘ ,eq',
        \'set invhlsearch'],
    \['▷ conmuta los numeros de linea                               ⌘ ,l',
        \'call ToggleRelativeAbsoluteNumber()'],
    \['▷ mostrar caracteres no imprimibles                          ⌘ ,eh',
        \'set list!'],
    \['▷ abrir/cerrar pliegue                                       ⌘ /',
        \'normal za'],
    \['▷ abrir todos los pliegues                                   ⌘ zR',
        \'normal zR'],
    \['▷ cerrar todos los pliegues                                  ⌘ zM',
        \'normal zM'],
    \['▷ copiar al portapapeles                                     ⌘ ,y',
        \'normal ,y'],
    \['▷ pegar desde el portapapeles                                ⌘ ,p',
        \'normal ,p'],
    \['▷ conmutar el paste mode                                     ⌘ ,P',
        \'normal ,P'],
    \['▷ eliminar espacios al final de la linea                     ⌘ ,et',
        \'normal ,et'],
    \['▷ estadisticas de texto para la posicion actual              ⌘ ,es',
        \'Unite output:normal\ ,es -no-cursor-line'],
    \['▷ frecuencia de cada palabra en el texto                     ⌘ ,ew',
        \'Unite output:WordFrequency'],
    \['▷ muestra los digrafos disponibles',
        \'digraphs'],
    \['▷ inserta texto lorem ipsum',
        \'exe "Loremipsum" input("numero de palabras: ")'],
    \['▷ muestra informacion sobre el caracter actual               ⌘ ga',
        \'normal ga'],
    \]
nnoremap <silent>[menu]e :Unite -silent -winheight=20 menu:texto <CR>
" }}}

" menu neobundle {{{
let g:unite_source_menu_menus.neobundle = {
    \ 'description' : '      gestionar plugins con neobundle
        \                       ⌘ [espacio]n',
    \}
let g:unite_source_menu_menus.neobundle.command_candidates = [
    \['▷ neobundle',
        \'Unite neobundle'],
    \['▷ neobundle log',
        \'Unite neobundle/log'],
    \['▷ neobundle lazy',
        \'Unite neobundle/lazy'],
    \['▷ neobundle update',
        \'Unite neobundle/update'],
    \['▷ neobundle search',
        \'Unite neobundle/search'],
    \['▷ neobundle install',
        \'Unite neobundle/install'],
    \['▷ neobundle check',
        \'Unite -no-empty -output:NeoBundleCheck'],
    \['▷ neobundle docs',
        \'Unite output:NeoBundleDocs'],
    \['▷ neobundle clean',
        \'NeoBundleClean'],
    \['▷ neobundle list',
        \'Unite output:NeoBundleList'],
    \['▷ neobundle direct edit',
        \'NeoBundleDirectEdit'],
    \]
nnoremap <silent>[menu]n :Unite -silent -start-insert menu:neobundle<CR>
" }}}

" menu git {{{
let g:unite_source_menu_menus.git = {
    \ 'description' : '            gestionar repositorios git
        \                            ⌘ [espacio]g',
    \}
let g:unite_source_menu_menus.git.command_candidates = [
    \['▷ tig                                                        ⌘ ,gt',
        \'normal ,gt'],
    \['▷ visor git              (gitv)                              ⌘ ,gv',
        \'normal ,gv'],
    \['▷ visor git buffer       (gitv)                              ⌘ ,gV',
        \'normal ,gV'],
    \['▷ git status             (fugitive)                          ⌘ ,gs',
        \'Gstatus'],
    \['▷ git diff               (fugitive)                          ⌘ ,gd',
        \'Gdiff'],
    \['▷ git commit             (fugitive)                          ⌘ ,gc',
        \'Gcommit'],
    \['▷ git log                (fugitive)                          ⌘ ,gl',
        \'exe "silent Glog | Unite -no-quit quickfix"'],
    \['▷ git log (todo)         (fugitive)                          ⌘ ,gL',
        \'exe "silent Glog -all | Unite -no-quit quickfix"'],
    \['▷ git blame              (fugitive)                          ⌘ ,gb',
        \'Gblame'],
    \['▷ git add/stage          (fugitive)                          ⌘ ,gw',
        \'Gwrite'],
    \['▷ git checkout           (fugitive)                          ⌘ ,go',
        \'Gread'],
    \['▷ git rm                 (fugitive)                          ⌘ ,gR',
        \'Gremove'],
    \['▷ git mv                 (fugitive)                          ⌘ ,gm',
        \'exe "Gmove " input("destino: ")'],
    \['▷ git push               (fugitive, salida por buffer)       ⌘ ,gp',
        \'Git! push'],
    \['▷ git pull               (fugitive, salida por buffer)       ⌘ ,gP',
        \'Git! pull'],
    \['▷ git command            (fugitive, salida por buffer)       ⌘ ,gi',
        \'exe "Git! " input("comando git: ")'],
    \['▷ git edit               (fugitive)                          ⌘ ,gE',
        \'exe "command Gedit " input(":Gedit ")'],
    \['▷ git grep               (fugitive)                          ⌘ ,gg',
        \'exe "silent Ggrep -i ".input("Pattern: ") | Unite -no-quit quickfix'],
    \['▷ git grep (mensajes)    (fugitive)                          ⌘ ,ggm',
        \'exe "silent Glog --grep=".input("Pattern: ")." | Unite -no-quit quickfix"'],
    \['▷ git grep (texto)       (fugitive)                          ⌘ ,ggt',
        \'exe "silent Glog -S".input("Pattern: ")." | Unite -no-quit quickfix"'],
    \['▷ git init                                                   ⌘ ,gn',
        \'Unite output:echo\ system("git\ init")'],
    \['▷ git cd                 (fugitive)',
        \'Gcd'],
    \['▷ git lcd                (fugitive)',
        \'Glcd'],
    \['▷ git browse             (fugitive)                          ⌘ ,gB',
        \'Gbrowse'],
    \]
nnoremap <silent>[menu]g :Unite -silent -winheight=26 -start-insert menu:git<CR>
" }}}

" menu code {{{
let g:unite_source_menu_menus.code = {
    \ 'description' : '           herramientas para escribir codigo
        \                     ⌘ [espacio]p',
    \}
let g:unite_source_menu_menus.code.command_candidates = [
    \['▷ ejecutar codigo python                     (pymode)        ⌘ ,r',
        \'Pyrun'],
    \['▷ mostrar docs python para palabra actual    (pymode)        ⌘ K',
        \'normal K'],
    \['▷ insertar un breakpoint                     (pymode)        ⌘ ,B',
        \'normal ,B'],
    \['▷ conmuta la revision con pylint             (pymode)',
        \'PyLintToggle'],
    \['▷ ejecutar con python2 en panel tmux         (vimux)         ⌘ ,rr',
        \'normal ,rr'],
    \['▷ ejecutar con python3 en panel tmux         (vimux)         ⌘ ,r3',
        \'normal ,r3'],
    \['▷ ejecutar con python2 y time en panel tmux  (vimux)         ⌘ ,rt',
        \'normal ,rt'],
    \['▷ ejecutar con pypy y time en panel tmux     (vimux)         ⌘ ,rp',
        \'normal ,rp'],
    \['▷ ejecutar comando en panel tmux             (vimux)         ⌘ ,rc',
        \'VimuxPromptCommand'],
    \['▷ repetir ultimo comando                     (vimux)         ⌘ ,rl',
        \'VimuxRunLastCommand'],
    \['▷ interrumpir ejecucion del panel tmux       (vimux)         ⌘ ,rs',
        \'VimuxInterruptRunner'],
    \['▷ inspeccionar panel tmux                    (vimux)         ⌘ ,ri',
        \'VimuxInspectRunner'],
    \['▷ cerrar panel tmux                          (vimux)         ⌘ ,rq',
        \'VimuxCloseRunner'],
    \['▷ autocompletado con rope                    (rope)          ⌘ C-[espacio]',
        \'RopeCodeAssist'],
    \['▷ ir a la definición                         (rope)          ⌘ C-C g',
        \'RopeGotoDefinition'],
    \['▷ reorganizar imports                        (rope)          ⌘ C-C r o',
        \'RopeOrganizeImports'],
    \['▷ refactorizar - renombrar                   (rope)          ⌘ C-C r r',
        \'RopeRename'],
    \['▷ refactorizar - extraer variable            (rope)          ⌘ C-C r l',
        \'RopeExtractVariable'],
    \['▷ refactorizar - extraer metodo              (rope)          ⌘ C-C r m',
        \'RopeExtractMethod'],
    \['▷ refactorizar - inline                      (rope)          ⌘ C-C r i',
        \'RopeInline'],
    \['▷ refactorizar - mover                       (rope)          ⌘ C-C r v',
        \'RopeMove'],
    \['▷ refactorizar - reestructurar               (rope)          ⌘ C-C r x',
        \'RopeRestructure'],
    \['▷ refactorizar - usar funcion                (rope)          ⌘ C-C r u',
        \'RopeUseFunction'],
    \['▷ refactorizar - introducir factor           (rope)          ⌘ C-C r f',
        \'RopeIntroduceFactory'],
    \['▷ refactorizar - cambiar firma               (rope)          ⌘ C-C r s',
        \'RopeChangeSignature'],
    \['▷ refactorizar - renombrar modulo actual     (rope)          ⌘ C-C r 1 r',
        \'RopeRenameCurrentModule'],
    \['▷ refactorizar - mover modulo actual         (rope)          ⌘ C-C r 1 m',
        \'RopeMoveCurrentModule'],
    \['▷ refactorizar - convertir modulo en paquete (rope)          ⌘ C-C r 1 p',
        \'RopeModuleToPackage'],
    \['▷ mostrar docs palabra actual                (rope)          ⌘ C-C r a d',
        \'RopeShowDoc'],
    \['▷ syntastic check                            (syntastic)',
        \'SyntasticCheck'],
    \['▷ syntastic errors                           (syntastic)',
        \'Errors'],
    \['▷ listar virtualenvs                         (virtualenv)',
        \'Unite output:VirtualEnvList'],
    \['▷ activar virtualenv                         (virtualenv)',
        \'VirtualEnvActivate'],
    \['▷ desactivar virtualenv                      (virtualenv)',
        \'VirtualEnvDeactivate'],
    \['▷ ejecutar coverage2                         (coveragepy)',
        \'call system("coverage2 run ".bufname("%")) | Coveragepy report'],
    \['▷ ejecutar coverage3                         (coveragepy)',
        \'call system("coverage3 run ".bufname("%")) | Coveragepy report'],
    \['▷ mostrar/ocultar informe de coverage        (coveragepy)',
        \'Coveragepy session'],
    \['▷ muestra/oculta marcas de coverage          (coveragepy)',
        \'Coveragepy show'],
    \['▷ contar lineas de codigo',
        \'Unite -default-action= output:call\\ LinesOfCode()'],
    \['▷ conmutar lineas de indentado                               ⌘ ,L',
        \'IndentLinesToggle'],
    \]
nnoremap <silent>[menu]p :Unite -silent -winheight=42 menu:code<CR>
" }}}

" menu markdown {{{
let g:unite_source_menu_menus.markdown = {
    \ 'description' : '       previsualizar documentos markdown extra
        \               ⌘ [espacio]k',
    \}
let g:unite_source_menu_menus.markdown.command_candidates = [
    \['▷ previsualizar',
        \'Me'],
    \['▷ refrescar',
        \'Mer'],
    \]
nnoremap <silent>[menu]k :Unite -silent menu:markdown<CR>
" }}}

" menu sesiones {{{
let g:unite_source_menu_menus.sesiones = {
    \ 'description' : '       sesiones
        \                                              ⌘ [espacio]h',
    \}
let g:unite_source_menu_menus.sesiones.command_candidates = [
    \['▷ cargar sesion',
        \'Unite session'],
    \['▷ guardar sesion (default)',
        \'UniteSessionSave'],
    \['▷ guardar sesion (personalizada)',
        \'exe "UniteSessionSave " input("nombre: ")'],
    \]
nnoremap <silent>[menu]h :Unite -silent menu:sesiones<CR>
" }}}

" menu marcadores {{{
let g:unite_source_menu_menus.marcadores = {
    \ 'description' : '     marcadores
        \                                            ⌘ [espacio]m',
    \}
let g:unite_source_menu_menus.marcadores.command_candidates = [
    \['▷ abrir marcadores',
        \'Unite bookmark:*'],
    \['▷ añadir marcador',
        \'UniteBookmarkAdd'],
    \]
nnoremap <silent>[menu]m :Unite -silent menu:marcadores<CR>
" }}}

" menu colorv {{{
function! GetColorFormat()
    let formats = {'r' : 'RGB',
                  \'n' : 'NAME',
                  \'s' : 'HEX',
                  \'ar': 'RGBA',
                  \'pr': 'RGBP',
                  \'pa': 'RGBAP',
                  \'m' : 'CMYK',
                  \'l' : 'HSL',
                  \'la' : 'HSLA',
                  \'h' : 'HSV',
                  \}
    let formats_menu = ["\n"]
    for [k, v] in items(formats)
        call add(formats_menu, "  ".k."\t".v."\n")
    endfor
    let fsel = get(formats, input('Elije un formato: '.join(formats_menu).'? '))
    return fsel
endfunction

function! GetColorMethod()
    let methods = {
                   \'h' : 'Hue',
                   \'s' : 'Saturation',
                   \'v' : 'Value',
                   \'m' : 'Monochromatic',
                   \'a' : 'Analogous',
                   \'3' : 'Triadic',
                   \'4' : 'Tetradic',
                   \'n' : 'Neutral',
                   \'c' : 'Clash',
                   \'q' : 'Square',
                   \'5' : 'Five-Tone',
                   \'6' : 'Six-Tone',
                   \'2' : 'Complementary',
                   \'p' : 'Split-Complementary',
                   \'l' : 'Luma',
                   \'g' : 'Turn-To',
                   \}
    let methods_menu = ["\n"]
    for [k, v] in items(methods)
        call add(methods_menu, "  ".k."\t".v."\n")
    endfor
    let msel = get(methods, input('Elije un metodo: '.join(methods_menu).'? '))
    return msel
endfunction

let g:unite_source_menu_menus.colorv = {
    \ 'description' : '         gestion de color
        \                                      ⌘ [espacio]c',
    \}
let g:unite_source_menu_menus.colorv.command_candidates = [
    \['▷ abrir colorv                                               ⌘ ,cv',
        \'ColorV'],
    \['▷ abrir colorv con el color bajo el cursor seleccionado      ⌘ ,cw',
        \'ColorVView'],
    \['▷ previsualiza colores en el buffer actual                   ⌘ ,cpp',
        \'ColorVPreview'],
    \['▷ selector de color                                          ⌘ ,cd',
        \'ColorVPicker'],
    \['▷ edita el color bajo el cursor                              ⌘ ,ce',
        \'ColorVEdit'],
    \['▷ edita el color bajo el cursor (y todas las coincidencias)  ⌘ ,cE',
        \'ColorVEditAll'],
    \['▷ inserta un color                                           ⌘ ,cii',
        \'exe "ColorVInsert " .GetColorFormat()'],
    \['▷ lista de colores relativa al color actual                  ⌘ ,cgh',
        \'exe "ColorVList " .GetColorMethod() "
        \ ".input("numero de colores? (opcional): ")
        \ " ".input("numero de pasos?  (opcional): ")'],
    \['▷ muestra lista de colores (colores Web W3C)                 ⌘ ,cn',
        \'ColorVName'],
    \['▷ elegir esquema de color (ColourLovers, Kuler)              ⌘ ,css',
        \'ColorVScheme'],
    \['▷ muestra esquemas de color favoritos                        ⌘ ,csf',
        \'ColorVSchemeFav'],
    \['▷ crear esquema de color                                     ⌘ ,csn',
        \'ColorVSchemeNew'],
    \['▷ crear variacion de tono entre dos colores ',
        \'exe "ColorVTurn2 " " ".input("Color 1 (hex): ")
        \" ".input("Color 2 (hex): ")'],
    \]
nnoremap <silent>[menu]c :Unite -silent menu:colorv<CR>
" }}}

" menu vim {{{
let g:unite_source_menu_menus.vim = {
    \ 'description' : '            vim
        \                                                   ⌘ [espacio]v',
    \}
let g:unite_source_menu_menus.vim.command_candidates = [
    \['▷ escoger esquema de color',
        \'Unite colorscheme -auto-preview'],
    \['▷ atajos de teclado',
        \'Unite mapping -start-insert'],
    \['▷ editar archivo de configuracion (vimrc)',
        \'edit $MYVIMRC'],
    \['▷ establecer el tipo de archivo',
        \'Unite -start-insert filetype'],
    \['▷ ayuda de vim',
        \'Unite -start-insert help'],
    \['▷ comandos de vim',
        \'Unite -start-insert command'],
    \['▷ funciones de vim',
        \'Unite -start-insert function'],
    \['▷ runtimepath de vim',
        \'Unite -start-insert runtimepath'],
    \['▷ salida de comando de vim',
        \'Unite output'],
    \['▷ fuentes de unite',
        \'Unite source'],
    \['▷ matar procesos',
        \'Unite -default-action=sigkill -start-insert process'],
    \['▷ lanzar ejecutable (dmenu like)',
        \'Unite -start-insert launcher'],
    \['▷ limpiar cache de Powerline',
        \'PowerlineClearCache'],
    \]
nnoremap <silent>[menu]v :Unite menu:vim -silent -start-insert<CR>
" }}}
" }}}

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#source('file_mru,file_rec,file_rec/async,grep,locate',
            \ 'ignore_pattern', join(['\.git/', 'tmp/', 'bundle/'], '\|'))

let g:unite_source_history_yank_enable = 1
let g:unite_enable_start_insert = 0
let g:unite_enable_short_source_mes = 0
let g:unite_force_overwrite_statusline = 0
let g:unite_prompt = '>>> '
let g:unite_marked_icon = '✓'
" let g:unite_candidate_icon = '∘'
let g:unite_winheight = 15
let g:unite_update_time = 200
let g:unite_split_rule = 'botright'
let g:unite_data_directory = $HOME.'/.vim/tmp/unite'
let g:unite_source_buffer_time_format = '(%d-%m-%Y %H:%M:%S) '
let g:unite_source_file_mru_time_format = '(%d-%m-%Y %H:%M:%S) '
let g:unite_source_directory_mru_time_format = '(%d-%m-%Y %H:%M:%S) '

if executable('ag')
    let g:unite_source_grep_command='ag'
    let g:unite_source_grep_default_opts='--nocolor --nogroup -a -S'
    let g:unite_source_grep_recursive_opt=''
    let g:unite_source_grep_search_word_highlight = 1
elseif executable('ack')
    let g:unite_source_grep_command='ack'
    let g:unite_source_grep_default_opts='--no-group --no-color'
    let g:unite_source_grep_recursive_opt=''
    let g:unite_source_grep_search_word_highlight = 1
endif

let g:junkfile#directory=expand($HOME."/.vim/tmp/junk")

" navegacion por la ventana de unite mas comoda
" autocmd FileType unite call s:unite_settings()
" function! s:unite_settings()
"   nmap <buffer> <Esc><Esc> <Plug>(unite_exit)
"   imap <buffer> <Esc><Esc> <Plug>(unite_exit)
"   imap <buffer> <Esc> <Plug>(unite_insert_leave)
" endfunction

" creacion del directorio para guardar sesiones si no existe
silent! call MakeDirIfNoExists(expand(unite_data_directory)."/session/")

" }}}

" Utl {{{

map <Leader>j :Utl <CR><Bar>:redraw!<CR>

let g:utl_cfg_hdl_scm_http_system = "silent !firefox %u &"
let g:utl_cfg_hdl_mt_application_pdf = 'silent :!zathura %p &'
let g:utl_cfg_hdl_mt_image_jpeg = 'silent :!sxiv %p &'
let g:utl_cfg_hdl_mt_image_gif = 'silent :!sxiv %p &'
let g:utl_cfg_hdl_mt_image_png = 'silent :!sxiv %p &'

" }}}

" VimFiler {{{

nnoremap <silent><Leader>X :VimFiler<CR>

let g:vimfiler_as_default_explorer = 1

let g:vimfiler_tree_leaf_icon = '├'
let g:vimfiler_tree_opened_icon = '┐'
let g:vimfiler_tree_closed_icon = '─'
let g:vimfiler_file_icon = '┄'
let g:vimfiler_marked_file_icon = '✓'
let g:vimfiler_readonly_file_icon = '✗'

let g:vimfiler_force_overwrite_statusline = 0

let g:vimfiler_time_format = '%d-%m-%Y %H:%M:%S'
let g:vimfiler_data_directory = $HOME.'/.vim/tmp/vimfiler'

" }}}

" Vim-markdown-extra-preview {{{

" map <LocalLeader>mp :Me<CR>
" map <LocalLeader>mr :Mer<CR>

" let g:VMEPextensions = ['extra', 'codehilite']
" let g:VMEPhtmlreader= '/usr/bin/chromium-browser'

" }}}

" vimux {{{

let g:VimuxUseNearestPane = 1

" ejecutar el buffer actual con el interprete de python
map <Leader>rr :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;python2 '.bufname("%"))<CR>
map <Leader>r3 :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;python3 '.bufname("%"))<CR>
map <Leader>rt :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;time python2 '.bufname("%"))<CR>
map <Leader>rp :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;time pypy '.bufname("%"))<CR>

map <Leader>rc :VimuxPromptCommand<CR>
map <Leader>rl :VimuxRunLastCommand<CR>
map <Leader>rs :VimuxInterruptRunner<CR>
map <Leader>ri :VimuxInspectRunner<CR>
map <Leader>rq :VimuxCloseRunner<CR>

" }}}

" Virtualenv {{{

let g:virtualenv_auto_activate = 1
let g:virtualenv_stl_format = '(%n)'

" }}}

" winresizer {{{

let g:winresizer_start_key = '<C-C><C-W>'
" cancelar pulsando ESC
" let g:winresizer_finish_with_escape = 1
let g:winresizer_keycode_finish = 27

" }}}

" zoomwintab {{{

map <Leader>z :ZoomWinTabToggle<CR>

" }}}

" FIN CONFIGURACION PLUGINS }}}

" TIPOS DE ARCHIVO  {{{ ======================================================

" DJANGO HTML (Templates) {{{

au BufRead,BufNewFile */templates/*.html setlocal filetype=htmldjango.html

" }}}

" JSON {{{ -------------------------------------------------------------------

" autocmd BufNewFile,BufRead *.json set ft=javascript
autocmd BufNewFile,BufRead *.json set ft=json

augroup json_autocmd
  autocmd!
  autocmd FileType json set autoindent
  autocmd FileType json set formatoptions=tcq2l
  autocmd FileType json set textwidth=78 shiftwidth=2
  autocmd FileType json set softtabstop=2 tabstop=8
  autocmd FileType json set expandtab
  autocmd FileType json set foldmethod=syntax
augroup END

" }}}

" LUA {{{

au BufRead,BufNewFile rc.lua setlocal foldmethod=marker

" }}}

" MARKDOWN {{{

" markdown filetype file
au BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn} set filetype=markdown
autocmd FileType markdown NeoBundleSource vim-markdown
autocmd FileType markdown NeoBundleSource vim-markdown-extra-preview

" }}}

" FIN TIPOS DE ARCHIVO }}}

" vim:foldmethod=marker
