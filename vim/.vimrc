"
" Author: Pablo Bernardo
" Twitter: @voylinux
" Blog: http://elkarmadelteclado.com
"

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

" VIM MDN {{{

NeoBundle 'voylinux/vim-mdn'

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

NeoBundle 'daylerees/colour-schemes', { "rtp": "vim-themes/"}

NeoBundle 'tomasr/molokai'

NeoBundle 'sjl/badwolf'

NeoBundle 'nielsmadan/harlequin'

NeoBundle 'joedicastro/vim-github256'

NeoBundle 'noahfrederick/vim-hemisu'

" }}}

" DCVS {{{
"
NeoBundle 'vim-scripts/vcscommand.vim'
"
" Para gestionar Git
NeoBundle 'tpope/vim-fugitive'
" Para señalar los cambios del archivo con respecto al repo Git
NeoBundle 'airblade/vim-gitgutter'

" Mercurial
NeoBundle 'vim-scripts/hgrev'

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
" NeoBundleLazy 'vim-scripts/hexman.vim', { 'autoload' :
            " \ { 'mappings' : [['ni', '<Plug>HexManager']]}}

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

let g:UltiSnipsSnippetDirectories=["UltiSnips", "ultisnips_my", "vim-snippets"]

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

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
NeoBundleLazy 'mattn/emmet-vim', {'autoload':
            \ {'filetypes': ['html', 'xhttml', 'css']}}
NeoBundleLazy 'joedicastro/vim-sparkup', {'autoload':
            \ {'filetypes': ['html', 'xhttml', 'css']}}

" }}}

" JS {{{

"NeoBundleLazy 'pangloss/vim-javascript', {'autoload':
"            \ {'filetypes': ['javascript']}}
NeoBundleLazy 'jelera/vim-javascript-syntax', {'autoload':
            \ {'filetypes': ['javascript']}}
NeoBundleLazy 'othree/javascript-libraries-syntax.vim', {'autoload':
            \ {'filetypes': ['javascript']}}
NeoBundleLazy 'wookiehangover/jshint.vim', {'autoload':
            \ {'filetypes': ['javascript']}}
NeoBundleLazy 'heavenshell/vim-jsdoc', {'autoload':
            \ {'filetypes': ['javascript']}}



" }}}

" GUI {{{

" Barra de estado mas elegante y efectiva
"NeoBundle 'joedicastro/vim-powerline'

" Vim airline
NeoBundle 'bling/vim-airline'

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

" NERDTree {{{

NeoBundle 'scrooloose/nerdtree'



" }}}

" Autocompletado Tern for VIM {{{


NeoBundle 'marijnh/tern_for_vim'

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
    let g:Powerline_symbols = 'unicode'
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

" Mantener el cursor centrado vericalmente {{{

set scrolloff=999
noremap <Leader>zz :let &scrolloff=999-&scrolloff<CR>

" }}}

" NERDTree  {{{

noremap <Leader>n :NERDTreeToggle<CR>


" }}}

" Usar kj como ESC : Salir a normal mode {{{
:inoremap kj <Esc>

" }}}

" Navegar por tabs {{{



nnoremap <C-Insert> :tabnew<CR>
nnoremap <Leader>t  :tabnew<CR>


nmap <silent> <C-k> :tabn<CR>
nmap <silent> <C-j> :tabp<CR>
imap <silent> <C-k> <esc><C-n>
imap <silent> <C-j> <esc><C-p>

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

" Mercurial (Vim-mercenary) {{{ ---------------------------------

" Manage Mercurial reposiries from VIM
NeoBundle 'phleet/vim-mercenary'


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

let g:po_translator = "Pablo Bernardo <voylinux@gmail.com>"

" }}}

" Vim airline {{{ -----------------------------------------------------------

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'


" Customization
"===============

" let g:airline_left_sep='>'
" let g:airline_right_sep='<'
let g:airline_detect_modified=1
let g:airline_detect_paste=1
let g:airline_detect_iminsert=0
let g:airline#extensions#branch#use_vcscommand = 1


let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"
let g:airline_theme='powerlineish'

" }}}

" PythonMode {{{ -------------------------------------------------------------

let g:pymode_breakpoint_key = '<Leader>B'

let g:pymode_lint_checker = 'pylint,pep8,mccabe,pep257'
let g:pymode_lint_ignore = ''
let g:pymode_lint_config = $HOME.'/dotfiles/pylint/pylint.rc'

let g:pymode_rope = 1


let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 1
"let g:pymode_rope_goto_def_newwin = 'new'
"let g:pymode_rope_guess_project = 0
"let g:pymode_rope_vim_completion = 1
"let g:pymode_rope_always_show_complete_menu = 1

" }}}

" Syntastic {{{

let g:syntastic_python_pylint_exe = "pylint2"

let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_style_error_symbol  = '⚡'
let g:syntastic_style_warning_symbol  = '⚡'

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

" NERDTree {{{

let NERDTreeShowHidden=1


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

" FUNCIONES PROPIAS {{{ ==============================================================

nnoremap s :exec "normal i".nr2char(getchar())."\e"<CR>"

" }}}

" vim:foldmethod=marker
