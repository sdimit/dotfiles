" bundles
set nocompatible
filetype off

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" file management
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'istib/vifm.vim'
NeoBundle 'vim-scripts/LargeFile'
" NeoBundle 'kana/vim-altr'

NeoBundle 'Shougo/vimproc.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'osyo-manga/unite-fold'
NeoBundle 'Shougo/unite-help'
NeoBundle 'Shougo/unite-session'
NeoBundle 'tacroe/unite-mark'
NeoBundle 'osyo-manga/unite-filetype'
NeoBundle 'osyo-manga/unite-quickfix'
NeoBundle 'thinca/vim-unite-history'
" NeoBundle 'hrsh7th/vim-versions'
" NeoBundle 'Shougo/unite-ssh'
" NeoBundle 'osyo-manga/unite-env'
NeoBundle 'Shougo/vimshell.vim'


" tag support
" NeoBundle 'majutsushi/tagbar'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'abudden/TagHighlight'

" git tools
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'gregsexton/gitv'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'vim-scripts/DirDiff.vim'
NeoBundle 'kmnk/vim-unite-giti'
NeoBundle 'moznion/unite-git-conflict.vim'
" NeoBundle 'mattn/gist-vim'
" gist

" snippets and completion
NeoBundle 'SirVer/ultisnips'
NeoBundle 'Shougo/neocomplcache'
" NeoBundle 'Valloric/YouCompleteMe'

" NeoBundle 'scrooloose/syntastic'

" general helpers
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'vim-scripts/YankRing.vim'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'Indent-Guides'
NeoBundle 'godlygeek/tabular'
NeoBundle 'AndrewRadev/switch.vim'
NeoBundle 'danro/rename.vim'
NeoBundle 'kshenoy/vim-signature'
NeoBundle 'vim-scripts/highlight.vim'
NeoBundle 'delimitMate.vim'
" NeoBundle 'terryma/vim-smooth-scroll'
" NeoBundle 'terryma/vim-multiple-cursors'
" NeoBundle 'vim-scripts/bufkill.vim'
" NeoBundle 'dhruvasagar/vim-table-mode'

" python
" NeoBundle 'klen/python-mode'
" NeoBundle 'nvie/vim-pyunit'
" vimpdb

" coffeescript
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'zeekay/vim-js2coffee'
" LiveScript
NeoBundle 'gkz/vim-ls'

" web development
NeoBundle 'tristen/vim-sparkup'
NeoBundle 'edsono/vim-matchit'
" NeoBundle 'mattn/zencoding-vim'
" NeoBundle 'vim-scripts/closetag.vim'
" NeoBundle 'vim-scripts/autotag'
NeoBundle 'leshill/vim-json'
NeoBundle 'wavded/vim-stylus'
NeoBundle 'groenewege/vim-less'
" NeoBundle 'skammer/vim-css-color'
NeoBundle 'digitaltoad/vim-jade'
" NeoBundle 'jaxbot/brolink.vim'
" NeoBundle 'skwp/vim-html-escape'

" access dash help in osx
NeoBundle 'rizzatti/funcoo.vim'
NeoBundle 'rizzatti/dash.vim'

" helpers to handle repls and running unit tests in tmux
" NeoBundle 'ervandew/screen'
" NeoBundle 'benmills/vimux'
" NeoBundle 'tpope/vim-dispatch'
" NeoBundle 'nvie/vim_bridge'
" NeoBundle 'tpope/vim-tbone'

NeoBundle 'kana/vim-textobj-entire'
NeoBundle 'kana/vim-textobj-fold'
NeoBundle 'kana/vim-textobj-indent'
NeoBundle 'kana/vim-textobj-user'
" NeoBundle 'kana/vim-textobj-diff'
NeoBundle 'gilligan/textobj-gitgutter'

NeoBundle 'MarcWeber/vim-addon-mw-utils'
NeoBundle 'duff/vim-scratch'
NeoBundle 'vim-scripts/utl.vim'
" NeoBundle 'vim-scripts/marvim'

" Search
NeoBundle 'henrik/git-grep-vim'
NeoBundle 'bronson/vim-visual-star-search'
NeoBundle 'henrik/vim-indexed-search'
NeoBundle 'nelstrom/vim-qargs'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'rking/ag.vim'

" Appearance
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'bling/vim-airline'

NeoBundle 'tpope/vim-fireplace'
NeoBundle 'guns/vim-clojure-static'
NeoBundle 'kien/rainbow_parentheses.vim'

" extras
" NeoBundle 'jtratner/vim-flavored-markdown'
" NeoBundle 'jceb/vim-orgmode'
" NeoBundle 'vim-scripts/vimwiki'
" NeoBundle 'lukerandall/haskellmode-vim'
" NeoBundle 'ledger/vim-ledger'
" NeoBundle 'jcf/vim-latex'


"

" GENERAL

filetype plugin on

NeoBundleCheck

let mapleader="\<space>"
let maplocalleader="\<space>"


set visualbell " no sounds
set nowarn
set showcmd

set shortmess=ITstOa
" I : Don’t show the intro message when starting vim
" T : truncate other messages in the middle if they are too long to fit on the command line, '...' will appear in the middle.
" s : no 'search hit BOTTOM, continuing at TOP' or search hit TOP, continuing at BOTTOM' messages
" t : truncate file message at start if it is too long to fit on command-line, '<' will appear in the left most column.  Ignored in Ex mode.
" O : message for reading a file overwrites previous message. Also for quickfix message (eg, ':cn').
" a : all of the following abbreviations
"    f = use '(3 of 5)' not '(file 3 of 5)'
"    i = use '[noeol]' not '[Incomplete last line]'
"    l = use '999L, 888C' not '999 lines, ' etc.
"    m = use '[+]' not '[Modified]'
"    n = use '[New]' not '[New File]'
"    r = use '[RO]' not '[readonly]'
"    w = use '[w]' not 'written' for file write msg
"    a = nd '[a]' not 'appended' for ':w >> file' cmd
"    x = use '[dos]' not '[dos format]', '[unix]' not '[unix format]' & '[mac]' not '[mac format]'

set cpoptions=aABceFsmq
" q = When joining lines, leave the cursor between joined lines
" m = When a new match is created (showmatch) pause for .5
" s = Set buffer options when entering the buffer
" F = :write command updates current file name
" e = Automatically add <CR> to the last line when using :@r
" c = Searching continues at the end of the match at the cursor position
" B = A backslash has no special meaning in mappings
" A = :write updates alternative file name
" a = :read updates alternative file name

" Run an external program through silent without messing up the screen in CLI
command! -nargs=1 Silent
      \ | execute ':silent !'.<q-args>
      \ | execute ':redraw!'

"

" EDITING

" use utf8
set encoding=utf8

set nonumber
let g:enable_numbers = 0

nmap = :+
nmap - :-

set nostartofline       " cursor can stay on blank characters
set scrolloff=10         "Start scrolling when we're 10 lines away from margins
" set sidescrolloff=15
" set sidescroll=1

set backspace=indent,eol,start " Allow backspace in insert mode

" to make cursor pass line borders
set whichwrap=b,s,<,>,[,],l,h

" window size (will only make it bigger)
if &columns < 85
  set columns=85
endif

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif

map <silent> <leader>n :set number!<CR>
map <silent> <leader>N :NumbersToggle<CR>

" nmap <silent> <c-p> <plug>yankstack_substitute_older_paste
" nmap <silent> <c-n> <plug>yankstack_substitute_newer_paste
" let g:yankstack_map_keys = 0
" let g:yankstack_did_setup = 1
" call yankstack#setup() " call this explicitly so conflicting mappings can be defined in this file
let g:yankring_history_dir = '$HOME'
let g:yankring_history_file = '.yankring-history'
let g:yankring_max_history = 300

" GUNDO
" open on the right so as not to compete with the nerdtree
let g:gundo_right = 1

" a little wider for wider screens
let g:gundo_width = 60

" YANKING AND PASTING

" paste with preceding space
" nmap <leader>p a<space><c-r>"<esc>

" paste in alternate file
vnoremap ap y<c-^>P<c-^>

" select pasted text
nnoremap <expr> gV "`[".getregtype(v:register)[0]."`]"

" system clipboard
map <leader>y "*y
nnoremap <leader><leader>p :set paste<CR>:put  *<CR>:set nopaste<CR>

nnoremap Y y$

" Duplicate a selection
vnoremap V y'>p

" Press Shift+P while in visual mode to replace the overwriting without
" overwriting the default register
vmap P p :call setreg('"', getreg('0')) <CR>

" visually select the last pasted text
" nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

nmap YY vHLy

" RSI bindings
imap <c-d> #
imap <c-h> =>
map £ #

" Fix for navigating long lines
" Wrapped lines goes down/up to next row, rather than next line in file.
nnoremap j gj
nnoremap k gk

" saner ESC key (and stay at position instead of going back to previous caret - strange vim default behaviour)
inoremap jj <esc>l
inoremap jk <esc>l
cnoremap jj <c-c>

" avoid shift key for invoking command line
" nnoremap ; :
" but preserve g; shortcut (remember: opposite is g,)
nnoremap g: g;

" Easier within-line navigation
nnoremap H ^
vnoremap H ^
nnoremap L $
vnoremap L g_

" so we can use $ to duplicate % (just because i keep confusing them)
noremap $ %

" or using traditional terminal mapping of swapping two letters
inoremap <c-t> <esc>hxpa

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" delete last character of line
nnoremap d. $x0

" noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
" noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>


" SWITCH plugin

nmap - :Switch<cr>

source ~/dotfiles/vim/switch.vim

" Keep the cursor in place while joining lines
nnoremap <silent> J mzJ`z:delmarks z<cr>

" Split line (sister to [J]oin lines)
" use c-s, because [S] is used by vim-seek backward search..
nnoremap <c-s> i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w:silent! delmark w<cr>

vnoremap - =

" Toggle 'keep current line in the center of the screen' mode
nnoremap <leader>C :let &scrolloff=999-&scrolloff<cr>

map <leader>cl :set cursorline!<cr>
map <leader>cc :set cursorcolumn!<cr>


" fix slight delay after pressing ESC then O
" http://ksjoberg.com/vim-esckeys.html
" set noesckeys
set timeout timeoutlen=1000 ttimeoutlen=100

"

" FILES

set noswapfile                 " turn off swap files
set nobackup
set nowb

set autoread                   " Reload files changed outside vim

" Save when buffers lose focus
au BufLeave * silent! :up
au InsertLeave * silent! :up

" nnoremap <leader>M :set modifiable<cr>
" nnoremap <leader>M :make<cr>

" shift key fixes
command! -bang -nargs=* -complete=file E e<bang> <args>
command! -bang -nargs=* -complete=file W w<bang> <args>
command! -bang -nargs=* -complete=file Wq wq<bang> <args>
command! -bang -nargs=* -complete=file WQ wq<bang> <args>
command! -bang Wa wa<bang>
command! -bang WA wa<bang>
command! -bang Q q<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>

" Kill buffer
nnoremap <leader>q :bd!<cr>
" Kill buffer and delete file
nnoremap <leader>RM :!rm %<cr>:bd!<cr>
" Quit vim
nnoremap <leader>Q :qa!<cr>

" Filesystem
nmap <leader>mk :!mkdir -p <c-r>=expand("%:p:h")."/"<cr>
" Make the current file executable
nmap <leader>x :Silent chmod +x %<cr>

" Rename (using plugin)
nmap <leader>rn :Rename <c-r>=expand("%:t")<cr>

" Change directory to the path of the current file
map <leader>cd :cd %:p:h<cr>

" copy current filename into system clipboard - mnemonic: (c)urrent(f)ilename
nnoremap <silent> <leader>cf :let @* = expand("%:~")<CR>
nnoremap <silent> <leader>cn :let @* = expand("%:t")<CR>

" expand current directory into command line
cnoremap %% <C-R>=expand('%:h').'/'<cr>


" Opens an edit command with the path of the currently edited file filled in
" nnoremap <leader>E :e <C-R>=expand("%:p:h") . "/" <CR>

" Use vifm as vim file manager
nnoremap <leader>E :EditVifm<cr>
nnoremap <leader>v :VsplitVifm<cr>
" nnoremap <leader>F :DiffVifm<cr>
nnoremap <leader>T :TabVifm<cr>

" shortcut to swap to alternate file
nnoremap :l <c-^>

set suffixesadd+=.py
set suffixesadd+=.js
set suffixesadd+=.coffee

" Use Q to intelligently close a window

" (if there are multiple windows into the same buffer)
" or kill the buffer entirely if it's the last window looking into that buffer
function! CloseWindowOrKillBuffer()
  let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))

  if number_of_windows_to_this_buffer > 1
    wincmd c
  else
    bdelete
  endif
endfunction

nnoremap <silent> Q :call CloseWindowOrKillBuffer()<CR>

autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /

" Open URL
command -bar -nargs=1 OpenURL :!open <args>
function! OpenURL()
  let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;:]*')
  echo s:uri
  if s:uri != ""
    exec "!open \"" . s:uri . "\""
  else
    echo "No URI found in line."
  endif
endfunction
nnoremap gu :call OpenURL()<CR>

" don't do all the usual background vim buffer stuff when loading large files
let g:LargeFile=5

" show alternate file
nmap <c-n><c-n> <Plug>(altr-forward)

" sample definition for jade and stylus templates
" call altr#define('%.jade', '../styles/%.styl')
" call altr#define('%.styl', '../templates/%.jade')



"

" FOLDING

"   zf{motion} / {Visual}zf      create a fold
"
"   :{range}fo[ld]               create a fold over range
"   zd                           delete fold at cursor
"   zE                           delete ("e"liminate) all folds
"
" opening/closing folds:
"   zo                           open fold at cursor
"   zc                           close fold at cursor
"   za                           toggle ("a"lternate) fold at cursor
"
"   zm                           increase foldlevel by one ("m"ore folding)
"   zr                           reduce foldlevel by one ("r"educe folding)
"
"   zM                           close all folds ("m"ost folding)
"   zR                           open all folds ("r"educe every fold)
"
"   zx                           reset folds but keep cursor line unfolded
"   zX                           reset folds
"
" moving over folds:
"   [z                           move to start of currently open fold
"   ]z                           move to end of currently open fold

set foldmethod=marker
set foldnestmax=3       "deepest fold is 3 levels
set foldenable
" set foldlevel=99

function! Foldingstyle()
  let line = getline(v:foldstart)
  let nucolwidth = &fdc + &number * &numberwidth
  let windowwidth = winwidth(0) - nucolwidth - 3
  let foldlinecount = v:foldend - v:foldstart
  " expand tabs into spaces
  let onetab = strpart('          ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')
  let line = strpart(line, 0, windowwidth - 2 -len(foldlinecount))
  let fillcharcount = windowwidth - len(line) - len(foldlinecount) - 4
  let fillchars = repeat(" ",fillcharcount)
  return line . '…' . fillchars . '…' . foldlinecount
endfunction
set foldtext=Foldingstyle()


nmap <Leader>Fm :set foldmethod=manual<cr>zM
nmap <Leader>Fi :set foldmethod=indent<cr>zM
nmap <Leader>Fk :set foldmethod=marker<cr>zM
nmap <Leader>Fe :set foldmethod=expr<cr>zM
nmap <Leader>Fs :set foldmethod=syntax<cr>zM

nmap <leader>F0 :set foldmethod=indent<cr>:set foldlevel=0<CR>
nmap <leader>F1 :set foldmethod=indent<cr>:set foldlevel=1<CR>
nmap <leader>F2 :set foldmethod=indent<cr>:set foldlevel=2<CR>
nmap <leader>F3 :set foldmethod=indent<cr>:set foldlevel=3<CR>
nmap <leader>F4 :set foldmethod=indent<cr>:set foldlevel=4<CR>

" fold HTML tag
nnoremap zt :set foldmethod=manual<cr>Vatzf

" select all fold
nnoremap vaf vaz

" 'Refocus' folds

" nnoremap <space> za
nnoremap <silent> zm zM<cr>

" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
" files.
function! AppendModeline()
  let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
        \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

"

" FORMAT

set nowrap
set linebreak                  " Wrap lines at convenient points

set formatoptions+=j            "fo:    remove comment leader when joining lines

" toggle line wrapping
nnoremap <leader>W :set wrap!<cr>


" use external command for formatting paragraphs
" set formatprg=par\ -w80\ T2\ \|\ sed\ 's/\ \ /\\t/g'
" set formatprg="iconv -f UTF-8 -t WINDOWS-1251 |"
"            \."par -w80 |"
"            \."iconv -f WINDOWS-1251 -t UTF-8"


" format paragraph: gq
nmap <leader>FP vip:!par 100<cr>
" format document
nmap <leader>FT ggVG:!par 100<cr>''


" Alignment

" :[range]center WIDTH
" :[range]left/right WIDTH

" create a left margin
function! LeftMargin()
  hi! link FoldColumn Normal
  silent set foldcolumn=12
  silent set wrapmargin=20
  silent set nonumber
  " :ggVGgq<cr>
endfunction

function! NoLeftMargin()
  hi! link FoldColumn Normal
  silent set foldcolumn=0
  silent set wrapmargin=0
  silent set number
endfunction

"  Clean code function
function! CleanCode()
  %retab          " Replace tabs with spaces
  %s/\r/\r/eg     " Turn DOS returns ^M into real returns
  %s=  *$==e      " Delete end of line blanks
endfunction
nmap <leader>co mz:call CleanCode()<cr>`z

" add space inside current parenthesis
map <leader>( vi(xi  <esc>P <esc>

"

" INDENT


filetype indent on
" set autoindent                 " copy indent from current when starting a new line
set nosmartindent
set smarttab
set expandtab                  " convert tabs into spaces

set shiftwidth=2
set softtabstop=2
set tabstop=2

" temporarily change tab size
nnoremap <leader><leader>2 :setlocal ts=2 sts=2 sw=2 et<cr>
nnoremap <leader><leader>4 :setlocal ts=4 sts=4 sw=4 et<cr>

" ALIGN/TABULARISE

vmap <leader>a=       :Tabularize /=<cr>
vmap <leader>a"       :Tabularize /"<cr>
vmap <leader>a:       :Tabularize /:<cr>
vmap <leader>a::      :Tabularize /:\zs<cr>
vmap <leader>a,       :Tabularize /,<cr>
vmap <leader>a-       :Tabularize /-<cr>
vmap <leader>a{       :Tabularize /{<cr>
vmap <leader>a#       :Tabularize /#<cr>
vmap <leader>a£       :Tabularize /#<cr>

"

" LOOK

set laststatus=2               " show the statusline in all windows
set noruler
set showmode                   " Don't show current mode down the bottom
set gcr=a:blinkon0             " Disable cursor blink
set nocursorline
set linespace=0             " don't insert extra pixel lines betweens rows
set lazyredraw              " do not redraw while running macros
set ttyfast                 " fast redraw screen


set t_Co=256
colorscheme solarized
let g:solarized_termtrans = 1
let g:solarized_contrast = 'high'

let g:airline_enable_fugitive=1
let g:airline_enable_syntastic=1
" themes: dark, light, simple, badwolf, solarized, solarized2
let g:airline_theme='dark'
let g:airline_left_sep = 'badwolf'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_linecolumn_prefix = '␊ '
let g:airline_linecolumn_prefix = '␤ '
let g:airline_linecolumn_prefix = '¶ '
let g:airline_fugitive_prefix = '⎇ '
let g:airline_paste_symbol = 'ρ'
let g:airline_paste_symbol = 'þ'
let g:airline_paste_symbol = '∥'
let g:airline_section_a = ''
let g:airline_section_x = ''
let g:airline_section_y = ''
" let g:airline_section_y = '%{getcwd()}'
let g:airline_section_z = ''
let g:airline_powerline_fonts=1

" colorscheme jellybeans

" Auto-detect solarized setting from the SOLARIZED env var
if $SOLARIZED == 'light'
  set background=light
else
  set background=dark
endif

nmap <leader>cs :colorscheme<space>

" " show file path in title bar
" set title
" set titlestring=%F

" ,hi           - show current Highlight group. if you don't like the color of something, use this, then use hi! link [groupname] [anothergroupname] in your vimrc.after to remap the color. You can see available colors using :hi

" Character Display
"==================

nnoremap <leader>$ :set list!<cr>
set listchars=tab:,.,trail:.,extends:#,nbsp:. " Highlight problematic whitespace
set list

set noshowmatch         " don't match brackets (using matchparen instd)

if has("gui_running")

  set guioptions=M

  " Disable the macvim toolbar and menu
  set guioptions-=TacelLrR
  set mousehide               " hide the mouse cursor when typing
  set lines=60
  set columns=190
  set guifont=PragmataPro:h14,

  call togglebg#map(",B") " solarized dark/light

else
  "dont load csapprox if we no gui support - silences an annoying warning
  let g:CSApprox_loaded = 1
endif

set mouse=a

" Get the current highlight group. Useful for then remapping the color
map <leader>HI :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" . " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>

" POWERLINE
" let g:Powerline_symbols='fancy'
" let g:Powerline_theme='solarized256'
" let g:Powerline_colorscheme='solarized256'

let g:airline_powerline_fonts=1

" Unfuck my screen
nnoremap <leader>u :syntax sync fromstart<cr>:redraw!<cr>

" Resize splits when the window is resized
au VimResized * :wincmd =


" This enables iterm cursor changes from vim.
if exists('$ITERM_PROFILE')
  if exists('$TMUX')
    let &t_SI = "\<Esc>[3 q"
    let &t_EI = "\<Esc>[0 q"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
end


" Dim inactive windows using 'colorcolumn' setting

function! s:DimInactiveWindows()
  for i in range(1, tabpagewinnr(tabpagenr(), '$'))
    let l:range = ""
    if i != winnr()
      if &wrap
        " HACK: when wrapping lines is enabled, we use the maximum number
        " of columns getting highlighted. This might get calculated by
        " looking for the longest visible line and using a multiple of
        " winwidth().
        let l:width=256 " max
      else
        let l:width=winwidth(i)
      endif
      let l:range = join(range(1, l:width), ',')
    endif
    call setwinvar(i, '&colorcolumn', l:range)
  endfor
endfunction

" augroup DimInactiveWindows
"   au!
"   au WinEnter * call s:DimInactiveWindows()
"   au WinEnter * set cursorline
"   au WinLeave * set nocursorline
" augroup END


"

" CONFIG

nmap <leader>ve :tabedit ~/dotfiles/vim/vimrc.vim<cr>
" automatically reread Vim's configuration after writing it
" autocmd! BufWritePost ~/dotfiles/vim/vimrc.vim source ~/dotfiles/vim/vimrc.vim

nmap <leader>ze :tabedit ~/dotfiles/zsh/zshrc<cr>
nmap <leader>Ge :tabedit ~/dotfiles/git/gitconfig<cr>
nmap <leader>te :tabedit ~/dotfiles/tmux/tmux.conf<cr>

" ,vc - (Vim Command) copies the command under your cursor and executes it in vim
vnoremap <leader>vc y:execute @@<cr>
nnoremap <leader>vc ^vg_y:execute @@<cr>

" google search with selection
vnoremap <leader>G "xy<esc>:!open "http://www.google.com/search?q=<c-r>x"<cr>
" google feeling lucky
vnoremap <leader>L "xy<esc>:!open "http://www.google.com/search?btnI&q=<c-r>x"<cr>
" stack overflow
vnoremap <leader>S "xy<esc>:!open "http://www.stackoverflow.com/search?q=<c-r>x"<cr>

" Dash search (brilliant OSX help tool)
vmap <leader>h <Plug>DashSearch
nmap <leader>h <Plug>DashSearch

" language specific
autocmd FileType coffee :DashKeywords lodash jquery javascript d3
autocmd FileType python :DashKeywords django python
autocmd FileType haskell :DashKeywords haskell
autocmd FileType stylus :DashKeywords css



" UNDO

set undodir=~/.vim/backups     " Keep undo history across sessions, by storing in file.
set undofile


" CLOJURE

let g:slime_target = "tmux"

" open a tmux pane and launch SBCL (Lisp interpreter)
" then hit <C-c> v to set configuration
" - socket name can be left to default
" - target pane needs to be set to :.X where pane number X is found by hitting the tmux command <C-a> Q


let g:vimclojure#ParenRainbow        = 1
let g:vimclojure#FuzzyIndent         = 1
let g:vimclojure#HighlightBuiltins   = 1
let g:vimclojure#HighlightContrib    = 1
let g:vimclojure#DynamicHighlighting = 1
let g:vimclojure#SplitPos            = "right"

let g:vimclojure#WantNailgun         = 0
let g:vimclojure#NailgunClient       = "~/.vim/bundle/vimclojure/bin/ng"
" TODO: fix this



" aw AddToLispWords
" tr ToggleParenRainbow
"
" lw DocLookupWord
" li DocLookupInteractive
" jw JavadocLookupWord
" ji JavadocLookupInteractive
" fd FindDoc
"
" mw MetaLookupWord
" mi MetaLookupInteractive
"
" sw SourceLookupWord
" si SourceLookupInteractive
"
" gw GotoSourceWord
" gi GotoSourceInteractive
"
" rf RequireFile
" rF RequireFileAll
"
" rt RunTests
"
" me MacroExpand
" m1 MacroExpand1
"
" ef EvalFile
" el EvalLine
" eb EvalBlock
" et EvalToplevel
" ep EvalParagraph
"
" sr StartRepl
" sR StartLocalRepl
"

" COFFEESCRIPT

autocmd BufRead *.coffee set filetype=coffee
autocmd BufRead *.coffee syn match error contained "\<console\>"
" autocmd BufWritePost,FileWritePost *.coffee :silent !coffee -c -m <afile>

autocmd FileType javascript,coffee setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType javascript,coffee setlocal makeprg=node\ %:r

autocmd FileType javascript,json setl nolist

autocmd FileType coffee setl fdm=expr fde=getline(v:lnum)=~'\(->$\|=>\)$'&&indent(v:lnum)<indent(v:lnum+1)?'a1':'s1'

" open corresponding js file
nmap <leader>jj :vertical expand("%:p:r")<CR>

" convert js into coffeescript on the fly (using eponymous npm bundle)
nmap <leader>jc :Js2Coffee<cr>
vmap <leader>jc :Js2Coffee<cr>

" coffeescript REPL
" -----------------
"
" using `vim-tbone` bundle and `nesh` node package
" run 'nesh -c' in adjacent tmux pane
" record `.Twrite <pane-number><cr>` into register
" (this only sends current line)
" and hit @@ to run repeatedly

" hide JS files
" let NERDTreeIgnore+=['\.js']

" jump to JS file number
command -nargs=1 C CoffeeCompile | :<args>

" Coffeescript tagbar support
let g:tagbar_type_coffee = {
      \ 'ctagstype' : 'coffee',
      \ 'kinds' : [
      \   'c:class',
      \   'm:methods',
      \   'f:functions',
      \ ]
      \ }

"

" CSS

" Compile LessCSS on save
" autocmd BufWritePost,FileWritePost *.less :silent !lessc <afile> <afile>:p:r.css

" :ColorToggle - turn on #abc123 color highlighting

" autocmd FileType css  setlocal foldmethod=indent

"

" DBEXT

" vnoremap <leader>es :let g:dbext_default_use_sep_result_buffer=1<CR>:DBExecVisualSQL<cr>
" nnoremap <leader>es :let g:dbext_default_use_sep_result_buffer=1<CR>:DBExecSQLUnderCursor<cr>


" vnoremap <leader>ES :let g:dbext_default_use_sep_result_buffer=0<CR>:DBExecVisualSQL<cr>
" nnoremap <leader>ES :let g:dbext_default_use_sep_result_buffer=0<CR>:DBExecSQLUnderCursor<cr>

" nnoremap <leader>SLT :let g:dbext_default_use_sep_result_buffer=0<CR>:DBListTable<cr>
" "
" ,sbp -> prompt from DB params

" ,se -> execute selected SQL query
" ,st -> SELECT * from visually selected table
" ,sdt -> describe selected table
" ,slt -> list tables (with prefix)
" ,sh -> sql history

let g:dbext_default_type='MYSQL'
let g:dbext_default_buffer_lines = 10
" use separate buffers for each DB query result
let g:dbext_default_use_sep_result_buffer = 1
let g:dbext_default_display_cmd_line =1
let g:dbext_default_window_use_horiz = 0  " Use vertical split
let g:dbext_default_window_width = 50

"

" DJANGO

" autocmd BufRead,BufNewFile *.html set filetype=html
nmap _dt :set ft=htmldjango.html<cr>
nmap _ht :set ft=html<cr>


" surround helpers for templating:
" s- :     <% %>
" s= :     <%= %>

" let g:last_relative_dir = ''

" fun! RelatedFile(file)
"     " This is to check that the directory looks djangoish
"     if filereadable(expand("%:h"). '/models.py') || isdirectory(expand("%:h") . "/templatetags/")
"         exec "edit %:h/" . a:file
"         let g:last_relative_dir = expand("%:h") . '/'
"         return ''
"     endif
"     if g:last_relative_dir != ''
"         exec "edit " . g:last_relative_dir . a:file
"         return ''
"     endif
"     echo "Cant determine where relative file is : " . a:file
"     return ''
" endfun

" fun SetAppDir()
"     if filereadable(expand("%:h"). '/models.py') || isdirectory(expand("%:h") . "/templatetags/")
"         let g:last_relative_dir = expand("%:h") . '/'
"         return ''
"     endif
" endfun
" autocmd BufEnter *.py call SetAppDir()



" django testing from within vim. not 'tested'(!) yet
" see http://gremu.net/blog/2010/integrate-your-python-test-runner-vim/

" let g:makeprg_django_app = 'python\ manage.py\ test\ -v\ 0'
" let g:makeprg_django_project = 'python\ manage.py\ test\ -v\ 0'
" set errorformat=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

" function! RunTestsForFile(args)
"   if @% =~ '\.py$'
"     let expandstr = '%:p:h' " dirname
"     while expand(expandstr) != '/'
"       let testpath = expand(expandstr)
"       if len(getfperm(testpath . '/tests')) > 0 || len(getfperm(testpath . '/tests.py')) > 0
"         call RunTests(expand(expandstr . ':t'), a:args)
"         return
"       endif
"       let expandstr .= ':h'
"     endwhile
"   endif
"   call RunTests('', a:args)
" endfunction

" function! RunTests(target, args)
"   silent ! echo
"   silent ! echo -e "\033[1;36mRunning all unit tests\033[0m"
"   silent w
"   if len(a:target)
"     execute 'set makeprg=' . g:makeprg_django_app
"   else
"     execute 'set makeprg=' . g:makeprg_django_project
"   endif
"   exec "make! " . a:target . " " . a:args
" endfunction

" function! JumpToError()
"   let has_valid_error = 0
"   for error in getqflist()
"     if error['valid']
"       let has_valid_error = 1
"       break
"     endif
"   endfor
"   if has_valid_error
"     for error in getqflist()
"       if error['valid']
"         break
"       endif
"     endfor
"     let error_message = substitute(error['text'], '^ *', '', 'g')
"     silent cc!
"     exec ":sbuffer " . error['bufnr']
"     call RedBar()
"     echo error_message
"   else
"     call GreenBar()
"     echo "All tests passed"
"   endif
" endfunction

" function! RedBar()
"   hi RedBar ctermfg=white ctermbg=red guibg=red
"   echohl RedBar
"   echon repeat(" ",&columns - 1)
"   echohl
" endfunction

" function! GreenBar()
"   hi GreenBar ctermfg=white ctermbg=green guibg=green
"   echohl GreenBar
"   echon repeat(" ",&columns - 1)
"   echohl
" endfunction

" nnoremap <leader>a :call RunTests('', '')<cr>:redraw<cr>:call JumpToError()<cr>
" nnoremap <leader>y :call RunTestsForFile('--failfast')<cr>:redraw<cr>:call JumpToError()<cr>

"

" GIT

nnoremap <leader>s  :Gstatus<cr>
nnoremap <leader>S  :Gstatus<cr><c-w>T

nnoremap <leader>d  :Gdiff<cr>
nnoremap <leader>gd  :Gdiff origin/master:%<left><left>
nnoremap <leader>gS  :Gdiff stash@{}<left>
" load revisions of current file
nnoremap <leader>gl  :Glog<cr><cr><leader>fo
" load repository in tab
nnoremap <leader>L  :Gitv<cr>
" show git history for current file
nnoremap <leader>l  :Gitv!<cr>
vnoremap <leader>l  :Gitv! --all<cr>
" search logs
nnoremap <leader>gg  :Glog --grep=
nnoremap <leader>gs  :Glog -S
" open git objects (such as current file on other branch)
nnoremap <leader>gE  :Gedit<space>
nnoremap <leader>ge  :Gvsplit<space>
nnoremap <leader>gs  :Gsplit<space>
nnoremap <leader>gr  :Gread<cr>
nnoremap <silent> <leader>gw  :Gwrite<cr>:up<cr>
nnoremap <leader>gW  :Gwrite<cr>:tabclose<cr>
nnoremap <leader>ga  :Git add --all<cr>:Gcommit<cr>
" nnoremap <leader>GA  :Git add .<cr>
nnoremap <leader>gb  :Gblame<cr>
nnoremap <leader>gh  :Gbrowse<cr>
vnoremap <leader>gh  :Gbrowse<cr>
nnoremap <leader>gH  :Gbrowse!<cr>
vnoremap <leader>gH  :Gbrowse!<cr>
nnoremap <leader>gco :Gcheckout<cr>

nnoremap <leader>gc :Gcommit<cr>
" write commit message in new tab and show preview
nnoremap <leader>gC :Gcommit<cr><c-w>T:DiffGitCached<cr>:wincmd L<cr>:wincmd p<cr>

nnoremap <leader>gm  :Gmove<cr>
nnoremap <leader>gR  :Git checkout -- %<cr><cr>,u
nnoremap <leader>gp  :Git push<CR>
nnoremap <leader>gP  :Git pull<CR>
nnoremap <leader>grsh :Git reset --hard<cr>
nnoremap <leader>gO :Git oops<cr>
nnoremap <leader>go :Gcommit<cr>cA

nnoremap <leader>gt  :GitGutterLineHighlightsToggle<cr>

nnoremap ]g  :GitGutterNextHunk<cr>
nnoremap [g  :GitGutterPrevHunk<cr>

" quickly access modified files in local git repo
"nnoremap <leader>M :CtrlPModified<cr>

" Send visual selection to gist.github.com as a private, filetyped Gist
" Requires the gist CLI
" vnoremap <leader>G :w !gist -p -t %:e \| pbcopy<cr>
" vnoremap <leader>UG :w !gist -p \| pbcopy<cr>

" TODO finish this

function! Greset()
  let wordUnderCursor = expand("<cword>")
  Git reset -- wordUnderCursor
endfunction

if has("autocmd")
  " Auto-close fugitive buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete

  " Unset 'list' in :Gstatus window (which usually contains tab characters).
  autocmd BufReadPost .git/index set nolist
  autocmd BufReadPost .git/index nnoremap <leader>r :call Greset()<cr>

  " Navigate up one level from fugitive trees and blobs
  autocmd User fugitive
        \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
        \   nnoremap <buffer> u :edit %:h<CR> |
        \ endif
endif

" get patch from diff (using the above)
" nmap <leader>do <leader>gddo<leader>gD

nnoremap d2 :diffget //2<cr>
nnoremap d3 :diffget //3<cr>

" stop gitv messing with my window navigation
let g:Gitv_DoNotMapCtrlKey = 1

let g:Gitv_WipeAllOnClose = 0

" autocmd FileType gitcommit DiffGitCached | wincmd L | wincmd p

augroup interactive-git-rebase
  au!
  au FileType gitrebase nnoremap <buffer> <silent> <c-p><c-p> :s/^#\?\w\+/pick/<cr>:noh<cr>
  au FileType gitrebase nnoremap <buffer> <silent> <c-r><c-r> :s/^#\?\w\+/reword/<cr>:noh<cr>
  au FileType gitrebase nnoremap <buffer> <silent> <c-e><c-e> :s/^#\?\w\+/edit/<cr>:noh<cr>
  au FileType gitrebase nnoremap <buffer> <silent> <c-s><c-s> :s/^#\?\w\+/squash/<cr>:noh<cr>
  au FileType gitrebase nnoremap <buffer> <silent> <c-f><c-f> :s/^#\?\w\+/fixup/<cr>:noh<cr>
  au FileType gitrebase nnoremap <buffer> <silent> <c-x><c-x> :s/^#\?\w\+/exec/<cr>:noh<cr>
  au FileType gitrebase nnoremap <buffer> <silent> <c-k><c-k> :s/^#\?/#/<cr>:noh<cr>
augroup END

let g:github_user = 'istib'


" HTML

" URL encode (unimpaired)
" {Visual}[u
" XML encode (unimpaired)
" {Visual}[x

" prevent c-n mapping conflict
let g:sparkupNextMapping=""


" JSON

" Prettify JSON files
autocmd BufRead,BufNewFile *.json set filetype=json
autocmd Syntax json source ~/.vim/bundle/json/syntax/json.vim

" format JSON
nmap <leader>jp :%!python -m json.tool<cr>:set ft=json<cr>
vmap <leader>jp :!python -m json.tool<cr>:set ft=json<cr>

augroup json_autocmd
  autocmd!
  autocmd FileType json set autoindent
  autocmd FileType json set formatoptions=tcq2l
  autocmd FileType json set textwidth=78 shiftwidth=2
  autocmd FileType json set expandtab
  autocmd FileType json set foldmethod=syntax
augroup END

"

" LATEX

au BufReadPost *.tex set syntax=tex
" instead of plaintex

" au BufReadPost *.tex set makeprg=xelatex\ \\\\-interaction=nonstopmode\ \\\\input\\{$*}
let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'
let g:tex_flavor='latex'
let g:Tex_BIBINPUTS='~/DPhil/'
" let g:Tex_MainFileExpression = '~/DPhil/dphil.tex'
let g:Imap_UsePlaceHolders=0
let g:Tex_EchoBibFields = 0
let g:Tex_GotoError=0
let g:Tex_IgnoreLevel=9
let g:Tex_Menus=0

" get rid of F7 mapping
let g:Tex_PromptedCommands=""

" customize keybindings a little
" nmap ,c i\cite{}<F4>
" imap <silent> <F4> <Esc>:call Tex_Complete("default","text")<CR>

" to surround selection with markup:
" hit the following in VISUAL mode: (without hitting S for surround.vim first)
" - for \emph:  `em
" - for \textbf: `bf
" etc.

autocmd Filetype tex nnoremap OO ,ll<cr>

let g:tagbar_type_tex = {
      \ 'ctagstype' : 'latex',
      \ 'kinds'     : [
      \ 's:sections',
      \ 'g:graphics:0:0',
      \ 'l:labels',
      \ 'r:refs:1:0',
      \ 'p:pagerefs:1:0'
      \ ],
      \ 'sort'    : 0,
      \ 'deffile' : expand('<sfile>:p:h:h') . '/ctags/latex.cnf'
      \ }

"

" MARKDOWN

" Sane(!) text-editing options

augroup ft_markdown
  au!

  " autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
  au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown

  autocmd BufNewFile,BufRead *.txt set filetype=markdown

  autocmd FileType markdown set formatoptions+=2,l formatoptions-=c smartcase wrap

  autocmd FileType markdown,tex set noautoindent nosmartindent nocindent linebreak nonumber nofoldenable
  autocmd FileType markdown,tex let &scrolloff=999-&scrolloff

  " margins and no statusline
  autocmd FileType markdown,tex hi! link FoldColumn Normal
  " autocmd FileType markdown,tex set laststatus=0 foldcolumn=5 wrapmargin=20

  " headings:
  autocmd FileType markdown nnoremap <buffer> <leader>1 yypVr=
  autocmd FileType markdown nnoremap <buffer> <leader>2 yypVr-
  autocmd FileType markdown nnoremap <buffer> <leader>3 I### <ESC>

  " separation line:
  autocmd FileType markdown nnoremap <buffer> <leader>ll o* * *<cr><cr><esc>

  " markdown links: (first yank the URL and then select the linked text in visual mode, then hit ',ml')
  autocmd FileType markdown vnoremap <buffer> <leader>l S]%a()<esc>P<esc>

augroup END

" preview
nnoremap <leader>mm :silent !open -a Marked.app '%:p'<cr>

let g:tagbar_type_markdown = {
      \ 'ctagstype': 'markdown',
      \ 'kinds': [
      \ '1:header1',
      \ '2:header2',
      \ '3:header3',
      \ '4:header4',
      \ '5:header5',
      \ '6:header6',
      \ '7:header7'
      \ ]
      \ }

"

" R

" VIM-R
" =====
"
" Start R (default)                                  \rf
" Close R (no save)                                  \rq
" Go (next R chunk)                                   gn
" Go (previous R chunk)                               gN
" File                                               \aa
" File (echo)                                        \ae
" Block (cur)                                        \bb
" Block (cur, echo)                                  \be
" Block (cur, down)                                  \bd
" Block (cur, echo and down)                         \ba
" Chunk (cur)                                        \cc
" Function (cur)                                     \ff
" Selection                                          \ss
" Paragraph                                          \pp
" Line                                                \l
" Line (and down)                                     \d
" Line (and new one)                                  \q
" Sweave (cur file)                                  \sw
" Sweave and PDF (cur file)                          \sp
" Sweave, BibTeX and PDF (cur file)                  \sb
"
" Print (cur)                                        \rp
" Names (cur)                                        \rn
" Structure (cur)                                    \rt
" Summary (cur)                                      \rs
" Plot (cur)                                         \rg
" Plot and summary (cur)                             \rb
"
" Arguments (cur)                                    \ra
" Example (cur)                                      \re
" Help (cur)                                         \rh
" Complete function name                            ^X^O
" Complete function arguments                       ^X^A
"
" Build tags file (cur dir)                  :RBuildTags
"
" Object Browser
" Show/Update                                        \ro
" Expand (all lists)                                 \r=
" Collapse (all lists)                               \r-
" Toggle (cur)                                     Enter
" -------------------------------------------------------
" Clear console                                      \rr
" Clear workspace                                    \rm


" vim-r-plugin with tmux
let vimrplugin_tmux = 2

let vimrplugin_screenvsplit = 1
let g:ScreenImpl = 'Tmux'
let vimrplugin_screenplugin = 1
let vimrplugin_applescript = 0
" let vimrplugin_conqueplugin = 0

" don't read R documentation in a Vim's buffer
let vimrplugin_vimpager = "tab"

let vimrplugin_r_args = "--quiet --no-restore"

autocmd FileType R  setlocal foldmethod=indent

" imap <C-f> <Plug>RCompleteArgs
" map <silent> <Leader>rL :call RAction("levels")<CR>
" map <silent> <Leader>rT :call RAction("tail")<CR>
" map <silent> <Leader>rH :call RAction("head")<CR>
" map <silent> <Leader>rN :call RAction("names")<CR>
" map <silent> <Leader>rC :call RAction("class")<CR>
" map <silent> <Leader>rD :call RAction("length")<CR>

let vimrplugin_underscore = 0

" replace '<' with '<<>>=\n@' in rnoweb files
let vimrplugin_rnowebchunk = 1


" background shading for comments
" hi def link rComment     Search

" fold SWeave chunck
nmap zZ ?<<<CR>V/@<CR>zf

" lines beginning with a single # are aligned at a specific column
let r_indent_comment_column = 30

augroup filetypedetect
  au! BufRead,BufNewFile *.r         setfiletype r
  au! BufRead,BufNewFile *.R         setfiletype r
  " syntax highlighting for SWeave
  au! BufRead,BufNewFile *.Rnw       setf noweb
augroup END


"

" SURROUND

" Ruby
" Use v or # to get a variable interpolation (inside of a string)}
let g:surround_113 = "#{\r}"   " v
let g:surround_35  = "#{\r}"   " #

" Select text in an ERB/Django file with visual mode and then press s- or s=
" Or yss- to do entire line.
let g:surround_45 = "<% \r %>"    " -
let g:surround_61 = "<%= \r %>"   " =

" avoid annoying error when trying to surround with lowercase s
vmap s S

" ,# Surround a word with #{ruby interpolation}
map <leader># ysiw#
" ,# Surround a word with quotes
map <leader>" ysiw"

" switch between ' and "
vnoremap <leader>' ""yls<c-r>={'"': "'", "'": '"'}[@"]<cr><esc>

" nice for lisp dialects
" imap <leader>( <esc>vES)i
" imap <leader>) <esc>v$hS)i
" imap <leader>" <esc>vES"i
" imap <leader>' <esc>vES'i

nnoremap [s i<space><esc>
nnoremap ]s a<space><esc>
"

" BUFFERS

" buffers hidden, not closed, when abandoned
set hidden

" open new windows right and below
set splitright
set splitbelow

" go file, but avoiding error message about leaving unsaved buffer
" map gf :edit <cfile><CR>
nnoremap gf :wincmd f<cr>

" NARROW REGION

let g:nrrw_rgn_vert = 1
let g:nrrw_rgn_wdth = 50

" no highlighting of narrowed region in source file
let g:nrrw_rgn_nohl = 1
let g:rrrw_rgn_protect = 0
" preferred position
let g:nrrw_topbot_leftright = 'botright'

" open selected in zoomed narrowregion buffer and center in buffer (used to
" focus on current paragraph, eg.)
vmap <silent> <leader>N :NR<cr><c-w>ogg14O<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>0o<esc>G2o<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>13o<esc>16G0<esc>
nmap <silent> <leader>N vip:NR<cr><c-w>ogg14O<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>0o<esc>G2o<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>13o<esc>16G0
" and exit gracefully
nmap "; :1,15d<cr>:$-14,$d<cr><c-w>o:wq<cr>

nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-l> <C-w>l
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-j> <C-w>j

" ctrl-w H  move current window to far left
" ctrl-w J  move current window to bottom
" ctrl-w K  move current window to top
" ctrl-w L  move current window to far right

" Create window splits easier.
nnoremap <silent> vv <C-w>v
nnoremap <silent> ss <C-w>s
nnoremap <silent> tt :tabedit %<cr>

" " resizing shortcuts
" nnoremap _ <C-w>-
" nnoremap + <C-w>+

" Faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" tags
"nnoremap <c-g> :CtrlPTag<cr>

" change list
"nnoremap <c-q> :CtrlPChangeAll<cr>

"nmap <silent> :: :CtrlPCurWD<CR>

"Move back and forth through previous and next buffers
nnoremap <silent> xz :bp<CR>
nnoremap <silent> zx :bn<CR>

" CTRL-P
"========

let g:ctrlp_custom_ignore = {
      \ 'dir':  'private|core\/static',
      \ 'file': '\.map$',
      \ }

let g:ctrlp_user_command = {
      \ 'types': {
      \ 1: ['.git', 'cd %s && git ls-files'],
      \ },
      \ 'fallback': 'find %s -type f'
      \ }

nmap <leader>f :CtrlPMRU<cr>

""let g:ctrlp_cmd = 'CtrlPMixed'      " search anything (in files, buffers and MRU files at the same time.)
"let g:ctrlp_working_path_mode = 'ra'  " search for nearest ancestor like .git, .hg, and the directory of the current file

"" TIP: create new files using C-E from within CtrlP picker

" Default to file searches
"let g:ctrlp_by_filename = 0
" switch to path search with ctrl-d inside plugin

" We don't want to use Ctrl-p as the mapping because
" it interferes with YankRing (paste, then hit ctrl-p)
"let g:ctrlp_map = ',0'

"let g:ctrlp_open_new_file = 'v'
"let g:ctrlp_open_multiple_files = 'v'

" open switcher with project root directory (determined by location of .git folder)
"" let g:ctrlp_working_path_mode = 2

" open files in existing buffers if they are already opened
"let g:ctrlp_switch_buffer = 'et'

"let g:ctrlp_use_caching = 1
"let g:ctrlp_clear_cache_on_exit=0     " speed up by not removing clearing cache evertime
"let g:ctrlp_cache_dir = $HOME.'/.cache/ctrlp'
"let g:ctrlp_dotfiles = 1
"let g:ctrlp_max_depth = 20
"let g:ctrlp_mruf_max = 200
"
" coffee: https://gist.github.com/michaelglass/5210282
"let g:ctrlp_buftag_types = {
" \ 'coffee'     : '--language-force=coffee --coffee-types=cmfvf',
" \ }

" NERDTREE

" Make nerdtree look nice
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:NERDTreeWinSize = 30
let NERDTreeHijackNetrw=1
let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr']

let NERDTreeMouseMode=2
let NERDTreeShowHidden=1

" Auto open nerd tree on startup
let g:nerdtree_tabs_open_on_gui_startup = 0
" Focus in the main content window
let g:nerdtree_tabs_focus_on_files = 1

"

" COMMAND MODE

set history=1000               " Store lots of :cmdline history
set cmdheight=2                " avoid 'Press ENTER to continue'

nmap q: :Unite history/command<cr>
nmap q/ :Unite history/search<cr>
nmap q" :Unite history/yank<cr>

" filter command line history with c-p and c-n
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

cnoremap <c-h> <left>
cnoremap <c-l> <right>
cnoremap <c-j> <c-n>
cnoremap <c-k> <c-p>

inoremap <expr> <C-E> col('.')>strlen(getline('.'))?"\<Lt>C-E>":"\<Lt>End>"

noremap! <expr> <SID>transposition getcmdpos()>strlen(getcmdline())?"\<Left>":getcmdpos()>1?'':"\<Right>"
noremap! <expr> <SID>transpose "\<BS>\<Right>".matchstr(getcmdline()[0 : getcmdpos()-2], '.$')
cmap   <script> <C-T> <SID>transposition<SID>transpose

" use ESC to exit cmdline window
autocmd CmdwinEnter * nnoremap <buffer> <esc> :q<cr>

"

" COMMENTING

" comment paragraph
" nmap <c-\> \\ip

nmap <c-\> \\\
imap <c-\> <esc>\\\i
vmap <c-\> \\\

" <c-/>a    comment as

" yank and past visual before toggle comment
vmap gyy ygvgc'>gp'.
nmap gyy mz:t-1<cr><c-\>`z:delmarks z<cr>
imap gyy <esc>:t-1<cr>gCcgi
" Using :t-1 instead of yyP to preserve registers

"

" COMPLETION

let g:UltiSnipsSnippetDirectories=["snippets"]
nnoremap <leader>UE :UltiSnipsEdit<cr>

" tab completion for e.g. :e is awesome with these
set wildmode=list:longest,full
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches

set wildignore=*.DS_Store                              " OSX bullshit
set wildignore+=*.aux,*.log,*.out,*.toc,*.bst          " LaTeX intermediate files
set wildignore+=*.gem
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.svg,*.jpeg         " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.pyc " compiled object files
set wildignore+=*.spl                                  " compiled spelling word lists
set wildignore+=*.sw?                                  " Vim swap files
set wildignore+=*vim/backups*
set wildignore+=log/**
set wildignore+=tmp/**

set wildignore+=tmp/**

" augroup is_keyword
"   " clear commands before resetting
"   autocmd!
"   " make sure `complete` works as expected for CSS class names whithout
"   " messing with motions (eg. '.foo-bar__baz') and we make sure all
"   " delimiters (_,-,$,%,.) are treated as word separators for motions
"   autocmd FileType * setl iskeyword=@,48-57,192-255
"   autocmd InsertEnter * setl iskeyword=@,48-57,192-255,$,%,-,_
"   autocmd InsertLeave * setl iskeyword=@,48-57,192-255
" augroup END

" ================ neocomplcache =======================
" A better autocomplete system!

let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_enable_smart_case = 1

" default # of completions is 100, that's crazy
let g:neocomplcache_max_list = 3

" words less than 3 letters long aren't worth completing
let g:neocomplcache_auto_completion_start_length = 4

" This makes sure we use neocomplcache completefunc instead of
" the one in rails.vim, otherwise this plugin will crap out
let g:neocomplcache_force_overwrite_completefunc = 1

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags

" autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'

" Filename completion
inoremap <c-]> <c-x><C-]>
inoremap <c-l> <c-x><C-l>
set completefunc=syntaxcomplete#Complete

" Better Completion
set completeopt=longest,menuone,preview

"

" DIFF

nmap dp :diffput<cr>
nmap do :diffget<cr>

nmap <leader>D :windo diffthis<cr>

" set fillchars=diff:⣿,vert:│
set fillchars=diff:_,vert:│,fold:_

" Find merge conflict markers
" map <leader>fc /\v^[<=>]{7}( .*\|$)<cr>

" IN VIMDIFF BUFFER

" do - diff obtain
" dp - diff put
" [c - previous difference
" ]c - next difference

" in three-way vimdiff

nmap d2 :diffget //2<cr>
" fetches the hunk from the target parent (on the left)
nmap d3 :diffget //3<cr>
" fetches the hunk from the merge parent (on the right)

" " Linediff plugin
nmap <leader>X :Linediff<cr>
vmap <leader>X :Linediff<cr>

" Close any corresponding diff buffer
function! CloseDiff()
  if (&diff == 0 || getbufvar('#', '&diff') == 0)
        \ && (bufname('%') !~ '^fugitive:' && bufname('#') !~ '^fugitive:')
    echom "Not in diff view."
    return
  endif

  " close current buffer if alternate is not fugitive but current one is
  if bufname('#') !~ '^fugitive:' && bufname('%') =~ '^fugitive:'
    if bufwinnr("#") == -1
      b #
      bd #
    else
      bd
    endif
  else
    bd #
  endif
endfunction
nnoremap <silent> <Leader>gD :call CloseDiff()<cr>

" Disable one diff window during a three-way diff allowing you to cut out the
" noise of a three-way diff and focus on just the changes between two versions
" at a time. Inspired by Steve Losh's Splice
function! DiffToggle(window)
    " Save the cursor position and turn on diff for all windows
    let l:save_cursor = getpos('.')
    windo :diffthis

    " Turn off diff for the specified window (but keep scrollbind) and move
    " the cursor to the left-most diff window
    exe a:window . "wincmd w"
    diffoff
    set scrollbind
    set cursorbind
    exe a:window . "wincmd " . (a:window == 1 ? "l" : "h")

    " Update the diff and restore the cursor position
    diffupdate
    call setpos('.', l:save_cursor)
endfunction
" Toggle diff view on the left, center, or right windows
nmap <silent> <leader>dl :call DiffToggle(1)<cr>
nmap <silent> <leader>dc :call DiffToggle(2)<cr>
nmap <silent> <leader>dr :call DiffToggle(3)<cr>


" Diff two registers
" Open a diff of two registers in a new tabpage. Close the tabpage when
" finished. If no registers are specified it diffs the most recent yank with
" the most recent deletion.
" Usage:
"   :DiffRegs
"   :DiffRegs @a @b

function! DiffRegsFunc(...)
    let l:left = a:0 == 2 ? a:1 : "@0"
    let l:right = a:0 == 2 ? a:2 : "@1"

    tabnew
    exe 'put! ='. l:left
    vnew
    exe 'put! ='. l:right

    windo call Scratch()
    windo diffthis
    winc t
endfunction
com -nargs=* DiffRegs call DiffRegsFunc(<f-args>)

"

"

" MARKS

set viminfo='100,f1            " Save up to 100 marks, enable capital marks

" using vim-signature plugin

" remove s/S from defaults
let g:SignatureIncludeMarks = 'abcdefghijklmnopqrstuvwxyABCDEFGHIJKLMNOPQRSTUVWXY'

sign define white text=!! linehl=StatusLine

function! AddSign()
  execute(":sign place ".line(".")." line=".line(".")." name=white file=".expand("%:p"))
endfunction

map <leader>sa :call AddSign()<CR>
map <leader>sr :sign unplace<CR>

" <c-e><c-e> Highlight current line
" <c-e><C-a> Advance color for next line highlight
" <c-e><C-r> Clear last line highlight
" <c-e><C-w> Highlight word under cursor (whole word match)
" <c-e><C-l> Highlight all lines having word under cursor (whole word match)
" <c-e><C-f> Highlight word under cursor (partial word match)
" <c-e><C-k> Highlight all lines having word under cursor (partial word match)
" <c-e><C-s> Highlight last search pattern
" <c-e><C-j> Highlight all lines having last search pattern
" <c-e><C-d> Clear last pattern highlight
" <c-e><C-n> Clear all highlights



" Use a bar-shaped cursor for insert mode, even through tmux.
if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif





"

" NAVIGATION

" next text object
" ================================================================================

" Motion for "next/last object". For example, "din(" would go to the next "()"
" pair and delete its contents.
onoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
xnoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
onoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>
xnoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>

function! s:NextTextObject(motion, dir)
  let c = nr2char(getchar())

  if c ==# "b"
    let c = "("
  elseif c ==# "B"
    let c = "{"
  elseif c ==# "d"
    let c = "["
  endif

  exe "normal! ".a:dir.c."v".a:motion.c
endfunction

" " http://stackoverflow.com/questions/6926034/creating-a-mapping-for-insert-mode-but-not-for-autocomplete-submode/6926691#6926691
" inoremap <expr> <c-e> pumvisible() ? "\<c-e>" : "\<c-o>A"
" inoremap <C-a> <C-o>I

" prefer exact caret location to mere line jumps
nmap '' ``

" select last change
nmap gV `[v`]

" Bring cursor to center (use :sus to actually bring vim to background)0
nnoremap <c-z> zvzz

" keep jumps in the middle of the window
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz

" Make sure Vim returns to the same line when you reopen a file.
augroup line_return
  au!
  au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" Only show cursorline in the current window and in normal mode.
augroup cline
  au!
  au WinLeave * set nocursorline
  au WinEnter * set cursorline
  au InsertEnter * set nocursorline
  au InsertLeave * set cursorline
augroup END

"

" PYTHON

augroup ft_python
  au!
  autocmd BufNewFile,BufRead *.py set filetype=python
  autocmd FileType python set expandtab
  " compile
  " autocmd Filetype python nmap <buffer> <leader>cc :w<Esc>:!python %<CR>
augroup END

" autocmd BufRead *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"

let pymode_options=0
"

" SEARCH

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

" use enter to start search (expect in quickfix window)
" nnoremap <expr> <cr> (&buftype is# "quickfix" ? ":.cc<cr>" : "/")
" vnoremap <expr> <cr> (&buftype is# "quickfix" ? ":.cc<cr>" : "/")

set incsearch           " Find the next match as we type the search
set nohlsearch          " Highlight searches by default
set ignorecase          " case insensitive search for lowercase...
set smartcase           " ...but if mixed case, go case-sensitive
set gdefault            " Add the g flag to search/replace by default

" Open a (CtrlP'ed) Quickfix window for the last search.
" nnoremap <silent> KS :execute 'vimgrep /'.@/.'/g %'<CR>:Unite quickfix<CR>
" vnoremap <silent> KS *:execute 'vimgrep /'.@/.'/g %'<CR>:Unite quickfix<CR>

" do visual-search and KS at once
vmap 8 *KS
vmap n *

" " Use Ack instead of Grep when available
" if executable("ack")
"   set grepprg=ack\ -H\ --nogroup\ --nocolor
" endif

nnoremap KC :GitGrep <c-r><c-w><CR>
vnoremap KC "sy:GitGrep "<c-r>s"<CR>

" nnoremap KA :GitGrep ""<left>
" vnoremap KA "sy:GitGrep "<c-r>s"<CR>
" nnoremap KL :GitGrep "<c-r>/"<cr>


" vnoremap KC "sy:GitGrep "<c-r>s"<CR>
" vnoremap KC "sy:Unite -input=<C-R>y grep:.<cr>

nnoremap KA :GitGrep ""<left>
nnoremap <silent> KA  :Unite -no-split grep:.<CR>
vnoremap KA "sy:GitGrep "<c-r>s"<CR>
nnoremap KL :GitGrep "<c-r>/"<cr>


" Escape and paste a register
" <c-x>{char} - paste register into search field, escaping sensitive chars
" http://stackoverflow.com/questions/7400743/
cnoremap <c-x> <c-r>=<SID>PasteEscaped()<cr>
function! s:PasteEscaped()
  echo "\\".getcmdline()."\""
  let char = getchar()
  if char == "\<esc>"
    return ''
  else
    let register_content = getreg(nr2char(char))
    let escaped_register = escape(register_content, '\'.getcmdtype())
    return substitute(escaped_register, '\n', '\\n', 'g')
  endif
endfunction

" HIGHLIGHTING

" toggle highlighting on/off
nnoremap <silent> <leader>H :set hlsearch! hlsearch?<CR>

" highlight whole line of search result
" nnoremap ,/l :let @/ = '.*\%(' . @/ . '\m\).*'<cr>
" or just word
" nnoremap ,/w :let @/ = '(@/)'<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" quick replace
nnoremap rr :%s//<left>
vnoremap rr :s//<left>

" replace current word
nnoremap rc :%s/<c-r><c-w>/
vnoremap rc :s/<c-r><c-w>/
" ...with similar and open command line view to edit the replacement
nnoremap RC :%s/<c-r><c-w>/<c-r><c-w>/<c-f>
vnoremap RC :s/<c-r><c-w>/<c-r><c-w>/<c-f>

" replace yanked
nnoremap ry :%s/<c-r>"/
vnoremap ry :s/<c-r>"/
" ...with similar
nnoremap RY :%s/<c-r>"/<c-r>"<c-f>
vnoremap RY :s/<c-r>"/<c-r>"<c-f>

" replace visual
vnoremap rv "xy:%s/<c-r>x/
vnoremap RV "xy:%s/<c-r>x/<c-r>x<c-f>

" replace last
vnoremap rl :s/~/
vnoremap RL :s/~/

" substitute repeat
nnoremap & :&&<cr>
xnoremap & :&&<cr>

" toggle quickfix window
nnoremap qo :Unite quickfix<cr>

" open quickfix at bottom of the window
let g:tlWindowPosition=1

let g:seek_enable_jumps = 1

"

" SPELLING

" nmap <silent> <leader><leader>s :setlocal spell!<cr>

" auto-correct current word
" imap <silent> ,s <esc>z=1<cr><cr>ea
nmap <silent> <leader>CS z=1<cr><cr>
" ...and go to next spelling mistake
" nmap <silent> <leader>S z=1<cr><cr>]s

" common typos
iabbr teh the
iabbr nign ning
iabbr lenght length

"

" SYNTAX

syntax on
syntax enable                  " turn on syntax highlighting

nmap <leader>FT :Unite filetype<cr>

let g:syntastic_check_on_open=0

" mark syntax errors with :signs
let g:syntastic_enable_signs=1

" automatically jump to the error when saving the file
let g:syntastic_auto_jump=0

" show the error list automatically
let g:syntastic_auto_loc_list=0

" don't care about warnings
let g:syntastic_quiet_warnings=0

" for html template files, you may want to disable syntastic's automatic checks using the passive mode:
nmap <leader>ns :SyntasticToggleMode<cr>

" Use tabs if it's a CSV file
autocmd BufNewFile,BufRead *.csv set noexpandtab nosmarttab

" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800

"

" TABS

nmap <leader><tab> :tabnew<cr>

nmap <leader>tc :tabclose<cr>

" gt              Move to next tab
" gT              Move to previous tab
" #gt             Move to tab number #

" :tabmove        Move current tab to the end
" :tabmove 0      Move current tab to the beginning
" :tabmove 1      Move current tab to become the 2nd tab


"

" TAGS

" Look for tags file in parent directories, upto "/"
set tags=./tags;tags;.git/;/

" Restore case-sensitivity for jumping to tags (set ic disables it)
nnoremap <silent> <C-]> :set noic<cr>g<C-]><silent>:set ic<cr>

" Jump to next tag match
nmap ]t :bd<cr>:tnext<cr>
" Jump to previous tag match
nmap [t :bd<cr>:tprevious<cr>


" jump to current tag
nnoremap <silent> T :UniteWithCursorWord -immediately tag<cr>

" open file (from cs require)
nnoremap <silent> <leader>gf "zyi':Unite file_rec/async file_mru -no-start-insert -immediately -input=<c-r>z<cr>

" open the taglist
nnoremap <silent> <leader>T :TagbarToggle<CR><C-l>

let g:tagbar_width=44
let g:tagbar_autoclose = 1
let g:tagbar_sort = 0

" omit odd empty lines
let g:tagbar_compact = 1

" capture cursor on popup
let g:tagbar_autofocus = 1

"

" TERMINAL

" set shell=zsh

" TMUX
" Prompt for a command to run
nmap <leader>tp :PromptVimTmuxCommand<cr>

" Run last command executed by RunVimTmuxCommand
nmap <leader>tl :RunLastVimTmuxCommand<cr>

" Inspect runner pane
nmap <leader>ti :InspectVimTmuxRunner<cr>

" Close all other tmux panes in current window
nmap <leader>tx :CloseVimTmuxPanes<cr>

" Interrupt any command running in the runner pane
nmap <leader>rs :InterruptVimTmuxRunner<cr>

" run tests in tmux (use :up[date] for cleaner saving)
nmap <silent> <leader>w :up<cr>:silent! RunLastVimTmuxCommand<cr>


"

" NGINX

autocmd BufRead nginx.conf set filetype=nginx

"

nmap <leader>BDB /data-bind=\".*\"<cr><c-e><c-s>
nmap <leader>BCL /console\.log<cr><c-e><c-s>
nmap <leader>BCC /^class<cr><c-e><c-l>

" VIMWIKI

" keep track of several wikis
" let g:vimwikilist=""
" ,ww index (,wt in tab)
" ,wi diary index
" ,w,w diary today (,w,t in tab)
" ,wh convert current page to HTML (,whh: and open)

"

" HASKELL

" au BufEnter *.hs compiler ghc
let g:haddock_browser="/Application/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

"

" UNITE


" Start in insert mode
let g:unite_enable_start_insert = 1

" Enable short source name in window
let g:unite_enable_short_source_names = 1

" Enable history yank source
let g:unite_source_history_yank_enable = 1

" Open in bottom right
let g:unite_split_rule = "botright"

" Shorten the default update date of 500ms
let g:unite_update_time = 200
let g:unite_source_rec_max_cache_files = 20000
let g:unite_source_file_mru_limit = 1000
let g:unite_cursor_line_highlight = 'TabLineSel'
let g:unite_source_file_mru_filename_format = ''
let g:unite_source_directory_mru_filename_format = ''
let g:unite_source_directory_mru_time_format = ''
let g:unite_source_file_mru_time_format = ''

nnoremap qq :UniteResume<cr>
nnoremap <leader>p :Unite -no-split -buffer-name=all file_rec/async:!<cr>
nnoremap <leader>o :Unite -no-split -buffer-name=outline outline<cr>
nnoremap <leader>i :set foldmethod=indent<cr>:Unite -no-split -buffer-name=fold fold<cr>
nnoremap <leader>t :Unite -no-split -buffer-name=tags tag<cr>

nnoremap <leader>f :Unite -buffer-name=mru file_mru<cr>
nnoremap <leader>F :UniteWithBufferDir -no-split -buffer-name=files file<cr>
nnoremap <leader>b :Unite -no-split -quick-match -buffer-name=buffer buffer<cr>

nnoremap <leader>R :Unite -no-split -buffer-name=register register<cr>
nnoremap <leader>P :Unite -no-split -buffer-name=sessions session<CR>
" nnoremap <leader>u :Unite -no-split -buffer-name=sources source<CR>
nnoremap <leader>y :Unite -no-split -buffer-name=yank    history/yank<cr>
nnoremap <leader>h :Unite -no-split -buffer-name=help help<CR>
" nnoremap <leader>l :UniteWithCursorWord -no-split -buffer-name=search_file line<CR>
nnoremap <leader>c :Unite -no-split -buffer-name=commands history/command command<CR>
nnoremap <leader>b :Unite -no-split -buffer-name=bookmarks -quick-match bookmark<CR>
nnoremap <leader>d :Unite -buffer-name=change-cwd -default-action=lcd directory_mru<CR>
nnoremap <leader><leader> :Unite -no-start-insert fold<CR>

nnoremap <silent> <leader>/  :Unite -no-split -buffer-name=search line<CR>

nnoremap <silent> <leader><leader>/  :Unite -no-split grep:.<CR>
nnoremap <silent> <leader>G  :Unite -no-split grep:.<CR>

nnoremap <silent> q/  :Unite -no-split history/search<CR>
nnoremap <leader>u  :Unite<space>
nnoremap <leader>M :Unite giti/status<cr>
nnoremap <leader>B :Unite giti/branch_all<cr>
nnoremap <leader>L :Unite giti/log -default-action=yank_hash<cr>

autocmd BufEnter *
\   if empty(&buftype)
\|      nnoremap <buffer> <C-]> :<C-u>UniteWithCursorWord -immediately tag<CR>
\|  endif

" Custom Unite settings

function! s:unite_settings()

  nmap <buffer> <ESC> <Plug>(unite_exit)
  imap <buffer> <ESC> <Plug>(unite_exit)
  imap <buffer> qq <Plug>(unite_exit)
  imap <buffer> <c-c> <Plug>(unite_exit)
  imap <buffer> kk <Plug>(unite_exit)
  " imap <buffer> <c-j> <Plug>(unite_select_next_line)
  imap <buffer> <c-j> <Plug>(unite_insert_leave)
  nmap <buffer> <c-j> <Plug>(unite_loop_cursor_down)
  nmap <buffer> <c-k> <Plug>(unite_loop_cursor_up)
  imap <buffer> <c-a> <Plug>(unite_choose_action)
  imap <buffer> <c-d> <Plug>(unite_select_next_page)
  imap <buffer> <Tab> <Plug>(unite_exit_insert)
  imap <buffer> jj <Plug>(unite_insert_leave)
  imap <buffer> <C-w> <Plug>(unite_delete_backward_word)
  nmap <buffer> h   <Plug>(unite_delete_backward_path)
  imap <buffer> <c-u>   <Plug>(unite_delete_backward_path)
  imap <buffer> <C-z>   <Plug>(unite_toggle_mark_current_candidate)
  imap <buffer> '     <Plug>(unite_quick_match_default_action)
  nmap <buffer> '     <Plug>(unite_quick_match_default_action)
  nmap <buffer> <C-r> <Plug>(unite_redraw)
  imap <buffer> <C-r> <Plug>(unite_redraw)
  nnoremap <buffer><expr> cd     unite#do_action('lcd')
  inoremap <buffer><expr> <C-s> unite#do_action('split')
  nnoremap <buffer><expr> <C-s> unite#do_action('split')
  inoremap <buffer><expr> <C-v> unite#do_action('vsplit')
  nnoremap <buffer><expr> <C-v> unite#do_action('vsplit')
  inoremap <buffer><expr> <C-t> unite#do_action('tabopen')
  nnoremap <buffer><expr> <C-t> unite#do_action('tabopen')
  inoremap <buffer><expr> <C-g> unite#do_action('grep')
  nnoremap <buffer><expr> <C-g> unite#do_action('grep')

  let unite = unite#get_current_unite()
  if unite.buffer_name =~# '^search'
    nnoremap <silent><buffer><expr> r     unite#do_action('replace')
  else
    nnoremap <silent><buffer><expr> r     unite#do_action('rename')
  endif
endfunction

autocmd FileType unite call s:unite_settings()

let g:unite_source_grep_max_candidates = 200

if executable('ag')
  " Use ag in unite grep source.
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nocolor --nogroup --hidden'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack-grep')
  " Use ack in unite grep source.
  let g:unite_source_grep_command = 'ack-grep'
  let g:unite_source_grep_default_opts = '--no-heading --no-color -a'
  let g:unite_source_grep_recursive_opt = ''
endif

" let g:unite_source_session_enable_auto_save = 1
" Load session automatically.
" autocmd VimEnter * UniteSessionLoad
"
" Use the fuzzy matcher for everything
call unite#filters#matcher_default#use(['matcher_fuzzy'])

" Use the rank sorter for everything
call unite#filters#sorter_default#use(['sorter_rank'])

" Set up some custom ignores
call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
      \ 'ignore_pattern', join([
      \ '\.git/',
      \ '\.neocomplcache/',
      \ ], '\|'))


let g:vimfiler_as_default_explorer = 1
let g:vimfiler_tree_leaf_icon = ' '
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_marked_file_icon = '✓'
let g:my_vimfiler_explorer_name = 'explorer'
let g:my_vimfiler_winwidth = 30
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_directory_display_top = 1
let g:vimfiler_quick_look_command = 'qlmanage -p '
let g:vimfiler_force_overwrite_statusline = 0
let g:vimfiler_time_format = "%H:%M %d/%m/%y"
let g:vimfiler_enable_auto_cd = 1
let g:vimfiler_default_columns = "size:time"


nnoremap <leader>e :VimFilerBufferDir<cr>
nnoremap <tab><tab> :VimFilerExplorer<cr>

autocmd FileType vimfiler call s:vimfiler_settings()
function! s:vimfiler_settings()
  nmap <buffer><expr><CR>  vimfiler#smart_cursor_map("\<PLUG>(vimfiler_expand_tree)", "e")
  nmap <buffer> za <Plug>(vimfiler_toggle_visible_dot_files)
  nmap <buffer> qq <Plug>(vimfiler_quick_look)
  nmap <buffer> <leader>f <Plug>(vimfiler_switch_to_history_directory)
  nmap <buffer> F <Plug>(vimfiler_find)
  nmap <buffer> S <Plug>(vimfiler_select_sort_type)
  nmap <buffer> a <Plug>(vimfiler_choose_action)
  nmap <buffer> <c-v> vimfiler#do_action('vsplit')
  nmap <buffer> h <Plug>(vimfiler_smart_h)
  nmap <buffer> l <Plug>(vimfiler_smart_l)
  nmap <buffer> <c-l> <c-w>l
  nmap <buffer> y <Plug>(vimfiler_mark_current_line)<Plug>(vimfiler_copy_file)
  nmap <buffer> m <Plug>(vimfiler_mark_current_line)<Plug>(vimfiler_move_file)
  nmap <buffer> d <Plug>(vimfiler_mark_current_line)<Plug>(vimfiler_delete_file)

endfunction

nnoremap <leader>gS :Unite file_rec/async -input=<C-R>=expand("%:t:r")<cr>_spec<cr>

source ~/dotfiles/vim/colors.vim
source ~/dotfiles/vim/personal.vim


" vim:set foldmethod=marks; set foldenable
