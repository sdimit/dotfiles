" BUNDLES {{{

set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" File Management
Bundle 'vim-scripts/LustyExplorer'
Bundle 'vim-scripts/LustyJuggler'
Bundle 'kien/ctrlp.vim'
Bundle 'tacahiroy/ctrlp-funky'
Bundle 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'

" Web Dev
Bundle 'kchmck/vim-coffee-script'
Bundle 'zeekay/vim-js2coffee'
Bundle 'tristen/vim-sparkup'
Bundle 'vim-scripts/closetag.vim'

" Working with code
Bundle 'tpope/vim-fugitive'
Bundle 'gregsexton/gitv'
Bundle 'tpope/vim-surround'
Bundle 'majutsushi/tagbar'
Bundle 'vim-scripts/AutoTag'

Bundle 'Shougo/neocomplcache'
Bundle 'SirVer/ultisnips'
" Bundle 'garbas/vim-snipmate'
" Bundle 'honza/snipmate-snippets'

Bundle 'tpope/vim-commentary'
Bundle 'vim-scripts/a.vim'
Bundle 'godlygeek/tabular'
Bundle 'AndrewRadev/switch.vim'
Bundle 'danro/rename.vim'
Bundle 'kshenoy/vim-signature'
Bundle 'ervandew/screen'
Bundle 'vim-scripts/YankRing.vim'
Bundle 'tpope/vim-unimpaired'
Bundle 'kana/vim-textobj-entire'
Bundle 'kana/vim-textobj-fold'
Bundle 'kana/vim-textobj-indent'
Bundle 'kana/vim-textobj-user'
Bundle 'lucapette/vim-textobj-underscore'
Bundle 'benmills/vimux'
Bundle 'tpope/vim-markdown'
Bundle 'sjl/gundo.vim'
Bundle 'tpope/vim-repeat'
Bundle 'chrisbra/NrrwRgn'
Bundle 'vim-scripts/ZoomWin'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'nvie/vim-pyunit'
Bundle 'vim-scripts/utl.vim'
Bundle 'nvie/vim_bridge'
Bundle 'claco/jasmine.vim'
Bundle 'vim-scripts/utl.vim'
" Bundle 'vim-scripts/dbext.vim'
" json/
" vimpdb

" Search
" Bundle 'mileszs/ack.vim'
Bundle 'henrik/git-grep-vim'
" Bundle 'jordansissel/vim-ackmore'
Bundle 'bronson/vim-visual-star-search'
" indexed-search/
" rootfinder
" reporoot
" rooter

" Appearance
Bundle 'altercation/vim-colors-solarized'
Bundle 'Lokaltog/vim-powerline'

" }}}

" GENERAL {{{

filetype plugin on


let mapleader=","
let maplocalleader=","


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

" }}}

" EDITING {{{

" use utf8
set encoding=utf8

set nonumber

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

" shortcut to turn off line numbers
map <silent> <leader>n :set number!<CR>

nmap ,u :GundoToggle<CR>

let g:yankring_history_dir = '$HOME'
let g:yankring_history_file = '.yankring-history'
let g:yankring_max_history = 300

" open on the right so as not to compete with the nerdtree
let g:gundo_right = 1

" a little wider for wider screens
let g:gundo_width = 60

" Don’t reset cursor to start of line when moving around.
set nostartofline

" change text inside quotes
nnoremap X ci"

" YANKING AND PASTING

" paste with trailing space
nmap <leader>P Pa<space><esc>l
nmap <leader>p a<space><c-r>"<esc>

nmap <leader><leader>P o<c-r>"<esc>

" select pasted text
nnoremap <expr> gV "`[".getregtype(v:register)[0]."`]"

" system clipboard
" Preserve indentation while pasting text from the OS X clipboard
nnoremap <leader><leader>p :set paste<CR>:put  *<CR>:set nopaste<CR>
map <leader>y "*y

" Yank entire buffer with gy
nmap gy :%y+<cr>

" If you visually select something and hit paste
" that thing gets yanked into your buffer. This
" generally is annoying when you're copying one item
" and repeatedly pasting it. This changes the paste
" command in visual mode so that it doesn't overwrite
" whatever in pasting it.
"
vnoremap p "_dP

" Press Shift+P while in visual mode to replace the overwriting without
" overwriting the default register
vmap P p :call setreg('"', getreg('0')) <CR>


" visually select the last pasted text
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'


" ALIGN/TABULARISE

vmap <Leader>a=       :Tabularize /=<cr>
vmap <Leader>a"       :Tabularize /"<cr>
vmap <Leader>a:       :Tabularize /:<cr>
vmap <Leader>a::      :Tabularize /:\zs<cr>
vmap <Leader>a,       :Tabularize /,<cr>
vmap <Leader>a{       :Tabularize /{<cr>
vmap <Leader>a<Bar>   :Tabularize /<cr>

" easier blank line insertions
nnoremap <leader>o :<C-u>exe 'normal m`'.v:count1.'o<C-v><C-[>``'<CR>
nnoremap <leader>O :<C-u>exe 'normal m`'.v:count1.'O<C-v><C-[>``'<CR>

" SWITCH plugin

nmap - :Switch<cr>
" vmap <leader>s :Switch<cr>

" ,cf - Copy Filename of current file into system (not vi) paste buffer
" ,yw - yank a word from anywhere within the word (so you don't have to go to the beginning of it)
" ,ow - overwrite a word with whatever is in your yank buffer - you can be anywhere on the word. saves having to visually select it
"
source ~/dotfiles/vim/switch.vim

" Turns visually selected camelCase into camel_case
vnoremap ,case :s/\v\C(([a-z]+)([A-Z]))/\2_\l\3/g<CR>




" " INCREMENT
" " ================================================================================
" function! AddSubtract(char, back)
"   let pattern = &nrformats =~ 'alpha' ? '[[:alpha:][:digit:]]' : '[[:digit:]]'
"   call search(pattern, 'cw' . a:back)
"   execute 'normal! ' . v:count1 . a:char
"   silent! call repeat#set(":\<C-u>call AddSubtract('" .a:char. "', '" .a:back. "')\<CR>")
" endfunction

" nnoremap <silent> ,i :<C-u>call AddSubtract("\<C-a>", '')<CR>
" nnoremap <silent> ,,i :<C-u>call AddSubtract("\<C-x>", '')<CR>


vnoremap u <nop>
vnoremap gu u

" Keep the cursor in place while joining lines
" nnoremap <silent> J mzJ`z:delmarks z<cr>

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w:silent! delmark w<cr>

vnoremap - =

" Toggle 'keep current line in the center of the screen' mode
nnoremap <leader>C :let &scrolloff=999-&scrolloff<cr>

" <C-R>= to enter simple calculator mode

" improved A normal mode key (jumps to right indentation)

nmap <expr> A MyA()
nnoremap ZA A
function! MyA()
    let l:prev_indent = indent(line('.') - 1)
    let l:indent_diff = l:prev_indent - indent(line('.'))
    let l:is_empty = len(getline('.')) == 0
    if l:indent_diff >= 0 && l:is_empty
        return 'ddko'

    elseif l:is_empty
        return 'I'
    else
        return 'ZA'
    endif
endfunction

" Can't be bothered to understand ESC vs <c-c> in insert mode
imap <c-c> <esc>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

" http://stackoverflow.com/questions/6228079/remove-newlines-from-a-register-in-vim/6235707#6235707
nnoremap <expr> gV    "`[".getregtype(v:register)[0]."`]"


" Indent guides

let g:indent_guides_auto_colors = 0
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=red   ctermbg=3
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=4

" ,ig - toggle visual indentation guides

map <leader>cl :set cursorline!<cr>
map <leader>cc :set cursorcolumn!<cr>

" GUndo
nmap <silent> <F4> :GundoToggle<CR>
imap <silent> <F4> <esc>:GundoToggle<CR>

" useful for right hand
imap <enter> <esc>

" Adds spaces around current block of lines
map <silent> <Leader><Space> :call <SID>AddSpaces()<Enter>
" Removes spaces around current block of lines
map <silent> <Leader><BS>    :call <SID>RemoveSpaces()<Enter>

function s:AddSpaces() range
	let separation = 2
	let blanks     = repeat([''], separation)
	call append(a:lastline, blanks)
	call append(a:firstline - 1, blanks)
endfunction

function s:RemoveSpaces()
	if getline('.') == ''
		let fromline = prevnonblank(line('.')) + 1
		let toline   = nextnonblank(line('.')) - 1
		call s:DeleteLines(fromline, toline, 0)
		return
	endif

	let toline = search('^$', 'bnW')
	if toline != 0
		let fromline = prevnonblank(toline) + 1
		call s:DeleteLines(fromline, toline)
	endif

	let fromline = search('^$', 'nW')
	if fromline != 0
		let toline = nextnonblank(fromline) - 1
		call s:DeleteLines(fromline, toline)
	endif
endfunction

function s:DeleteLines(fromline, toline, ...)
	let toline = a:toline < 1 ? line('$') : a:toline
	silent execute a:fromline . ',' . toline . 'delete'
	if a:0 == 0 || a:0 == 1 && a:1
		normal ``
	endif
endfunction

" }}}

" ERRORS {{{

" command! ErrorsToggle call ErrorsToggle()
" function! ErrorsToggle() " {{{
"   if exists("w:is_error_window")
"     unlet w:is_error_window
"     exec "q"
"   else
"     exec "Errors"
"     lopen
"     let w:is_error_window = 1
"   endif
" endfunction " }}}

" command! -bang -nargs=? QFixToggle call QFixToggle(<bang>0)
" function! QFixToggle(forced) " {{{
"   if exists("g:qfix_win") && a:forced == 0
"     cclose
"     unlet g:qfix_win
"   else
"     copen 10
"     let g:qfix_win = bufnr("$")
"   endif
" endfunction " }}}

" " nmap <silent> <F4> :ErrorsToggle<cr>
" nmap <silent> <F4> :QFixToggle<cr>

" }}}

" FILES {{{

" go to preceding/next file in directory (unimpaired)
" [o and ]o

" :e!           : return to unmodified file
" :w /some/path/%:r   : save file in another directory, but with the same name
"
" ,gf - same as vim normal gf (go to file), but in a vertical split

set noswapfile                 " turn off swap files
set nobackup
set nowb

set autoread                   " Reload files changed outside vim

" Aggresive file saving
" avoiding thousands of daily ':w's
  " for smoother web editing
  " (hence heavy reliance on VCS)

au InsertLeave * silent! :w
" Save when buffers lose focus too (for changes made in normal mode)
au BufLeave * silent! :w


" nnoremap s :w!<cr>

nnoremap ,Q :q!<cr>

" Kill buffer (but unlike :q! doesn't involve leaving vim)
nnoremap ,q :bd!<cr>
" Kill buffer and delete file
nnoremap ,,q :!rm %<cr>:bd!<cr>

" Filesystem
nmap <leader>mk :!mkdir -p <c-r>=expand("%:p:h")."/"<cr>
nmap <leader>rm :!rm %<cr>
nmap <leader>x :!chmod 755 %<cr>

" copy current filename into system clipboard - mnemonic: (c)urrent(f)ilename
nnoremap <silent> ,cf :let @* = expand("%:~")<CR>
nnoremap <silent> ,cn :let @* = expand("%:t")<CR>

" expand current directory into command line
cnoremap %% <C-R>=expand('%:h').'/'<cr>

cnoremap w' w<CR>

" Make the current file executable
nmap <leader>% :Silent chmod +x %<cr>

" Opens an edit command with the path of the currently edited file filled in
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Use ranger as vim file manager
function! Ranger()
  silent !ranger --choosefile=/tmp/chosen
  if filereadable('/tmp/chosen')
    exec 'edit ' . system('cat /tmp/chosen')
    call system('rm /tmp/chosen')
  endif
  redraw!
endfunction
nmap <tab><tab> :call Ranger()<cr>

set suffixesadd+=.py
set suffixesadd+=.js

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

" w!! to write a file as sudo
cmap w!! w !sudo tee % >/dev/null

set autochdir
" if exists('+autochdir')
"   " so :e is relative to current file
"   set autochdir
" else
"   autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /
" endif

" open (OSX-way) current file: most useful with html file opening in default browser
nmap ,o :!open %<cr>

nmap <F5> :make<cr>

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

" }}}

" FOLDING {{{

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


nmap <Leader>fm :set foldmethod=manual<cr>
nmap <Leader>fi :set foldmethod=indent<cr>
nmap <Leader>fk :set foldmethod=marker<cr>
nmap <Leader>fs :set foldmethod=syntax<cr>

nmap <leader>f0 :set foldlevel=0<CR>
nmap <leader>f1 :set foldlevel=1<CR>
nmap <leader>f2 :set foldlevel=2<CR>
nmap <leader>f3 :set foldlevel=3<CR>
nmap <leader>f4 :set foldlevel=4<CR>
nmap <leader>f5 :set foldlevel=5<CR>
nmap <leader>f6 :set foldlevel=6<CR>
nmap <leader>f7 :set foldlevel=7<CR>
nmap <leader>f8 :set foldlevel=8<CR>
nmap <leader>f9 :set foldlevel=9<CR>

" fold HTML tag
nnoremap zt Vatzf

" select all fold
nnoremap vaf vaz

" 'Refocus' folds

nnoremap <space> za
nnoremap <silent> zm zM<cr>

" }}}

" FORMAT {{{

" <C-t>           tabularize

" guu     : lowercase line
" gUU     : uppercase line
" Vu
" Lowercase line
" VU
" Uppercase line
" g~~
" Invert case
" vEU
" Switch word to uppercase
" vE~
" Modify word case

set wrap

" toggle line wrapping
nmap <leader><leader>w :set wrap!<cr>


" use external command for formatting paragraphs
" set formatprg=par\ -w80\ T2\ \|\ sed\ 's/\ \ /\\t/g'
"set formatprg="iconv -f UTF-8 -t WINDOWS-1251 |"
"            \."par -w80 |"
"            \."iconv -f WINDOWS-1251 -t UTF-8"


" format paragraph: gq
nmap ffp vip:!par 100<cr>
nmap ,ffp vip:!par
" format document
nmap fft ggVG:!par 100<cr>''
nmap ,fft ggVG:!par


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

" map <silent> <F6> :<C-U>execute LeftMargin()<CR>
" map <silent> <F7> :<C-U>execute NoLeftMargin()<CR>

" g <C-g>           word count


"  Clean code function
function! CleanCode()
  %retab          " Replace tabs with spaces
  %s/\r/\r/eg     " Turn DOS returns ^M into real returns
  %s=  *$==e      " Delete end of line blanks
  echo "Cleaned up this mess."
endfunction
nmap <leader>co :call CleanCode()<cr>


" via: http://rails-bestpractices.com/posts/60-remove-trailing-whitespace
" Strip trailing whitespace
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
command! StripTrailingWhitespaces call <SID>StripTrailingWhitespaces()

nmap ,,W :StripTrailingWhitespaces<CR>

" }}}

" INDENT {{{

set autoindent                 " copy indent from current when starting a new line
set smartindent
set smarttab
set expandtab                  " convert tabs into spaces

set shiftwidth=2
set softtabstop=2
set tabstop=2

" temporarily change tab size
nnoremap <Leader>2 :setlocal ts=2 sts=2 sw=2 et<Enter>
nnoremap <Leader>4 :setlocal ts=4 sts=4 sw=4 et<Enter>

filetype indent on

" Indent whole file, remove trailing whitespaces then save
nmap <leader>I <Esc>:nohl<cr>mygg=G,I'y:w<cr>

" Remove trailing whitespaces and ^M chars
" nmap <leader>W :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))<cr>

" }}}

" LOOK {{{

set laststatus=2               " show the statusline in all windows

set t_Co=256

colorscheme solarized
set background=dark
let g:solarized_termtrans = 1
let g:solarized_contrast = 'high'

nmap ,cs :colorscheme<space>

set noruler
set showmode                   " Don't show current mode down the bottom
set gcr=a:blinkon0             " Disable cursor blink
set cursorline

" show file path in title bar
set title
set titlestring=%F

" ,hi           - show current Highlight group. if you don't like the color of something, use this, then use hi! link [groupname] [anothergroupname] in your vimrc.after to remap the color. You can see available colors using :hi

" Screen Drawing
"===============

set linespace=0             " don't insert extra pixel lines betweens rows
set lazyredraw              " do not redraw while running macros
" set ttyfast                 " fast redraw screen

" Character Display
"==================

" set list                " show nonprinting chars (set with listchars)
nnoremap <leader>$ :set list!<cr>
" set list listchars=extends:», tab:▸, trail:›
" set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:<
set listchars=tab:,.,trail:.,extends:#,nbsp:. " Highlight problematic whitespace


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

" Custom color overwrites
hi! link txtBold Identifier
hi! link zshVariableDef Identifier
hi! link zshFunction Function
hi! link MatchParen DiffText

hi! link SignColumn   LineNr
hi! link VertSplit   LineNr
hi! link ShowMarksHLl DiffAdd
hi! link ShowMarksHLu DiffChange
hi! ColorColumn ctermfg=0 ctermbg=8

" Get the current highlight group. Useful for then remapping the color
map ,hi :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" . " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>

" POWERLINE
let g:Powerline_symbols='fancy'
let g:Powerline_theme='istib'
let g:Powerline_colorscheme='istib'


set linebreak                  " Wrap lines at convenient points

" Unfuck my screen
nnoremap <leader>u :syntax sync fromstart<cr>:redraw!<cr>

" show/hide status bar.
" FIXME: make a single key toggle
nmap <silent> <F12> :set laststatus=2<cr>
nmap <silent> <F11> :set laststatus=0<cr>

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


" }}}

" BINDINGS {{{

" ========================================
" RSI Prevention - keyboard remaps
" ========================================
" Certain things we do every day as programmers stress
" out our hands. For example, typing underscores and
" dashes are very common, and in position that require
" a lot of hand movement. Vim to the rescue

" Fix for navigating long lines
" Wrapped lines goes down/up to next row, rather than next line in file.
nnoremap j gj
nnoremap k gk

" saner ESC key (and stay at position instead of going back to previous caret - strange vim default behaviour)
inoremap jj <esc>l
inoremap jk <esc>l
cnoremap jj <c-c>

" add space at cursor without exiting normal mode
nnoremap ,<space> i<space><esc>
nnoremap ,,<space> a<space><esc>

" avoid shift key for invoking command line
nnoremap ; :
" but preserve g; shortcut (remember: opposite is g,)
nnoremap g: g;

" Easier within-line navigation, and I never use the default behavior
noremap H ^
noremap L $
vnoremap L g_

" so we can $ to duplicate % (just because i keep confusing them)
noremap $ %

" " painless moving in insert mode
inoremap <c-l> <right>
inoremap <c-h> <left>

" or using traditional terminal mapping of swapping two letters
inoremap <c-t> <esc>hxpa
nnoremap <c-t> hhxpl

" flip last two words  (simpler option is 'dwwP')
inoremap ,<c-t> <esc>BE"zdiWb"zPa<space><esc>$
nnoremap ,<c-t> BE"zdiWb"zPa<space><esc>$

" TODO: use anonymous paste register to avoid polluting yankring

nnoremap Y y$

" Visual mode: V
" Duplicate a selection
vnoremap V y'>p

" append to current word (note: c-a would make a little bit more sense as it'd be consistent with a and A, but it is reserved to tmux)
inoremap <c-e> <esc>Ea
" nnoremap <c-e> Ea

" scrolling up/down with capital letters too (avoids joining lines instead) in visual mode
vnoremap J j
vnoremap K k

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" delete last character of word
nnoremap d> ex

" delete last character of line
nnoremap d. $x0

" delete first character of line
nnoremap d, 0x$

" clear line and stay in normal mode
nnoremap dD cc<esc>

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

" }}}

" CONFIG {{{

nmap ,ve :tabedit ~/dotfiles/vim/vimrc.vim<cr>
nmap ,V :NERDTree ~/dotfiles/vim/bundle/<cr>
" automatically reread Vim's configuration after writing it
" autocmd! BufWritePost ~/dotfiles/vim/vimrc.vim source ~/dotfiles/vim/vimrc.vim

nmap ,ze :tabedit ~/dotfiles/zsh/zshrc<cr>
nmap ,Ge :tabedit ~/dotfiles/git/gitconfig<cr>
nmap ,te :tabedit ~/dotfiles/tmux/tmux.conf<cr>

" ,vc - (Vim Command) copies the command under your cursor and executes it in vim
vnoremap <leader>vc y:execute @@<cr>
nnoremap <leader>vc ^vg_y:execute @@<cr>
nmap <silent> ,VC :so %<CR>

"}}}

" UNDO {{{

set undodir=~/.vim/backups     " Keep undo history across sessions, by storing in file.
set undofile

"}}}

" CLOJURE {{{

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

" background shading for comments
hi def link clojureComment     Search


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
" }}}

" COFFEESCRIPT {{{

autocmd BufRead *.coffee set filetype=coffee
autocmd BufWritePost,FileWritePost *.coffee :silent !iced -c -b <afile>

autocmd FileType javascript,coffee setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType javascript,coffee setlocal makeprg=node\ %:r
" autocmd BufRead *.coffee setlocal makeprg=node\ %:r

" convert js into coffeescript on the fly (using eponymous npm bundle)
nmap ,jc :Js2Coffee<cr>
vmap ,jc :Js2Coffee<cr>

" hide JS files
" let NERDTreeIgnore+=['\.js']

" jump to JS file number
command -nargs=1 C CoffeeCompile | :<args>

" build ctags for coffeescript only
nnoremap ,tT :!ctags -R --languages==coffee,javascript --exclude=libs/* --c++-kinds=+p --fields=+iaS --extra=+q .<CR><CR>

" Coffeescript tagbar support
let g:tagbar_type_coffee = {
  \ 'ctagstype' : 'coffee',
  \ 'kinds' : [
  \   'n:namespace',
  \   'c:class',
  \   'o:object',
  \   'm:methods',
  \   'f:functions',
  \   'i:instance variables',
  \   'v:var:1',
  \ ],
  \ 'sro' : ".",
  \ 'scope2kind' : {
  \   'o' : 'object',
  \   'f' : 'function',
  \   'm' : 'method',
  \   'v' : 'var',
  \   'i' : 'ivar'
  \ },
  \ 'kind2scope' : {
  \  'function' : 'f',
  \  'method' : 'm',
  \  'var' : 'v',
  \  'ivar' : 'i',
  \ 'object' : 'o'
  \}
\ }

" }}}

" CSS {{{

" Compile LessCSS on save
" autocmd BufWritePost,FileWritePost *.less :silent !lessc <afile> <afile>:p:r.css

" :ColorToggle - turn on #abc123 color highlighting

autocmd FileType css  setlocal foldmethod=indent shiftwidth=2 softtabstop=2


" }}}

" DBEXT {{{

vnoremap <leader>es :let g:dbext_default_use_sep_result_buffer=1<CR>:DBExecVisualSQL<cr>
nnoremap <leader>es :let g:dbext_default_use_sep_result_buffer=1<CR>:DBExecSQLUnderCursor<cr>


vnoremap <leader>ES :let g:dbext_default_use_sep_result_buffer=0<CR>:DBExecVisualSQL<cr>
nnoremap <leader>ES :let g:dbext_default_use_sep_result_buffer=0<CR>:DBExecSQLUnderCursor<cr>

nnoremap <leader>SLT :let g:dbext_default_use_sep_result_buffer=0<CR>:DBListTable<cr>
"
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

" }}}

" DJANGO {{{

" autocmd FileType python set ft=python.django " For SnipMate
" autocmd BufRead,BufNewFile *.html set filetype=html
" prevent automatic django filetype (for snippets)
nmap _dt :set ft=htmldjango.html<cr>
nmap _ht :set ft=html<cr>

" surround helpers for templating:
" s- :     <% %>
" s= :     <%= %>

" django testing from within vim. not 'tested'(!) yet
" see http://gremu.net/blog/2010/integrate-your-python-test-runner-vim/

let g:makeprg_django_app = 'python\ manage.py\ test\ -v\ 0'
let g:makeprg_django_project = 'python\ manage.py\ test\ -v\ 0'
set errorformat=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

function! RunTestsForFile(args)
    if @% =~ '\.py$'
        let expandstr = '%:p:h' " dirname
        while expand(expandstr) != '/'
            let testpath = expand(expandstr)
            if len(getfperm(testpath . '/tests')) > 0 || len(getfperm(testpath . '/tests.py')) > 0
                call RunTests(expand(expandstr . ':t'), a:args)
                return
            endif
            let expandstr .= ':h'
        endwhile
    endif
    call RunTests('', a:args)
endfunction

function! RunTests(target, args)
    silent ! echo
    silent ! echo -e "\033[1;36mRunning all unit tests\033[0m"
    silent w
    if len(a:target)
        execute 'set makeprg=' . g:makeprg_django_app
    else
        execute 'set makeprg=' . g:makeprg_django_project
    endif
    exec "make! " . a:target . " " . a:args
endfunction

function! JumpToError()
    let has_valid_error = 0
    for error in getqflist()
        if error['valid']
            let has_valid_error = 1
            break
        endif
    endfor
    if has_valid_error
        for error in getqflist()
            if error['valid']
                break
            endif
        endfor
        let error_message = substitute(error['text'], '^ *', '', 'g')
        silent cc!
        exec ":sbuffer " . error['bufnr']
        call RedBar()
        echo error_message
    else
        call GreenBar()
        echo "All tests passed"
    endif
endfunction

function! RedBar()
    hi RedBar ctermfg=white ctermbg=red guibg=red
    echohl RedBar
    echon repeat(" ",&columns - 1)
    echohl
endfunction

function! GreenBar()
    hi GreenBar ctermfg=white ctermbg=green guibg=green
    echohl GreenBar
    echon repeat(" ",&columns - 1)
    echohl
endfunction

" nnoremap <leader>a :call RunTests('', '')<cr>:redraw<cr>:call JumpToError()<cr>
" nnoremap <leader>y :call RunTestsForFile('--failfast')<cr>:redraw<cr>:call JumpToError()<cr>

" }}}

" GIT {{{

nnoremap <F3> :Gstatus<cr>
nnoremap <leader>gd  :Gdiff<cr>
nnoremap <leader>gS  :Gdiff stash@{}<left>
nnoremap <leader>gl  :Glog<cr><cr><leader>fo
nnoremap <leader>GL  :Git log -p %<cr>
nnoremap <leader>gG  :Glog --grep=
nnoremap <leader>gC  :Glog -S
nnoremap <leader>gL  :Gitv<cr>
nnoremap <leader>GL  :Gitv!<cr>
vnoremap <leader>gL  :Gitv! --all<cr>
nnoremap <leader>gw  :Gwrite<cr>
nnoremap <leader>gE  :Gedit<space>
nnoremap <leader>ge  :Gvsplit<space>
nnoremap <leader>gs  :Gsplit<space>
nnoremap <leader>gr  :Gread<cr>
nnoremap <leader>ga  :Git add --all<cr>:Gcommit<cr>
nnoremap <leader>GA  :Git add .<cr>
nnoremap <leader>gb  :Gblame<cr>
nnoremap <leader>gh  :Gbrowse<cr>
vnoremap <leader>gh  :Gbrowse<cr>
nnoremap <leader>gg  :Ggrep<space>
nnoremap <leader>gco :Gcheckout<cr>
nnoremap <leader>gci :Gcommit<cr>
nnoremap <leader>gm  :Gmove<cr>
nnoremap <leader>gR  :Gremove<cr>
nnoremap <leader>gp  :Git push<CR>
nnoremap <leader>gP  :Git pull<CR>
nnoremap <leader>grsh :Git reset --hard<cr>
nnoremap <leader>go :Git oops<cr>

" Send visual selection to gist.github.com as a private, filetyped Gist
" Requires the gist CLI
" vnoremap <leader>G :w !gist -p -t %:e \| pbcopy<cr>
" vnoremap <leader>UG :w !gist -p \| pbcopy<cr>

if has("autocmd")
  " Auto-close fugitive buffers
  autocmd BufReadPost fugitive://* set bufhidden=delete

  " Navigate up one level from fugitive trees and blobs
  autocmd User fugitive
    \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
    \   nnoremap <buffer> u :edit %:h<CR> |
    \ endif
endif

" SHORTCUTS WITHIN :Gstatus
"
"   <C-N> next file
"   <C-P> previous file
"   <CR>  |:Gedit|

"   -     |:Git| add
"   -     |:Git| reset (staged files)

"   C     |:Gcommit|
"   cA    |:Gcommit| --amend --reuse-message=HEAD
"   ca    |:Gcommit| --amend

"   D     |:Gdiff|
"   ds    |:Gsdiff|
"   dp    |:Git!| diff (p for patch; use :Gw to apply)
"   dp    |:Git| add --intent-to-add (untracked files)
"   dv    |:Gvdiff|
"   O     |:Gtabedit|
"   o     |:Gsplit|
"   p     |:Git| add --patch
"   p     |:Git| reset --patch (staged files)
"   q     close status
"   R     reload status
"   S     |:Gvsplit|


" Open a split for each dirty file in git
function! OpenChangedFiles()
  only " Close all windows, unless they're modified
  let status = system('git status -s | grep "^ \?\(M\|A\)" | cut -d " " -f 3')
  let filenames = split(status, "\n")
  if len(filenames) > 0
    exec "edit " . filenames[0]
    for filename in filenames[1:]
      exec "sp " . filename
    endfor
  end
endfunction
command! OpenChangedFiles :call OpenChangedFiles()

nnoremap ,GO :OpenChangedFiles<CR>

" stop gitv messing with my window navigation
let g:Gitv_DoNotMapCtrlKey = 1

cabbrev git Git

augroup ft_fugitive
    au!
    au BufNewFile,BufRead .git/index setlocal nolist
augroup END

" autocmd FileType gitcommit DiffGitCached | wincmd p

"}}}

" HTML {{{

" URL encode (unimpaired)
" {Visual}[u
" XML encode (unimpaired)
" {Visual}[x

" ,he - Html Escape
" ,hu - Html Unescape

" :ColorToggle - turn on #abc123 color highlighting (useful for css)

" shortcut to operate on tag inside
omap t it


" }}}

" JSON {{{

" Prettify JSON files
autocmd BufRead,BufNewFile *.json set filetype=json
autocmd Syntax json source ~/.vim/bundle/json/syntax/json.vim

" format JSON
nmap ,jp :%!python -m json.tool<cr>:set ft=json<cr>
vmap ,jp :!python -m json.tool<cr>:set ft=json<cr>

augroup json_autocmd
  autocmd!
  autocmd FileType json set autoindent
  autocmd FileType json set formatoptions=tcq2l
  autocmd FileType json set textwidth=78 shiftwidth=2
  " autocmd FileType json set softtabstop=2 tabstop=8
  autocmd FileType json set expandtab
  autocmd FileType json set foldmethod=syntax
augroup END

" }}}

" LATEX {{{

au BufReadPost *.tex set syntax=tex
" instead of plaintex

let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'

hi! link initexComment Structure

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

set grepprg=grep\ -nH\ $*
set iskeyword+=:

" some indentation for style
" set sw=2

" customize keybindings a little
" nmap ,c i\cite{}<F4>
imap <silent> <F4> <Esc>:call Tex_Complete("default","text")<CR>

" wrap selection in cite call
vmap ,c S}i\cite<esc>

" restore folds automatically
" au BufWinLeave *.tex silent mkview!
" au BufWinEnter *.tex silent loadview
" TODO: make this on 'enter file' not buffer

" shape single quotations in latex compliant
nmap ,' F'r`
nmap ,,' F"r`a`<esc>f"xa''<esc>

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

" }}}

" MARKDOWN {{{

" Sane(!) text-editing options

augroup ft_markdown
    au!

    autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
    autocmd BufNewFile,BufRead *.txt set filetype=markdown

    autocmd FileType markdown set formatoptions+=2,l
    autocmd FileType markdown set formatoptions-=c
    autocmd FileType markdown set smartcase
    autocmd FileType markdown set textwidth=80

    autocmd FileType markdown,tex set noautoindent nosmartindent nocindent linebreak nonumber nofoldenable
    " autocmd FileType markdown,tex silent setlocal spell spelllang=en_us
    autocmd FileType markdown,tex let &scrolloff=999-&scrolloff

    " margins and no statusline
    autocmd FileType markdown,tex hi! link FoldColumn Normal
    autocmd FileType markdown,tex set laststatus=0
    autocmd FileType markdown,tex silent set foldcolumn=5
    autocmd FileType markdown,tex silent set wrapmargin=20

    " headings:
    autocmd FileType markdown nnoremap <buffer> <leader>1 yypVr=
    autocmd FileType markdown nnoremap <buffer> <leader>hh yypVr=
    autocmd FileType markdown nnoremap <buffer> <leader>2 yypVr-
    autocmd FileType markdown nnoremap <buffer> <leader>3 I### <ESC>

    " separation line:
    autocmd FileType markdown nnoremap <buffer> <leader>ll o* * *<cr><cr><esc>

    " markdown links: (first yank the URL and then select the linked text in visual mode, then hit ',ml')
    autocmd FileType markdown vnoremap <buffer> <leader>ml S]%a()<esc>P<esc>

augroup END


hi! link markdownBold Function
hi! link markdownItalic Function

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

" }}}

" R {{{

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

imap <C-f> <Plug>RCompleteArgs
map <silent> <Leader>rL :call RAction("levels")<CR>
map <silent> <Leader>rT :call RAction("tail")<CR>
map <silent> <Leader>rH :call RAction("head")<CR>
map <silent> <Leader>rN :call RAction("names")<CR>
map <silent> <Leader>rC :call RAction("class")<CR>
map <silent> <Leader>rD :call RAction("length")<CR>

let vimrplugin_underscore = 0

" replace '<' with '<<>>=\n@' in rnoweb files
let vimrplugin_rnowebchunk = 1

" more explicit shading for Sweave sections
hi! link rnowebDelimiter pandocStrikeoutTable

" background shading for comments
" hi def link rComment     Search

" fold SWeave chunck
nmap zZ ?<<<CR>V/@<CR>zf



" surround selection as argument to function call
:vmap ,b S)i

" surround selection with braces for LaTex section call
:vmap ,B SBi

" lines beginning with a single # are aligned at a specific column
let r_indent_comment_column = 30


augroup filetypedetect
  au! BufRead,BufNewFile *.r         setfiletype r
  au! BufRead,BufNewFile *.R         setfiletype r
  " syntax highlighting for SWeave
  au! BufRead,BufNewFile *.Rnw       setf noweb
augroup END


" }}}

" SURROUND {{{

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
map ,# ysiw#
" ,# Surround a word with quotes
map ," ysiw"


" wrap words
nmap ,( vES)
nmap ,) v$hS)
nmap ," vES"

" handlebar surround
vmap ,} S}gvS}

" nice for lisp dialects
imap ,( <esc>vES)i
imap ,) <esc>v$hS)i
imap ," <esc>vES"i
imap ,' <esc>vES'i

" }}}

" TASKPAPER {{{

let g:task_paper_follow_move = 0
let g:task_paper_date_format = "%d-%m-%Y"

hi! link taskpaperDone Comment
hi! link taskpaperCancelled Comment

" ,tT     Show tasks marked as today
" ,td     Mark task as done
" ,tx     Mark task as cancelled
" ,tt     Mark task as today
" ,tm     Move task to specified project
" ,tP     Focus on the current project
" ,tg     Go to specified project
" ,t/     Search for items including keyword
" ,ts     Search for items including tag
" ,tD     Archive @done items
" ,tX     Show tasks marked as cancelled
" ,tp     Fold all projects
" ,t.     Fold all notes
" ,tj     Go to next project
" ,tk     Go to previous project

"}}}

" TIMESTAMP {{{

" timestamp.vim
let g:timestamp_modelines=15
let g:timestamp_rep='%Y-%m-%d'
let g:timestamp_regexp='\c\%(\<\%(last \)\?\%(changed?\|modified\):\s\+\)\@<=\%(\d\{4}\D.\{-}\d\{2}\|TIMESTAMP\)'

au filetype md let g:timestamp_regexp='\c\%(date\):\s\+\)\@<=\%(\d\{4}\D.\{-}\d\{2}\|TIMESTAMP\)'

" }}}

" BUFFERS {{{

" buffers hidden, not closed, when abandoned
set hidden

" open new windows right and below
set splitright
set splitbelow

" go file, but avoiding error message about leaving unsaved buffer
" map gf :edit <cfile><CR>
map gf :wincmd f<cr>

" use ,gf to go to file in a vertical split
nnoremap <silent> ,gf :vertical botright wincmd f<CR>

nmap <c-n> :bnext<cr>
nmap <c-p> :bprev<cr>

nmap <silent> :' :vnew<cr>
nmap <silent> :" :vnew<cr><c-w>o
nmap <silent> ;' :vnew<cr><c-w>o

" GOLDEN RATIO

let g:golden_ratio_autocommand = 1

"NARROW REGION

let g:nrrw_rgn_vert = 1
let g:nrrw_rgn_wdth = 50

" no highlighting of narrowed region in source file
let g:nrrw_rgn_nohl = 1
let g:rrrw_rgn_protect = 0
" preferred position
let g:nrrw_topbot_leftright = 'botright'

" open selected in zoomed narrowregion buffer and center in buffer (used to
" focus on current paragraph, eg.)
vmap <silent> ;" :NR<cr><c-w>ogg14O<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>0o<esc>G2o<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>13o<esc>16G0<esc>
nmap <silent> ;" vip:NR<cr><c-w>ogg14O<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>0o<esc>G2o<esc>50i<space><esc>i~~~~~~~~~~~~~~~~~<esc>13o<esc>16G0
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

" resizing shortcuts
nnoremap _ <C-w>-
nnoremap + <C-w>+

" LUSTYJUGGLER AND LUSTYEXPLORER
"================================
"
let g:LustyJugglerDefaultMappings = 0
let g:LustyExplorerDefaultMappings = 0
let g:LustyJugglerShowKeys = 'a'
let g:LustyJugglerAltTabMode = 1
let g:LustyJugglerSuppressRubyWarning = 1

" NB: line numbers below apply to version 4.3

" swapped bindings for 'ascend one directory at prompt' (C-w) and 'clear input' (C-u) directly in the plugin file (because it accessed raw binding codes). relevant lines in plugin file: 903 and 848

" TODO: same for 'open selected match in a new h[o]rizontal split' (C-o) to C-s

" added <space> mapping to open file/folder (add '32' on line 847,  and change 32 to 33 on line 840)
" TODO: fork original source!!

nmap <silent> :j :LustyBufferExplorer<CR>
nnoremap <c-g> :CtrlPMixed<cr>

" quick switch to last (Cmd-Tab like)
nmap <silent> :l :LustyJugglePrevious<CR>
nmap <silent> :: :LustyFilesystemExplorerFromHere<CR>

"Move back and forth through previous and next buffers
nnoremap <silent> xz :bp<CR>
nnoremap <silent> zx :bn<CR>

" CTRL-P
"========

let g:ctrlp_custom_ignore = {
    \ 'file': '\.exe$\|\.so$\|\.dll$' }

let g:ctrlp_user_command = {
    \ 'types': {
        \ 1: ['.git', 'cd %s && git ls-files'],
    \ },
    \ 'fallback': 'find %s -type f'
\ }

let g:ctrlp_working_path_mode = 2

" ctrl + m for MRU (which FSR also maps to enter, neat)
nmap <c-f> :CtrlPMRU<cr>

" TIP: create new files using C-E from within CtrlP picker

" TODO: nnoremap :<space>

" Default to file searches
let g:ctrlp_by_filename = 1
" switch to path search with ctrl-d inside plugin

" We don't want to use Ctrl-p as the mapping because
" it interferes with YankRing (paste, then hit ctrl-p)
let g:ctrlp_map = ',0'

let g:ctrlp_open_new_file = 'v'
let g:ctrlp_open_multiple_files = 'v'

" open switcher with project root directory (determined by location of .git folder)
let g:ctrlp_working_path_mode = 2

" open files in existing buffers if they are already opened
let g:ctrlp_switch_buffer = 0

let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_cache_dir = $HOME.'/.cache/ctrlp'
let g:ctrlp_dotfiles = 1
let g:ctrlp_max_depth = 20
let g:ctrlp_mruf_max = 200

" NERDTREE

" Make nerdtree look nice
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:NERDTreeWinSize = 30
let NERDTreeHijackNetrw=1
let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr']

let NERDTreeMouseMode=2
let NERDTreeShowHidden=1


nmap <F1> :NERDTreeTabsToggle<cr>

noremap <leader>. :execute "NERDTree ".expand("%:p:h")<cr>
" noremap <Leader>z :edit .<cr>

noremap <Leader>b :NERDTreeFromBookmark<space>
noremap <Leader>B :CtrlPBookmarkDir<cr>
nmap <silent> <leader>i :LustyFilesystemExplorer ~/Inbox<CR>

" current file in NERDTree
noremap <Leader>f :NERDTreeFind<cr>

" Auto open nerd tree on startup
let g:nerdtree_tabs_open_on_gui_startup = 0
" Focus in the main content window
let g:nerdtree_tabs_focus_on_files = 1

" }}}

" CMDLINE {{{

set history=1000               " Store lots of :cmdline history
set cmdheight=2                " avoid 'Press ENTER to continue'

" Leave the return key alone when in command line windows, since it's used
" to run commands there.

" autocmd! CmdwinEnter * :unmap <cr>
" autocmd! CmdwinLeave * :call MapCR()
" function! MapCR()
"   nnoremap <cr> :nohlsearch<cr>
" endfunction
" call MapCR()

" @: to repeat the last executed command


" Command line:
"
" ctrl-w          delete word
" ctrl-u          delete line
" ctrl-v          enter ex mode
"
" Registers
" =========
" "/  Last search
" "_  Black hole register
" ":  most recent executed command
" "%  name of current file
" ".  last inserted text


cmap <c-h> <left>
cmap <c-l> <right>
cmap <c-j> <c-n>
cmap <c-k> <c-p>

" SUPER MODE

" CTRL-R CTRL-W   : pull word under the cursor into a command line or search
" CTRL-R CTRL-A   : pull whole word including punctuation
" CTRL-R -        : pull small register
" CTRL-R [0-9a-z] : pull named registers
" CTRL-R %        : pull file name (also #)

" insert result from command into buffer!
" :r! cmd
" ex :r! ls | grep pattern

"  Entering !! in normal mode is translated to  :.!
"  Appending a command sends the current line to the command replacing it with command's result
" !!date              : Replace current line with date
" !!which command     : Replace current line with the absolute path to command
" !!tr -d AEIO        : translate current line deleting all occurrences of A, E, I, and O from the current line


" stolen from pope's vim-rsi
cnoremap <expr> <C-D> getcmdpos()>strlen(getcmdline())?"\<Lt>C-D>":"\<Lt>Del>"

inoremap <expr> <C-E> col('.')>strlen(getline('.'))?"\<Lt>C-E>":"\<Lt>End>"

noremap! <expr> <SID>transposition getcmdpos()>strlen(getcmdline())?"\<Left>":getcmdpos()>1?'':"\<Right>"
noremap! <expr> <SID>transpose "\<BS>\<Right>".matchstr(getcmdline()[0 : getcmdpos()-2], '.$')
cmap   <script> <C-T> <SID>transposition<SID>transpose

" use ESC to exit cmdline window
autocmd CmdwinEnter * nnoremap <buffer> <esc> :q<cr>

" }}}

" COMMENTING {{{

" comment paragraph
" nmap <c-\> \\ip

nmap <c-\> \\\
vmap <c-\> \\\

" <c-/>a    comment as

" yank and past visual before toggle comment
vmap gyy ygvgc'>gp'.
nmap gyy mz:t-1<cr>gCc`zmz
imap gyy <esc>:t-1<cr>gCcgi
" Using :t-1 instead of yyP to preserve registers

" }}}

" COMPLETION {{{

" ^X ^L           whole line completion!

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

" Nicer highlighting of completion popup
highlight Pmenu ctermfg=234 ctermbg=24


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

autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'

" Filename completion
inoremap <c-f> <c-x><c-f>

" Better Completion
set completeopt=longest,menuone,preview

" }}}

" DIFF {{{

set fillchars=diff:⣿,vert:│
" set fillchars=diff:_,vert:│

" IN VIMDIFF BUFFER

" do - diff obtain
" dp - diff put
" [c - previous difference
" ]c - next difference

" in three-way vimdiff

" :diffget //2 - fetches the hunk from the target parent (on the left)
" :diffget //3 - fetches the hunk from the merge parent (on the right)


nmap d2 :diffget //2<cr>
nmap d3 :diffget //3<cr>

nmap ,D :windo diffthis<cr>

" " Linediff plugin
" nmap ,d :Linediff<cr>
" nmap ,,D :LinediffReset<cr>
" vmap ,d :Linediff<cr>
" vmap ,,D :LinediffReset<cr>

" Close any corresponding diff buffer
function! MyCloseDiff()
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
nnoremap <silent> <Leader>gD :call MyCloseDiff()<cr>

" }}}

" MARKS {{{

set viminfo='100,f1            " Save up to 100 marks, enable capital marks

" using vim-signature plugin

" remove s/S from defaults
let g:SignatureIncludeMarks = 'abcdefghijklmnopqrtuvwxyzABCDEFGHIJKLMNOPQRTUVWXYZ'

" lighter background color for marks
highlight SignColumn ctermbg=0 ctermfg=9

" }}}

" MOTION {{{

" join lines from insert mode
imap <c-j> <esc>Ji

" bubble up and down (unimpaired)
" [e and ]e

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

" http://stackoverflow.com/questions/6926034/creating-a-mapping-for-insert-mode-but-not-for-autocomplete-submode/6926691#6926691
inoremap <expr> <c-e> pumvisible() ? "\<c-e>" : "\<c-o>A"
inoremap <C-a> <C-o>I

" }}}

" NAVIGATION {{{

" prefer exact caret location to mere line jumps
nmap '' ``

" select last change
nmap gV `[v`]

" jump to actual paragraph edges, not the empty lines between
nnoremap ,( {{j
nnoremap ,) }}k

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

" DVM.VIM plugin
"
nmap <c-e><c-r> <Plug>DWMRotateCounterclockwise
nmap <C-r><C-e> <Plug>DWMRotateClockwise

nmap <c-e><c-n> <Plug>DWMNew
nmap <c-e><c-c> <Plug>DWMClose
nmap <c-e><c-f> <Plug>DWMFocus

nmap <c-e><c-l> <Plug>DWMGrowMaster
nmap <c-e><c-h> <Plug>DWMShrinkMaster
"
" }}}

" NUMBERS {{{

" Motion for numbers.  Handy for CSS. 
onoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
xnoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
onoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
onoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>

function! s:NumberTextObject(whole)
    normal! v
    while getline('.')[col('.')] =~# '\v[0-9]'
        normal! l
    endwhile
    if a:whole
        normal! o
        while col('.') > 1 && getline('.')[col('.') - 2] =~# '\v[0-9]'
            normal! h
        endwhile
    endif
endfunction

" }}}

" PYTHON {{{

augroup ft_python
    au!
    autocmd BufNewFile,BufRead *.py set filetype=python
    autocmd FileType python set expandtab
    autocmd FileType python set tabstop=4
    autocmd FileType python set softtabstop=4
    autocmd FileType python set shiftwidth=4
    " compile
    autocmd Filetype python nmap <buffer> <leader>cc :w<Esc>:!python %<CR>
augroup END

autocmd BufRead *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"

" }}}

" SEARCH {{{

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v

set incsearch           " Find the next match as we type the search
set nohlsearch          " Highlight searches by default
set ignorecase          " case insensitive search for lowercase...
set smartcase           " ...but if mixed case, go case-sensitive
set gdefault            " Add the g flag to search/replace by default

" Open a Quickfix window for the last search.
nnoremap <silent> KS :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

nnoremap <silent> KB :LustyBufferGrep<CR>

"
" Use Ack instead of Grep when available
if executable("ack")
  set grepprg=ack\ -H\ --nogroup\ --nocolor
endif

nnoremap KC :GitGrep <c-r><c-w><CR>
vnoremap KC "sy:GitGrep "<c-r>s"<CR>

nnoremap KA :GitGrep ""<left>
nnoremap KL :GitGrep "<c-r>/"<cr>

" HIGHLIGHTING

" toggle highlighting on/off
nnoremap <silent> ,h :set hlsearch! hlsearch?<CR>

" clear highlights
noremap <silent> <leader>/ :nohls<CR>


" highlight whole line of search result
nnoremap ,/l :let @/ = '.*\%(' . @/ . '\m\).*'<cr>
" or just word
nnoremap ,/w :let @/ = '(@/)'<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" quick replace
nnoremap RR :%s//<left>
nnoremap rr :%s//<left>
vnoremap RR :s//<left>
vnoremap rr :s//<left>

" replace current word
nnoremap RC :%s/<c-r><c-w>/
nnoremap rc :%s/<c-r><c-w>/
vnoremap RC :s/<c-r><c-w>/
vnoremap rc :s/<c-r><c-w>/

" replace yanked
nnoremap RY :%s/<c-r>"/<c-r>"
nnoremap ry :%s/<c-r>"/
vnoremap RY :s/<c-r>"/<c-r>"
vnoremap ry :s/<c-r>"/

" replace visual
vnoremap RV "xy:%s/<c-r>x/<c-r>x
vnoremap rv "xy:%s/<c-r>x/

" replace from last search
" nnoremap <Leader>s :%s///<Left>
" vnoremap <Leader>s :s///<Left>

" substitute repeat
nnoremap & :&&<cr>
xnoremap & :&&<cr>

" Visual Mode */#

function! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction

vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR><c-o>

" toggle quickfix window
nmap <silent> ,fc :cclose<CR>
nmap <silent> ,fo :copen<CR>

" open quickfix at bottom of the window
let g:tlWindowPosition=1

" }}}

" SNIPPETS {{{

" Snippets are activated by Shift+Tab
" let g:snippetsEmu_key = "<S-Tab>"

" Insert date
iab <expr> dmy strftime("%d-%m-%Y")
iab <expr> ymd strftime("%Y-%m-%d")
iab <expr> mdy strftime("%m-%d-%Y")
iab <expr> ydm strftime("%Y-%d-%m")

" Insert date and time
iab <expr> dtt strftime("%Y-%m-%d %X %a")

" }}}

" SPELLING {{{

nmap <silent> ,,s :setlocal spell!<cr>

" auto-correct current word
" imap <silent> ,s <esc>z=1<cr><cr>ea
" nmap <silent> ,s z=1<cr><cr>
" ...and go to next spelling mistake
nmap <silent> ,S z=1<cr><cr>]s

" }}}

" SYNTAX {{{

syntax on
syntax enable                  " turn on syntax highlighting

nmap ,ft :set filetype=

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
nmap ,ns :SyntasticToggleMode<cr>

" Use tabs if it's a CSV file
autocmd BufNewFile,BufRead *.csv set noexpandtab nosmarttab

" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800

" }}}

" TABS {{{

nmap ,<tab> :tabnew<cr>

nmap ,tc :tabclose<cr>

" gt              Move to next tab
" gT              Move to previous tab
" #gt             Move to tab number #

" :tabmove        Move current tab to the end
" :tabmove 0      Move current tab to the beginning
" :tabmove 1      Move current tab to become the 2nd tab


" }}}

" TAGS {{{

" Rebuild ctags
nnoremap ,tt :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR><CR>

" Jump to next tag match
nmap ]t :bd<cr>:tnext<cr>
" Jump to previous tag match
nmap [t :bd<cr>:tprevious<cr>

"open the taglist (method browser) using ,t
nnoremap <silent> <F2> :TagbarToggle<CR><C-l>

let g:tagbar_width=36
let g:tagbar_autoclose = 1

" omit odd empty lines
let g:tagbar_compact = 1

" capture cursor on popup
let g:tagbar_autofocus = 1

<<<<<<< HEAD
nnoremap :k :CtrlPTag<cr>
=======
let g:tagbar_ctags_bin="/usr/local/Cellar/ctags/5.8/bin/ctags"

noremap :k :CtrlPFunky<cr>
>>>>>>> dea23785fa7fc8dff2f4337b3a5f79847e7f2d30

" if 1 " has('eval')
"   let g:Tlist_Ctags_Cmd="ctags --exclude='*.js'"
" endif

set tags=./tags;
" Look for tags file in parent directories, upto "/"
set tags+=tags;/

" }}}

" TERMINAL {{{

" nmap ,ct :ConqueTermVSplit zsh<cr>

set shell=/bin/zsh

" }}}

" VIMWIKI {{{

" keep track of several wikis
" let g:vimwikilist=""
" ,ww index (,wt in tab)
" ,wi diary index
" ,w,w diary today (,w,t in tab)
" ,wh convert current page to HTML (,whh: and open)

" }}}

" TMUX {{{

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

" run tests in tmux
nmap <silent> ,w :w<cr>:RunLastVimTmuxCommand<cr>

" }}}

source ~/dotfiles/vim/personal.vim

" RSI bindings
imap <c-k> _
imap <c-j> ->
" easier way to reach for hash key
imap <c-d> #
inoremap £ #


" vim:set foldmethod=marks; set foldenable
