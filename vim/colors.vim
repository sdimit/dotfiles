" language-specific
hi! link clojureComment     Search
hi! link initexComment Structure
hi! link markdownBold Function
hi! link markdownItalic Function
hi! link rnowebDelimiter pandocStrikeoutTable
" more 'peaceful' solarized
hi! ColorColumn ctermfg=0 ctermbg=8
hi! CursorLineNr ctermfg=0 guifg=#657b83
hi! Pmenu ctermfg=234 ctermbg=24
hi! LineNr cterm=bold ctermfg=0 ctermbg=none
hi! Visual ctermfg=0 ctermbg=7
hi! VisualNOS ctermfg=0 ctermbg=7
hi! link SignColumn CursorLineNr
hi! link IncSearch Search
hi! link LineNr VertSplit
hi! link TabLine LineNr
hi! link TabLineSel pandocStrikeoutTable
hi! link TabLineFill LineNr
" hi! link Folded VertSplit
hi! link Folded pandocDefinitionBlock
hi! link VertSplit CursorLineNr
hi! link txtBold Identifier
hi! link zshVariableDef Identifier
hi! link zshFunction Function
hi! link MatchParen DiffText
hi! link ShowMarksHLl DiffAdd
hi! link ShowMarksHLu DiffChange
hi! NonText ctermfg=8 ctermbg=8
hi! link IndentGuidesOdd  NonText
hi! link IndentGuidesEven LineNr


" Enforce the colors set here in every buffer
au VimEnter * so ~/dotfiles/vim/colors.vim
" set background=light
"
" set statusline=
" set statusline +=%#Identifier#\ %n\ %*                      " buffer number
" set statusline +=%#Number#%y%*                              " file type
" set statusline +=%#String#\ %<%t%*                          " full path
" set statusline +=%#SpecialKey#%m%*                          " modified flag
" " set statusline +=%#Identifier#\ %{PomodoroStatus()}%*       " pomodoro status
" set statusline +=%#Identifier#%=%5l%*                       " current line
" set statusline +=%#SpecialKey#/%L%*                         " total lines
" " set statusline +=%#Identifier#%4v\ %*                       " virtual column number
" " set statusline +=%#SpecialKey#0x%04B\ %*                    " character under cursor

