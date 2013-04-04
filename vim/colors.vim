" language-specific
hi! link clojureComment     Search
hi! link initexComment Structure
hi! link markdownBold Function
hi! link markdownItalic Function
hi! link rnowebDelimiter pandocStrikeoutTable

" more 'peaceful' solarized
hi! ColorColumn ctermfg=0 ctermbg=8
hi! Pmenu ctermfg=234 ctermbg=24
hi! LineNr ctermbg=12
hi! link SignColumn CursorLineNr
hi! link IncSearch Search
hi! link LineNr VertSplit
hi! link TabLine LineNr
hi! link TabLineSel pandocStrikeoutTable
hi! link TabLineFill LineNr

hi! link Folded VertSplit
" hi! Folded ctermfg=235

hi! link VertSplit CursorLineNr
hi! link txtBold Identifier
hi! link zshVariableDef Identifier
hi! link zshFunction Function

hi! link MatchParen DiffText
hi! link ShowMarksHLl DiffAdd
hi! link ShowMarksHLu DiffChange

" Enforce the colors set here
au VimEnter * so ~/dotfiles/vim/colors.vim
