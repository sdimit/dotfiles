hi! ColorColumn ctermfg=0 ctermbg=8
hi! Pmenu ctermfg=234 ctermbg=24
hi! SignColumn ctermbg=0 ctermfg=9

hi! link txtBold Identifier
hi! link zshVariableDef Identifier
hi! link zshFunction Function
hi! link MatchParen DiffText
hi! link SignColumn   LineNr
hi! link VertSplit   LineNr
hi! link ShowMarksHLl DiffAdd
hi! link ShowMarksHLu DiffChange
hi! link clojureComment     Search
hi! link initexComment Structure
hi! link markdownBold Function
hi! link markdownItalic Function
hi! link rnowebDelimiter pandocStrikeoutTable
hi! link taskpaperDone Comment
hi! link taskpaperCancelled Comment

" Enforce the colors set here
au VimEnter * so ~/dotfiles/vim/colors.vim
