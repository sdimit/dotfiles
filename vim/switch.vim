let g:switch_definitions =
      \ [
      \   ['true', 'false'],
      \   ['left', 'right'],
      \   ['div', 'span'],
      \   ['show', 'hide'],
      \   ['height', 'width'],
      \   ['top', 'bottom'],
      \   ['max', 'min'],
      \   ['login', 'signup']
      \ ]
" let g:switch_definitions =
"       \ [
"       \   {
"       \     'True':  'False',
"       \     'False': 'True',
"       \   },
"       \   {
"       \     'true':  'false',
"       \     'false': 'true',
"       \   },
"       \   {
"       \     'left':  'right',
"       \     'right': 'left',
"       \   },
"       \   {
"       \     '<div':  '<span',
"       \     '<span': '<div',
"       \   },
"       \   {
"       \     '\(.*\)->': '\1=>',
"       \     '\(.*\)=>': '\1->',
"       \   },
"       \   {
"       \     '@\(\w\+\) =': '\1:',
"       \     '\(\w\+\):': '@\1 =',
"       \   },
"       \ ]
