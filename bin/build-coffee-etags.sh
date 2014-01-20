# find -L . -name '*.coffee' | grep -v 'node_modules' | xargs /usr/local/Cellar/emacs-mac/emacs-24.3-mac-4.5/bin/etags
find -L . -name '*.coffee' | grep -v 'node_modules' | xargs /usr/local/Cellar/ctags/5.8/bin/ctags -e
