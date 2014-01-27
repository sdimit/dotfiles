
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

(setq popwin:special-display-config
      '(("*Help*"  :height 20)
        ("*Completions*" :noselect t :height 12)
        ("*Ido Completions*" :noselect t :height 12)
        ("*Messages*" :height 20)
        ("*Apropos*" :noselect t :height 30)
        ("*compilation*" :noselect t)
        ("*Backtrace*" :height 30)
        ("*Messages*" :height 30)
        ("*ag*" :noselect t :height 14)
        ("*Occur*" :height 14)
        ("*Python Doc*" :height 10)
        ("\\*ansi-term\\*.*" :regexp t :height 30)
        ("*shell*" :height 30)
        ("*gists*" :height 30)
        ("*sldb.*":regexp t :height 30)
        ("*nrepl-error*" :height 30 :stick t)
        ("*nrepl-doc*" :height 15 :stick t)
        ("*nrepl-src*" :height 30 :stick t)
        ("*nrepl-result*" :height 15 :stick t)
        ("*cider-result*" :height 15 :stick t)
        ("*cider-doc*" :height 15 :stick t)
        ("*nrepl-macroexpansion*" :height 30 :stick t)
        ("*Kill Ring*" :height 30)
        ("*helm lsgit*" :height 10)
        ("*helm mini*" :height 12)
        ("*helm bookmarks*" :height 12)
        ("*helm kill ring*" :height 8)
        ("*Helm Find Files*" :height 12)
        ("*Compile-Log*" :height 30 :stick t)
        ("*HTTP Response*" :height 20 :stick t)
        ("*nose-test*" :height 10 :stick t)
        ("*cake-test*" :height 10 :stick t)
        ("^\\*direx-jedi.*\\*$" :regexp t :width 35 :position left :stick t)
        ("*[Django: core (core.settings)] ./manage.py shell*" :height 30 :stick t)
        ("*Django: core (core.settings)" :height 20 :stick t)
        (direx:direx-mode :position left :width 30 :dedicated t)
        ("*git-gutter+-diff*" :height 10 :stick t)))

(provide 'init-popwin)
