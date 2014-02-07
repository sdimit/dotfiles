(add-to-list 'load-path (concat user-emacs-directory "config"))

(defvar starter-kit-packages (list
                              'auto-complete
                              'autopair
                              'browse-kill-ring
                              'css-mode
                              'exec-path-from-shell
                              'expand-region
                              'hl-line+
                              'latex-pretty-symbols
                              ;; 'mac-key-mode
                              'magit
                              'markdown-mode
                              'maxframe
                              'multiple-cursors
                              'pandoc-mode
                              'python-mode
                              'ipython
                              'jedi
                              'redo+
                              'ruby-mode
                              's
                              'smex
                              'typopunct
                              'yaml-mode
                              'yasnippet
                              'auctex
                              'r-autoyas
                              )
  "Libraries that should be installed by default.")

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("tromey"    . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; On your first run, this should pull in all the base packages.

;(if package-archive-contents
;    (package-refresh-contents)
;  (dolist (package starter-kit-packages)
;    (unless (or (member package package-activated-list)
;               (functionp package))
;      (message "Installing %s" (symbol-name package))
;      (package-install package))))

;; Make sure the PATH variable is set properly. (Uses exec-path-from-shell package.)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-elpa)
