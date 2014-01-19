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

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
               (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
         (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.

(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents))
  (starter-kit-elpa-install))

;; Make sure the PATH variable is set properly. (Uses exec-path-from-shell package.)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-elpa)
