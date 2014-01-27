(defconst emacs-start-time (current-time))

;; (require 'cask "~/.emacs.d/Cask")
;; (cask-initialize)

;; init.el --- Where all the magic begins

(add-to-list 'load-path (concat user-emacs-directory "config"))

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

; don't launch GUI
(x-focus-frame nil)

;; Common Lisp compatability
(require 'cl-lib)

;; Package Locations
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Load up Org Mode and Babel
;; load up the main file
;; org-mode windmove compatibility

(setq org-replace-disputed-keys t)
(require 'org)

  (setq dotfiles-dir (file-name-directory
                      (or load-file-name (buffer-file-name))))

  (add-to-list 'load-path dotfiles-dir)
  (setq autoload-file (concat dotfiles-dir "loaddefs.el"))
  (setq package-user-dir (concat dotfiles-dir "elpa"))
  (setq custom-file (concat dotfiles-dir "custom.el"))

(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))


(require 'init-elpa)
(require 'init-evil-mode)
(require 'init-ibuffer)
(require 'init-smartparens)
(require 'init-editing)
(require 'init-appearance)
(require 'init-command-mode)
(require 'init-navigation)
(require 'init-search)
(require 'init-help)
(require 'init-completion)
(require 'init-macros)
(require 'init-latex)
(require 'init-web-mode)
(require 'init-lisp)
(require 'init-clojure)
(require 'init-python)
(require 'init-helm)
(require 'init-git)
(require 'init-projectile)
(require 'init-pandoc)
(require 'init-org-mode)
(require 'init-files)
(require 'init-dired)
(require 'init-processes)
(require 'init-terminal)
(require 'init-10to8)
(require 'init-coffeescript)
(require 'init-json)
(require 'init-angular)
(require 'init-popwin)
(require 'init-pomodoro)
(require 'init-registers)
(require 'init-javascript)
(require 'init-experimental)


(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
(put 'narrow-to-region 'disabled nil)
