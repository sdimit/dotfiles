(defconst emacs-start-time (current-time))

;; (require 'cask "~/.emacs.d/Cask")
;; (cask-initialize)

;; init.el --- Where all the magic begins

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

(require 'package)
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"   . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

;;; Add support to package.el for pre-filtering available packages
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
           (funcall package-filter-function
                    (car package)
                    (package-desc-vers (cdr package))
                    archive))
    ad-do-it))

(defvar melpa-exclude-packages
  '(slime)
  "Don't install Melpa versions of these packages.")

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (not (string-equal archive "melpa"))
            (not (memq package melpa-exclude-packages))))))

(package-initialize)

(setq system-specific-config (concat dotfiles-dir system-name ".el")
      system-specific-literate-config (concat dotfiles-dir system-name ".org")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-literate-config (concat dotfiles-dir user-login-name ".org")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(setq elisp-source-dir (concat dotfiles-dir "src"))
(add-to-list 'load-path elisp-source-dir)
(add-to-list 'load-path (concat user-emacs-directory "config"))

;; don't launch GUI
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

(add-to-list 'load-path dotfiles-dir)
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'rainbow-blocks)

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)


(require 'init-elpa)
(require 'init-evil-mode)
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-smartparens)
(require 'init-appearance)
(require 'init-editing)
(require 'init-chords)
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
(require 'init-mail)
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
(require 'init-irc)
(require 'init-mail)
(require 'init-w3m)
(require 'init-deft)

(load custom-file 'noerror)
