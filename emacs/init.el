;(require 'cask "~/.emacs.d/Cask")
;(cask-initialize)

;; init.el --- Where all the magic begins

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

; don't launch GUI
(x-focus-frame nil)

; (add-to-list 'load-path (expand-file-name
;                        "lisp" (expand-file-name
;                                "org" (expand-file-name
;                                       "src" dotfiles-dir))))

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

(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
