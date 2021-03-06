
(require 'pretty-mode)
(global-pretty-mode t)

(require 'pretty-symbols)

(require 'sublimity)
(sublimity-global-mode)
(sublimity-map)

;; Less flickery display
(setq redisplay-dont-pause t)

;; minimize fringe
(setq-default indicate-empty-lines nil)
(put 'upcase-region 'disabled nil)

(fringe-mode 4)

(setq initial-scratch-message "")
(setq inhibit-splash-screen t)
(setq redisplay-dont-pause t)
(set-scroll-bar-mode nil)
;;  (menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; (require 'light-symbol-mode)
;; (defface light-symbol-custom-face
;;  '(:inherit nil
;;    :foreground "white") "")

(setq light-symbol-face 'light-symbol-custom-face)

(light-symbol-mode)

(require 'highlight-sexp)

(defun light-theme ()
  (interactive)
  (let ((theme 'leuven))
    (load-theme theme t)
    (setq current-theme theme)
    (setq hl-sexp-background-color "#eee8d5")
    (highlight-sexp-mode nil)
    ;;(global-hl-line-mode nil)
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("black" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("black" bar))))

(defun dark-theme ()
  (interactive)
  (let ((theme 'solarized-dark))
    (load-theme theme t)
    (setq hl-sexp-background-color "#073642")
    (highlight-sexp-mode t)
    (setq evil-emacs-state-cursor '("yellow" box))
    (setq evil-normal-state-cursor '("white" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("white" bar))))


(setq evil-normal-state-tag   (propertize "<N>" 'face '((:background "black"  :foreground "grey" )))
      evil-visual-state-tag   (propertize "<V>" 'face '((:background "orange" :foreground "black")))
      evil-emacs-state-tag    (propertize "<E>" 'face '((:background "yellow" :foreground "black")))
      evil-insert-state-tag   (propertize "<I>" 'face '((:background "red"    :foreground "black")))
      evil-motion-state-tag   (propertize "<M>" 'face '((:background "blue")))
      evil-operator-state-tag (propertize "<O>" 'face '((:background "purple"))))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (not (member major-mode '(term-mode
                                          magit-commit-mode
                                          magit-status-mode
                                          magit-log-mode
                                          magit-diff-mode
                                          w3m-mode
                                          cider-repl-mode
                                          inferior-lisp-mode
                                          inferior-python-mode
                                          compilation-mode
                                          prodigy-view-mode
                                          calendar-mode
                                          bs-mode ; for ace-jump-line
                                          mu4e-headers-mode
                                          mu4e-compose-mode
                                          mu4e-view-mode
                                          gnus-article-mode
                                          gnus-summary-mode
                                          Custom-mode
                                          ibuffer-mode)))
              (setq show-trailing-whitespace t))))

(nmap (kbd "C-c C-w") 'whitespace-cleanup)

(dark-theme)
;; (light-theme)

;; more readable camelCase
(glasses-mode)
;; Subword mode (consider CamelCase chunks as words)
(global-subword-mode 1)

(setq evil-default-cursor t)

(defun set-default-font ()
  (interactive)
  (set-face-attribute 'default nil
                      :family "PragmataPro"
                      :height 180
                      :weight 'semi-light
                      :width 'normal))

(set-default-font)

(setq line-spacing 2)

;; No current line highlighting
;; (global-hl-line-mode nil)

(custom-set-faces
 '(region ((t (:background "alternateSelectedControlColor" :foreground "white" :background "#073642")))))

(defun fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
                       (if (frame-parameter f 'fullscreen) nil 'fullboth)))

(global-set-key (kbd "C-c C-f") 'fullscreen)

(add-hook 'after-make-frame-functions 'fullscreen)


(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(require 'smart-mode-line)

(sml/setup)

(add-to-list 'sml/hidden-modes " GitGutter")
(add-to-list 'sml/hidden-modes " Lisp Interaction")
(add-to-list 'sml/hidden-modes " Emacs-Lisp")
(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " Eldoc-eval")
(add-to-list 'sml/hidden-modes " PgLn")
(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " hl-sexp")
(add-to-list 'sml/hidden-modes " Fill")
(add-to-list 'sml/hidden-modes " AC")
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " Projectile")
(add-to-list 'sml/hidden-modes " Wrap")
(add-to-list 'sml/hidden-modes " vl")
(add-to-list 'sml/hidden-modes " Undo-Tree")

(add-to-list 'sml/replacer-regexp-list '("^~/10to8/Native/native/src/core/"      ":DT:"))
(add-to-list 'sml/replacer-regexp-list '("^~/10to8/Native/native/src/apps/jeltz/app/"      ":Jeltz:"))
(add-to-list 'sml/replacer-regexp-list '("^~/10to8/Native/native/src/apps/colin/app"     ":Colin:"))

;; (powerline-center-evil-theme)

;; Nicer scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

(setq minimap-window-location 'right)

(defun disable-all-pretty-highlighting ()
  (pretty-mode -1)
  (pretty-symbols-mode -1))

(defun face-default-face ()
  (interactive)
  (let ((faded-color (face-attribute 'font-lock-comment-face :foreground)))
    (set-face-foreground 'default faded-color)))

                                        ; Font lock mode variations to maybe speed up scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      ;;  jit-lock-defer-time 0.05
      font-lock-support-mode 'jit-lock-mode)

(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

                                        ;If you never expect to have to display bidirectional scripts, like
                                        ;Arabic, you can make that the default:
(setq-default bidi-paragraph-direction 'left-to-right)

(require 'page-break-lines)
(global-page-break-lines-mode)

;; Turn off 3d modeline
(set-face-attribute 'mode-line nil :box nil)

(global-font-lock-mode t)

(auto-compression-mode t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1))
(menu-bar-mode -1)

(mouse-wheel-mode t)

(setq visible-bell                   t
      ring-bell-function             'ignore
      echo-keystrokes                0.1
      font-lock-maximum-decoration   t
      font-lock-verbose              nil
      inhibit-startup-message        t
      transient-mark-mode            t
      color-theme-is-global          t
      delete-by-moving-to-trash      t
      shift-select-mode              nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style     'forward
      whitespace-style               '(trailing lines space-before-tab
                                       indentation space-after-tab)
      whitespace-line-column         100
      xterm-mouse-mode               t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
(setq custom-safe-themes t)

(defun reset-fringe ()
  (interactive)
  (set-fringe-mode nil))

(defun increase-fringe ()
  (interactive)
  (message fringe-mode)
  (let ((new-value (+ 4 (or fringe-mode 0))))
    (set-fringe-mode new-value)))

(provide 'init-appearance)
