(winner-mode 1)
(global-set-key (kbd "C-c <up>") 'winner-undo)
(global-set-key (kbd "C-c <down>") 'winner-redo)

;;  TODO use "window prefix" (e.g. "C-a" as in tmux)
;;  (global-set-key (kbd "C-. l") 'evil-window-right)
;;  (global-set-key (kbd "C-. h") 'evil-window-left)

(require 'direx)

(require 'acme-search "~/.emacs.d/src/acme-search.el")

(nmap " 1" 'direx:jump-to-directory-other-window)

(define-key direx:direx-mode-map (kbd "RET") 'direx:find-item-other-window)
(define-key direx:direx-mode-map (kbd "j") 'direx:next-item)
(define-key direx:direx-mode-map (kbd "k") 'direx:previous-item)
(define-key direx:direx-mode-map [mouse-1] 'direx:mouse-1)
;(define-key direx:direx-mode-map [mouse-1] 'direx:mouse-2)

(defun jump-to-tag ()
  (interative)
  (if (eq major-mode 'clojure-mode)
      (call-interactively 'cider-jump)
    (call-interactively 'find-tag-generic)))

(nmap (kbd "M-.") 'jump-to-tag)

(defun find-tag-generic ()
  (interactive)
  (cond
   ((eq major-mode 'python-mode)
    (call-interactively 'jedi:goto-definition))
   ((eq major-mode 'clojure-mode)
    (call-interactively 'cider-jump))
   (t
    (call-interactively 'evil-jump-to-tag))))

(defun find-tag-generic-other-window ()
  (interactive)
  (evil-window-vsplit)
  (call-interactively 'find-tag-generic))

(defun find-tag-on-mouse-click (event)
  "py-charm envy..."
  (interactive "e")
  (let ((posn (elt event 1)))
    (with-selected-window (posn-window posn)
      (goto-char (posn-point posn))
      (find-tag-generic))))

(nmap (kbd "C-]") 'find-tag-generic)
(nmap (kbd "C-c C-]") 'find-tag-generic-other-window)
(nmap (kbd "<down-double-mouse-1>") 'find-tag-on-mouse-click)
(nmap (kbd "<M-double-mouse-1>") 'projectile-ag)
(vmap (kbd "<M-double-mouse-1>") 'projectile-ag)


;;  (define-key evil-normal-state-map [escape] 'winner-undo)

(windmove-default-keybindings 'meta)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(setq windmove-wrap-around t)

;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(defun toggle-minimap ()
  (interactive)
  (if (minimap-visible-p)
      (minimap-kill)
    (minimap-create)))

;; (global-evil-tabs-mode t)
;; (nmap " tk" 'elscreen-kill)
;; (nmap " tn" 'elscreen-create)
;; (nmap " tN" (bind (evil-tabs-tabedit (buffer-file-name))))
;; (nmap " tt" 'elscreen-next)
;; (nmap " tT" 'elscreen-previous)

(defun assign-f1-bookmark-to-buffer ()
  (interactive)
  (setq f1-bookmark-buffer (buffer-name (current-buffer)))
  (nmap (kbd "<f1>") (bind (switch-to-buffer-other-window f1-bookmark-buffer))))

(defun assign-f2-bookmark-to-buffer ()
  (interactive)
  (setq f2-bookmark-buffer (buffer-name (current-buffer)))
  (nmap (kbd "<f2>") (bind (switch-to-buffer-other-window f2-bookmark-buffer))))

;;  TODO make generic

;;  (defun assign-key-bookmark-to-buffer (key)
;;    (interactive "M")
;;    (setq v (make-symbol (concat key "key")))
;;    (message (concat "hi" v)))

(require 'smex)
(smex-initialize)

(nvmap ":" 'smex)
;;(nvmap ":" 'helm-M-x)

(global-set-key (kbd "M-x") 'smex) ; for emacs-mode
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(nvmap (kbd "SPC :") 'evil-ex)

(setq smex-show-unbound-commands t)
(smex-auto-update 30)

(setq mac-option-modifier 'meta)

;;  some shortcuts from evil-ex for M-x

(defun w ()
  ":w shortcut"
  (interactive)
  (save-buffer))

(defun only ()
  ":only"
  (interactive)
  (delete-other-windows))

(nmap (kbd "C-/") 'only)

;; (golden-ratio-mode)
;; (nmap " ~" 'golden-ratio)

(provide 'init-navigation)
