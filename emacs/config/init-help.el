
(defun describe-major-mode ()
  "Show inline doc for current major-mode."
  ;; code by Kevin Rodgers. 2009-02-25
  (interactive)
  (describe-function major-mode))

(global-set-key (kbd "C-h m") 'describe-major-mode)

(require 'discover)
(global-discover-mode 1)

(global-set-key (kbd "C-h a") 'apropos)
;;  same as C-h f
(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-s") 'find-function)
(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-s") 'find-function)
(global-set-key (kbd "C-h W") 'find-function-on-key)
;;   (global-set-key (kbd "C-h C-s") 'smex-find-function)

(global-set-key (kbd "C-h h") nil)

(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'describe-function)
(define-key emacs-lisp-mode-map (kbd "C-c C-s") 'find-function)

;;  Dash (OSX)

(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)

(require 'dash-at-point)
(nmap " d" 'dash-at-point)
;;  TODO use ido completion
(nmap " D" 'dash-at-point-with-docset)

(add-hook 'js3-mode-hook
          (lambda () (setq dash-at-point-docset "js")))

(add-hook 'python-mode-hook
          (lambda () (setq dash-at-point-docset "python")))

(add-hook 'coffee-mode-hook
          (lambda () (setq dash-at-point-docset "coffee")))


(defun howdoi (beg end)
  "Replace the region from BEG to END with the howdoi search results for that text."
  (interactive "r")
  (let ((terms (buffer-substring beg end)))
    (shell-command-on-region
     beg
     end
     (format "%s %s" "howdoi" (shell-quote-argument terms))
     t)))



(require 'popup)

(defun describe-thing-in-popup ()
  (interactive)
  (let ((description (save-window-excursion
                       (help-xref-interned (symbol-at-point))
                       (switch-to-buffer "*Help*")
                       (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(nmap (kbd "C-c d") 'describe-thing-in-popup)

(defun search-config-files (string)
  (interactive (list
                (read-from-minibuffer "Search: " (ag/dwim-at-point))))
  (ag/search string "~/.emacs.d/config" t))

(global-set-key (kbd "C-c e e") 'search-config-files)

(provide 'init-help)
