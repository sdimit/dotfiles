
(defun describe-major-mode ()
  "Show inline doc for current major-mode."
  (interactive)
  (describe-function major-mode))

(global-set-key (kbd "C-h m") 'describe-major-mode)

(global-set-key (kbd "C-h M") 'helm-man-woman)

(global-set-key (kbd "C-h b") 'helm-descbinds)

;; (require 'discover)
;; (global-discover-mode 1)

(global-set-key (kbd "C-h a") 'apropos)
;;  same as C-h f
(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-r") (bind (ido-find-file-in-dir "~/Reference")))
(global-set-key (kbd "C-h C-s") 'find-function-other-window)
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

;; shortcut for underscore/lodash
(nmap " _" (bind (dash-at-point-run-search (thing-at-point 'symbol) "_")))
(nmap " $" (bind (dash-at-point-run-search (thing-at-point 'symbol) "jquery")))

(add-hook 'js3-mode-hook
          (lambda () (setq dash-at-point-docset "js")))

(add-hook 'python-mode-hook
          (lambda () (setq dash-at-point-docset "python")))

(add-hook 'coffee-mode-hook
          (lambda () (setq dash-at-point-docset "coffee")))


(defun howdoi (search)
  (interactive "MHow do I: ")
  (let ((result-buffer (generate-new-buffer-name "*howdoi*")))
    (shell-command (format "%s %s" "howdoi" (shell-quote-argument search))
                   result-buffer
                   nil)
    (pop-to-buffer result-buffer nil t)
    (org-mode)
    (whitespace-cleanup)
    (remove-dos-eol)
    (toggle-truncate-lines 1)))

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

(defun search-config-files (not-from-point)
  "use universal-argument to take symbol at point"
  (interactive "P")
  (let* ((search-prompt (if not-from-point "" (ag/dwim-at-point)))
         (search-term (read-from-minibuffer "Search: " search-prompt)))
    (ag/search search-term "~/.emacs.d/config" t)))

(global-set-key (kbd "C-c e e") 'search-config-files)

(global-set-key (kbd "C-h C-c") (bind (ido-find-file-in-dir "~/.emacs.d/config")))
(nmap " 9" (bind (ido-find-file-in-dir "~/.emacs.d/config")))

(provide 'init-help)
