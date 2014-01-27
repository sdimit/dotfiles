
(require 'thingatpt)
(require 'imenu)

;; show wrap guide
(require 'fill-column-indicator)
(fci-mode)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

(setq c-basic-offset 2)

(nmap (kbd "M-d") 'mark-defun)
(nmap (kbd "M-p") 'mark-paragraph)

;; (nmap (kbd "C-RET") 'other-window)

(global-set-key (kbd "C-\\") 'comment-dwim)
(vmap (kbd "C-/") 'comment-dwim)
(vmap (kbd "C-\\") 'comment-dwim)

(vmap " n" 'narrow-paragraph)
(vmap " ," 'commas-to-newlines)

(setq fill-column 80)
;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (require 'hl-line+)
  (toggle-hl-line-when-idle 1)
  (global-hl-line-mode nil))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(nmap (kbd "[ m") 'flymake-goto-prev-error)
(nmap (kbd "] m") 'flymake-goto-next-error)

;; Don't break lines
(setq-default truncate-lines t)

(setq-default global-visual-line-mode nil)

(add-hook 'inferior-python-mode-hook
          (lambda () (setq-default truncate-lines nil)))

(add-hook 'javascript-mode-hook
          (lambda () (auto-complete-mode t)))

(add-hook 'cider-repl-mode-hook
          (lambda () (setq-default truncate-lines nil)))


(defun delete-till-nonblank-char ()
  (interactive)
  (let ((num-spaces-ahead (skip-chars-forward "\s")))
    (when (< 0 num-spaces-ahead)
      (backward-char num-spaces-ahead)
      (delete-char num-spaces-ahead))))

(defun buffer-contains-string-p (string)
  "Does the current buffer contain STRING? Case sensitive."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward string nil t)))))

(setq ns-function-modifier 'hyper)

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; default tab-width is two spaces
(setq-default tab-width 2
              js-indent-level 2
              c-basic-offset 2
              indent-tabs-mode nil)

(require 'highlight-indentation)

(add-hook 'coffee-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(require 'shift-text)
(nmap "[e" 'shift-text-up)
(nmap "]e" 'shift-text-down)

(setq c-basic-offset 2)


(global-set-key "\C-\\" 'comment-region)

;; show the matching parentheses immediately
(setq show-paren-delay 0)

(require 'evil-matchit)
(global-evil-matchit-mode)

(defun custom-align () (interactive)
  (let ((start (region-beginning))
        (end   (region-end)))
    (align-regexp start end "=")))

(vmap " a=" (bind (align-regexp (region-beginning) (region-end) "=")))

(defun visual-shift-left (start end)
  (interactive "r")
  (save-excursion
    (evil-shift-left start end))
  (evil-visual-restore))

;; (defun visual-shift-right (start end)
;;   (interactive "r")
;;   (evil-shift-right start end)
;;   (evil-visual-restore))

(defun commas-to-newlines (start end)
  (interactive "r")
  (shell-command-on-region
   start end
   "tr , '\n'"
   nil t))

(defun narrow-paragraph (start end)
  "Narrow region to 80 columns"
  (interactive "r")
  (let ((command "par 79"))
    (shell-command-on-region start end
                             command
                             nil t)))

(define-key evil-visual-state-map "<" 'visual-shift-left)

;; (define-key evil-visual-state-map ">" 'visual-shift-right)

(define-key evil-normal-state-map " =" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map " -" 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "C-c DEL")
  (bind (delete-region (point-min) (point-max))))

(require 'surround)
(global-surround-mode 1)

(idle-highlight t)

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;; TODO use for coffee and clojure
(defun narrow-to-ruby-block ()
  (save-excursion
    (let ((start (progn (ruby-beginning-of-block) (point)))
          (end (progn (ruby-end-of-block) (point))))
      (narrow-to-region start end))))

(defun send-current-line-to-next-window ()
  "Send current line to next window"
  (interactive)
  (let ((current-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (target (window-buffer (next-window))))
    (with-current-buffer target
      (insert current-line))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                 (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Folding

(defun toggle-folding-buffer (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(define-key evil-normal-state-map "zA" 'toggle-folding-buffer)

(define-key evil-normal-state-map "zf" 'fold-this)

;; Transpose chars
;; Emulate vim behaviour

(defun my-transpose-chars ()
  (interactive)
  (transpose-chars -1)
  (evil-forward-char))

(imap (kbd "C-t") (bind (my-transpose-chars)))

(nmap (kbd "-") 'rotate-text)

(require 'multiple-cursors)
;; When you have an active region that spans multiple lines, the following will add a cursor to each line:

;; global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;; (nmap (kbd "M-.") 'mc/mark-next-like-this)
;; (nmap (kbd "M-,") 'mc/mark-previous-like-this)
;; (nmap (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (nmap (kbd "M-\\") 'set-rectangular-region-anchor)

(defun comment-and-duplicate-line ()
  "Copy current line to line below and comment current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties beg end))
         (column (current-column)))
    (comment-region beg end)
    (goto-char (line-end-position))
    (newline)
    (insert line)
    (move-to-column column)))

(nmap (kbd "gyy") 'comment-and-duplicate-line)

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(add-to-list 'load-path (concat dotfiles-dir "contrib/expand-region"))
(require 'expand-region)

(provide 'init-editing)
