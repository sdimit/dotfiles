
(evil-mode 1)

;; don't use evil in the following mode
(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
      '(inferior-emacs-lisp-mode
        comint-mode
        compilation-mode
        eshell-mode
        inferior-python-mode
        inferior-ess-mode
        dired-mode
        direx:direx-mode
        term-mode
        deft-mode
        nrepl-mode
        nrepl-repl-mode
        cider-repl-mode
        skewer-repl-mode
        makey-key-mode
        process-menu-mode
        prodigy-mode
        git-rebase-mode
        Custom-mode
        pycscope-list-entry-mode
        magit-blame-mode
        jabber-chat-mode
        magit-branch-manager-mode))

;; normal mode for test runner
(evil-set-initial-state 'shell-mode 'normal)

;;  evil leader mode
;; (global-evil-leader-mode)

;;  (evil-leader/set-leader "<space>")
;;  (setq evil-leader/in-all-states t)

(setq evil-want-C-u-scroll t
      evil-cross-lines t
      evil-move-cursor-back nil
      evil-ex-complete-emacs-commands t
      evil-ex-substitute-global t
      evil-shift-width 2
      evil-symbol-word-search t) ;; use symbol, not word for */#)

(defmacro bind (&rest commands)
  "Convenience macro which creates a lambda interactive command."
  `(lambda () (interactive) ,@commands))

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun nmap (kbinding fun) (define-key evil-normal-state-map kbinding fun))
(defun vmap (kbinding fun) (define-key evil-visual-state-map kbinding fun))
(defun imap (kbinding fun) (define-key evil-insert-state-map kbinding fun))
(defun nvmap (kbinding fun) (progn (nmap kbinding fun) (vmap kbinding fun)))

(nmap (kbd "C-u")         'evil-scroll-up)
(nmap (kbd "<RET>")       'evil-scroll-down)
(nmap (kbd "<backspace>") 'evil-scroll-up)
(nmap (kbd "`")           'universal-argument)
(nmap (kbd "C-`")         'universal-argument)
;;   (nmap (kbd "M-`")         'repeat-complex-command)

;;  (global-set-key "\\" 'evil-execute-in-normal-state)

;;  (define-key evil-normal-state-map ":l" 'cycle-buffer)

(nmap " k" 'helm-bookmarks)
(nmap " p" 'helm-browse-project)
(nmap " g" 'helm-git-grep)
(nmap " t" 'helm-etags-select)
(nmap " h" 'highlight-regexp)
(nmap " H" 'highlight-from-isearch)
(nmap " u" 'unhighlight-regexp)
(nmap " j" 'dired-jump)
(nmap " n" 'linum-mode)
(nmap " O" 'helm-browse-code)
(nmap " o" 'idomenu) ;tags
;; (nmap " O" 'helm-org-headlines)
(nmap " /" 'helm-swoop)
(nmap " A" 'ag)
(nmap " a" 'ag-here-from-point)
(nmap " e" 'helm-find-files)
(nmap " i" 'deft)
(nmap "  " 'ido-switch-buffer)
(nmap "gu" 'browse-url)
(nmap "gU" (bind (let ((browse-url-browser-function 'browse-url-default-browser))
                   (call-interactively 'browse-url))))
(vmap "gu" 'google-region)
(nmap "Q" 'kill-current-buffer)
(global-set-key (kbd "C-S-k") 'kill-current-buffer)
(nmap "q" 'delete-window)
(global-set-key (kbd "C-q") 'bury-buffer)

;; (nvmap " vf" 'mark-defun)
; TODO: as evil motion

(nmap " yf" 'copy-filename-of-current-buffer)
(nmap " yp" 'copy-full-path-of-current-buffer)

(nmap (kbd "SPC RET") 'ido-find-file)
(nmap (kbd "SPC TAB") 'ibuffer-other-window)
(nmap (kbd "SPC \\") 'recentf-ido-find-file)

(nmap "j" 'evil-next-visual-line)
(nmap "k" 'evil-previous-visual-line)
(nvmap "H" 'evil-first-non-blank)
(nvmap "L" 'evil-last-non-blank)

(nmap (kbd "C-f") 'evil-find-char-backward)

(nmap (kbd "[ RET") (bind (evil-insert-newline-above) (forward-line)))
(nmap (kbd "] RET") (bind (evil-insert-newline-below) (forward-line -1)))
(nmap (kbd "[ SPC") (bind (evil-insert -1) (insert " ") (evil-normal-state)))
(nmap (kbd "] SPC") (bind (evil-append 1) (insert " ") (evil-backward-char 2) (evil-normal-state)))
;;  (nmap (kbd "C-w") 'subword-backward-kill)
(nmap (kbd "M-<backspace>") 'delete-till-nonblank-char)
(nmap (kbd "[ e") (kbd "ddkP"))
(nmap (kbd "] e") (kbd "ddp"))
(nmap (kbd "[ b") 'previous-buffer)
(nmap (kbd "] b") 'next-buffer)
(nmap (kbd "[ q") 'previous-error)
(nmap (kbd "] q") 'next-error)

(provide 'init-evil-mode)
