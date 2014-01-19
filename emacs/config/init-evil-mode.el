
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
        magit-branch-manager-mode))


;;  evil leader mode
;; (global-evil-leader-mode)

;;  (evil-leader/set-leader "<space>")
;;  (setq evil-leader/in-all-states t)

(setq evil-want-C-u-scroll t
      evil-cross-lines t
      evil-move-cursor-back nil)

(defmacro bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
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
;;   (nmap (kbd "M-`")         'repeat-complex-command)

;;  (global-set-key "\\" 'evil-execute-in-normal-state)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun start-shell ()
  (interactive)
  (ansi-term explicit-shell-file-name))

;;  (define-key evil-normal-state-map ":l" 'cycle-buffer)

(nmap " k" 'helm-bookmarks)
(nmap " p" 'helm-browse-project)
(nmap " g" 'helm-buffers-list)
(nmap " T" 'helm-etags-select)
(nmap " f" 'helm-mini)
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
(nmap " ll" 'align-regexp)
(nmap " LL" 'align-cljlet)
(nmap " q" 'multi-term)
(nmap "gu" 'browse-url)
(vmap "gu" 'google-region)
(nmap "Q" 'kill-current-buffer)
(global-set-key (kbd "C-S-k") 'kill-current-buffer)
(nmap "q" 'delete-window)

;; (nvmap " vf" 'mark-defun)

(nmap " yf" 'copy-filename-of-current-buffer)
(nmap " yp" 'copy-full-path-of-current-buffer)

(nmap (kbd "SPC RET") 'ido-find-file)
(nmap (kbd "SPC TAB") 'ibuffer-other-window)

(nmap "j" 'evil-next-visual-line)
(nmap "k" 'evil-previous-visual-line)
(nvmap "H" 'evil-first-non-blank)
(nvmap "L" 'evil-last-non-blank)

(nmap (kbd "C-f") 'evil-find-char-backward)

(nmap (kbd "[ RET") (bind (evil-insert-newline-above) (forward-line)))
(nmap (kbd "] RET") (bind (evil-insert-newline-below) (forward-line -1)))
(nmap (kbd "[ SPC") (bind (evil-insert -1) (insert " ") (evil-normal-state)))
(nmap (kbd "] SPC") (bind (evil-append   1) (insert " ") (evil-backward-char 2) (evil-normal-state)))
;;  (nmap (kbd "C-w") 'subword-backward-kill)
(nmap (kbd "M-<backspace>") 'delete-till-nonblank-char)
(nmap (kbd "[ e") (kbd "ddkP"))
(nmap (kbd "] e") (kbd "ddp"))
(nmap (kbd "[ b") 'previous-buffer)
(nmap (kbd "] b") 'next-buffer)
(nmap (kbd "[ q") 'previous-error)
(nmap (kbd "] q") 'next-error)

(provide 'init-evil-mode)
