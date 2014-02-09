
;;make sure ansi colour character escapes are honoured
(require 'ansi-color)

(ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; kill buffer when terminal process is killed
;;  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
;;    (if (memq (process-status proc) '(signal exit))
;;        (let ((buffer (process-buffer proc)))
;;          ad-do-it
;;          (kill-buffer buffer))
;;      ad-do-it))
;;  (ad-activate 'term-sentinel)

(setq multi-term-program "/bin/zsh")

;; export PROMPT_COMMAND="";

;; utf8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;;  don't replace region/buffer with term error message
(setq shell-command-default-error-buffer t)


(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'multi-term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-m" . term-send-raw)
    ("C-t" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-s" . isearch-forward)
    ("M-r" . isearch-backward)
    ("C-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("C-c C-j" . term-line-mode)
    ("C-c C-k" . term-char-mode)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'multi-term)

(defun open-term ()
  (interactive)
;;  (split-window-horizontally-and-switch)
  (multi-term))

(defun open-term-below ()
  (interactive)
  (split-window-vertically-and-switch)
  (multi-term))

(nmap (kbd "[ t") 'multi-term-next)
(nmap (kbd "] t") 'multi-term-prev)

(nmap " q" 'open-term)
(nmap " Q" 'open-term-below)

(add-hook 'term-mode-hook 'auto-complete-mode)

(defun grab-quoted-text-and-yank ()
  "in shell, grab the string in `[text]' quote above point
   and paste it at point"
  (interactive)
  (save-excursion
    (re-search-backward "`\\(.*\\)'"))
  (insert (match-string 1)))

(defun start-shell ()
  (interactive)
  (ansi-term explicit-shell-file-name))

(provide 'init-terminal)
