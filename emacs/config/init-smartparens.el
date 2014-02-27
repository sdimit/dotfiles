
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

(defun sp-wrap-next-sexp ()
  (interactive)
  (progn (insert "()")
         (evil-backward-char 1)
         (sp-forward-slurp-sexp)
         (evil-insert 1)))

(defun sp-wrap-next-quote ()
  (interactive)
  (progn (insert "\"\"")
         (evil-backward-char 1)
         (sp-forward-slurp-sexp)))

(define-key sp-keymap (kbd "C-M-n") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-j") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-M-l") 'sp-forward-symbol)
(define-key sp-keymap (kbd "C-M-h") 'sp-backward-symbol)
(define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-wrap-next-sexp)
(define-key sp-keymap (kbd "C-M-\"") 'sp-wrap-next-quote)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-~") 'sp-kill-symbol)
(define-key sp-keymap (kbd "C-M-y") 'sp-copy-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-<right>") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<up>") 'sp-splice-sexp-killing-around)
(define-key sp-keymap (kbd "C-M-<down>") 'sp-splice-sexp-killing-forward)
;; (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-M-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "C-M-s s") 'sp-split-sexp)

(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
(define-key sp-keymap (kbd "C-M-=") 'sp-indent-defun)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; don't pair apostrophes
(sp-pair "'" nil :unless '(sp-point-after-word-p))

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

(provide 'init-smartparens)
