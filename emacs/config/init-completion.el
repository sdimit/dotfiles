(require 'auto-complete)
(require 'auto-complete-config)

(setq ac-auto-show-menu 0.4
      ac-quick-help-delay 0.2
      ac-use-fuzzy t
      ac-auto-start t
      ac-comphist-file (concat user-emacs-directory ".cache/ac-comphist.dat")
      ac-quick-help-height 30
      ac-show-menu-immediately-on-auto-complete t)

(setq-default ac-dwim nil)


(require 'fuzzy)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete" dotfiles-dir))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/local-autocomplete")

(setq ac-modes (append ac-modes '(org-mode)))
(ac-config-default)
(define-key ac-complete-mode-map [tab] 'ac-expand)

(ac-flyspell-workaround)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(after 'auto-complete
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))

(setq-default ac-sources '(ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-dictionary
                           ac-source-filename))

(require 'yasnippet)
; (yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "C-c C-j") 'yas-expand)

;; Yasnippets, always
(eval-after-load "yasnippet"
  '(setq-default ac-sources (append '(ac-source-yasnippet) ac-sources)))

(defadvice ac-expand (before advice-for-ac-expand activate)
  (when (yas-expand)
    (ac-stop)))

;; Hippie expand: look in buffer before filenames please
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


(defun hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line
                                            try-expand-line-all-buffers)))
    (hippie-expand nil)))

(define-key evil-insert-state-map "\C-l" 'hippie-expand)
(define-key evil-insert-state-map (kbd "C-x C-l") 'hippie-expand-lines)

(define-abbrev global-abbrev-table "atr" "attributes")
(define-abbrev global-abbrev-table "tempalte" "template")
(abbrev-mode 1)

  (when (> emacs-major-version 21)
    (ido-mode t)
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-max-prospects 10))

(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

(provide 'init-completion)
