
(require 'org)

;;  The following displays the contents of code blocks in Org-mode files
;;  using the major-mode of the code.  It also changes the behavior of
;;  =TAB= to as if it were used in the appropriate major mode.  This means
;;  that reading and editing code form inside of your Org-mode files is
;;  much more like reading and editing of code using its major mode.
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;;  Don't ask for confirmation on every =C-c C-c= code-block compile.

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . t)
   (ruby . t)
   (python . t)
   (js . t)
   (haskell . t)
   (clojure . t)
   ))

(org-babel-lob-ingest
 (expand-file-name
  "library-of-babel.org"
  (expand-file-name
   "babel"
   (expand-file-name
    "contrib"
    (expand-file-name
     "org"
     (expand-file-name "src" dotfiles-dir))))))

(setq org-completion-use-ido t)
(require 'org-special-blocks)
(if window-system (require 'org-mouse))

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook
                         'yas-org-very-safe-expand)
            ))

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

(defun evil-org-new-line ()
  (interactive)
  (org-end-of-line)
  (org-return)
  (evil-insert 1))

(defun evil-org-insert-todo-heading ()
  (interactive)
  (org-end-of-line)
  (org-insert-todo-heading nil)
  (evil-insert 1))

;; regular normal state shortcuts.
(evil-define-key 'normal evil-org-mode-map
  "gh" 'outline-up-heading
  "gj" 'org-forward-same-level
  "gk" 'org-backward-same-level
  "gl" 'outline-next-visible-heading
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  "t" 'org-todo
  "T" 'evil-org-insert-todo-heading
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "-" 'org-ctrl-c-minus
  "<" 'org-metaleft
  ">" 'org-metaright
  "o" 'evil-org-new-line)


;; normal & insert state shortcuts.
(mapcar (lambda (state)
          (evil-define-key state evil-org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown)) '(normal insert))

(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)))

(setq org-default-notes-file "~/Inbox/notes.org")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda ()
;;                            (org-bullets-mode 1)
;;                            (turn-on-font-lock)
;;                            ))

;;  When in an org-mode buffer, bind TeX-insert-quote to =C-c "=. Turned off by default.

;;  (add-hook 'org-mode-hook 'smart-quote-keys)

;;  (defun smart-quote-keys ()
;;    (require 'typopunct)
;;    (typopunct-change-language 'english)
;;    (local-set-key (kbd "C-c \'") 'typopunct-insert-single-quotation-mark)
;;    (local-set-key (kbd "C-c \"") 'typopunct-insert-quotation-mark)
;;    )


;;  Configure org-mode so that when you edit source code in an indirect buffer (with C-c '), the buffer is opened in the current window. That way, your window organization isn't broken when switching.

(setq org-src-window-setup 'current-window)

;; (require 'org-mac-link)

(nmap (kbd "C-x C-o l") 'org-mac-chrome-insert-frontmost-url)

(provide 'init-org-mode)
