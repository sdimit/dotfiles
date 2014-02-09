(require 'org)
(require 'org-mac-link "/Users/admin/dotfiles/emacs/elpa/org-mac-link-20140107.519/org-mac-link.el")


(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;;  Don't ask for confirmation on every =C-c C-c= code-block compile.
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . t)
   (clojure . t)
   (js . t)
   (python . t)
   ))

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
  "gj" 'outline-next-visible-heading
  "gk" 'outline-previous-visible-heading
;;  "gl" 'outline-
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

(setq org-default-notes-file "~/Inbox/notes.org")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cL" 'org-mac-chrome-insert-frontmost-url)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;  Configure org-mode so that when you edit source code in an
;;  indirect buffer (with C-c '), the buffer is opened in the current
;;  window. That way, your window organization isn't broken when
;;  switching.
(setq org-src-window-setup 'current-window)

(provide 'init-org-mode)
