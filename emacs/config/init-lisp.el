
(defun scratch ()
  (switch-to-buffer "*scratch*"))

(defun my-align-let ()
  (interactive)
  (if (eq major-mode 'clojure-mode)
      (align-cljlet)
    (align-let)))

(load-file "~/.emacs.d/src/align-let.el")
(nmap " LL" 'my-align-let)

;; (global-set-key (kbd "C-c e e") 'eval-and-replace)
;; (global-set-key (kbd "C-c e r") 'eval-region)
;; (global-set-key (kbd "C-c e f") 'eval-defun)
;; (global-set-key (kbd "C-c e b") 'eval-buffer)
;; (global-set-key (kbd "C-c e s") 'scratch)

;; (global-set-key (kbd "C-h e k") 'find-function-on-key)
;; (global-set-key (kbd "C-h e e") 'view-echo-area-messages)
;; (global-set-key (kbd "C-h e l") 'find-library)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
;; (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; (defun auto-recompile-file-maybe ()
;;   (when auto-recompile
;;     (byte-compile-file buffer-file-name)))

;; (defun add-after-save-hook ()
;;   (make-local-hook 'after-save-hook)
;;   (add-hook 'after-save-hook 'auto-recompile-file-maybe))

;; (add-hook 'emacs-lisp-mode-hook 'add-after-save-hook)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(global-rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)

;; (add-hook 'ielm-mode-hook 'ielm-auto-complete)

(defun my-eval-region ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (cond
     ((eq major-mode 'coffee-mode)
      (coffee-compile-region start end))
     ((eq major-mode 'stylus-mode)
      (my-stylus-compile-region start end))
     ((eq major-mode 'clojure-mode)
      (cider-eval-region start end))
     (t (eval-region start end)))))

(vmap (kbd "C-c C-r") 'my-eval-region)

;; symbols for some overlong function names
(eval-after-load 'emacs-lisp-mode
  '(font-lock-add-keywords
    'emacs-lisp-mode
    (mapcar
     (lambda (pair)
       `(,(car pair)
         (0 (progn (compose-region
                    (match-beginning 0) (match-end 0)
                    ,(cadr pair))
                   nil))))
     '(("\\<defun\\>" ?ƒ)
       ("\\<or\\>" ?<)
       ("\\<not\\>" ?^)
       ("\\<nil\\>" ?Ø)
       ("\\<interactive\\>" ?ι)
       ))))

(require 'highlight-sexp)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)

(defun trace-errors ()
  (interactive)
  (if (eq nil debug-on-error)
      (progn
        (setq debug-on-error t)
        (message "enabled"))
    (progn
      (setq debug-on-error nil)
      (message "disabled"))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))


(provide 'init-lisp)
