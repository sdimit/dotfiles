
(add-hook 'clojure-mode-hook 'highlight-sexp-mode)

;; fourclojure
(add-to-list 'load-path "~/.emacs.d/src/4clj-el/")
(require 'four-clj)

(font-lock-add-keywords 'clojure-mode
                        '(("(\\|)" . 'esk-paren-face)))

(defface esk-clojure-trace-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'starter-kit-faces)

(setq esk-clojure-trace-face 'esk-clojure-trace-face)

;; ;; font-lock
;;  (dolist (x '((true        т)
;;               (false       ғ)
;;               (:keys       ӄ)
;;               (nil         Ø)
;;               (partial     ∂)
;;               (with-redefs я)
;;               (fn          ƒ)
;;               (comp        º)
;;               (not         ¬)
;;               (apply       ζ)
;;               (interaction ι)
;;               (a-fn1       α)
;;               (a-fn2       β)
;;               (a-fn3       γ)
;;               (no-op       ε)))
;;    (eval-after-load 'clojure-mode
;;      '(font-lock-add-keywords
;;        'clojure-mode `((,(concat "[\[({[:space:]]"
;;                                  "\\(" (symbol-name (first x)) "\\)"
;;                                  "[\])}[:space:]]")
;;                         (0 (progn (compose-region (match-beginning 1)
;;                                                   (match-end 1) ,(symbol-name (second x)))
;;                                   nil))))))
;;    (eval-after-load 'clojure-mode
;;      '(font-lock-add-keywords
;;        'clojure-mode `((,(concat "^"
;;                                  "\\(" (symbol-name (first x)) "\\)"
;;                                  "[\])}[:space:]]")
;;                         (0 (progn (compose-region (match-beginning 1)
;;                                                   (match-end 1) ,(symbol-name (second x)))
;;                                   nil))))))
;;    (eval-after-load 'clojure-mode
;;      '(font-lock-add-keywords
;;        'clojure-mode `((,(concat "[\[({[:space:]]"
;;                                  "\\(" (symbol-name (first x)) "\\)"
;;                                  "$")
;;                         (0 (progn (compose-region (match-beginning 1)
;;                                                   (match-end 1) ,(symbol-name (second x)))
;;                                   nil)))))))


;; Cider configuration
(require 'cider)
(setq nrepl-hide-special-buffers t
      cider-repl-pop-to-buffer-on-connect nil
      cider-popup-stacktraces nil
      cider-repl-popup-stacktraces nil)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
;; (define-key cider-mode-map (kbd "C-c d") 'ac-nrepl-popup-doc)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))

;; specify the print length to be 100 to stop infinite sequences killing things.
(defun live-nrepl-set-print-length ()
  (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

(add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

(defun toggle-nrepl-buffer ()
  "Toggle the nREPL REPL on and off"
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*cider-repl*")
      (delete-window)
    (cider-switch-to-repl-buffer nil)))

;; Switch a Clojure nrepl to ClojureScript

(defun nrepl-start-noderepl ()
  (interactive)
  (save-excursion
    (nrepl-switch-to-repl-buffer nil)
    (insert "(require 'cljs.repl.node) (cljs.repl.node/run-node-nrepl)")
    (nrepl-send-input)))

;;Kibit
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

;; require 'cider)

(setq nrepl-hide-special-buffers t)

;; temporary fix for cider melpa issues
(defun cider--library-version ()
  "Get the version in the nrepl library header."
  ;; (-when-let (version (pkg-info-library-version 'cider))
  ;;   (pkg-info-format-version version))
  "0.3.0-SNAPSHOT")

(defun return-or-eval-last-and-forward ()
  (interactive)
  (if (eq major-mode 'clojure-mode)
      (progn
        (cider-eval-last-sexp)
        (sp-forward-sexp))
    (call-interactively 'evil-scroll-down)))

(nmap (kbd "RET") 'return-or-eval-last-and-forward)

(defun return-or-eval-pretty-last-and-forward ()
  (interactive)
  (if (eq major-mode 'clojure-mode)
      (progn
        (cider-eval-pprint-last-sexp)
        (sp-forward-sexp))))

;; (nmap (kbd "C-RET") 'return-or-eval-pretty-last-and-forward)

;; (defun coffee-open-above ()
;;   (interactive)
;;   (if (eq major-mode 'coffee-mode)
;;     (progn (evil-previous-visual-line)
;;         (coffee-open-below))
;;     (evil-open-above 1)))

(provide 'init-clojure)
