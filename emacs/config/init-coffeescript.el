
;; (setq coffee-command "/usr/local/bin/nesh")
;; (setq coffee-args-repl '("-c -e /usr/local/lib/node_modules/underscore/underscore.js'"))

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (auto-complete-mode)
  ;; (coffeelintnode-hook)
  ;; (require 'flymake-coffee)
  ;; (flymake-coffee-load)
  ;; (unless (eq buffer-file-name nil) (flymake-mode 1)) ;dont invoke flymake on temporary buffers for the interpreter
  )

(add-hook 'coffee-mode-hook (lambda() (coffee-custom)))

(add-hook 'coffee-mode-hook 'pretty-symbols-mode)


(pretty-add-keywords 'coffee-mode `(("@attributes"  . "Ѧ")))

(defun coffee-open-below ()
  (interactive)
  (if (eq major-mode 'coffee-mode)
      (progn (evil-append-line 1)
             (coffee-newline-and-indent))
    (evil-open-below 1)))

;; (defun coffee-open-above ()
;;   (interactive)
;;   (if (eq major-mode 'coffee-mode)
;;     (progn (evil-previous-visual-line)
;;         (coffee-open-below))
;;     (evil-open-above 1)))


(define-key evil-normal-state-map "o" 'coffee-open-below)


(setq coffeelintnode-node-program "/usr/local/bin/coffeelint")
(setq coffeelintnode-coffeelint-excludes (list 'max_line_length))
(setq coffeelintnode-coffeelint-includes '())
(setq coffeelintnode-coffeelint-set "")

;; Start the server when we first open a coffee file and start checking
(setq coffeelintnode-autostart 'true)

(provide 'init-coffeescript)