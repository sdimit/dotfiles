
(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
;;  (define-key global-map "\C-x a" 'ack)

;;  Interface with Ag, the silver search

(require 'ag)
(setq ag-highlight-search t)
;;  (setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

;;  Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;  Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)

(defun highlight-from-isearch ()
  (interactive)
  (let ((input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (highlight-regexp input)))

(defun ag-from-point ()
  "Search using ag in current directory,
         with STRING defaulting to the symbol under point."
  (interactive)
  (let ((ido-report-no-match nil)
        (ido-auto-merge-work-directories-length -1)
        (directory (file-name-directory (or load-file-name buffer-file-name)))
        (string (ag/dwim-at-point))
        (ag-function (apply-partially 'ag/search 'string)))
    (ido-file-internal 'ag-function)))

(defun ag-here-from-point ()
  "Search using ag in current directory,
         with STRING defaulting to the symbol under point."
  (interactive)
  (let ((directory (file-name-directory (or load-file-name buffer-file-name)))
        (string (ag/dwim-at-point)))
    (ag/search string directory)))

(defun show-ag () (interactive) (switch-to-buffer-other-window "*ag*"))
(defun kill-ag () (interactive) (kill-buffer "*ag*"))

(define-key evil-motion-state-map (kbd "C-'") 'next-error)
(define-key evil-motion-state-map (kbd "C-:") 'previous-error)
(define-key evil-motion-state-map (kbd "C-;") 'show-ag)
(define-key evil-motion-state-map (kbd "C-\"") 'kill-ag)

;; (defun evil-ex-search-and-replace ()
;;   ""
;;   (interactive)
;;   (evil-ex)
;;   (insert "s///cg"))

;; (vmap "rr" 'evil-ex-search-and-replace)


(defun evilcvn--change-symbol(fn)
  (let ((old (thing-at-point 'symbol)))
    (funcall fn)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/"))))
  )

(defun change-symbol-in-whole-buffer ()
  "mark the region in whole buffer and use string replacing UI in evil-mode
  to replace the symbol under cursor"
  (interactive)
  (let ((old (thing-at-point 'symbol)))
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "%s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>//")))
    (left-char))
  )

(defun change-symbol-in-region ()
  "mark the region in whole buffer and use string replacing UI in evil-mode
  to replace the symbol under cursor"
  (interactive)
  (evilcvn--change-symbol 'mark-whole-buffer)
  )

(defun change-symbol-in-defun ()
  "mark the region in defun (definition of function) and use string replacing UI in evil-mode
  to replace the symbol under cursor"
  (interactive)
  (evilcvn--change-symbol 'mark-defun)
  )

(nmap " rr" 'change-symbol-in-whole-buffer)
(nmap " rf" 'change-symbol-in-defun)


(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;;  (define-key evil-normal-state-map (kbd "C-]") 'helm-etags-select)

;;  Convenience Function to search for regexps build with re-builder

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(defun google-region ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(defun browse-url-with-chrome (args)
  ;; TODO
  (let ((browse-url-browser-function (quote browse-url)))
    (browse-url)))

(provide 'init-search)
