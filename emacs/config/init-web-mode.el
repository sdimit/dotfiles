
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
(define-key web-mode-map (kbd "C-f") 'web-mode-fold-or-unfold)
(define-key web-mode-map (kbd "C-'") 'web-mode-mark-and-expand)

(set-face-attribute 'web-mode-html-tag-face nil :foreground "DarkViolet")

(add-hook 'web-mode-hook 'zencoding-mode)

(require 'multi-web-mode)

;; Use multi-web-mode for editing code embedded in HTML.
(setq mweb-default-major-mode 'html-mode)
(let ((mweb-possible-tags
       '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
         (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
         (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
         (ruby-mode "<\\%=\\|<\\% " "\\-%>\\|\\%>"))))
  (dolist (cell mweb-possible-tags)
    (when (fboundp (car cell))
      (push cell mweb-tags))))
(setq mweb-filename-extensions '("html" "phtml" "erb"))
(multi-web-global-mode 1)

(pretty-add-keywords 'html-mode `(("data-bind" . "Δ")
                                  ("attributes"  . "Ѧ")))

(add-hook 'html-mode-hook 'pretty-symbols-mode)
(add-hook 'html-mode-hook 'highlight-knockout)

(defun highlight-knockout ()
  (highlight-regexp "data-bind=\"\\(.*\\)\"" 'hi-red-b))

(defun pretty-indent-html ()
  (interactive)
  (sgml-mode)
  (mark-whole-buffer)
  (call-interactively 'sgml-pretty-print)
  (html-mode))

(provide 'init-web-mode)
