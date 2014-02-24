
(defvar markdown-imenu-generic-expression
  '(("title"  "^\\(.+?\\)[\n]=+$" 1)
    ("h2-"    "^\\(.+?\\)[\n]-+$" 1)
    ("h1"   "^#\\s-+\\(.+?\\)$" 1)
    ("h2"   "^##\\s-+\\(.+?\\)$" 1)
    ("h3"   "^###\\s-+\\(.+?\\)$" 1)
    ("h4"   "^####\\s-+\\(.+?\\)$" 1)
    ("h5"   "^#####\\s-+\\(.+?\\)$" 1)
    ("h6"   "^######\\s-+\\(.+?\\)$" 1)
    ("fn"   "^\\[\\^\\(.+?\\)\\]" 1) ))

(defun my/markdown-mode-hook ()
  (setq imenu-generic-expression markdown-imenu-generic-expression)
  (add-to-list 'ac-sources 'ac-source-look))
(add-hook 'markdown-mode-hook 'my/markdown-mode-hook)

(provide 'init-markdown)
