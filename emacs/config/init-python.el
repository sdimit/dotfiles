
(autoload 'python-mode "python-mode" "Python Mode." t)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq python-shell-interpreter "python"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code ""
      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code "")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "PYTHONPATH")

(setq-default flymake-python-pyflakes-extra-arguments '("--ignore=E501"))
;;  don't bug me about E501 (warning about lines > 80 chars)

;;  (add-to-list 'helm-boring-file-regexp-list '("\\.pyc"))

(defvar nose-use-verbose nil)

(defun pudb ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import pudb; pudb.set_trace()")
  (newline-and-indent)
  (highlight-lines-matching-regexp "^[ ]*import pudb; pudb.set_trace()"))

(defun ipdb ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (newline-and-indent)
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

;;  PYCSCOPE
(add-to-list 'load-path "~/.emacs.d/src/xpycscope/")
(require 'xpycscope)

(setq pycscope-use-face nil)
(setq pycscope-display-pycscope-buffer nil)
(setq pycscope-truncate-lines t)

;;  (setq jedi:setup-keys nil)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method 'popup)

(defun elpy-nav-forward-class-definition ()
  "Move forward to the next class definition."
  (interactive)
  (if (save-excursion
        (forward-char 1)
        (re-search-forward "^ *\\(class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-max))))

(defun elpy-nav-backward-class-definition ()
  "Move forward to the previous class definition."
  (interactive)
  (if (save-excursion
        (forward-char -1)
        (re-search-backward "^ *\\(class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-min))))

(defadvice jedi:goto-definition (after scroll-line-to-center last)
  (evil-normal-state))

;;   (call-interactively 'evil-scroll-line-to-center)
;;   (evil-normal-state))

(require 'elpy)
;;  (elpy-enable)

(defun run-nose-test-new-buffer ()
  (interactive)
  (run-nose-test)
  (switch-to-buffer-other-window "*nose-test*")
  (delete-other-windows))

(add-hook 'python-mode-hook (lambda ()
            (highlight-indentation-current-column-mode)
;;          (outline-minor-mode 1)
;;          (light-symbol-mode)
            (pretty-symbols-mode)
            (auto-complete-mode)
            (jedi:setup)
            (which-function-mode t)
            ;; (define-key python-mode-map (kbd "C-]") 'pycscope-find-global-definition-no-prompting)
            ;; (define-key python-mode-map (kbd "C-t") 'pycscope-pop-mark)
            ;; (define-key python-mode-map (kbd "C-.") 'pycscope-find-global-definition)
            ;; (define-key python-mode-map "(" 'elpy-nav-backward-statement)
            ;; (define-key python-mode-map ")" 'elpy-nav-forward-statement)
            ;; (define-key python-mode-map "[" 'elpy-nav-backward-definition)
            ;; (define-key python-mode-map "]" 'elpy-nav-forward-definition)
            ;; (define-key python-mode-map "{" 'elpy-nav-backward-class-definition)
            ;; (define-key python-mode-map "}" 'elpy-nav-forward-class-definition)
            ;; (define-key python-mode-map " c" 'elpy-occur-definitions)
            (define-key python-mode-map (kbd "C-c C-c") 'run-nose-test)
            (define-key python-mode-map (kbd "C-c C-v") 'run-nose-test-new-buffer)
            (define-key python-mode-map (kbd "C-c d") 'jedi:show-doc)
            (define-key python-mode-map (kbd "C-c C-n") 'jedi:dot-complete)
            (nmap " c"  'jedi-direx:pop-to-buffer)))

;;  (setq flymake-python-pyflakes-executable "flake8")
;;  (require 'flymake-python-pyflakes)
;;  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(pretty-add-keywords 'python-mode `(("True"  . "т")
                                    ("False" . "ғ")
                                    ("is not" . "≠")
                                    ("def" . "ƒ")
                                    ("return" . "↞")
                                    ;;  ("class" . "ϰ")
                                    ("self\\." . "@")
                                    ("kwargs" . "ӄ")))

;;  highlight Django templating stuff
(defvar django-tag-face (make-face 'django-tag-face))
(set-face-foreground 'django-tag-face "Orange")

(defvar django-variable-face (make-face 'django-variable-face))
(set-face-foreground 'django-variable-face "Green")

(defvar django-comment-face (make-face 'django-comment-face))
(set-face-foreground 'django-comment-face "Gray")

(font-lock-add-keywords
 'html-mode
 '(
   ("\\({%[^%]*%}\\)" 1 django-tag-face prepend)
   ("\\({{[^}]*}}\\)" 1 django-variable-face prepend)
   ("\\({#[^}]*#}\\)" 1 django-comment-face prepend)
   ("\\({% comment %}\\(.\\|
\\)*{% endcomment %}\\)" 1 django-comment-face prepend)
   ))


;;  skeletons for Django template tags
(define-skeleton template-tag-skeleton
  "Insert a {% foo %} template tag"
  "Template tag name: "
  "{% " str " %}")
(define-skeleton template-variable-skeleton
  "Insert a {{ foo }} template variable"
  "Template variable: "
  "{{ " str " }}")
(define-skeleton template-comment-skeleton
  "Insert a {# foo #} template variable"
  "Comment: "
  "{# " str " #}")
(define-skeleton template-block-skeleton
  "Insert {% block foo %}{% endblock %}"
  "Block name: "
  "{% block " str " %}\n" - "\n{% endblock %}")
(define-skeleton template-if-else-skeleton
  "Insert {% if foo %}{% else %}{% endif %}"
  "If condition: "
  "{% if " str " %}\n" - "\n{% else %}\n\n{% endif %}")
(define-skeleton template-if-skeleton
  "Insert {% if foo %}{% endif %}"
  "If condition: "
  "{% if " str " %}" - "{% endif %}")
(define-skeleton underscore-skeleton
  "Insert <%= foo %>"
  "Contents: "
  "<%= " str " %>")

(defvar template-skeletons
  '(template-tag-skeleton
    template-variable-skeleton
    template-comment-skeleton
    template-block-skeleton
    template-if-skeleton
    template-if-else-skeleton
    underscore-skeleton))

(defun insert-django-skeleton ()
  (interactive)
  (let* ((skeleton-names (mapcar 'symbol-name template-skeletons))
         (skeleton-chosen (ido-completing-read "HTML skeleton: " skeleton-names)))
    (funcall (intern skeleton-chosen))))


;;  (define-key html-mode-map "\C-ct" 'insert-django-skeleton)

;;  (defun visit-parent-django-template ()
;;    "In a buffer containg {% extends \"foo.html\" %}, visit foo.html."
;;    (interactive)
;;    (let (start-pos end-pos template-name)
;;      (save-excursion
;;        (widen)
;;        (goto-char (point-min))
;;        ;; Find the extends tag
;;        (while (not (looking-at "{% ?extends"))
;;          (forward-char 1))
;;        ;; Find the opening " of the file name.
;;        (while (not (looking-at "\""))
;;          (forward-char 1))
;;        (forward-char)
;;        (setq start-pos (point))

;;        ;; Find the closing "
;;        (while (not (looking-at "\""))
;;          (forward-char 1))
;;        (setq end-pos (point))

;;        (setq template-name (buffer-substring-no-properties start-pos end-pos)))

;;      ;; Open this file, assuming it's in the same directory.
;;      ;; TODO: Search the current VCS checkout for it.
;;      (find-file template-name)))

(provide 'init-python)
