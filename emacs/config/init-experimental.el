
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(defun url-decode-region (beg end)
  (interactive "r")
  (let ((content (url-unhex-string (buffer-substring beg end))))
    (goto-char end)
    (newline)
    (insert content)))

(defun url-encode-region (beg end)
  (interactive "r")
  (let ((content (url-hexify-string (buffer-substring beg end))))
    (goto-char end)
    (newline)
    (insert content)))

(defun find-project-root (dir)
  (f--up (f-dir? (f-expand ".git" it)) dir))

(defun sp-kill-sexp-with-a-twist-of-lime ()
  (interactive)
  (if (sp-point-in-string)
      (let ((end (plist-get (sp-get-string) :end)))
        (kill-region (point) (1- end)))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (if (or (comment-only-p beg end)
             (s-matches? "\\s+" (buffer-substring-no-properties beg end)))
          (kill-line)
        (sp-kill-sexp)))))

(defun find-function-or-variable-at-point ()
  "Find directly the function/variable at point in the other window."
  (interactive)
  (let ((var (variable-at-point))
        (func (function-called-at-point)))
    (cond
     ((not (eq var 0)) (find-variable-other-window var))
     (func (find-function-other-window func))
     (t (message "Neither function nor variable found!"))))
  (setq ac-disable-faces nil))

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))


(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defun filter-buffer-by-mode (mode)
  (-filter (lambda (buffer) (string= mode (buffer-mode buffer)))
           (buffer-list)))

;;  (defadvice git-commit-commit (after delete-window activate)
;;    (mapcar 'delete-buffer (filter-buffer-by-mode "magit-status-mode") ) )

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;  ideal keybinding
;;  C-k C-k kill buffer

;; Mac-like key defaults
(global-set-key (kbd "M-n") 'create-new-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(setq next-error-recenter 20)

(defun delete-last-char-on-line ()
  (interactive)
  (end-of-line)
  (backward-char)
  (delete-char 1)
  (beginning-of-line))

;;  (nmap (kbd "M-d C-.") 'delete-last-char-on-line)

(defadvice find-tag (around find-tag-around)
  "Before calling `find-tag', set correct TAGS-files."
  (tags-reset-tags-tables)
  (let ((tags-file-name (find-file-up "TAGS")))
    ad-do-it))
(ad-activate 'find-tag)

(defun find-file-up (file-name &optional dir)
  (let ((f (expand-file-name file-name (or dir default-directory)))
        (parent (file-truename (expand-file-name ".." dir))))
    (cond ((string= dir parent) nil)
          ((file-exists-p f) f)
          (t (find-file-up file-name parent)))))

(defun split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun delete-last-char-on-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-char -1)))

;; (nmap (kbd "M-d C-.") 'delete-last-char-on-line)

;; (evil-leader/set-key ":" 'helm-complex-command-history)
                                        ;(helm-mode 1)
;; (global-set-key (kbd "s-.") 'helm-complete-file-name-at-point)
;; ; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; (defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; (define-key my-keys-minor-mode-map (kbd "C-.") 'execute-extended-command)

;; (define-minor-mode my-keys-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   t " my-keys" 'my-keys-minor-mode-map)

;; (my-keys-minor-mode 1)

                                        ; do not do this in minibuffer
;; (defun my-minibuffer-setup-hook ()
;;   (my-keys-minor-mode 0))

;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(defun wot ()
  (interactive)
  (shell-command "afplay ~/Documents/say_what.mp4"))

(defun year-fact (year)
  "could use request.el, but almost overkill"
  (let ((year-n (string-to-number year)))
    (if (and (not (eq nil year-n)) (> year-n 0))
        (let* ((api-root "http://numbersapi.com")
               (url (concat api-root "/" year "/year"))
               (curl-command (concat "curl --silent " url)))
          (shell-command-as-string curl-command))
      (message "Can't read year"))))

(defun year-fact-from-git-branch ()
  (interactive)
  (message (year-fact (extract-jira-ticket-number (shell-command-as-string "git rev-parse --abbrev-ref head")))))

(defun align-colon-in-indent ()
  (interactive)
  (let ((beg (car (evil-indent--same-indent-range)))
        (end (cadr (evil-indent--same-indent-range))))
    (mark-region beg end)
    (align-regexp beg end ":")))

(defun mark-region (beg end)
  (interactive)
  (goto-char beg)
  (push-mark)
  (goto-char end))

(defun align-colon-in-region (beg end)
  (interactive "r")
  (align-regexp beg end ":"))

(global-set-key (kbd "C-c :") 'align-colon-in-indent)

;; (defun w3m-create-imenu ()
;;   "Create imenu index from pod2html output."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (looking-at "Location: \\(about://perldoc/[^#]+\\)")
;;       (let ((base (match-string 1))
;;             beg end
;;             list)
;;         (w3m-view-source)
;;         (search-forward "<!-- INDEX BEGIN -->")
;;         (setq beg (point))
;;         (search-forward "<!-- INDEX END -->")
;;         (setq end (point))
;;         (goto-char beg)
;;         (while (re-search-forward "<a href=\"\\(#[^\"]+\\)\">\\([^<]+\\)" end t)
;;           (push (cons (match-string 2) (match-string 1)) list))
;;         (w3m-view-source)
;;         (nreverse list)))))

;; (defun w3m-goto-function (name anchor)
;;   (if (string-match "^about://perldoc/" w3m-current-url)
;;       (w3m-goto-url (concat w3m-current-url anchor))
;;     (imenu-default-goto-function name anchor)))

;; (defun w3m-install-imenu ()
;;   (setq imenu-create-index-function 'w3m-create-imenu
;;         imenu-default-goto-function 'w3m-goto-function))

(defvar readability-token "5c4d8e64788a2ff5484b42fc351bbf3fa2b9d370"
  "API token")

(defun readability-parse (url)
  (let* ((api-root    "http://readability.com/api/content/v1/parser")
         (url-param   (concat "?url=" url))
         (token-param (concat "&token=" readability-token)))
    (concat api-root url-param token-param)))

(defun w3m-readability-current-page ()
  (interactive)
  (let* ((url w3m-current-url))
    (browse-url (readability-parse url))))

;; (require 'request)
;; (request
;;  (readability-parse "http://www.theguardian.com/books/2014/feb/20/edward-snowden-files-nsa-gchq-luke-harding")
;;  :parser 'json-read
;;  :success (function*
;;            (lambda (&key data &allow-other-keys)
;;              (let* ((content-url (assoc-default 'short_url data)))
;;                (browse-url content-url)))))

;; (readability-parse "http://blog.readability.com/2011/02/step-up-be-heard-readability-ideas/" "")

(defun insert-result-from-shell-command ()
  (interactive)
  (let ((cmd (read-string "Shell command: ")))
    (insert (shell-command-to-string cmd))))

(defun turn-on-reading-mode ()
  (interactive)
  (delete-other-windows)
  (evil-window-vsplit)
  (follow-mode)
  (beginning-of-buffer))

(defun turn-off-reading-mode ()
  (interactive)
  (delete-other-windows)
  (follow-mode -1))

(defun flush-lines-in-buffer (pattern)
  (interactive (list (read-input "Flush lines matching: ")))
  (save-excursion
    (beginning-of-buffer)
    (let ((inhibit-read-only t))
      (flush-lines pattern))))

(defun keep-lines-in-buffer (pattern)
  (interactive (list (read-input "Keep lines matching: ")))
  (save-excursion
    (beginning-of-buffer)
    (let ((inhibit-read-only t))
      (keep-lines pattern))))

(provide 'init-experimental)
