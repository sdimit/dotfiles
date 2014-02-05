
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

;; TODO: add to all prog mode
(add-hook 'coffee-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))))
(add-hook 'python-mode-hook
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

;; TODO
(defun get-last-message ()
  "ideally some kill-ring navigation. or helm??"
  (get-buffer "*Messages*")
  ;; grab last line and put into kill-ring
  )

;;  ideal keybinding
;;  C-k C-k kill buffer

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)


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

;;;###autoload

(defadvice find-tag (around find-tag-around)
  "Before calling `find-tag', set correct TAGS-files."
  (tags-reset-tags-tables)
  (let ((tags-file-name (find-file-up "TAGS")))
    ad-do-it))
(ad-activate 'find-tag)

;;;###autoload
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

(provide 'init-experimental)
