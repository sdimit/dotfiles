
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(defun syn ()
  "shows nouns in text"
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           "syn -n"
                           nil
                           t)
  (ansi-color-apply-on-region (point-min)
                              (point-max)))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

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

(defun stk/find-function-or-variable-at-point ()
  "Find directly the function/variable at point in the other window.
     I don't care if is a function or a variable... just go there, Emacs!"
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

;; TODO: add to all prog mode
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

;; Quick switch to scratch buffers

(defmacro scratch-key (key buffer-name mode)
  `(global-set-key ,key (lambda ()
                          (interactive)
                          (switch-to-buffer ,buffer-name)
                          (unless (eq major-mode ',mode)
                            (,mode)))))

(scratch-key (kbd "C-S-s") "*scratch*"    emacs-lisp-mode)
(scratch-key (kbd "C-S-d") "*javascript*" js2-mode)
(scratch-key (kbd "C-S-a") "*lisp*"       lisp-mode)
;; (scratch-key (kbd "C-S-c") "*clojure*"    clojure-mode)
(scratch-key (kbd "C-S-x") "*css*"        css-mode)
(scratch-key (kbd "C-S-h") "*html*"       html-mode)



;; (defun push-first-button ()
;;   "Find and push the first button in this buffer, intended for `help-mode'."
;;   (interactive)
;;   (block :find-button
;;     (goto-char (point-min))
;;     (while (< (point) (point-max))
;;       (if (get-text-property (point) 'button)
;;           (return-from :find-button (push-button))
;;         (forward-char)))))
;;
;; (define-key help-mode-map "f" 'push-first-button)

(setq deft-extension "org"
      deft-text-mode 'org-mode
      deft-auto-save-interval 10.0)

(defun get-last-message ()
  "ideally some kill-ring navigation. or helm??"
  (get-buffer "*Messages*")
  ;; grab last line and put into kill-ring
  )

(defun send-region-to-end-of-file ()
  "writing tools package or init file of someone had that...")

;;  ideal keybinding
;;  C-k C-k kill buffer

(defun nuke-useless-buffers ()
  "git, dired, emacs temp buffers"
  )

(require 'sr-speedbar)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun append-to-emacs-config (start end)
  (interactive "r")
  (append-region-to-file start end "~/Inbox/new-stuff.el"))

(defun append-region-to-file (start end filename)
  "function takes current region, and writes it to specified file"
  (write-region start end filename t)
  (kill-region start end))

(setq recentf-max-saved-items 100)
(recentf-mode)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)


(defun yank-annotated ()
  "Yanks the region, puts a filename and line numbers. Lots of fun"
  (interactive)
  (let* ((start-pos (save-excursion (goto-char (region-beginning))
                                    (beginning-of-line)
                                    (point)))
         (end-pos (save-excursion (goto-char (region-end))
                                  (end-of-line)
                                  (point)))
         (start-line (line-number-at-pos (region-beginning)))
         (content (buffer-substring start-pos end-pos))
         (filename buffer-file-name)
         (r (with-temp-buffer
              (insert content)
              (newline)
              (while (line-move -1 t)
                (beginning-of-line)
                (insert (format "%4d | " (+ (line-number-at-pos) start-line -1))))

              (goto-char (point-min))
              (insert filename)
              (newline)
              (newline)
              (buffer-string))))
    ;; (message "%s %s %d %d %d" filename content start-pos end-pos start-line)
    (message "%s" r)
    (kill-new r)
    (deactivate-mark)))

;; Mac-like key defaults
(global-set-key (kbd "M-n") 'create-new-buffer)
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-use-imenu-flag t
      speedbar-file-unshown-regexp "flycheck-.*"
      sr-speedbar-width 40
      sr-speedbar-width-x 40
      sr-speedbar-auto-refresh nil
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

(setq next-error-recenter 20)

(defun delete-last-char-on-line ()
  (interactive)
  (end-of-line)
  (backward-char)
  (delete-char 1)
  (beginning-of-line))

;;  (nmap (kbd "M-d C-.") 'delete-last-char-on-line)

;;;###autoload
(progn
  (defadvice find-tag (around find-tag-around)
    "Before calling `find-tag', set correct TAGS-files."
    (tags-reset-tags-tables)
    (let ((tags-table-list (rwd-tag-file-list)) (tags-file-name))
      ad-do-it))
  (ad-activate 'find-tag))

;;;###autoload
(defun rwd-find-file-up (file-name &optional dir)
  (let ((f (expand-file-name file-name (or dir default-directory)))
        (parent (file-truename (expand-file-name ".." dir))))
    (cond ((string= dir parent) nil)
          ((file-exists-p f) f)
          (t (rwd-find-file-up file-name parent)))))

;;;###autoload
(defun rwd-tag-file-list ()
  (let ((dir (rwd-find-file-up "TAGS")))
    (if dir (list dir) nil)))

(defun split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun ido-invoke-in-other-window ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

(defun ido-invoke-in-horizontal-split ()
  "signals ido mode to split horizontally and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun ido-invoke-in-vertical-split ()
  "signals ido mode to split vertically and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defadvice ido-read-internal (around ido-read-internal-with-minibuffer-other-window activate)
  (let* (ido-exit-minibuffer-target-window
         (this-buffer (current-buffer))
         (result ad-do-it))
    (cond
     ((equal ido-exit-minibuffer-target-window 'other)
      (if (= 1 (count-windows))
          (split-window-horizontally-and-switch)
        (other-window 1)))
     ((equal ido-exit-minibuffer-target-window 'horizontal)
      (split-window-horizontally-and-switch))

     ((equal ido-exit-minibuffer-target-window 'vertical)
      (split-window-vertically-and-switch))
     ((equal ido-exit-minibuffer-target-window 'frame)
      (make-frame)))
    (switch-to-buffer this-buffer) ;; why? Some ido commands, such as textmate.el's textmate-goto-symbol don't switch the current buffer
    result))

(defadvice ido-init-completion-maps (after ido-init-completion-maps-with-other-window-keys activate)
  (mapcar (lambda (map)
            (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
            (define-key map (kbd "C-2") 'ido-invoke-in-vertical-split)
            (define-key map (kbd "C-3") 'ido-invoke-in-horizontal-split)
            (define-key map (kbd "C-v") 'ido-invoke-in-vertical-split)
            (define-key map (kbd "C-s") 'ido-invoke-in-horizontal-split)
            (define-key map (kbd "C-4") 'ido-invoke-in-other-window)
            (define-key map (kbd "C-5") 'ido-invoke-in-new-frame))
          (list ido-buffer-completion-map
                ido-common-completion-map
                ido-file-completion-map
                ido-file-dir-completion-map)))

;; (defun delete-last-char-on-line ()
;;   (interactive)
;;   (end-of-line)
;;   (backward-char)
;;   (delete-char 1)
;;   (beginning-of-line))
;;
;; (nmap (kbd "M-d C-.") 'delete-last-char-on-line)

(provide 'init-experimental)
