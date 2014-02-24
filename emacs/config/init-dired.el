
(load "dired-x")

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook (lambda ()
                             (define-key dired-mode-map "h" 'dired-up-directory)
                             (define-key dired-mode-map "l" 'diredp-find-file-reuse-dir-buffer)
                             (define-key dired-mode-map "/" 'dired-isearch-filenames)
                             (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
                             (define-key dired-mode-map "j" 'diredp-next-line)
                             (define-key dired-mode-map "~" (bind (dired "~")))
                             (define-key dired-mode-map "k" 'diredp-previous-line)
                             (define-key dired-mode-map "K" 'dired-do-delete)
                             (define-key dired-mode-map "R" 'dired-efap)
                             (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
                             (define-key dired-mode-map ":" 'smex)
                             (define-key dired-mode-map "G" 'end-of-buffer)
                             (define-key dired-mode-map "." 'dired-dotfiles-toggle)
                             (define-key dired-mode-map "g" 'beginning-of-buffer)
                             (define-key dired-mode-map "Q" 'quit-window)))

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(nmap " I" (lambda () (interactive) (ido-find-file-in-dir "~/Inbox/")))

(require 'wdired)
(require 'dired+)
(require 'dired-details)
(require 'dired-efap)

(load "dired-x")

(toggle-diredp-find-file-reuse-dir 1)

;; Make dired less verbose
(setq-default dired-details-hidden-string "")
(dired-details-install)


(add-hook 'dired-after-readin-hook 'dired-file-name-filter-handler)

(setq font-lock-maximum-decoration nil)

(defvar dired-file-name-filter nil
  "*File name filter. Only files with name matching the regexp dired-file-name-filter are shown in the dired buffer.")

(make-variable-buffer-local 'dired-file-name-filter)

(defvar dired-filter-name-marker 16)

(defun dired-file-name-filter (filter)
  "Set variable `dired-file-name-filter' to filter."
  (interactive "sFile name filter regexp (or empty string for no filter):")
  (setq dired-file-name-filter (if (= (length filter) 0) nil filter))
  (dired-revert))

(define-key dired-mode-map [menu-bar regexp filter] '(menu-item "Filter" dired-file-name-filter :help "Set file name filter."))

(define-key dired-mode-map (kbd "% f") 'dired-file-name-filter)

(defun dired-file-name-filter-handler ()
  "To be hooked into `dired-after-readin-hook'."
  (when dired-file-name-filter
    (goto-char (point-min))
    (insert "Dired Filter Name Filter:" dired-file-name-filter)
    (let ((dired-marker-char dired-filter-name-marker))
      (dired-map-dired-file-lines
       '(lambda (name)
          (unless (string-match dired-file-name-filter name)
            (dired-mark 1)
            )))
      (dired-do-kill-lines nil (concat "Filter" dired-file-name-filter " omitted %d line%s")))))

(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode
"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

(setq dired-details-initially-hide t)

(eval-after-load 'dired
  '(progn
     (setq-default dired-details-hidden-string "")
     (define-key dired-mode-map "(" 'dired-details-toggle)
     (define-key dired-mode-map ")" 'dired-details-toggle)
     (define-key dired-mode-map "/" 'diredext-exec-git-command-in-shell)

     (require 'dired+)
     (setq dired-recursive-deletes 'top)
     ))

(defun dired-open-in-mac ()
  "Remap 'o' in dired mode to open a file"
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(define-key dired-mode-map "o" 'dired-open-in-mac)

(provide 'init-dired)
