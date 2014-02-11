
;; let command be meta too
(setq mac-command-modifier 'meta)

;; follow symlinks and don't ask questions
(setq vc-follow-symlinks t)

(setq confirm-nonexistent-file-or-buffer nil)
(setq auto-save-interval 10)

(require 'helm-ls-git)

;;  Make sure buffers update when files change
(global-auto-revert-mode)

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)
(define-key evil-normal-state-map " w" 'save-all)


(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

;; sort ido filelist by mtime instead of alphabetically
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
            (lambda (x) (and (char-equal (string-to-char x) ?.) x))
            ido-temp-list))))
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

(defun my-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(nmap " rn" 'my-rename-current-buffer-file)

(defun my-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(nmap " RM" 'my-delete-current-buffer-file)

(defun copy-yank-str (msg)
  (kill-new msg)
  (with-temp-buffer
    (insert msg)
    (shell-command-on-region (point-min) (point-max)
                             (cond
                              ((eq system-type 'cygwin) "putclip")
                              ((eq system-type 'darwin) "pbcopy")
                              ))))

(defun copy-filename-of-current-buffer ()
  "copy file name (NOT full path) into the yank ring and OS clipboard"
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (file-name-nondirectory buffer-file-name))
      (kill-new filename)
      (copy-yank-str filename)
      (message "filename %s => clipboard & yank ring" filename)
      )))

(defun copy-full-path-of-current-buffer ()
  "copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))
    (copy-yank-str (file-truename buffer-file-name))
    (message "full path of current buffer => clipboard & yank ring")
    ))

(global-set-key (kbd "C-x y f") 'copy-full-path-of-current-buffer)

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;  Save Place in Opened Files
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saved-places"))
(require 'saveplace)

(defun create-new-buffer ()
  (interactive)
  (switch-to-buffer-other-window (generate-new-buffer-name "*new*")))

(defun create-new-buffer-in-same-window ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

;; Create a new instance of emacs
(when window-system
  (defun new-emacs-instance ()
    (interactive)
    (let ((path-to-emacs
           (locate-file invocation-name
                        (list invocation-directory) exec-suffixes)))
      (call-process path-to-emacs nil 0 nil))))

(global-set-key (kbd "C-c C-n") 'create-new-buffer-in-same-window)
(global-set-key (kbd "C-c n") 'create-new-buffer)
;;(global-set-key (kbd "C-c C-n") (bind (create-new-buffer) (delete-other-windows)))
;(global-set-key (kbd "C-c N") 'new-emacs-instance)


(require 'f)

(defun find-file-relative-to-current (&optional relative-index cycle)
  "switches buffer to a nearby file
         arguments:
         - relative-index: signed integer (default 1) which specifies the step relative to current file
         - cycle: boolean (default true), specifies whether to jump to other edge when reaching end of dir
           FIXME
          -- requires dash.el and f.el"
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (dir-contents (directory-files current-dir))
         (dir-files (-filter 'f-file? dir-contents))
         (current-file  (file-name-nondirectory (buffer-file-name)))
         (index-current-file (-elem-index current-file dir-files))
         (dir-files-length (length dir-files))
         (target-index (incf index-current-file (or relative-index 1)))
         (cycle (if (boundp 'cycle) cycle t))
         (index (cond
                 ((and cycle (>= target-index dir-files-length)) 0)
                 ((and cycle (< target-index 0)) (decf dir-files-length))
                 (t target-index)))
         (nextfile (nth index dir-files)))
    (find-file nextfile)))

(defun next-file-in-dir ()
  "like in vim-unimpaired."
  (interactive) (find-file-relative-to-current 1))

(defun previous-file-in-dir ()
  "like in vim-unimpaired."
  (interactive) (find-file-relative-to-current -1))

(nmap (kbd "] f") 'next-file-in-dir)
(nmap (kbd "[ f") 'previous-file-in-dir)

(setq default-major-mode 'text-mode)

(defun zshrc ()
  (interactive)
  (find-file "~/.zshrc"))

(defun zshhistory ()
  (interactive)
  (find-file "~/.zsh_history"))

(defun inbox-dired ()
  (interactive)
  (dired "~/Inbox"))


;; Stop creating backup~ and #auto-save# files

(setq make-backup-files nil)
(setq auto-save-default nil)

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

  (setq-default locate-command "mdfind")

(defun nuke-useless-buffers ()
  "git, dired, emacs temp buffers"
  )

(defun kill-region-to-config-file (start end)
  "takes current region, and writes it to one emacs config file.
   makes sure to keep the (provide 'feature) line at the bottom of file"
  (interactive "r")
  (let ((filename (ido-read-file-name "Append to: " "~/.emacs.d/config/")))
    (write-region start end filename t)
    (kill-region start end)
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-min))
      (re-search-forward "^(provide '")
      (goto-char (line-beginning-position))
      (kill-line 1)
      (goto-char (point-max))
      (newline)
      (yank))))

(defun save-region-into-emacs-inbox (start end)
  "takes current region, and writes it to one emacs config file."
  (interactive "r")
  (let ((filename "~/Inbox/Emacs/inbox.el"))
    (write-region start end filename t)))


(defun kill-and-append-region-to-file (start end)
  "function takes current region, and writes it to specified file"
  (interactive "r")
  (let ((filename (ido-read-file-name "Kill and append to: ")))
    (write-region start end filename t)
    (kill-region start end)))

(setq recentf-max-saved-items 100)
(recentf-mode)

;; Ignore case when completing...filenames too
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)
;; Scroll with the compilation output
(setq compilation-scroll-output t)
(setq compilation-window-height 18)

(defun auto-chmod ()
  "If we're in a script buffer, then chmod +x that script."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (shell-command (concat "chmod u+x " buffer-file-name))
       (message (concat "Saved as script: " buffer-file-name))))

(setq tramp-default-method "ssh")

(defun recentf-ido-find-file ()
  ;;from http://www.xsteve.at/prg/emacs/power-user-tips.html
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string
                                     (concat home "/") "~/"
                                     path))
                                  recentf-list)
                          nil t))))


(defun open-html-from-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(recentf-mode 1)

(auto-compression-mode t)

  (if (eq system-type 'darwin)
      (setq system-name (car (split-string system-name "\\."))))

(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(provide 'init-files)
