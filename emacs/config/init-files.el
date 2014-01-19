
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
  "Create a new buffer named *new*[num]."
  (interactive)
  (switch-to-buffer-other-window (generate-new-buffer-name "*new*"))
  (lisp-interaction-mode))

;; Create a new instance of emacs
(when window-system
  (defun new-emacs-instance ()
    (interactive)
    (let ((path-to-emacs
           (locate-file invocation-name
                        (list invocation-directory) exec-suffixes)))
      (call-process path-to-emacs nil 0 nil))))

(global-set-key (kbd "C-c n") 'create-new-buffer)
(global-set-key (kbd "C-c C-n") (bind (create-new-buffer) (delete-other-windows)))
(global-set-key (kbd "C-c N") 'new-emacs-instance)


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

(global-set-key (kbd "C-h C-c") (bind (ido-find-file-in-dir "~/.emacs.d/config/")))


;; Stop creating backup~ and #auto-save# files

(setq make-backup-files nil)
(setq auto-save-default nil)

(provide 'init-files)
