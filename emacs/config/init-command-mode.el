(require 'ido)
(require 'flx-ido)

(ido-mode t)
(ido-vertical-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)

(define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)

;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)


(setq ido-enable-tramp-completion nil)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; Bind `~` to go to homedir when in ido-find-file; http://whattheemacsd.com/setup-ido.el-02.html
(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))


;; no duplicates in command history
(setq comint-input-ignoredups t)

(setq comint-scroll-to-bottom-on-input t)

(defun shell-command-and-replace-region (start end)
  (interactive "r")
  (shell-command-on-region start
                           end
                           (read-shell-command "Shell command on region: ")
                           t))

(defun shell-command-and-replace-buffer ()
  (interactive)
  (shell-command-and-replace-region (point-min) (point-max)))

(defun shell-command-on-file ()
  (interactive)
  (let* ((command (read-shell-command "Shell command on file: "))
         (filename (file-name-nondirectory buffer-file-name))
         (complete-command (concat command " '" filename "'")))
    (shell-command complete-command)))

(nmap (kbd "!") 'shell-command)
(vmap (kbd "!") 'shell-command-on-region)
(nmap (kbd "C-c !") 'shell-command-and-replace-buffer)
(nmap (kbd "C-x !") 'shell-command-on-file)
(vmap (kbd "C-c !") 'shell-command-and-replace-region)

(defun focus-minibuffer ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; FIXME
;; (global-set-key (kbd "C-f") 'focus-minibuffer)

;; (require 'zlc)
;; (setq zlc-select-completion-immediately t)

;; no duplicates in command history
(setq comint-input-ignoredups t)

(setq enable-recursive-minibuffers t)

(defun my-minibuffer-insert-word-at-point ()
  "Get word at point in original buffer and insert it to minibuffer."
  (interactive)
  (let (word beg)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (save-excursion
        (skip-syntax-backward "w_")
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq word (buffer-substring-no-properties beg (point)))))
    (when word
      (insert word))))

;; (add-hook 'minibuffer-setup-hook (lambda () (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-at-point)))

(add-hook 'minibuffer-setup-hook (lambda () (local-set-key (kbd "C-w") 'ido-delete-backward-updir)))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (define-key ido-completion-map (kbd "M-.") 'smex-find-function)
;; (define-key ido-completion-map (kbd "C-c C-d") 'smex-find-function)

(add-hook 'ack-mode-hook 'ansi-color-for-comint-mode-on)


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

(provide 'init-command-mode)
