
(define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)

(ido-vertical-mode t)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
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

(require 'zlc)
(setq zlc-select-completion-immediately t)

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

(provide 'init-command-mode)
