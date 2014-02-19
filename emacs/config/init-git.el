(setq ediff-highlight-all-diffs nil)

(set-variable 'magit-emacsclient-executable "/usr/local/Cellar/emacs-mac/emacs-24.3-mac-4.5/bin/emacsclient")

(setq magit-save-some-buffers 'dontask)
(setq magit-stage-all-confirm nil)
(setq magit-unstage-all-confirm nil)

(setq magit-default-tracking-name-function
      (lambda (remote branch) branch))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (set-fill-column 72)
            (auto-fill-mode 1)))
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit '(progn
                           (set-face-foreground 'magit-diff-add "green3")
                           (set-face-foreground 'magit-diff-del "red3")))

(defun magit-status-only ()
  (interactive)
  (magit-status default-directory)
  (delete-other-windows))

(defun magit-log-current-file ()
  (interactive)
  (magit-file-log (buffer-file-name (current-buffer))))

(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

(defun magit-delete-remote-branch ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (let* ((current-branch (shell-command-as-string "git rev-parse --abbrev-ref HEAD"))
        (remote-branch (concat ":" current-branch)))
    (magit-run-git (list "push" "origin" remote-branch))))

;; (define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

;; close popup when commiting

(defadvice git-commit-commit (after delete-window activate)
  (delete-window))

(global-git-gutter+-mode t)

(setq git-gutter+-modified-sign ".")
(set-face-foreground 'git-gutter+-modified "orange4")
(set-face-foreground 'git-gutter+-added "green4")
(set-face-foreground 'git-gutter+-deleted "red4")
(set-face-background 'git-gutter+-modified nil)
(set-face-background 'git-gutter+-added nil)
(set-face-background 'git-gutter+-deleted nil)

(nmap " mka" 'git-gutter+-stage-hunks) ; [m]agit hun[k] [a]dd
(nmap " mkd" (bind (git-gutter+-popup-hunk)
                   (switch-to-buffer-other-window "*git-gutter+-diff*")))
;;  [m]agit hun[k] show [d]iff
(nmap " mkK" 'git-gutter+-revert-hunk) ; [m]agit hun[k] [K]ill
(nmap " mkm" 'git-messenger:popup-message) ; [m]agit hun[k] show [m]essage
(nmap "[g" 'git-gutter+-next-hunk)
(nmap "]g" 'git-gutter+-previous-hunk)

(nmap " mbb" 'magit-blame-mode)
(nmap " mbl" 'magit-blame-locate-commit)
(nmap " mdd" 'magit-prompt-diff)
(nmap " mdm" 'ediff-current-file-with-master)
(nmap " mD" (bind (call-interactively 'magit-diff)
                  (switch-to-buffer "*magit-diff*")
                  (delete-other-windows)))
(nmap " mv" 'magit-interactive-checkout)
(nmap " mV" 'magit-branch-manager)
(nmap " ms" 'magit-status)
(nmap " `" 'magit-status)
(nmap " mS" 'magit-status-with-details)
(nmap " ml" 'magit-log-current-file)
(nmap " mA" 'magit-log)
(nmap " mc" 'magit-commit)
(nmap " mO" 'magit-oops)
(nmap " mL" (bind (magit-show-commit "HEAD")))
(nmap " mr" 'magit-rebase-step)
(nmap " mR" 'magit-interactive-rebase)
(nmap " mf" (bind (magit-git-command "fetch --all")))
(nmap " mF" (bind (magit-git-command "pull --rebase")))


(setq git-messenger:show-detail t)

(defun magit-review ()
  (interactive)
  (magit-diff "master")
  (switch-to-buffer "*magit-diff*")
  (delete-other-windows))

(require 'helm-open-github)

(defun -get-github-url-for-file-region (file &optional start end)
  (let ((host (helm-open-github--host))
        (remote-url (helm-open-github--remote-url))
        (branch (helm-open-github--branch))
        (marker (helm-open-github--highlight-marker start end)))
    (helm-open-github--file-url host remote-url branch file marker)))


(defun yank-github-url-for-region ()
  "copies url of current selected region into clipboard (for easy sharing in IM)
      depends on helm-open-github)"
  (interactive)
  (if (not mark-active)
      (print "no region selected")
    (let* ((file (buffer-file-name))
           (start (region-beginning))
           (end (region-end))
           (root (helm-open-github--root-directory))
           (repo-path (file-relative-name file root))
           (start-line (line-number-at-pos start))
           (end-line (line-number-at-pos end)))
      (kill-new (-get-github-url-for-file-region repo-path start-line end-line)))))

;; (defun navigate-to-commit-on-github ()
;;   (interactive)
;;   (yank-github-url-for-commit-at-point)
;;   (browse-url (yank)))

;; magit
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)

(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "t" 'magit-toggle-file-section
  ":" 'smex
  "w" 'evil-forward-WORD-begin
  "W" 'magit-wazzup
  "J" 'open-jira-ticket-from-point
  "h" 'magit-toggle-diff-refine-hunk)

(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "w" 'evil-forward-WORD-begin
  "J" 'open-jira-ticket-from-point
  "W" 'magit-wazzup)

(evil-add-hjkl-bindings magit-commit-mode-map 'emacs
  "t" 'magit-toggle-file-section
  ":" 'smex
  "w" 'evil-forward-WORD-begin
  "W" 'magit-wazzup
  "J" 'open-jira-ticket-from-point
  "h" 'magit-toggle-diff-refine-hunk)

(evil-add-hjkl-bindings magit-log-mode-map 'emacs
  "t" 'magit-toggle-file-section
  ":" 'smex
  "w" 'evil-forward-WORD-begin
  "W" 'magit-wazzup
  "J" 'open-jira-ticket-from-point
  "h" 'magit-toggle-diff-refine-hunk)

(defun ediff-current-file-on-git ()
  (interactive)
  (ediff-revision (buffer-file-name (current-buffer))))

(defun magit-prompt-diff ()
  (interactive)
  (let* ((revision (magit-read-rev "Diff with: " "master")))
    (ediff-vc-internal revision "HEAD")))

(defun ediff-current-file-with-master ()
  (interactive)
  (ediff-vc-internal "master" "HEAD"))

(add-hook 'magit-log-edit-mode-hook
          (lambda () (flyspell-mode t)))

(defun git-write-file ()
  (interactive)
  (magit-stage-item (buffer-file-name (current-buffer))))

(defalias 'conflicts-keep-current 'smerge-keep-current)
(defalias 'conflicts-keep-first 'smerge-keep-mine)
(defalias 'conflicts-keep-second 'smerge-keep-other)

(require 'smerge-mode)
;;  (define-key smerge-mode-map (kbd "<C-return>") 'conflicts-keep-current)
;;  (define-key smerge-mode-map (kbd "<f8>") 'smerge-prev)
;;  (define-key smerge-mode-map (kbd "<f9>") 'smerge-next)

(setq magit-completing-read-function 'magit-ido-completing-read)

(defun show-commit-at-point ()
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (commit-info (git-messenger:commit-info-at-line file line nil))
         (commit-id (car commit-info)))
    (magit-show-commit commit-id)
    (switch-to-buffer-other-window "*magit-commit*")))

(nmap " mkl" 'show-commit-at-point)

(defun git-purr ()
  (interactive)
  (magit-run-git "pull --rebase"))

(defun 10to8-git-flow-command (flow-command ticket-number)
  (let* ((branch-suffix (concat "TTE-" ticket-number))
         (flow-prefix "flow")
         (default-directory "~/10to8/Native/native/src")
         (complete-command (concat flow-prefix
                                   " "
                                   flow-command
                                   " "
                                   branch-suffix)))
    (magit-git-command complete-command default-directory)))

;;  options: fetch from origin, and keep branch
;;  cf https://github.com/nvie/gitflow/wiki/Command-Line-Arguments

(defun 10to8-flow-start-feature (ticket-number)
  (interactive "MTicket number: ")
  (10to8-git-flow-command "feature start"
                          ticket-number))

(defun 10to8-flow-finish-feature (ticket-number)
  (interactive "MTicket number: ")
  (10to8-git-flow-command "feature finish -k"
                          ticket-number))

(defun 10to8-flow-start-hotfix (ticket-number)
  (interactive "MTicket number: ")
  (10to8-git-flow-command "hotfix start"
                          ticket-number))

(defun 10to8-flow-finish-hotfix (ticket-number)
  (interactive "MTicket number: ")
  (10to8-git-flow-command "hotfix finish -k"
                          ticket-number))

(defun 10to8-flow-publish-feature (ticket-number)
  (interactive "MTicket number: ")
  (10to8-git-flow-command "feature publish"
                          ticket-number))

(defun shell-command-as-string (cmd)
  (with-temp-buffer
    (shell-command-on-region (point-min) (point-max)
                             cmd t)
    (buffer-string)))

(defun extract-jira-ticket-ref (branch-ref)
  "takes the part after feature/ or hotfix/,
     if applicable"
  (concat "TTE-" (extract-jira-ticket-number branch-ref)))

(defun extract-jira-ticket-number (branch-ref)
  (let* ((ref (if (s-contains? "/" branch-ref)
                           (cadr (split-string branch-ref "/"))
                         branch-ref))
             (trimmed-ref (replace-regexp-in-string "b?\n$" "" ref))
             (result      (string-match "[^0-9]*\\([0-9]*\\)[^0-9]*" trimmed-ref))
             (return-val  (match-string 1 trimmed-ref)))
        return-val))

(defun get-current-ticket-name ()
  (let* ((branch-ref (shell-command-as-string "git rev-parse --abbrev-ref head") )
         (ticket-name (extract-jira-ticket-ref branch-ref)))
    ticket-name))

(defun -open-jira-ticket (ticket-ref)
  (if (not (eq nil ticket-ref))
      (progn
        (call-process "~/10to8/scripts/browser.sh")
        (browse-url (concat "https://tento8.atlassian.net/browse/" ticket-ref)))))

(defun open-jira-ticket (ticket-ref)
  (interactive "MTicket number: ")
  (-open-jira-ticket (concat "TTE-" ticket-ref)))

(defun open-jira-ticket-from-current-branch ()
  (interactive)
  (let* ((ticket-name (get-current-ticket-name)))
    (-open-jira-ticket ticket-name)))

(defun open-jira-ticket-from-point ()
  (interactive)
  (let* ((at-point (substring-no-properties (thing-at-point 'symbol)))
         (ticket-name (extract-jira-ticket-ref at-point)))
    (-open-jira-ticket ticket-name)))

(defun open-jira-ticket-from-region (start end)
  (interactive "R")
  (let* ((at-point (substring-no-properties (buffer-substring start end)))
         (ticket-name (extract-jira-ticket-ref at-point)))
    (-open-jira-ticket ticket-name)))

(defun magit-oops ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

(eval-after-load "magit"
  (lambda ()
    (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

;; full screen magit-status

;; (defadvice magit-status (around magit-fullscreen activate)
;;   (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-status-with-details ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status)))

(defun magit-interactive-checkout ()
  (interactive)
  (let* ((revision (magit-read-rev "Switch to")))
    (magit-run-git "checkout" revision)))

(require 's)
(defun kill-ediff-buffers ()
  (let ((ediff-buffers (-filter (lambda (buffer) (s-matches? "~" (buffer-name buffer)))
                                (buffer-list))))
    (-each ediff-buffers 'kill-buffer)))

(add-hook 'ediff-cleanup-hook 'kill-ediff-buffers)

(defun magit-switch-buffer ()
  "Interactively switch to another magit-status buffer."
  (interactive)
  (switch-to-buffer
   (format
    "*magit: %s*"
    (ido-completing-read
     "Magit buffer: "
     (loop for buffer in (buffer-list)
           as name = (buffer-name buffer)
           when (string-match "^\\*magit: \\(.+\\)\\*$" name)
           collect (match-string 1 name))))))

  (eval-after-load 'diff-mode
    '(progn
       (set-face-foreground 'diff-added "green4")
       (set-face-foreground 'diff-removed "red3")))

  (eval-after-load 'magit
    '(progn
       (set-face-foreground 'magit-diff-add "green3")
       (set-face-foreground 'magit-diff-del "red3")))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'init-git)
