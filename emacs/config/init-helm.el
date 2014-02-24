(require 'helm)

(setq helm-buffer-details-flag nil)
(setq helm-ff-transformer-show-only-basename t)
(setq helm-quick-update t
      helm-idle-delay 0.01

      helm-input-idle-delay 0.01)
(setq helm-M-x-always-save-history t)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-i") 'helm-select-action)

(eval-after-load "helm"
  '(progn
     (set-face-attribute 'helm-source-header nil
                         :height 1.0 :weight 'semi-bold :family nil)))

;; helm faces
(eval-after-load "helm-files"
  '(progn
     (define-key helm-find-files-map (kbd "C-M-u") 'helm-find-files-down-one-level)))

;; my helm utilities
(defun my/helm-git-project-source (pwd)
  (cl-loop for (description . option) in
           '(("Modified Files" . "--modified")
             ("Untracked Files" . "--others --exclude-standard")
             ("All Files" . ""))
           for cmd = (concat "git ls-files " option)
           collect
           `((name . ,(format "%s (%s)" description pwd))
             (init . (lambda ()
                       (with-current-buffer (helm-candidate-buffer 'global)
                         (call-process-shell-command ,cmd nil t))))
             (candidates-in-buffer)
             (action . (("Open File" . find-file)
                        ("Open Directory" . (lambda (file)
                                              (dired (file-name-directory file))))
                        ("Open File other window" . find-file-other-window)
                        ("Insert buffer" . insert-file))))))

(defun my/helm-git-project-files ()
  (interactive)
  (let ((topdir (locate-dominating-file default-directory ".git/")))
    (unless topdir
      (error "I'm not in Git Repository!!"))
    (let ((default-directory topdir)
          (sources (my/helm-git-project-source topdir)))
      (helm-other-buffer sources "*Helm Git Project*"))))

(defun my/helm-etags-select (arg)
  (interactive "P")
  (let ((tag  (helm-etags-get-tag-file))
        (helm-execute-action-at-once-if-one t))
    (when (or (equal arg '(4))
              (and helm-etags-mtime-alist
                   (helm-etags-file-modified-p tag)))
      (remhash tag helm-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-source-etags-select :keymap helm-etags-map
              :input (concat (thing-at-point 'symbol) " ")
              :buffer "*helm etags*"
              :default (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
      (message "Error: No tag file found, please create one with etags shell command."))))

(provide 'init-helm)
