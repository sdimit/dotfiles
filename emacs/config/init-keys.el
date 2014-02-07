;;; minor mode to manage my personal kbd map
;;
(defgroup istib-mode nil
  "Sliim personal keybindings"
  :group 'projext)

(defvar istib-mode-keymap-prefix (kbd "C-.")
  "Istib-Mode keymap prefix.")

(defvar istib-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "h g") 'helm-ls-git-ls)
      (define-key prefix-map (kbd "h p") 'helm-pydoc)
      (define-key prefix-map (kbd "h f") 'helm-find-files)
      (define-key prefix-map (kbd "h q") 'helm-quickrun)
      (define-key prefix-map (kbd "h s") 'helm-github-stars)
      (define-key prefix-map (kbd "h w") 'helm-swoop)
      (define-key prefix-map (kbd "h r") 'helm-git-grep)
      (define-key prefix-map (kbd "h y") 'helm-c-yas-complete)
      (define-key prefix-map (kbd "p i") 'projext-show-current-project)
      (define-key prefix-map (kbd "p t") 'projext-regenerate-tags)
      (define-key prefix-map (kbd "p c t") 'projext-clean-project-tags)
      (define-key prefix-map (kbd "p c d") 'projext-clean-project-desktop)
      (define-key prefix-map (kbd "p c a") 'projext-clean-project)
      (define-key prefix-map (kbd "p f") 'projext-find)
      (define-key prefix-map (kbd "p s") 'project-persist-save)
      (define-key prefix-map (kbd "p k") 'project-persist-close)
      (define-key prefix-map (kbd "p d") 'direx-project:jump-to-project-root-other-window)
      (define-key prefix-map (kbd "p n") 'project-persist-create)
      (define-key prefix-map (kbd "p h") 'helm-project-persist)
      (define-key prefix-map (kbd "p x") 'project-persist-delete)
      (define-key prefix-map (kbd "e a") 'emms-add-directory-tree)
      (define-key prefix-map (kbd "e n") 'emms-next)
      (define-key prefix-map (kbd "e b") 'emms-previous)
      (define-key prefix-map (kbd "e p") 'emms-pause)
      (define-key prefix-map (kbd "e h") 'helm-emms)
      (define-key prefix-map (kbd "e l") 'emms-smart-browse)
      (define-key prefix-map (kbd "e d") 'emms-play-playlist-directory-tree)
      (define-key prefix-map (kbd "f n") 'flymake-goto-next-error)
      (define-key prefix-map (kbd "f p") 'flymake-goto-prev-error)
      (define-key prefix-map (kbd "q q") 'quickrun)
      (define-key prefix-map (kbd "q e") 'quickrun-region)
      (define-key prefix-map (kbd "q a") 'quickrun-with-arg)
      (define-key prefix-map (kbd "q s") 'quickrun-shell)
      (define-key prefix-map (kbd "q r") 'quickrun-replace-region)
      (define-key prefix-map (kbd "g s") 'magit-status)
      (define-key prefix-map (kbd "g l") 'magit-log)
      (define-key prefix-map (kbd "a") 'auto-complete)
      (define-key prefix-map (kbd "w") 'whitespace-mode)
      (define-key prefix-map (kbd "i") 'iedit-mode)
      (define-key prefix-map (kbd "b") 'emacsd-helm-buffers-right-side)
      (define-key prefix-map (kbd "k") 'set-cmd-to-kbd)
      (define-key prefix-map (kbd "d") 'direx:jump-to-directory-other-window)
      (define-key prefix-map (kbd "t") 'proced)
      (define-key prefix-map (kbd "s") 'eshell)

      (define-key map istib-mode-keymap-prefix prefix-map))
    map)
  "Keymap for Istib-Mode mode.")

;;;###autoload
(define-globalized-minor-mode istib-mode-global-mode
  istib-mode
  istib-mode 1)

;;;###autoload
(define-minor-mode istib-mode
  "Minor mode for istib's keybindings map."
  :lighter " Istib-Mode"
  :keymap istib-mode-map
  :group 'istib-mode)

(provide 'istib-mode)
(skbd-global-mode)

(defun emacsd-set-cmd-to-kbd (command keybinding)
  "Bind COMMAND to a KEYBINDING quickly."
  (interactive "sCommand: \nsKeybinding: ")
  (setq cmd command)
  (define-key skbd-mode-map (kbd keybinding) `(lambda ()
                                         (interactive)
                                         (let ((default-directory ,default-directory))
                                           (compile ,cmd)))))
