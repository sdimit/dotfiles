(require 'deft)

(setq deft-extension "org"
      deft-directory "~/Notes"
      deft-text-mode 'org-mode
      deft-auto-save-interval 10.0)

(global-set-key (kbd "C-x C-n") 'deft-find-file)
(define-key deft-mode-map (kbd "C-g") 'quit-window)

(provide 'init-deft)
