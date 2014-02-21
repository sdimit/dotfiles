(require 'helm)

(setq helm-buffer-details-flag nil)
(setq helm-ff-transformer-show-only-basename t)
(setq helm-quick-update t
      helm-idle-delay 0.01

      helm-input-idle-delay 0.01)
(setq helm-M-x-always-save-history t)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-i") 'helm-select-action)

;; bind helm-for-files

(provide 'init-helm)
