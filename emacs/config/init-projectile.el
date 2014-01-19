
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(nmap " 1" 'direx:jump-to-directory-other-window)

(require 'projectile)

;;  (projectile-global-mode)

(setq projectile-enable-caching t)
;;  broken because of font-family, apparently

(defcustom projectile-switch-project-action 'helm-projectile
  ""
  :group 'projectile
  :type 'symbol)

(global-set-key (kbd "C-x f") 'helm-projectile)

(provide 'init-projectile)
