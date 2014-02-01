;; Autocomplete defaults
;; ESC to get out of autocomplete menu
(ac-config-default)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(setq ac-auto-show-menu 0.2
      ac-auto-start 3
      ac-quick-help-delay 2.0
      ac-ignore-case nil
      ac-candidate-menu-min 2
      ac-use-quick-help nil
      ac-limit 10)

(setq-default ac-sources '(ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-dictionary
                           ac-source-filename))
