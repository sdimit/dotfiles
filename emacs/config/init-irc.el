(setq rcirc-default-nick "istib")
(setq rcirc-default-user-name "istib")

(add-hook 'jabber-chat-mode-hook (lambda () (toggle-truncate-lines -1)))

(add-hook 'jabber-chat-mode-hook
          (lambda ()
            (save-excursion
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (search-forward "ROCKS")
                (beginning-of-line)
                (kill-line)))))

(provide 'init-irc)
