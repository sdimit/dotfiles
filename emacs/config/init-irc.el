(setq rcirc-default-nick "istib")
(setq rcirc-default-user-name "istib")

(add-hook 'jabber-chat-mode-hook (lambda () (toggle-truncate-lines -1)))

(defun remove-annoying-jabber-messages ()
  (interactive)
  (save-excursion
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (search-forward "ROCKS")
                (beginning-of-line)
                (kill-line))))

(add-hook 'jabber-chat-mode-hook 'remove-annoying-jabber-messages)

;; TODO: finish
(defun timeago-date-from-string (date-string)
  "use simple date conversion using moment.js"
  (shell-command-to-string (concat "node ~/.emacs.d/src/timeago/timeago.js '" date-string "'")))

(defun replace-jabber-dates ()
  (interactive)
  (save-excursion
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (while (re-search-forward "^\\[.\\{16\\}\\]")
                  (let* ((timeago (timeago-date-from-string (match-string 0)))
                         (replacement (replace-regexp-in-string "\n$" "" timeago)))
                    (replace-match replacement)))
                )))

(provide 'init-irc)
