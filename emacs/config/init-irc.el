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

(defun get-last-jira-ticket-in-buffer ()
  (interactive)
  (save-excursion
    (re-search-backward "[0-9]\\{4\\}")
    (open-jira-ticket-from-point)))

;; (defun on-jabber-message (from buffer text title)
;;   (message "### JABBER: %s says: '%s'" from text))

;; (add-hook 'jabber-muc-hooks 'on-jabber-message)
;; (add-hook 'jabber-message-hooks 'on-jabber-message)

(defun jabber-custom-message (nick group buffer text)
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if nick
        (when (or jabber-muc-alert-self
                  (not (string= nick (cdr (assoc group *jabber-active-groupchats*)))))
          (format "%s says on %s: %s" nick text)))))

(setq jabber-alert-muc-function 'jabber-custom-message)

(defun helm-jabber ()
  "Provides a list of the logged messages.
Primary helm action is to paste selection into the active buffer.
Requires dash.el"
  (interactive)
  (let* ((active-buffer          (current-buffer))
         (messages               (with-current-buffer (get-buffer "*Messages*")
                                    (buffer-substring-no-properties (point-min) (point-max))))
         (complete-message-list  (reverse (split-string messages "\n")))
         (message-list           (-select (lambda (msg) (string-match "says on jabber" msg))
                                          complete-message-list))
         (unique-message-list    (-uniq message-list)))
    (helm
     :sources '((name . "Jabber Messages")
                (candidates . unique-message-list)
                (action . (lambda (candidate) (with-current-buffer active-buffer (insert candidate))))))))

(defun open-last-link-in-buffer ()
  (interactive)
  (save-excursion
    (re-search-backward "http")
    (let ((browse-url-browser-function 'browse-url-default-macosx-browser))
      (browse-url-at-point))))

(provide 'init-irc)
