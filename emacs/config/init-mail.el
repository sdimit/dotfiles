(require 'mu4e "~/.emacs.d/src/mu4e/mu4e.el")

(require 'smtpmail)

(nmap " M" 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Mail"))

(setq mu4e-drafts-folder "/gmail/drafts")
(setq mu4e-sent-folder   "/gmail/sent_mail")
(setq mu4e-trash-folder  "/gmail/trash")

(setq mu4e-html2text-command "w3m -dump -T text/html")
(setq mu4e-use-fancy-chars t)
(setq mu4e-view-fields '(:from :to :cc :subject :date :mailing-list :attachments))

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX"      . ?i)
        ("/gmail/sent_mail"  . ?s)
        ("/gmail/all_mail"   . ?a)
        ("/gmail/trash"      . ?t)
        ("/tento8/INBOX"     . ?I)
        ("/tento8/sent_mail" . ?S)
        ("/tento8/all_mail"  . ?A)
        ("/tento8/trash"     . ?T)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)


;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; show images
(setq mu4e-show-images t)

   ;;; message view action
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))

(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-msgv-action-view-in-browser) t)

(setq mu4e-compose-signature "")

(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587)

(provide 'init-mail)
