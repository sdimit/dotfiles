(require 'mu4e "~/.emacs.d/src/mu4e/mu4e.el")

(require 'smtpmail)

(nmap " M" 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Mail")
      mu4e-drafts-folder "/gmail/drafts"
      mu4e-sent-folder   "/gmail/sent_mail"
      mu4e-trash-folder  "/gmail/trash"

      mu4e-html2text-command "w3m -dump -T text/html"
      mu4e-use-fancy-chars t
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      mu4e-confirm-quit nil
      mu4e-view-fields '(:from :to :cc :subject :date :mailing-list :attachments))

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

;; (setq mu4e-combined-inbox-bookmark "(maildir:/INBOX OR maildir:/[Gmail].Starred) AND NOT flag:trashed"
;;       mu4e-bookmarks `((,mu4e-combined-inbox-bookmark                             "Act-on inbox"                  ?i)
;;             ("flag:unread AND NOT maildir:/me AND NOT flag:trashed"    "Unread messages"               ?v)
;;             ("maildir:/INBOX AND flag:unread AND NOT flag:trashed"     "Unread to me"                  ?m)
;;             ("maildir:/INBOX AND flag:replied AND NOT flag:trashed"    "Replied to me"                 ?r)
;;             ("maildir:/accounts AND flag:unread AND NOT flag:trashed"  "Unread to accounts"            ?a)
;;             ("maildir:/others AND flag:unread AND NOT flag:trashed"    "Unread not to me"              ?n)
;;             ("mime:application/pdf AND NOT flag:thrashed"              "Messages with documents"       ?d)))

;; (defun djr/mu4e-open-message-in-google (msg)
;;   (let* ((msgid (mu4e-message-field msg :message-id))
;; 	 (url (concat "https://mail.google.com/mail/u/0/?shva=1#search/rfc822msgid%3A"
;; 		      (url-encode-url msgid))))
;;     (start-process "" nil "open" url)))

;; (add-to-list 'mu4e-view-actions '("gopen in gmail" . djr/mu4e-open-message-in-google) t)
;; (add-to-list 'mu4e-view-actions '("bview in browser" . mu4e-action-view-in-browser) t)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "~/bin/mailrun.sh")

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
