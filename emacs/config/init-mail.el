(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "stephane@thebati.net" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

(setq mail-host-address "smtp.gmail.com")
(setq user-full-name "Stephane")
(setq user-mail-address "stephane@thebati.net")

(provide 'init-mail)
