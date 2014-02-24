(require 'helm)

(defun helm-paste-message ()
  "Provides a list of the logged messages.
Primary helm action is to paste selection into the active buffer.
Requires dash.el"
  (interactive)
  (let* ((active-buffer          (current-buffer))
         (boring-messages-regexp '("Wrote" "Saving" "^Compiling" "^Checking" "^Generating" "^Followed link"
                                   "Ag finished" "^Evaluating..." "^Making completion"
                                   "^Back to" "^Entering" "Mark set"
                                   "byte-code" "^(No changes" "helm-paste-message"
                                   "^nil"
                                   "^Quit" "\\[mu4e\\]" "\\[yas\\]"))
         (messages               (with-current-buffer (get-buffer "*Messages*")
                                    (buffer-substring-no-properties (point-min) (point-max))))
         (complete-message-list  (reverse (split-string messages "\n")))
         (message-list           (-filter (lambda (msg) (-all? (lambda (boring-re) (not (string-match boring-re msg)))
                                                          boring-messages-regexp))
                                          complete-message-list))
         (unique-message-list    (-uniq message-list)))
    (helm
     :sources '((name . "Messages")
                (candidates . unique-message-list)
                (action . (lambda (candidate) (with-current-buffer active-buffer (insert candidate))))))))


(nmap " 0" 'helm-paste-message)

(provide 'init-utils)
