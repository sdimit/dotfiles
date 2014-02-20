(require 'w3m)

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

(setq w3m-confirm-leaving-secure-page nil)

;; remove trailing whitespace
(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace)
              )
            (set-fringe-mode (/ (- (frame-pixel-width)
                                   (* 100 (frame-char-width)))
                                4))
            (set-prose-font)))

(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

(defun reddit (reddit)
  "Opens the REDDIT in w3m-new-session"
  (interactive (list
                (read-string "Enter the reddit (default: psycology): " nil nil "psychology" nil)))
  (browse-url (format "http://m.reddit.com/r/%s" reddit))
  )

(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
   )
  (browse-url
   (concat
    "http://en.m.wikipedia.org/w/index.php?search="
    search-term
    ))
  )

(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' prepended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))


(defvar w3m-isearch-links-do-wrap nil
  "Used internally for fast search wrapping.")

(defun w3m-isearch-links (&optional regexp)
  (interactive "P")
  (let ((isearch-wrap-function
         #'(lambda ()
             (setq w3m-isearch-links-do-wrap nil)
             (if isearch-forward
                 (goto-char (window-start))
               (goto-char (window-end)))))
        (isearch-search-fun-function
         #'(lambda () 'w3m-isearch-links-search-fun))
        post-command-hook   ;inhibit link echoing
        do-follow-link
        (isearch-mode-end-hook
         (list  #'(lambda nil
                    (when (and (not isearch-mode-end-hook-quit)
                             (w3m-anchor))
                      (setq do-follow-link t))))))
    (setq w3m-isearch-links-do-wrap t)
    (isearch-mode t
                  regexp
                  ;; fast wrap
                  #'(lambda nil
                      (if isearch-success
                          (setq w3m-isearch-links-do-wrap t)
                        (when w3m-isearch-links-do-wrap
                          (setq w3m-isearch-links-do-wrap nil)
                          (setq isearch-forward
                                (not isearch-forward))
                          (isearch-repeat isearch-forward))))
                  t)
    (when do-follow-link
      (w3m-view-this-url))))

(defun w3m-isearch-links-search-fun (string &optional bound no-error)
  (let* (isearch-search-fun-function
         (search-fun  (isearch-search-fun))
         error
         (bound  (if isearch-forward
                     (max (or bound 0)
                          (window-end))
                   (min (or bound (window-start))
                        (window-start)))))
    (condition-case err
        (while (and (apply search-fun (list string bound))
                  (not (w3m-anchor (point)))))
      (error (setq error err)))
    (if error
        (if (not no-error)
            (signal (car error) (cadr error)))
      (point))))

(define-key w3m-mode-map [?f] 'w3m-isearch-links)

(provide 'init-w3m)
