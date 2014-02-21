(require 'w3m)
(require 'pinboard-api)

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
            ;; (set-fringe-mode (/ (- (frame-pixel-width)
            ;;                        (* 100 (frame-char-width)))
            ;;                     4))
            ;;(set-prose-font)
            (smartparens-mode -1)))

(defun w3m-pinboard-add-current-buffer ()
  (interactive)
  (pinboard-api-add w3m-current-url w3m-current-title))

(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

(defun reddit (reddit)
  "Opens the REDDIT in w3m-new-session"
  (interactive (list
                (read-string "Enter the reddit (default: emacs): " nil nil "emacs" nil)))
  (browse-url (format "http://m.reddit.com/r/%s" reddit))
  )

;; (defun w3m-search (prompt search-term url)
;;   "Search for SEARCH-TERM on URL"
;;   (interactive
;;    (let ((prompt (or prompt "Search: "))
;;          (term   (if mark-active
;;                    (buffer-substring (region-beginning) (region-end))
;;                    (word-at-point)))
;;          (url (or url "http://google.com/search?="))))
;;    (list
;;     (read-string
;;      (format (concat prompt " (%s):") term) nil nil term))))
;; (browse-url (concat url search-term)))

;; (defun wikipedia-search (search-term)
;;   "Search for SEARCH-TERM on wikipedia"
;;   (interactive)
;;   (w3m-search "Wikipedia" "" "http://en.m.wikipedia.org/w/index.php?search="))

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


(add-hook 'w3m-mode-hook '(lambda ()
                            (define-key w3m-mode-map [?f] 'w3m-isearch-links)
                            (define-key w3m-mode-map [?n] 'w3m-next-anchor)
                            (define-key w3m-mode-map (kbd "C-x C-g") 'w3m-open-in-default-browser)
                            (define-key w3m-mode-map [?P] 'w3m-pinboard-add-current-buffer)
                            (define-key w3m-mode-map [?p] 'w3m-previous-anchor)))

(defun w3m-open-in-default-browser ()
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser)
        (url w3m-current-url))
    (browse-url url)))

;; (let ((map (make-keymap)))
;;   (suppress-keymap map)
;;   (define-key map "n"                    'w3m-next-anchor)
;;   (define-key map "p"                    'w3m-previous-anchor)
;;   (define-key map [backspace]            'w3m-scroll-down-or-previous-url)
;;   (define-key map [delete]               'w3m-scroll-down-or-previous-url)
;;   (define-key map "\C-?"                 'w3m-scroll-down-or-previous-url)
;;   (define-key map "\t"                   'w3m-next-anchor)
;;   (define-key map [(shift tab)]          'w3m-previous-anchor)
;;   (define-key map [(shift iso-left-tab)] 'w3m-previous-anchor)
;;   (define-key map "\C-m"                 'w3m-view-this-url)
;;   (define-key map [(shift return)]       'w3m-view-this-url-new-session)
;;   (define-key map [(shift kp-enter)]     'w3m-view-this-url-new-session)
;;   (define-key map [(button2)]            'w3m-mouse-view-this-url)
;;   (define-key map [(shift button2)]      'w3m-mouse-view-this-url-new-session)
;;   (define-key map "a"                    'w3m-bookmark-add-current-url)
;;   (define-key map "\M-a"                 'w3m-bookmark-add-this-url)
;;   (define-key map "+"                    'w3m-antenna-add-current-url)
;;   (define-key map "A"                    'w3m-antenna)
;;   (define-key map "c"                    'w3m-print-this-url)
;;   (define-key map "C"                    'w3m-print-current-url)
;;   (define-key map "d"                    'w3m-download)
;;   (define-key map "D"                    'w3m-download-this-url)
;;   ;; (define-key map "D"                 'w3m-download-with-wget)
;;   ;; (define-key map "D"                 'w3m-download-with-curl)
;;   (define-key map "g"                    'w3m-goto-url)
;;   (define-key map "G"                    'w3m-goto-url-new-session)
;;   (define-key map "H"                    'w3m-gohome)
;;   (define-key map "I"                    'w3m-toggle-inline-images)
;;   (define-key map "\M-i"                 'w3m-save-image)
;;   (define-key map "M"                    'w3m-view-url-with-external-browser)
;;   ;; (define-key map "n"                    'w3m-view-next-page)
;;   ;; (define-key map "p"                    'w3m-view-previous-page)
;;   (define-key map "N"                    'w3m-namazu)
;;   (define-key map "o"                    'w3m-history)
;;   (define-key map "O"                    'w3m-db-history)
;;   (define-key map "q"                    'w3m-close-window)
;;   (define-key map "Q"                    'w3m-quit)
;;   (define-key map "R"                    'w3m-reload-this-page)
;;   (define-key map "s"                    'w3m-search)
;;   (define-key map "S" (lambda () (interactive)
;;                                    (let ((current-prefix-arg t))
;;                                    (call-interactively 'w4m-search))))
;;   (define-key map "u"                    'w3m-view-parent-page)
;;   (define-key map "v"                    'w3m-bookmark-view)
;;   (define-key map "W"                    'w3m-weather)
;;   (define-key map "="                    'w3m-view-header)
;;   (define-key map "\\"                   'w3m-view-source)
;;   (define-key map ">"                    'scroll-left)
;;   (define-key map "<"                    'scroll-right)
;;   (define-key map "."                    'beginning-of-buffer)
;;   (define-key map "^"                    'w3m-view-parent-page)
;;   (define-key map "]"                    'w3m-next-form)
;;   (define-key map "["                    'w3m-previous-form)
;;   (define-key map "}"                    'w3m-next-image)
;;   (define-key map "{"                    'w3m-previous-image)
;;   (define-key map "\C-c\C-c"             'w3m-submit-form)
;;   (setq istib-w3m-map map))

;; (add-hook 'w3m-mode-hook '(lambda () (use-local-map istib-w3m-map)))

(defun w3m-browse-url-in-new-session (url &rest ignored)
  (w3m-browse-url url t))

(defun browse-on-mac (url &rest ignored)
  (let ((browse-url-browser-function (quote browse-url-generic))
	(browse-url-generic-program "open"))
    (browse-url url)))

(setq browse-url-browser-function '(("twitter\\|youtube" . browse-on-mac)
				    ("." . w3m-browse-url-in-new-session)))


(provide 'init-w3m)
