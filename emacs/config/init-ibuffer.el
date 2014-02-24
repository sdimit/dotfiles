(require 'ibuffer)

(define-key ibuffer-mode-map "K" 'ibuffer-do-delete)
(define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
(define-key ibuffer-mode-map "k" 'ibuffer-backward-line)
(define-key ibuffer-mode-map "f" 'ibuffer-jump-to-buffer)

(nmap ",," 'evil-buffer)
(nmap ",." 'ace-jump-buffer)

;; default grouping of ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("10to8" (filename . "10to8"))
               ("org" (mode . org-mode))
               ("dirs" (mode . dired-mode))
               ("python" (mode . python-mode))
               ("git" (or
                       (mode . magit-status-mode)
                       (mode . magit-diff-mode)))
               ("js" (or
                      (mode . js2-mode)
                      (mode . js3-mode)
                      (mode . coffee-mode)))
               ("clojure" (or
                           (mode . clojure-mode)
                           (mode . cider-repl-mode)))
               ("doc" (or
                       (mode . markdown-mode)
                       (mode . text-mode)))
               ("templates" (or
                             (mode . web-mode)
                             (mode . jade-mode)
                             (mode . css-mode)
                             (mode . less-mode)
                             (mode . style-mode)))
               ("comms" (or
                         (mode . rcirc-mode)
                         (mode . jabber-roster-mode)
                         (mode . erc-mode)
                         (mode . jabber-chat-mode)))
               ("emacs" (or
                         (name . "^.+\\.el$")
                         (name . "^\\*.+\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't ask to kill buffers
(setq ibuffer-expert t)


(require 'ibuffer-git)

;;nicely format the ibuffer and include git-status
(setq ibuffer-formats '((mark modified read-only git-status-mini " "
                              (name 18 18 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (git-status 8 8 :left :elide)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)))

    (setq ibuffer-show-empty-filter-groups nil)

(defun ibuffer-menu-grep-delete (str)
  (interactive
   (list (read-string "Delete Mark Regexp: ")))
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (re-search-forward str nil t)
      (save-excursion
        (ibuffer-mark-for-delete nil))
      (end-of-line))))

(eval-after-load "ibuffer"
  '(progn
     (define-key ibuffer-mode-map "R" 'ibuffer-menu-grep-delete)))

(provide 'init-ibuffer)
