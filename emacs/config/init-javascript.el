
(defun skewer-coffee-eval (coffee-code)
  "Requests the browser to evaluate a coffeescipt string."
  ;; XXX should escape double quote characters
  (skewer-eval (concat "CoffeeScript.eval(\""
                       (s-replace "\n" "\\n" (s-trim coffee-code))
                       "\");")
               #'skewer-post-minibuffer))

(defun skewer-coffee-eval-region ()
  "Sends the coffeescript code the region encloses, or -- if
  there's no active region -- sends the current line."
  (interactive)
  (skewer-coffee-eval
   (if (region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (thing-at-point 'line))))

(defun skewer-coffee-eval-defun ()
  "Evaluates the current 'sentence', which is usually a complete function."
  (interactive)
  (skewer-coffee-eval (thing-at-point 'sentence)))

(defun skewer-coffee-eval-buffer ()
  "Evaluates the current buffer as CoffeeScript."
  (interactive)
  (skewer-coffee-eval (buffer-substring-no-properties (point-min) (point-max))))

(defvar skewer-coffee-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") 'skewer-coffee-eval-region)
      (define-key map (kbd "C-M-x")   'skewer-coffee-eval-defun)
      (define-key map (kbd "C-c C-k") 'skewer-coffee-eval-buffer)))
  "Keymap for skewer-coffee-mode.")

  ;;;###autoload
(define-minor-mode skewer-coffee-mode
  "Minor mode for interactively loading coffeescript forms."
  :lighter " skewer-coffee"
  :keymap  skewer-coffee-mode-map
  :group   skewer)

(pretty-add-keywords 'js3-mode `(("true"      . "т")
                                 ("false"     . "ғ")
                                 ("undefined" . "∅")
                                 ("function"  . "ƒ")
                                 ("return"    . "↞")
                                 ("null"      . "∅")
                                 ("this."     . "@")))

(pretty-add-keywords 'coffee-mode `(("true"      . "т")
                                    ("false"     . "ғ")
                                    ("null"      . "∅")
                                    ("undefined" . "∅")
                                    ("return"    . "↞")))


(setq-default js3-global-externs
              '("module" "exports" "require" "__dirname" "process" "console" "define"
                "JSON" "$" "_" "Backbone" "buster" "sinon" "moment" "Date" "React"))

(defun run-process-silently-if-successful (cmd buffer-name)
  "Only show result if failed"
  (let* ((proc-buffer (get-buffer-create buffer-name))
         (result (call-process-shell-command cmd nil proc-buffer t))
         (output (with-current-buffer proc-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Process completed without errors"))
          (t
           (message nil)
           (split-window-vertically)
           (set-window-buffer (next-window) proc-buffer)))))


;; (setq grunt-cmd "grunt --no-color --config ~/grunt.js")
(setq grunt-cmd "grunt --no-color --config")

(defun grunt ()
  "Run grunt"
  (interactive)
  (run-process-silently-if-successful grunt-cmd "*grunt*"))

;; (run-process-silently-if-successful "cd ~/10to8/Native/native/src/apps/jeltz && cake test:once" "jeltz")

(provide 'init-javascript)
