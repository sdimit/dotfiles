
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
                "setTimeout" "clearTimeout" "JSON" "$" "_" "Backbone" "buster" "sinon" "moment" "Date" "React"
                "chrome" "localStorage"))

;; (setq grunt-cmd "grunt --no-color --config ~/grunt.js")
(setq grunt-cmd "grunt --no-color --config")

(defun grunt ()
  "Run grunt"
  (interactive)
  (run-process-silently-if-successful grunt-cmd "*grunt*"))

;; (run-process-silently-if-successful "cd ~/10to8/Native/native/src/apps/jeltz && cake test:once" "jeltz")

(require 'tern)
(require 'tern-auto-complete)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(after 'tern
  (after 'auto-complete
    (require 'tern-auto-complete)
    (tern-ac-setup)))

(provide 'init-javascript)
