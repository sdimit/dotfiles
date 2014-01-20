;; (defcustom virtualenv-workon-starts-python nil
;;   "If non-nil the `virtualenv-workon' will also start python."
;;   :group 'virtualenv
;;   :type 'boolean)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs/")

(defun start-10to8 ()
  (interactive)
  (venv-workon "Native")
  (setq python-django-project-root "~/10to8/Native/native/src/")
  (python-django-open-project "~/10to8/Native/native/src/core" "core.settings"))

(defun open-10to8-db ()
  (interactive)
  (cd "/usr/local/var/postgres/pg_log/")
  (ido-find-file-read-only)
  (auto-revert-tail-mode))

(defun run-django-command (django-cmd &optional cmd-args)
  (let ((project-buffer-name "*Django: core (core.settings)*"))
    (start-10to8)
    (switch-to-buffer-other-window project-buffer-name)
    (if cmd-args
        (funcall django-cmd cmd-args)
      (funcall django-cmd))))

(defun 10to8-runserver ()
  (interactive)
  (let ((server-settings     "localhost:8000 --nothreading")
        (server-buffer-name  "*[Django: core (core.settings)] ./manage.py runserver localhost:8000*"))
    (if (get-buffer server-buffer-name)
        (switch-to-buffer-other-window server-buffer-name)
      (progn (run-django-command 'python-django-qmgmt-runserver server-settings)
             (switch-to-buffer-other-window server-buffer-name)))))

(defun 10to8-shell ()
  (interactive)
  (let ((shell-buffer-name  "*[Django: core (core.settings)] ./manage.py shell*"))
    (if (get-buffer shell-buffer-name)
        (switch-to-buffer-other-window shell-buffer-name)
      (progn
        (run-django-command 'python-django-qmgmt-shell)
        (switch-to-buffer-other-window shell-buffer-name)))))

(defun send-to-10to8-shell (string)
  (interactive)
  (let* ((shell-buffer-name  "*[Django: core (core.settings)] ./manage.py shell*")
         (shell-process (get-buffer-process shell-buffer-name)))
    (comint-send-string shell-process string)
    (when (or (not (string-match "\n$" string))
             (string-match "\n[ \t].*\n?$" string))
      (comint-send-string shell-process "\n"))))

;; (require 's)

;; (defun send-region-to-10to8-shell (start end)
;;   (interactive "r")
;;   (let* ((string (buffer-substring start end)))
;;     (with-temp-buffer
;;       (insert string)
;;       (send-to-10to8-shell (buffer-substring-no-properties (buffer-string))))))


(defun search-deep-thought (string)
  (interactive (list
                (read-from-minibuffer "Search: " (ag/dwim-at-point))))
  (ag/search string "~/10to8/Native/native/src/core" t))

(defun search-jeltz (string)
  "follow symlinks"
  (interactive (list
                (read-from-minibuffer "Search: " (ag/dwim-at-point))))
  (let ((ag-arguments (cons "--follow" ag-arguments)))
    (ag/search string "~/10to8/Native/native/src/apps/jeltz/app" t)))

(defun search-colin (string)
  "follow symlinks"
  (interactive (list
                (read-from-minibuffer "Search: " (ag/dwim-at-point))))
  (let ((ag-arguments (cons "--follow" ag-arguments)))
    (ag/search string "~/10to8/Native/native/src/apps/colin/app" t)))

(require 'nose)
(add-to-list 'nose-project-root-files ".project")

(defun nose-in-shell ()
  "runs test in eshell"
  (interactive)
  (let ((pmt-command "cd ~/10to8/Native/native/src && python manage.py test")
        (pmt-options "--noinput"))
    (switch-to-buffer-other-window "*eshell*")
    (append-to-buffer "*eshell*" (format "%s %s %s:%s" pmt-command pmt-options buffer-file-name (nose-py-testable)))))

;; (eshell-command (format "%s %s %s:%s" pmt-command pmt-options buffer-file-name (nose-py-testable)))

(defun run-nose-test ()
  "runs test in eshell"
  (interactive)
  (let* ((pmt-command "cd ~/10to8/Native/native/src && source ~/.virtualenvs/Native/bin/activate && python manage.py test")
         (pmt-options "--noinput --nocapture --failfast --verbosity=0")
         (command (format "%s %s %s:%s" pmt-command pmt-options buffer-file-name (nose-py-testable))))
    (save-buffer)
    (async-shell-command command "*nose-test*")
    (evil-normal-state)))

(defun run-nose-test-watch ()
  "runs test in eshell"
  (interactive)
  (let* ((pmt-command "cd ~/10to8/Native/native/src && source ~/.virtualenvs/Native/bin/activate && python manage.py test")
         (pmt-options "--noinput --nocapture --failfast --with-watch --verbosity=0")
         (command (format "%s %s %s:%s" pmt-command pmt-options buffer-file-name (nose-py-testable))))
    (save-buffer)
    (async-shell-command command "*nose-test*")
    (evil-normal-state)))

(defun run-jeltz-test ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/jeltz && cake test:once"))
    (async-shell-command command "*cake-test*")))

(defun run-jeltz-build-test ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/jeltz && cake build:test:once"))
    (async-shell-command command "*cake-test*")))

(defun run-colin-test ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/colin && npm test"))
    (async-shell-command command "*cake-test*")))

(global-set-key (kbd "C-c k k") 'search-deep-thought)

(global-set-key (kbd "C-c j m") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/models/")))
(global-set-key (kbd "C-c j v") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/views/")))
(global-set-key (kbd "C-c j c") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/controllers/")))
(global-set-key (kbd "C-c j M") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/modules/")))
(global-set-key (kbd "C-c j t") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/templates/")))
(global-set-key (kbd "C-c j s") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/styles/")))
(global-set-key (kbd "C-c j a") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/")))
(global-set-key (kbd "C-c j j") 'search-jeltz)
(global-set-key (kbd "C-x j") (bind (cd "~/10to8/Native/native/src/apps/jeltz/app/")))

(global-set-key (kbd "C-c c m") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/models/")))
(global-set-key (kbd "C-c c v") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/views/")))
(global-set-key (kbd "C-c c C") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/controllers/")))
(global-set-key (kbd "C-c c M") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/modules/")))
(global-set-key (kbd "C-c c t") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/templates/")))
(global-set-key (kbd "C-c c s") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/styles/")))
(global-set-key (kbd "C-c c a") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/")))
(global-set-key (kbd "C-c c c") 'search-colin)
(global-set-key (kbd "C-x c") (bind (cd "~/10to8/Native/native/src/apps/colin/app")))

(global-set-key (kbd "C-x x") (bind (cd "~/10to8/Native/native/src/core")))
(global-set-key (kbd "C-c k a") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/core/")))

(prodigy-define-service
  :name "deepthought - runserver"
  :command "python"
  :args '("manage.py" "runserver")
  :cwd "~/10to8/Native/native/src/"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (venv-workon "Native"))
  :on-output (lambda (service output)
                 (when (s-matches? "Quit the server with CONTROL-C" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8 redux normal full))

(prodigy-define-service
  :name "deepthought - messaging"
  :command "python"
  :args '("manage.py" "run_messaging")
  :cwd "~/10to8/Native/native/src/"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (venv-workon "Native"))
  :tags '(10to8 full))

(prodigy-define-service
  :name "deepthought - celery"
  :command "python"
  :args '("manage.py" "celery" "worker" "-E" "-l" "info")
  :cwd "~/10to8/Native/native/src/"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (venv-workon "Native"))
  :tags '(10to8 normal))

(prodigy-define-service
  :name "deepthought - sockjs"
  :command "python"
  :args '("manage.py" "run_sockjs_server")
  :cwd "~/10to8/Native/native/src/"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (venv-workon "Native"))
  :tags '(10to8 normal))

(prodigy-define-service
  :name "deepthought - rabbit"
  :command "rabbitmq-server"
  :cwd "~/10to8/Native/native/src/"
  :init (lambda () (venv-workon "Native"))
  :on-output (lambda (service output)
                 (when (s-matches? "completed" output)
                   (prodigy-set-status service 'ready)))
  :kill-process-buffer-on-stop t
  :kill-signal 'sigkill
  :tags '(10to8 normal))

(prodigy-define-service
  :name "deepthought - redis"
  :command "redis-server"
  :cwd "~/10to8/Native/native/src/"
  :init (lambda () (venv-workon "Native"))
  :kill-process-buffer-on-stop t
  :kill-signal 'sigkill
  :on-output (lambda (service output)
                 (when (s-matches? "The server is now ready" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8 normal))

(prodigy-define-service
  :name "make current user root"
  :command "python"
  :args '("manage.py" "root_user" "stephane@thebati.net")
  :cwd "~/10to8/Native/native/src/"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (venv-workon "Native"))
  :tags '(10to8))

(prodigy-define-service
  :name "jeltz - serve"
  :command "serv"
  :args '("3333")
  :cwd "~/10to8/Native/native/src/apps/jeltz/public"
  :path '("~/dotfiles/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output (lambda (service output)
                 (when (s-matches? "Serving HTTP" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8 redux normal))

(prodigy-define-service
  :name "jeltz - build"
  :command "brunch"
  :args '("build")
  :cwd "~/10to8/Native/native/src/apps/jeltz/"
  :path '("~/usr/local/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output (lambda (service output)
                 (when (s-matches? "compiled" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8))

(prodigy-define-service
  :name "jeltz - watch"
  :command "brunch"
  :args '("watch" "--server" "-p" "3333")
  :cwd "~/10to8/Native/native/src/apps/jeltz/"
  :path '("~/usr/local/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output (lambda (service output)
                 (when (s-matches? "compiled" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8))

(prodigy-define-service
  :name "jeltz - test runner"
  :command "/usr/local/bin/cake"
  :args '("test:once")
  :cwd "~/10to8/Native/native/src/apps/jeltz"
  :path '("/usr/local/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :tags '(10to8))

(prodigy-define-service
  :name "colin - serve"
  :command "serv"
  :args '("3334")
  :cwd "~/10to8/Native/native/src/apps/colin/public"
  :path '("~/dotfiles/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output (lambda (service output)
                 (when (s-matches? "Serving HTTP" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8))

(prodigy-define-service
  :name "colin - build"
  :command "brunch"
  :args '("build")
  :cwd "~/10to8/Native/native/src/apps/colin/"
  :path '("~/usr/local/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output (lambda (service output)
                 (when (s-matches? "compiled" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8))

(prodigy-define-service
  :name "colin - watch"
  :command "brunch"
  :args '("watch" "--server" "-p" "3334")
  :cwd "~/10to8/Native/native/src/apps/colin/"
  :path '("~/usr/local/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output (lambda (service output)
                 (when (s-matches? "compiled" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8))

(prodigy-define-service
  :name "colin - test runner"
  :command "/usr/local/bin/cake"
  :args '("test:once")
  :cwd "~/10to8/Native/native/src/apps/colin"
  :path '("/usr/local/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :tags '(10to8))

(prodigy-define-service
  :name "jeltz - build etags"
  :command "build-coffee-etags.sh"
  :cwd "~/10to8/Native/native/src/apps/jeltz/app"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :tags '(10to8))

(prodigy-define-service
  :name "colin - build etags"
  :command "build-coffee-etags.sh"
  :cwd "~/10to8/Native/native/src/apps/colin/app"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :tags '(10to8))

;; (require 'butler)
;;
;; (add-to-list butler-servers
;;              '(jenkins "10to8"
;;                        (server-address . "https://jenkins.10to8.com/")
;;                        (auth-file . "~/.authinfo")))

(provide 'init-10to8)
