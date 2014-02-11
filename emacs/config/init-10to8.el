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
  (let ((server-settings     "localhost:8000");;" --nothreading")
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

(defun 10to8-magit-status ()
  (interactive)
  (let ((default-directory "~/10to8/Native/native/src"))
    (magit-status default-directory)
    (delete-other-windows)))

(nmap " [" '10to8-magit-status)

;; (require 's)

;; (defun send-region-to-10to8-shell (start end)
;;   (interactive "r")
;;   (let* ((string (buffer-substring start end)))
;;     (with-temp-buffer
;;       (insert string)
;;       (send-to-10to8-shell (buffer-substring-no-properties (buffer-string))))))


(defun search-deep-thought (from-point)
  "use universal-argument to take symbol at point"
  (interactive "P")
  (let* ((search-prompt (if from-point (ag/dwim-at-point) ""))
         (search-term (read-from-minibuffer "Search: " search-prompt)))
    (ag/search search-term "~/10to8/Native/native/src/core" t)))

(defun search-jeltz (from-point)
  "use universal-argument to take symbol at point"
  (interactive "P")
  (let* ((search-prompt (if from-point (ag/dwim-at-point) ""))
         (search-term (read-from-minibuffer "Search: " search-prompt))
         (ag-arguments (cons "--follow" ag-arguments)))
    (ag/search search-term "~/10to8/Native/native/src/apps/jeltz/app" t)))

(defun search-colin (from-point)
  "use universal-argument to take symbol at point"
  (interactive "P")
  (let* ((search-prompt (if from-point (ag/dwim-at-point) ""))
         (search-term (read-from-minibuffer "Search: " search-prompt))
         (ag-arguments (cons "--follow" ag-arguments)))
    (ag/search search-term "~/10to8/Native/native/src/apps/colin/app" t)))

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
         (command     (format "%s %s %s:%s" pmt-command pmt-options buffer-file-name (nose-py-testable))))
    (save-buffer)
    (async-shell-command command "*nose-test*")
    ;; (with-current-buffer "*nose-test*"
    ;;   (compilation-mode))
    (evil-normal-state)))

(defun run-nose-test-watch ()
  "runs test in eshell"
  (interactive)
  (let* ((pmt-command "cd ~/10to8/Native/native/src && source ~/.virtualenvs/Native/bin/activate && python manage.py test")
         (pmt-options "--noinput --nocapture --failfast --with-watch --verbosity=0")
         (command     (format "%s %s %s:%s" pmt-command pmt-options buffer-file-name (nose-py-testable))))
    (save-buffer)
    (async-shell-command command "*nose-test*")
    (evil-normal-state)))

(defun run-jeltz-test ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/jeltz && cake test"))
    (async-shell-command command "*cake-test*")))

(defun run-jeltz-test-once ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/jeltz && cake test:once"))
    (async-shell-command command "*cake-test*")))

(defun run-jeltz-build-test ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/jeltz && cake build:test:once"))
    (async-shell-command command "*cake-test*")))

(defun run-colin-test ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/colin && cake test"))
    (async-shell-command command "*cake-test*")))

(defun run-colin-test-once ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/colin && cake test:once"))
    (async-shell-command command "*cake-test*")))

(defun run-colin-build-test ()
  (interactive)
  (let ((command "cd ~/10to8/Native/native/src/apps/colin && cake build:test:once"))
    (async-shell-command command "*cake-test*")))

(global-set-key (kbd "C-c k k") 'search-deep-thought)

(global-set-key (kbd "C-c j m") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/models/")))
(global-set-key (kbd "C-c j v") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/views/")))
(global-set-key (kbd "C-c j c") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/controllers/")))
(global-set-key (kbd "C-c j M") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/modules/")))
(global-set-key (kbd "C-c j t") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/templates/")))
(global-set-key (kbd "C-c j s") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/styles/")))
(global-set-key (kbd "C-c j T") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/test/")))
(global-set-key (kbd "C-c j a") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/jeltz/app/")))
(global-set-key (kbd "C-c j j") 'search-jeltz)
(global-set-key (kbd "C-x j") (bind (cd "~/10to8/Native/native/src/apps/jeltz/app/")))

(global-set-key (kbd "C-c c m") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/models/")))
(global-set-key (kbd "C-c c v") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/views/")))
(global-set-key (kbd "C-c c C") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/controllers/")))
(global-set-key (kbd "C-c c M") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/modules/")))
(global-set-key (kbd "C-c c t") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/templates/")))
(global-set-key (kbd "C-c c s") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/app/styles/")))
(global-set-key (kbd "C-c c T") (bind (ido-find-file-in-dir "~/10to8/Native/native/src/apps/colin/test/")))
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
  :tags '(10to8 redux jeltz-dev normal full))

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
  :tags '(10to8 normal full))

(prodigy-define-service
  :name "deepthought - sockjs"
  :command "python"
  :args '("manage.py" "run_sockjs_server")
  :cwd "~/10to8/Native/native/src/"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (venv-workon "Native"))
  :tags '(10to8 normal jeltz-dev full))

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
  :tags '(10to8 normal full jeltz-dev))

;; (prodigy-define-service
;;   :name "deepthought - redis"
;;   :command "redis-server"
;;   :cwd "~/10to8/Native/native/src/"
;;   :init (lambda () (venv-workon "Native"))
;;   :kill-process-buffer-on-stop t
;;   :kill-signal 'sigkill
;;   :on-output (lambda (service output)
;;                  (when (s-matches? "The server is now ready" output)
;;                    (prodigy-set-status service 'ready)))
;;   :tags '(10to8 normal full jeltz-dev))

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
  :tags '(10to8 redux normal full))

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
  :tags '(10to8 jeltz-dev))

;; (prodigy-define-service
;;   :name "jeltz - test runner"
;;   :command "/usr/local/bin/cake"
;;   :args '("test:once")
;;   :cwd "~/10to8/Native/native/src/apps/jeltz"
;;   :path '("/usr/local/bin/")
;;   :kill-signal 'sigkill
;;   :kill-process-buffer-on-stop t
;;   :tags '(10to8))

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
  :tags '(10to8 normal full jeltz-dev))

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
  :tags '(10to8 full))

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
  :tags '(10to8 colin-dev))

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

(prodigy-define-service
  :name "website - build"
  :command "brunch"
  :args '("build")
  :cwd "~/10to8/Native/native/src/apps/website/"
  :path '("~/usr/local/bin/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :on-output (lambda (service output)
                 (when (s-matches? "compiled" output)
                   (prodigy-set-status service 'ready)))
  :tags '(10to8))

(prodigy-define-service
  :name "new db"
  :command "~/10to8/scripts/newdb.sh"
  :path '("~/10to8/scripts/")
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (-each prodigy-services 'prodigy-stop-service)
              (venv-workon "Native"))
  :tags '(10to8))

(prodigy-define-service
  :name "new db - gen data"
  :command "python"
  :args '("manage.py" "gen_data" "-e" "1")
  :cwd "~/10to8/Native/native/src/"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
  :init (lambda () (venv-workon "Native"))
  ;; :init-sync (lambda (done)
  ;;           (venv-workon "Native")
  ;;           (let ((rabbit-matches (-filter (lambda (s)
  ;;                                      (string= "deepthought - rabbit" (cadr s)))
  ;;                                       prodigy-services))
  ;;                 (rabbit-service (car rabbit-matches)))
  ;;             (prodigy-start-service rabbit-service done)))
  :tags '(10to8))

(require 'butler)

(setq butler-server-list
      '((jenkins "jenkins.10to8.com"
                 (server-address . "https://jenkins.10to8.com/")
                 (auth-file . "~/.authinfo"))))


(defun start-10to8-jabber ()
  (interactive)
  (call-interactively 'jabber-connect-all)
  (call-interactively 'jabber-muc-autojoin)
  (global-set-key (kbd "C-x C-j d") (bind (switch-to-buffer-other-window "*-jabber-groupchat-10to8dev@chatrooms.10to8.com-*")))
  (global-set-key (kbd "C-x C-j e") (bind (switch-to-buffer-other-window "*-jabber-groupchat-10to8everyone@chatrooms.10to8.com-*")))
  (nmap " ]d" (bind (switch-to-buffer-other-window "*-jabber-groupchat-10to8dev@chatrooms.10to8.com-*")))
  (nmap " ]e" (bind (switch-to-buffer-other-window "*-jabber-groupchat-10to8everyone@chatrooms.10to8.com-*")))
  )

(defun jeltz-servers-start ()
  "make toggle menu instead (using ido-prodigy-menu)"
  (interactive)
  (prodigy-start-services-with-tag "jeltz-dev"))

(setq jiralib-url "https://tento8.atlassian.net")
(setq jiralib-user-login-name "stephane.reissfelder")


(defun 10to8-switch-to-branch-new-db ()
  (interactive)
  (let* ((default-directory "~/10to8/Native/native/src")
         (revision (magit-read-rev "Switch to"))
         (new-db-task (find-prodigy-service-with-name "new db"))
         (rabbit-task (find-prodigy-service-with-name "deepthought - rabbit"))
         (gen-data-task (find-prodigy-service-with-name "new db - gen data")))
    (magit-run-git "checkout" revision)
    (prodigy-stop-all-services)
    (prodigy-start-service new-db-task (lambda () (prodigy-start-service gen-data-task)))))

(defun 10to8-show-commit-from-clipboard ()
  (interactive)
  (let ((ref-from-killring (substring-no-properties (car kill-ring)))
        (default-directory "~/10to8/Native/native/src"))
    (magit-show-commit (read-from-minibuffer "Commit: " ref-from-killring))))

(defun 10to8-show-commit-from-point ()
  (interactive)
  (let ((ref-from-point (ag/dwim-at-point))
        (default-directory "~/10to8/Native/native/src"))
    (magit-show-commit ref-from-point)))

(venv-workon "Native")

(provide 'init-10to8)
