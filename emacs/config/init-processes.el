
(require 'server)
(unless (server-running-p) (server-start))

;;  don't ask about killing processes attached to buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(add-hook 'ack-mode-hook
          (lambda ()
            (set-process-query-on-exit-flag (get-buffer-process
                                             (current-buffer))
                                            nil)))

(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag (get-buffer-process
                                             (current-buffer))
                                            nil)))


(require 'prodigy)

(define-key prodigy-mode-map (kbd "j") 'prodigy-next)
(define-key prodigy-mode-map (kbd "k") 'prodigy-prev)
(define-key prodigy-mode-map (kbd "S") 'prodigy-start)
(define-key prodigy-mode-map (kbd "K") 'prodigy-stop)
(define-key prodigy-mode-map (kbd "RET") 'prodigy-display-process)

(defun delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
              (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(nmap " 2" 'prodigy)
(nmap " 3" (bind (list-processes) (switch-to-buffer-other-window "*Process List*")))

(define-key process-menu-mode-map (kbd "K") 'delete-process-at-point)

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

(defun ido-prodigy-menu ()
  (interactive)
  (let* ((ido-prodigy-choices (mapcar (lambda (serv)
                                        (let* ((status (prodigy-service-started-p serv))
                                               (service-name (cadr serv))
                                               (stopped-label service-name)
                                               (started-label  (upcase service-name)))
                                          (if status started-label
                                            stopped-label)))
                                      prodigy-services)))
    (ido-completing-read "Service: " ido-prodigy-choices)))

(defun find-prodigy-service-with-name (service-name)
  (let ((matches (-filter (lambda (s) (string= service-name (cadr s)))
                          prodigy-services)))
    (car matches)))

(defun prodigy-apply-to-services (services fn)
  (prodigy-with-refresh
   (-each services fn)))

(defun prodigy-stop-all-services ()
  (prodigy-apply-to-services prodigy-services
                             'prodigy-stop-service))

(defun prodigy-start-services-with-tag (tag)
  (prodigy-apply-to-services
   (prodigy-services-tagged-with (intern tag))
   'prodigy-start-service))

(defun run-bin-script ()
  "prompts to run a CLI scripts for user bin folder"
  (interactive)
  (let* ((bin-scripts (directory-files "~/bin"))
         (script (ido-completing-read "Script: " bin-scripts)))
    (call-process script)))


(provide 'init-processes)
