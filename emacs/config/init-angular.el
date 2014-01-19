(defun highlight-angular-anchor ()
  (interactive)
  (highlight-regexp "ng-\w*=" 'hi-red))

(provide 'init-angular)
