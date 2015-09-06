;;; init-org-mobile.el -- Configuration for org mobile stuff.
;;; Commentary:
;;; Code:

(setq org-mobile-directory (expand-file-name "mobileorg/" rr/org-files-directory)
      org-mobile-inbox-for-pull (expand-file-name "refile.org" rr/org-files-directory))

(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (dolist (file org-user-agenda-files)
                (if (string= (file-truename (expand-file-name file))
                             (file-truename (buffer-file-name)))
                    (org-mobile-push-with-delay 1200))))))

 ;; refreshes agenda file each day
(run-at-time "23:59" 86400 '(lambda () (org-mobile-push-with-delay 120)))
(run-at-time "23:58" 86400 'org-mobile-pull)

(provide 'init-org-mobile)
;;; init-org-mobile.el ends here
