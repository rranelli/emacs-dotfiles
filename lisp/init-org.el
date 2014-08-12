;;; package -- Summary
;;; Commentary:
;;; Code:
(if (file-exists-p "~/Dropbox/org/life.org")
    ;; Set up org-agenda files
    (let ((org-dir "~/Dropbox/org/")
          (org-files '("diary.org"
                       "life.org"
                       "study.org"
                       "refile.org"
                       "opensource.org"
                       "locaweb.org"
                       "emacs.org")))

      (setq org-user-agenda-files
            (mapcar (lambda (filename) (concat org-dir filename)) org-files))

      ;; loading org custom
      (require 'org-mode-custom)
      ;; Setting up babel support for languages
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((sql . t)))

      ;; Setting up fonfitication
      (setq org-src-fontify-natively t)
      (add-hook 'sql-mode-hook (lambda () (sql-highlight-mysql-keywords)))

      ;; make org beautifull
      (setq
       org-odd-levels-only t
       org-hide-leading-stars t
       org-startup-indented t)

      ;; removing useless conflict keys on org-mode.
      (define-key org-agenda-mode-map (kbd "C-c p") nil)
      (define-key org-mode-map (kbd "M-h") nil))

  (message "skipping org-mode load"))

(provide 'init-org)
;;; init-org.el ends here
