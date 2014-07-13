;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'org-faces)

(if  (file-exists-p "~/Dropbox/org/life.org")
    ;; Set up org-agenda files
    (progn
      (setq org-user-agenda-files '("~/Dropbox/org/diary.org"
                                    "~/Dropbox/org/life.org"
                                    "~/Dropbox/org/study.org"
                                    "~/Dropbox/org/refile.org"
                                    "~/Dropbox/org/eengsoft.org"
                                    "~/Dropbox/org/locaweb.org"
                                    ))
      ;; loading org custom
      (load "org-mode-custom.el")
      ;; Setting up babel support for languages
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((sql . t)))

      ;; Setting up fonfitication
      (setq org-src-fontify-natively t)
      (add-hook 'sql-mode-hook (lambda () (sql-highlight-mysql-keywords))))
  (message "skipping org-mode load"))

(provide 'init-org)
;;; init-org.el ends here
