;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'org-faces)

(if  (file-exists-p "~/Dropbox/org/life.org")
    ;; Set up org-agenda files
    (progn
      (defvar org-dir "~/Dropbox/org/")
      (defvar org-files '("diary.org"
                          "life.org"
                          "study.org"
                          "refile.org"
                          "eengsoft.org"
                          "locaweb.org"))

      (setq org-user-agenda-files
            (mapcar (lambda (filename) (concat org-dir filename)) org-files))

      ;; loading org custom
      (require 'org-mode-custom)
      ;; Setting up babel support for languages
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((sql . t)))

      (setq org-src-fontify-natively t)
      (add-hook 'sql-mode-hook (lambda () (sql-highlight-mysql-keywords)))

  ;; make org beautifull
  (setq
   org-odd-levels-only t
   org-hide-leading-stars t
   org-startup-indented t)

  ;; Setting up fonfitication

  (message "skipping org-mode load")))

(provide 'init-org)
;;; init-org.el ends here
