;;; package -- Summary
;;; Commentary:
;;; Code:

(defcustom rr-org-files-directory "~/Copy/org/"
  "Directory for org files."
  :group 'init-org)

(if (file-exists-p rr-org-files-directory)
    ;; Set up org-agenda files
    (let* ((dir-files (directory-files rr-org-files-directory t directory-files-no-dot-files-regexp))
	   (org-files (remove-if #'(lambda (name) (string-match "archive" name)) dir-files)))

      (setq org-user-agenda-files org-files)

      ;; loading org custom
      (require 'org-mode-custom)

      ;; Setting up babel support for languages
      (org-babel-do-load-languages'org-babel-load-languages
       '((emacs-lisp . t)
	 (dot . t)
	 (ditaa . t)
	 (R . t)
	 (python . t)
	 (ruby . t)
	 (gnuplot . t)
	 (clojure . t)
	 (sh . t)
	 (ledger . t)
	 (org . t)
	 (plantuml . t)
	 (latex . t)
	 (sql . t)))

      ;; Setting up fonfitication
      (setq org-src-fontify-natively t)

      ;; make org beautifull
      (setq
       org-odd-levels-only t
       org-hide-leading-stars t
       org-startup-indented nil)

      ;; removing useless conflict keys on org-mode.
      (define-key org-agenda-mode-map (kbd "C-c p") nil)
      (define-key org-mode-map (kbd "M-h") nil)

      (define-key org-mode-map (kbd "C-c C-f") nil)

      ;; set-up encryption
      (require 'org-crypt)
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
      (setq org-crypt-key nil)

      (message "Org-mode config loaded."))
  (message "skipping org-mode load."))

(provide 'init-org)
;;; init-org.el ends here
