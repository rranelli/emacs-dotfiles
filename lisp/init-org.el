;;; init-org.el -- Sanely configures org-mode related stuff.
;;; Commentary:
;;; Code:
(defcustom rr/org-files-directory "~/SpiderOak Hive/org/"
  "Directory for org files."
  :group 'init-org)

;; make org beautifull
(if (file-exists-p rr/org-files-directory)
    ;; Set up org-agenda files
    (let* ((dir-files (directory-files rr/org-files-directory t directory-files-no-dot-files-regexp))
	   (org-files (delete-if
		       #'(lambda (name) (or
				    (not (string-match "\.org$" name))
				    (string-match "archive" name)
				    (string-match "\??/\.#" name)))
                       dir-files)))

      (setq org-user-agenda-files org-files)
      (setq org-journal-dir (expand-file-name "journal" rr/org-files-directory))
      (setq org-journal-file-format "%Y.org")

      ;; loading org custom
      (require 'org-mode-custom)

      ;; adding a hook to save org stuff more frequently
      (add-hook 'after-save-hook 'org-save-all-org-buffers)
      (add-hook 'org-mode-hook 'custom-add-watchwords)

      ;; prettify org
      (setq
       org-odd-levels-only t
       org-hide-leading-stars t
       org-startup-indented nil)

      ;; Setting up babel support for languages
      (setq org-babel-sh-command "bash"
            org-export-babel-evaluate nil)

      (setq org-src-fontify-natively t)
      (org-babel-do-load-languages'org-babel-load-languages
       '((emacs-lisp . t)
	 (dot . t)
	 (python . t)
	 (ruby . t)
	 (haskell . t)
	 (java . t)
	 (clojure . t)
	 (sh . t)
	 (org . t)
	 (latex . t)
	 (sql . t)))

      ;; (require 'init-org-mobile)
      (require 'init-org-notify)

      ;; setup keybindings
      (global-set-key (kbd "C-o") 'org-agenda)
      (global-set-key (kbd "M-g v") 'org-clock-goto)
      (rr/expose-bindings org-agenda-mode-map '("C-c p"))
      (rr/expose-bindings org-mode-map '("M-h" "C-c C-f" "C-a"))

      (message "Org-mode agenda config loaded."))
  (message "skipping org-mode agenda load."))

(provide 'init-org)
;;; init-org.el ends here
