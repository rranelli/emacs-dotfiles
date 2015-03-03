;;; package -- Summary
;;; Commentary:
;;; Code:

(defcustom rr/org-files-directory "~/Copy/org/"
  "Directory for org files."
  :group 'init-org)

;; make org beautifull
(if (file-exists-p rr/org-files-directory)
    ;; Set up org-agenda files
    (let* ((dir-files (directory-files rr/org-files-directory t directory-files-no-dot-files-regexp))
	   (org-files (remove-if
		       #'(lambda (name) (or
				    (string-match "archive" name)
				    (string-match "\??/\.#" name))) dir-files)))

      (setq org-user-agenda-files org-files)

      ;; loading org custom
      (require 'org-mode-custom)

      ;; Setting up babel support for languages
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

      ;; prettify org
      (setq
       org-odd-levels-only t
       org-hide-leading-stars t
       org-startup-indented nil)

      ;; adding a hook to save org stuff more frequently
      (add-hook 'after-save-hook 'org-save-all-org-buffers)
      (add-hook 'org-mode-hook 'custom-add-watchwords)

      ;; set-up encryption
      (require 'org-crypt)
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
      (setq org-crypt-key nil)

      ;; setup keybindings
      (expose-bindings org-agenda-mode-map '("C-c p"))
      (expose-bindings org-mode-map '("M-h" "C-c C-f"))

      (message "Org-mode agenda config loaded."))
  (message "skipping org-mode agenda load."))

(provide 'init-org)
;;; init-org.el ends here
