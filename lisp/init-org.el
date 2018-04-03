;;; init-org.el -- Sanely configures org-mode related stuff.
;;; Commentary:
;;; Code:
(use-package org-journal
  :after org)

(use-package org
  :commands org-mode
  :mode ("\\.org$")

  :custom
  (rr/org-files-directory "~/Dropbox/org/")
  (org-odd-levels-only t)
  (org-hide-leading-stars t)
  (org-startup-indented nil)
  (org-babel-sh-command "bash")
  (org-export-babel-evaluate nil)
  (org-src-fontify-natively t)

  :bind
  (:map global-map
        ("C-o" . org-agenda)
        ("M-g v" . org-clock-goto))

  :config
  (defun rr/dump-inbox-to-refile ()
          (interactive)
          (shell-command " cd ~/Dropbox/org; cat refile.org inbox.org > refile2.org; mv refile{2,}.org; echo > inbox.org; echo ok"))

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
        (setq org-journal-file-format "%Y-%m-%d.org")
        (setq org-journal-date-format "%A, %x
   - Food     (0-5) :
   - Exercise (0-5) :
   - Alcohol  (y/n) :")

        ;; loading org custom
        (require 'org-mode-custom)

        ;; adding a hook to save org stuff more frequently
        (add-hook 'after-save-hook 'org-save-all-org-buffers)
        (add-hook 'org-mode-hook 'custom-add-watchwords)

        (org-babel-do-load-languages 'org-babel-load-languages
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
           ;; (elixir . t)
           (sql . t)))

        (require 'init-org-notify)

        (rr/expose-bindings org-agenda-mode-map '("C-c p"))
        (rr/expose-bindings org-mode-map '("M-h" "C-c C-f" "C-a"))

        (message "Org-mode agenda config loaded."))
    (message "skipping org-mode agenda load.")))

(provide 'init-org)
;;; init-org.el ends here
