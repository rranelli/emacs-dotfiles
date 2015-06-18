;;; init-org.el -- Sanely configures org-mode related stuff.
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
				    (not (string-match "\.org$" name))
				    (string-match "archive" name)
				    (string-match "\??/\.#" name)))
                       dir-files)))

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

      (defadvice epg--start (around advice-epg-disable-agent disable)
        "Make epg--start not able to find a gpg-agent"
        (let ((agent (getenv "GPG_AGENT_INFO")))
          (setenv "GPG_AGENT_INFO" nil)
          ad-do-it
          (setenv "GPG_AGENT_INFO" agent)))

      (defun epg-enable-agent ()
        "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent"
        (interactive)
        (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
        (ad-activate 'epg--start)
        (message "EasyPG gpg-agent re-enabled"))

      (defun epg-disable-agent ()
        "Make EasyPG bypass any gpg-agent"
        (interactive)
        (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
        (ad-activate 'epg--start)
        (message "EasyPG gpg-agent bypassed"))

      (epg-disable-agent)

      ;; org notification magic
      (require 'org-notify)
      (org-notify-start 60)
      (org-notify-add 'default
                      '(:time "1h" :period "15m" :duration 25 :actions -notify/window)
                      '(:time "4h" :period "45m" :duration 25 :actions -notify/window)
                      '(:time "1d" :period "90m" :duration 25 :actions -notify/window)
                      '(:time "3d" :period "3h" :duration 25 :actions -notify/window))

      ;; setup keybindings
      (rr/expose-bindings org-agenda-mode-map '("C-c p"))
      (rr/expose-bindings org-mode-map '("M-h" "C-c C-f" "C-a"))

      (message "Org-mode agenda config loaded."))
  (message "skipping org-mode agenda load."))

(provide 'init-org)
;;; init-org.el ends here
