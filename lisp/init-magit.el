;;; init-magit.el -- Configures the amazing and magic magit interface to git.
;;; Commentary:
;;; Code:
(use-package magit-gh-pulls
  :ensure t)

(use-package magit
  :ensure t
  :config
  (defadvice magit-status
      (before magit-save-before-status activate)
    "Save all buffers before magit status."
    (save-some-buffers t))
  (defun magit-insert-recent-commits (&rest args)
    "this is a monkeypatch to hide recent commits forever")

  :custom
  (magit-last-seen-setup-instructions "1.4.0" "Remove warning for `magit-auto-revert-mode'")
  (magit-status-buffer-switch-function 'switch-to-buffer "Always open magit in the same window")
  (magit-default-tracking-name-function 'magit-default-tracking-name-branch-only "Don't add remote prefix when creating a tracking branch")
  (magit-log-arguments '("--graph" "--color" "--decorate" "-n256") "colorize the magit log")
  (magit-push-always-verify nil "Don't prompt everytime for push destination")
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     ") "Show gravatars")
  (magit-commit-arguments '("--no-verify" "--signoff" "--gpg-sign=8AB21633BDD12B22"))

  :bind (:map magit-mode-map
         ("s" . magit-status)
         ("X" . magit-reset-hard)
         ("C-1" . magit-section-show-level-1-all)
         ("C-2" . magit-section-show-level-2-all)
         ("C-3" . magit-section-show-level-3-all)
         ("C-4" . magit-section-show-level-4-all)
         ("C-x C-s" . ignore)
         ("M-1" . nil)
         ("M-2" . nil)
         ("M-3" . nil)
         :map magit-branch-section-map
         ("RET" . magit-checkout)
         ("S-<return>" . magit-branch-and-checkout)
         :map text-mode-map
         ("C-c C-e" . rr/eval-and-replace)
         :map magit-log-mode-map
         ("C-c C-f" . nil))

  :hook ((magit-mode-hook . turn-on-magit-gh-pulls)))

(use-package git-timemachine
  :ensure t
  :config
  (defun rr/magit-current-timemachine-review ()
    (interactive)
    (magit-show-commit (car git-timemachine-revision)))

  (define-key git-timemachine-mode-map "s" 'rr/magit-current-timemachine-review))

(provide 'init-magit)
;;; init-magit.el ends here
