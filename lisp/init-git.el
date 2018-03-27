(use-package magit-gh-pulls
  :hook (magit-mode . turn-on-magit-gh-pulls))

(use-package magit
  :diminish t

  :custom
  (magit-last-seen-setup-instructions "1.4.0")
  (magit-status-buffer-switch-function 'switch-to-buffer)
  (magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
  (magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
  (magit-push-always-verify nil)
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (magit-commit-arguments '("--no-verify" "--signoff" "--gpg-sign=8AB21633BDD12B22"))

  :bind
  (:map magit-mode-map
        ("s"          . magit-status)
        ("X"          . magit-reset-hard)
        ("C-1"        . magit-section-show-level-1-all)
        ("C-2"        . magit-section-show-level-2-all)
        ("C-3"        . magit-section-show-level-3-all)
        ("C-4"        . magit-section-show-level-4-all)
        ("C-x C-s"    . ignore)
        ("M-1"        . nil)
        ("M-2"        . nil)
        ("M-3"        . nil))
  (:map magit-branch-section-map
        ("RET"        . magit-checkout)
        ("S-<return>" . magit-branch-and-checkout))
  (:map text-mode-map
        ("C-c C-e"    . rr/eval-and-replace))
  (:map magit-log-mode-map
        ("C-c C-f"    . nil))

  :config
  (defadvice magit-status
      (before magit-save-before-status activate)
    "Save all buffers before magit status."
    (save-some-buffers t))
  (defun magit-insert-recent-commits (&rest args)
    "this is a monkeypatch to hide recent commits forever"))

(use-package git-timemachine
  :config
  (defun rr/magit-current-timemachine-review ()
    (interactive)
    (magit-show-commit (car git-timemachine-revision)))

  :bind
  (:map git-timemachine-mode-map
        ("s" . rr/magit-current-timemachine-review)))

(provide 'init-git)
