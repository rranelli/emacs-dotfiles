;;; init-helm.el --- Configures helm usage preferences and keybindings.
;;; Commentary:
;;; Code:
(use-package helm-config :ensure nil)

(use-package helm
  :after helm-config

  :custom
  ;; the following configuratin makes helm-find WAY better
  (helm-split-window-in-side-p t)
  (helm-exit-idle-delay 0.01)
  (helm-ff-transformer-show-only-basename nil)
  (helm-ls-git-show-abs-or-relative 'relative)
  (helm-buffer-max-length 45)
  ;; Jeez, no fuzzy finding please!
  (helm-M-x-fuzzy-match nil)
  (helm-locate-fuzzy-match nil)
  (helm-recentf-fuzzy-match nil)

  :bind
  (:map global-map
        ("M-x"     . helm-M-x)
        ("C-x b"   . helm-buffers-list)
        ("M-l"     . helm-buffers-list)
        ("C-x C-f" . helm-find-files)
        ("C-c h"   . helm-command-prefix))
  (:map helm-map
        ("C-h"     . delete-backward-char)))

(provide 'init-helm)
;;; init-helm.el ends here
