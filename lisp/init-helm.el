;;; init-helm.el --- Configures helm usage preferences and keybindings.
;;; Commentary:
;;; Code:
(require 'helm-config)

(helm-mode t)

;; the following configuratin makes helm-find WAY better
(setq helm-split-window-in-side-p t
      helm-exit-idle-delay 0.01
      helm-ff-transformer-show-only-basename nil
      helm-ls-git-show-abs-or-relative 'relative
      helm-buffer-max-length 45)

;; Jeez, no fuzzy finding please!
(setq helm-M-x-fuzzy-match nil)
(setq helm-locate-fuzzy-match nil)
(setq helm-recentf-fuzzy-match nil)

;; -- keybindings --
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-l") 'helm-buffers-list)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c h") 'helm-command-prefix)

(define-key helm-map (kbd "C-h") 'delete-backward-char)

(provide 'init-helm)
;;; init-helm.el ends here
