;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'helm-ls-git)

(require 'helm-config)

(helm-mode t)
(diminish 'helm-mode)

;; the following configuratin makes helm-find WAY better
(setq helm-split-window-in-side-p t
      helm-exit-idle-delay 0.01
      helm-ff-transformer-show-only-basename nil
      helm-ls-git-show-abs-or-relative 'relative
      helm-buffer-max-length 45)

;; helpers for more familiar helm find-file navigation
(defun helm-find-files-sensitive-backspace ()
  "Deletes whole directory in helm find files mode on backspace."
  (interactive)
  (if (char-equal ?/ (char-before))
      (helm-find-files-up-one-level 1)
    (backward-delete-char 1)))

;; keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-l") 'helm-buffers-list)

(global-set-key (kbd "C-c C-f") 'helm-browse-project)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-x p") 'helm-apropos)

;; helm better navigation
(define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-sensitive-backspace)
(define-key helm-find-files-map (kbd "<DEL>") 'helm-find-files-sensitive-backspace)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-sensitive-backspace)

(provide 'init-helm)
;;; init-helm.el ends here
