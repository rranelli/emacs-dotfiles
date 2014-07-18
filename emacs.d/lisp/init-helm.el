;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'helm-config)
(require 'helm-ls-git)

(helm-mode t)

;; the following configuratin makes helm-find WAY better
(setq helm-split-window-in-side-p t
      helm-exit-idle-delay 0.01
      helm-ff-transformer-show-only-basename nil
      helm-ls-git-show-abs-or-relative 'relative)

;; helpers for more familiar helm find-file navigation
(defun helm-find-files-sensitive-backspace ()
  "Deletes whole directory in helm find files mode on backspace."
  (interactive)
  (if (char-equal ?/ (char-before))
      (helm-find-files-up-one-level 1)
    (backward-delete-char 1)))

(defun git-project-dirtree ()
  "Open dirtree using the git root."
  (interactive)
  (dirtree (helm-ls-git-root-dir) "")
  (let* ((width (window-width (frame-selected-window)))
         (desired-width 38)
         (delta (- desired-width width)))
    (window-resize (frame-selected-window) delta t)))

;; keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-l") 'helm-buffers-list)

(global-set-key (kbd "C-c C-f") 'helm-browse-project)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; helm better navigation
(define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-sensitive-backspace)
(define-key helm-find-files-map (kbd "<DEL>") 'helm-find-files-sensitive-backspace)
(define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-sensitive-backspace)

(define-key helm-map (kbd "C-;") 'helm-select-action)

(provide 'init-helm)
;;; init-helm.el ends here
