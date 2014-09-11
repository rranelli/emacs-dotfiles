;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'magit)

(diminish 'magit-auto-revert-mode)

;; Always open magit in the same window
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Don't add remote prefix when creating a tracking branch
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(defadvice magit-status
  (before magit-save-before-status activate)
  "Save all buffers before magit status."
  (save-some-buffers t))


(global-set-key (kbd "C-c g") 'magit-status)

;; magit showlevels redefinition
(define-key magit-mode-map (kbd "s") 'magit-status)
(define-key magit-mode-map (kbd "M-1") 'delete-other-windows)
(define-key magit-mode-map (kbd "M-2") 'split-window-vertically)
(define-key magit-mode-map (kbd "M-3") 'split-window-horizontally)
(define-key magit-mode-map (kbd "C-1") 'magit-show-level-1-all)
(define-key magit-mode-map (kbd "C-2") 'magit-show-level-2-all)
(define-key magit-mode-map (kbd "C-3") 'magit-show-level-3-all)
(define-key magit-mode-map (kbd "C-4") 'magit-show-level-4-all)

(provide 'init-magit)
;;; init-magit.el ends here
