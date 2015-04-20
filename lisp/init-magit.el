;;; init-magit.el -- Configures the amazing and magic magit interface to git.
;;; Commentary:
;;; Code:
(require 'magit)

;; Always open magit in the same window
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Don't add remote prefix when creating a tracking branch
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(defadvice magit-status
  (before magit-save-before-status activate)
  "Save all buffers before magit status."
  (save-some-buffers t))


(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "<f8>") 'magit-status)

;; magit showlevels redefinition
(define-key magit-mode-map (kbd "s") 'magit-status)
(define-key magit-mode-map (kbd "C-1") 'magit-show-level-1-all)
(define-key magit-mode-map (kbd "C-2") 'magit-show-level-2-all)
(define-key magit-mode-map (kbd "C-3") 'magit-show-level-3-all)
(define-key magit-mode-map (kbd "C-4") 'magit-show-level-4-all)

(define-key magit-mode-map (kbd "C-x C-s") 'noop)

(rr/expose-bindings magit-mode-map '("M-1" "M-2" "M-3"))

(provide 'init-magit)
;;; init-magit.el ends here
