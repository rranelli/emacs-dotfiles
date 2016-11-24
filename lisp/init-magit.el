;;; init-magit.el -- Configures the amazing and magic magit interface to git.
;;; Commentary:
;;; Code:
(require 'magit)

(setq
 ;; Remove warning for `magit-auto-revert-mode'
 magit-last-seen-setup-instructions "1.4.0"
 ;; Always open magit in the same window
 magit-status-buffer-switch-function 'switch-to-buffer
 ;; Don't add remote prefix when creating a tracking branch
 magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
 ;; colorize the magit log
 magit-log-arguments '("--graph" "--color" "--decorate" "-n256")
 ;; Don't prompt everytime for push destination
 magit-push-always-verify nil
 ;; Show gravatars
 magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(defadvice magit-status
    (before magit-save-before-status activate)
  "Save all buffers before magit status."
  (save-some-buffers t))

(rr/expose-bindings magit-log-mode-map '("C-c C-f"))

;; magit showlevels redefinition
(rr/define-bindings magit-mode-map
                    '(("s" . magit-status)
                      ("X" . magit-reset-hard)
                      ("C-1" . magit-section-show-level-1-all)
                      ("C-2" . magit-section-show-level-2-all)
                      ("C-3" . magit-section-show-level-3-all)
                      ("C-4" . magit-section-show-level-4-all)
                      ("C-x C-s" . ignore)))

(define-key magit-branch-section-map (kbd "RET") 'magit-checkout)

(rr/expose-bindings magit-mode-map '("M-1" "M-2" "M-3"))

(provide 'init-magit)
;;; init-magit.el ends here
