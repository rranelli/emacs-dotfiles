;;; package -- Summary
;;; Commentary:
;;; Code:
(diminish 'paredit-mode "Par")
(diminish 'eldoc-mode)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; eldoc hooks!
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'superword-mode)

(defun remove-elc-on-save ()
  "If you're saving an elisp file, the .elc is probably useless."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; -- keybindings --
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(define-key paredit-mode-map (kbd "M-h") 'paredit-backward-kill-word)

(provide 'init-lisp)
;;; init-lisp.el ends here
