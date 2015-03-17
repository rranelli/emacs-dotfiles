;;; init-lisp.el -- Configures nice-to-have features for ELisp development.
;;; Commentary:
;;; Code:
(diminish 'eldoc-mode)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; eldoc hooks!
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (if (> (point) (mark))
      (backward-kill-sexp)
    (kill-sexp))
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))

(defun macroexpand-point (sexp)
  "Expands macro at point/region containing SEXP."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(defun remove-elc-on-save ()
  "If you're saving an elisp file, the .elc is probably useless."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; -- keybindings --
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(provide 'init-lisp)
;;; init-lisp.el ends here
