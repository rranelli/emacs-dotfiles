;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'smartparens-config)

(define-key lisp-mode-map (kbd "C-k") 'sp-kill-hybrid-sexp)
(define-key lisp-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key lisp-mode-map (kbd "C-M-u") 'sp-up-sexp)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
