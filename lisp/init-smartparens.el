;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'smartparens-config)

(define-key lisp-mode-map (kbd "C-k") 'sp-kill-hybrid-sexp)
(define-key lisp-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key lisp-mode-map (kbd "C-M-u") 'sp-up-sexp)

;; TODO: Add wrappers for the following stuff
;; (wrap-region-add-wrapper "`" "`")
;; (wrap-region-add-wrapper "|" "|")
;; (wrap-region-add-wrapper "=" "=")
;; (wrap-region-add-wrapper "*" "*")
;; (wrap-region-add-wrapper "/" "/")
;; (wrap-region-add-wrapper "_" "_")

(provide 'init-smartparens)
;;; init-smartparens.el ends here
