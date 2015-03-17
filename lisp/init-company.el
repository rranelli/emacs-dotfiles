;;; init-company.el -- Configures company-mode autocompletion.
;;; Commentary:
;;; Code:
(require 'company)
(global-company-mode)

(diminish 'company-mode "")

(global-set-key (kbd "M-/") 'company-complete)

(define-bindings company-active-map
  `(("C-n" . company-select-next)
    ("C-p" . company-select-previous)))

(provide 'init-company)
;;; init-company.el ends here
