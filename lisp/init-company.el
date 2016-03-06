;;; init-company.el -- Configures company-mode autocompletion.
;;; Commentary:
;;; Code:
(require 'company)
(global-company-mode)

(add-to-list 'company-backends 'company-ansible)

(global-set-key (kbd "M-/") 'company-complete)

(rr/define-bindings company-active-map
  `(("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-h" . backward-delete-char)))

(provide 'init-company)
;;; init-company.el ends here
