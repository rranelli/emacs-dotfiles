;;; init-company.el -- Configures company-mode autocompletion.
;;; Commentary:
;;; Code:
(use-package company
  :config
  (global-company-mode)
  (add-to-list 'company-backends 'company-ansible)
  :bind
  (:map global-map
        ("M-/" . company-complete))
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-h" . backward-delete-char)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'init-company)
;;; init-company.el ends here
