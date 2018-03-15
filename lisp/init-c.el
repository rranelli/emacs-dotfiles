;;; init-clojure.el -- Configures simple and usefull things for working with C.
;;; Commentary:
;;; Code:
(use-package cc-mode
  :config
  (rr/expose-default-bindings c-mode-base-map)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(use-package semantic
  :hook
  (c-mode . semantic-mode)
  (c-mode . semantic-idle-summary-mode)
  (c-mode . semantic-stickyfunc-mode)

  :config
  (global-semantic-idle-scheduler-mode 1)
  (global-semanticdb-minor-mode 1))

(use-package company-c-headers
  :after cc-mode
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package cedet
  :ensure nil)

(provide 'init-c)
;;; init-c.el ends here
