;;; init-clojure.el -- Configures simple and usefull things for working with C.
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'cedet)
(require 'semantic)

;; completion for C headers
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c-mode-hook 'semantic-idle-summary-mode)
(add-hook 'c-mode-hook 'semantic-stickyfunc-mode)

(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)

;; bindings
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(define-key c-mode-base-map (kbd "C-c f") 'ff-find-other-file)

(provide 'init-c)
;;; init-c.el ends here
