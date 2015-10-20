;;; init-clojure.el -- Configures simple and usefull things for working with C.
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'cedet)
(require 'semantic)

(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c-mode-hook 'semantic-idle-summary-mode)
(add-hook 'c-mode-hook 'semantic-stickyfunc-mode)

(global-semantic-idle-scheduler-mode 1)
(global-semanticdb-minor-mode 1)

;; bindings
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(provide 'init-c)
;;; init-c.el ends here
