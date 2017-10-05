;;; init-js.el --- Configures js editing
;;; Commentary:
;;; Code:
(require 'js2-mode)
(require 'indium)

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))

(add-hook 'js2-mode-hook 'indium-interaction-mode)

(provide 'init-js)
;;; init-js.el ends here
