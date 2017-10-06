;;; init-js.el --- Configures js editing
;;; Commentary:
;;; Code:
(require 'js2-mode)
(require 'indium)

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))

(setq js2-strict-trailing-comma-warning nil)

(add-hook 'js2-mode-hook 'indium-interaction-mode)

(rr/expose-default-bindings js2-mode-map)

(provide 'init-js)
;;; init-js.el ends here
