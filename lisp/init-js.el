;;; init-js.el --- Configures js editing
;;; Commentary:
;;; Code:
(use-package js2-mode
  :mode "\\.js"
  :custom
  (js2-strict-trailing-comma-warning nil)

  :config
  (rr/expose-default-bindings js2-mode-map))

(use-package indium
  :hook
  (js2-mode . indium-interaction-mode))

;; run `npm install -g tern' and add `.tern-project' to your project
;; into .tern-project:  {"plugins": {"node": {}, "es_modules": {}}}
(use-package tern
  :hook
  (js2-mode . tern-mode))

(provide 'init-js)
;;; init-js.el ends here
