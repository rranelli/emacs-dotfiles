;;; init-registers.el -- Adapts some of better-registers.el configurations to play nice with other modes
;;; Commentary:
;;; Code:
(setq-default better-registers-use-C-r nil)
(use-package better-registers
  :defer 5

  :config
  (rr/expose-bindings better-registers-map '("<f1>" "C-j" "C-x r"))

  :bind
  (:map global-map ("M-j" . better-registers-jump-to-register)))

(provide 'init-registers)
;;; init-registers.el ends here
