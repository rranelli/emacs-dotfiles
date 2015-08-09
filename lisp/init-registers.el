;;; init-registers.el -- Adapts some of better-registers.el configurations to play nice with other modes
;;; Commentary:
;;; Code:

;; Do not use C-r for better-registers-map. This is just too much agressive and
;; fucks up them terminals
(setq-default better-registers-use-C-r nil)
(require 'better-registers)

;; globally set M-j to jump to registers since this is ok.
(global-set-key (kbd "M-j") 'better-registers-jump-to-register)

;; fixes some more agressive keybinding change. God, F1 ? really ?
(rr/expose-bindings better-registers-map
                    '("<f1>" "C-j" "C-x r"))

;; `C-x r' will also work for the registers map
(define-key better-registers-map (kbd "M-m") better-registers-r-map)

(provide 'init-registers)
;;; init-registers.el ends here
