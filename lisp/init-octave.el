;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'octave)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.oct$" . octave-mode))

;; -- keybindings --
(expose-rr-default-bindings octave-mode-map)
(define-key octave-mode-map (kbd "C-c j d") 'octave-help)
(define-key octave-mode-map (kbd "C-c r b") 'octave-send-buffer)

;; This fixes an issue where the plot hangs.
(setq inferior-octave-startup-args '("-i"))

(provide 'init-octave)
;;; init-octave.el ends here
