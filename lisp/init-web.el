;;; init-wel.el -- Configures web-mode for edition of html templates
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.html.eex$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))

(defun rr/web-mode-conf ()
  (setq web-mode-extra-auto-pairs
        '(("eex"  . (("do" "end")))
          ))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (smartparens-mode 0))


;; -- keybindings --
(rr/define-bindings web-mode-map
                    '(("C-c M-y" . emmet-expand-line)
                      ("M-n" . web-mode-tag-next)
                      ("M-p" . web-mode-tag-previous)
                      ("C-c f" . web-mode-fold-or-unfold)
                      ("M-u" . web-mode-mark-and-expand)))

;; -- hooks --
(add-hook 'web-mode-hook 'rr/web-mode-conf)

(provide 'init-web)
;;; init-web.el ends here
