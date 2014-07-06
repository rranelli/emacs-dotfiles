;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'yasnippet)

(yas-global-mode 1)
(add-hook 'prog-mode-hook
          '(lambda () (yas-minor-mode)))

(setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

;; add to autocomplete
(if (require 'init-ac)
    (add-to-list 'ac-sources ac-source-yasnippet))

;; -- keybindings --
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(global-set-key (kbd "M-y") 'yas-expand)

(provide 'init-yas)
;;; init-yas.el ends here
