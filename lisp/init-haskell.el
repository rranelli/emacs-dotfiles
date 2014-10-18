;;; Package --- Summary
;;; Commentary:
;;; Code:
(setq haskell-font-lock-symbols t)

(remove-hook 'haskell-mode-hook 'pretty-symbols-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)

(provide 'init-haskell)
;;; init.el ends here
