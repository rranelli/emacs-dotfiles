;;; Package --- Summary
;;; Commentary:
;;; Code:
(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)

;; -- haskell font lock --
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(add-hook 'haskell-mode-hook 'paredit-mode)
(remove-hook 'haskell-mode-hook 'pretty-symbols-mode)

(defvar rr-haskell-font-lock-extra-symbols
  '(("<alpha>" . #X03B1)
    ("<beta>" . #X03B2)
    ("<gamma>" . #X03B3)
    ("<delta>" . #X03B4)
    ("''" . #X2033)
    ("'" . #X2032)
    (".." . #X2026)
    ("`elem`" . #X2208)
    ("elem" . #X2208)))

(eval-after-load 'haskell-font-lock
  '(progn
     (mapcar
      (lambda (entry) (add-to-list 'haskell-font-lock-symbols-alist entry))
      rr-haskell-font-lock-extra-symbols)
     (setq haskell-font-lock-keywords
	   (haskell-font-lock-keywords-create nil))))

(provide 'init-haskell)
;;; init.el ends here
