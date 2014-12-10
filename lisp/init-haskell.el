;;; Package --- Summary
;;; Commentary:
;;; Code:
(setq haskell-font-lock-symbols t)

(setq haskell-program-name "ghci") ;; ghci rules !

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)

;; -- haskell font lock --
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'paredit-mode)
(remove-hook 'haskell-mode-hook 'pretty-symbols-mode)

;; this is some ugly hack I hope will not be necessary in the future
(unless (boundp 'haskell-mode-after-save-handler)
  (defun haskell-mode-after-save-handler ()
    nil))

(defvar rr-haskell-font-lock-extra-symbols
  '(("<alpha>" . #X03B1)
    ("<beta>" . #X03B2)
    ("<gamma>" . #X03B3)
    ("<delta>" . #X03B4)
    (".." . #X2026)
    ("`elem`" . #X2208)
    ("elem" . #X2208)
    ("^" . #X2191)))

(eval-after-load 'haskell-font-lock
  '(progn
     (remove-alist 'haskell-font-lock-symbols-alist "()")
     (mapcar
      (lambda (entry) (add-to-list 'haskell-font-lock-symbols-alist entry))
      rr-haskell-font-lock-extra-symbols)
     (setq haskell-font-lock-keywords
	   (haskell-font-lock-keywords-create nil))))

(provide 'init-haskell)
;;; init.el ends here
