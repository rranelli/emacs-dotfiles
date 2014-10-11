;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'cider)

;; -- pretty lambda --
(add-to-list 'pretty-symbol-patterns
	     `(?λ lambda "\\<fn\\>" (clojure-mode)))

;; -- bindings --
(expose-bindings cider-mode-map '("C-c C-f"))

;; -- hooks --
(add-hook 'clojure-mode-hook 'paredit-mode)

(provide 'init-clojure)
;;; init-clojure.el ends here
