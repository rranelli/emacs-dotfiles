;;; init-clojure.el -- Configures simple and usefull things for working with clojure.
;;; Commentary:
;;; Code:
(require 'cider)

;; -- pretty lambda --
(add-to-list 'pretty-symbol-patterns
	     `(?λ lambda "\\<fn\\>" (clojure-mode)))

;; -- bindings --
(expose-bindings cider-mode-map '("C-c C-f"))

;; -- hooks --
;; TODO: Add smartparens things like lisp mode

(provide 'init-clojure)
;;; init-clojure.el ends here
