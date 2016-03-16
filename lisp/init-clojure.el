;;; init-clojure.el -- Configures simple and usefull things for working with clojure.
;;; Commentary:
;;; Code:
(require 'cider)
;; -- pretty lambda --
(add-to-list 'pretty-symbol-patterns
	     `(?λ lambda "\\<fn\\>" (clojure-mode)))

;; -- bindings --
(rr/expose-bindings cider-mode-map '("C-c C-f"))

(rr/define-bindings cider-mode-map '(("C-c ?"   . cider-doc)
                                     ("C-c C-v" . cider-eval-buffer)))

(provide 'init-clojure)
;;; init-clojure.el ends here
