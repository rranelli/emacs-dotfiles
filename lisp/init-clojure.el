;;; init-clojure.el -- Configures simple and usefull things for working with clojure.
;;; Commentary:
;;; Code:
(require 'cider)
;; -- pretty lambda --
(add-to-list 'pretty-symbol-patterns
	     `(?𝝺 lambda "\\<fn\\>" (clojure-mode)))

;; -- bindings --
(rr/expose-bindings cider-mode-map '("C-c C-f"))

(rr/define-bindings cider-mode-map '(("C-c ?"   . cider-doc)
                                     ("C-c C-v" . cider-eval-buffer)))

(defun endless/eval-overlay (value point)
  ""
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r) (endless/eval-overlay r (point))))

(provide 'init-clojure)
;;; init-clojure.el ends here
