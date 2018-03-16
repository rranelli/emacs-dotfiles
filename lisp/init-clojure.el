;;; init-clojure.el -- Configures simple and usefull things for working with clojure.
;;; Commentary:
;;; Code:
(use-package cider)

(setq cider-use-tooltips nil)
(require 'cider)

;; -- pretty lambda --
(add-to-list 'pretty-symbol-patterns
	     `(?𝝺 lambda "\\<fn\\>" (clojure-mode)))

;; eval overlay for elisp
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

;; -- bindings --
(rr/expose-bindings cider-mode-map '("C-c C-f"))
(rr/define-bindings cider-mode-map '(("C-c C-d"   . cider-doc)
                                     ("C-c C-v" . cider-eval-buffer)))

;; -- hooks --
(add-hook 'cider-mode-hook (lambda () (tooltip-mode -1)))

(provide 'init-clojure)
;;; init-clojure.el ends here
