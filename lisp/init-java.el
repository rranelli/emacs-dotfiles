;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'javadoc-lookup)
(require 'maven-test-mode)

;; Uncomment line for adding more artifacts to documentation lookup
;; (javadoc-add-artifacts [com.nullprogram native-guide "0.2"])

;; -- keybindings --
(define-bindings java-mode-map
  '(("C-c j d" . javadoc-lookup)
    ("C-c j s" . sort-java-imports)
    ("C-c j i" . add-java-import)))

(add-hook 'java-mode-hook (lambda ()
			    (c-set-style "cc-mode")
			    (set (make-local-variable 'c-basic-offset) 2)))

(provide 'init-java)
;;; init-java.el ends here
