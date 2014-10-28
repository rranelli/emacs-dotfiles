;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'javadoc-lookup)
(require 'maven-test-mode)

(javadoc-add-artifacts [com.nullprogram native-guide "0.2"]
                       [org.apache.commons commons-math3 "3.0"])

;; -- keybindings --
(define-bindings java-mode-map
  '(("C-c j d" . javadoc-lookup)
    ("C-c j s" . sort-java-imports)
    ("C-c j i" . add-java-import)))

;; -- hooks --
(add-hook 'java-mode-hook 'maven-test-mode)

(provide 'init-java)
;;; init-java.el ends here
