;;; init-java.el -- Configures nice-to-have features for Java development.
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'javadoc-lookup)
(require 'maven-test-mode)

;; Uncomment line for adding more artifacts to documentation lookup
;; (javadoc-add-artifacts [com.nullprogram native-guide "0.2"])

;; -- keybindings --
(rr/define-bindings java-mode-map
  '(("C-c C-d" . javadoc-lookup)
    ("C-c j s" . sort-java-imports)
    ("C-c j i" . add-java-import)))

(add-hook 'java-mode-hook (lambda ()
			    (c-set-style "cc-mode")
			    (setq tab-width 4
				  indent-tabs-mode t
				  c-basic-offset 4)
                            (remove-hook (make-local-variable 'before-save-hook)
					 'delete-trailing-whitespace)))

(provide 'init-java)
;;; init-java.el ends here
