;;; init-java.el -- Configures nice-to-have features for Java development.
;;; Commentary:
;;; Code:
(use-package cc-mode :ensure nil)

(use-package java-mode
  :ensure nil
  :mode ("\\.java$")

  :config
  (defun rr/java-mode-setup ()
    (c-set-style "cc-mode")
    (setq tab-width 4 indent-tabs-mode t c-basic-offset 4))

  :bind
  (:map java-mode-map
       ("C-c C-d" . javadoc-lookup)
       ("C-c j s" . sort-java-imports)
       ("C-c j i" . add-java-import))

  :hook
  (java-mode . rr/java-mode-setup))

(use-package javadoc-lookup
  :after java-mode
  :config
  (javadoc-add-artifacts [com.nullprogram native-guide "0.2"]))

(use-package maven-test-mode
  :after java-mode)

(provide 'init-java)
;;; init-java.el ends here
