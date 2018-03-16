;;; init-scala.el -- Configures utilities and nice-to-have features for Scala development.
;;; Commentary:
;;; Code:
(use-package scala-mode
  :mode "\\.scala$"

  :config
  (setq pretty-symbol-patterns
        (append pretty-symbol-patterns
                `((?⟶ lambda "->" (scala-mode))
                  (?⟵ lambda "<-" (scala-mode))
                  (?⟹ lambda "=>" (scala-mode))))))
(use-package ensime
  :hook
  (scala-mode . ensime-mode)

  :config
  (rr/expose-default-bindings ensime-mode-map)

  :bind
  (:map scala-mode-map
        ("C-c , c" . ensime-sbt-do-compile)
        ("C-c , a" . ensime-sbt-do-test)
        ("M-/" . ensime-company)))

(provide 'init-scala)
;;; init-scala.el ends here
