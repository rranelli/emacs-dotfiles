;;; init-scala.el -- Configures utilities and nice-to-have features for Scala development.
;;; Commentary:
;;; Code:
(require 'scala-mode2)
(require 'ensime)

;; -- hooks --
(add-hook 'scala-mode-hook 'ensime-mode)

;; pretty symbols
(setq pretty-symbol-patterns
      (append pretty-symbol-patterns
	      `((?→ lambda "->" (scala-mode))
                (?⟵ lambda "<-" (scala-mode))
                (?➾ lambda "=>" (scala-mode)))))

(rr/define-bindings scala-mode-map
                    '(("C-c , c" . ensime-sbt-do-compile)
                      ("C-c , a" . ensime-sbt-do-test)
                      ("M-/" . ensime-company)))

(rr/expose-default-bindings ensime-mode-map)

(provide 'init-scala)
;;; init-scala.el ends here
