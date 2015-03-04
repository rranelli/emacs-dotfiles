;;; package -- summary
;;; Commentary:
;;; Code:
(require 'elixir-mode)
(require 'alchemist)

;; -- hooks --
(add-hook 'elixir-mode-hook 'alchemist-mode)

(define-bindings alchemist-mode-map
  '(("C-c , y" . alchemist-project-open-tests-for-current-file)
    ("C-c , a" . alchemist-mix-test)
    ("C-c , s" . alchemist-mix-test-at-point)
    ("C-c , v" . alchemist-mix-test-this-buffer)))

(provide 'init-elixir)
;;; init-elixir.el ends here
