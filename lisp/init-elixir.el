;;; package -- summary
;;; Commentary:
;;; Code:
(require 'elixir-mode)
(require 'alchemist)

;; -- hooks --
(add-hook 'elixir-mode-hook 'alchemist-mode)

;; fill doc strings
(defun rr/elixir-fill-doc-strings ()
  (interactive)
  (save-excursion
    (re-search-backward "@\\(?:module\\)?doc +\"\"\"" nil t)
    (re-search-forward "\"\"\"" nil t)
    (set-mark (point))
    (re-search-forward "\"\"\"" nil t)
    (re-search-backward "^ *\"\"\"" nil t)
    (backward-char)
    (fill-region (point) (mark))))

;; pretty symbols
(setq pretty-symbol-patterns
      (append pretty-symbol-patterns
	      `((?→ lambda "->" (elixir-mode))
		(?𝆑 lambda "\\<fn\\>" (elixir-mode))
		;; (?` lambda "\\<quote\\>" (elixir-mode))
		;; (?, lambda "\\<unquote\\>" (elixir-mode))
		)))

(define-bindings alchemist-mode-map
  '(("C-c , y" . alchemist-project-open-tests-for-current-file)
    ("C-c , a" . alchemist-mix-test)
    ("C-c , s" . alchemist-mix-test-at-point)
    ("C-c , v" . alchemist-mix-test-this-buffer)))

(provide 'init-elixir)
;;; init-elixir.el ends here
