;;; init-elixir.el -- Configures utilities and nice-to-have features for Elixir development.
;;; Commentary:
;;; Code:
(require 'elixir-mode)
(require 'alchemist)

;; -- hooks --
(add-hook 'elixir-mode-hook 'alchemist-mode)

;; pretty symbols
(setq pretty-symbol-patterns
      (append pretty-symbol-patterns
	      `((?→ lambda "->" (elixir-mode))
                (?⤌ lambda "<-" (elixir-mode))
		(?𝆑 lambda "\\<fn\\>" (elixir-mode)))))

(rr/define-bindings elixir-mode-map
                    '(("M-q" . elixir-mode-fill-doc-string)))

(rr/define-bindings alchemist-mode-map
                    '(("C-c , t" . alchemist-project-toggle-file-and-tests)
                      ("C-c , y" . alchemist-project-toggle-file-and-tests-other-window)
                      ("C-c , a" . alchemist-mix-test)
                      ("C-c , s" . alchemist-mix-test-at-point)
                      ("C-c , v" . alchemist-mix-test-this-buffer)
                      ("C-c , r" . alchemist-mix-rerun-last-test)
                      ("C-c , c" . alchemist-mix-compile)))

(define-key elixir-mode-map (kbd "C-c C-s") 'inferior-elixir)

(defun rr/iex-pry-command ()
  "Format an `iex' command to call a test with `pry'."
  (interactive)
  (->> (rr/show-file-name)
       (format "iex -S mix test %s")
       (kill-new)))

(provide 'init-elixir)
;;; init-elixir.el ends here
