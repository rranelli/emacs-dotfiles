;;; init-elixir.el -- Configures utilities and nice-to-have features for Elixir development.
;;; Commentary:
;;; Code:
(require 'elixir-mode)
(require 'alchemist)

;; Do not change mode-line color based on test result
(setq alchemist-test-status-modeline nil)

;;
;;; hooks
;;
(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'alchemist-test-report-mode-hook
          (lambda ()
            (toggle-truncate-lines 0)
            (text-scale-set -1)))
(add-hook 'flycheck-before-syntax-check-hook
          (lambda ()
            (when (equal 'elixir-mode major-mode)
              (setq default-directory
                    (alchemist-project-root-or-default-dir)))))

;; pretty symbols
(setq pretty-symbol-patterns
      (append pretty-symbol-patterns
	      `((?⟶ lambda "->" (elixir-mode))
                (?⟵ lambda "<-" (elixir-mode))
		(?𝆑 lambda "\\<fn\\>" (elixir-mode)))))

(flycheck-define-checker elixir
  "An Elixir syntax checker using the Elixir interpreter."
  :command ("/home/renan/code/linuxsetup/scripts/rr-mix-compile-anywhere"  ; Prevent tedious module redefinition warning.
            source)
  ;; Elixir compiler errors
  :error-patterns ((error line-start "** (" (zero-or-more not-newline) ") "
                          (file-name) ":" line ": " (message) line-end)
                   ;; Warnings from Elixir >= 0.12.4
                   (warning line-start (file-name) ":" line ": warning:" (message) line-end))
  :modes elixir-mode)

;; elixir checker using elixirrc; left here for convenience
;; (flycheck-define-checker elixir
;;   "An Elixir syntax checker using the Elixir interpreter."
;;   :command ("elixirc"  ; Prevent tedious module redefinition warning.
;;             "-o" temporary-directory
;;             "--ignore-module-conflict"
;;             source-original)
;;   :predicate (lambda () (not (string-equal "exs" (file-name-extension (buffer-file-name)))))
;;   ;; Elixir compiler errors
;;   :error-patterns ((error line-start "** (" (zero-or-more not-newline) ") "
;;                           (file-name) ":" line ": " (message) line-end)
;;                    ;; Warnings from Elixir >= 0.12.4
;;                    (warning line-start (file-name) ":" line ": warning:" (message) line-end))
;;   :modes elixir-mode)

(add-to-list 'flycheck-checkers 'elixir)
;;
;;; helper functions
;;
(defun rr/iex-pry-command ()
  "Format an `iex' command to call a test with `pry'."
  (interactive)
  (kill-new (format "iex -S mix test %s"
                    (rr/show-file-name))))

(defun rr/iex-pry ()
  (interactive)
  (let ((default-directory (alchemist-project-root))
        (cmd (rr/iex-pry-command)))
    (ansi-term "/bin/bash" "iex-pry")
    (sleep-for 1) ;; see emacs/24.5/lisp/term.el.gz:1440 ...
    ;; nothing I can do
    (term-line-mode)
    (goto-char (point-max))
    (insert cmd)
    (term-char-mode)))

(defun rr/set-mix-env ()
  (interactive)
  (setenv "MIX_ENV" (read-string "MIX_ENV= " "dev")))

;;
;;; bindings
;;
(rr/define-bindings elixir-mode-map
                    '(("M-q" . elixir-mode-fill-doc-string)))

(rr/define-bindings alchemist-mode-map
                    '(("C-c , t" . alchemist-project-toggle-file-and-tests)
                      ("C-c , y" . alchemist-project-toggle-file-and-tests-other-window)
                      ("C-c , a" . alchemist-mix-test)
                      ("C-c , s" . alchemist-mix-test-at-point)
                      ("C-c , v" . alchemist-project-run-tests-for-current-file)
                      ("C-c , r" . alchemist-mix-rerun-last-test)
                      ("C-c , c" . alchemist-mix-compile)
                      ("C-c , S" . rr/iex-pry)))

(define-key elixir-mode-map (kbd "C-c C-s") 'inferior-elixir)

(provide 'init-elixir)
;;; init-elixir.el ends here
