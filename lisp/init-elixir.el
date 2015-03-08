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

(defun rr/alchemist-toggle-file-and-tests-other-window ()
  (interactive)
  (if (rr/alchemist--is-test-file-p)
      (rr/alchemist--project-open-file-for-current-tests 'find-file-other-window)
    (rr/alchemist--project-open-tests-for-current-file 'find-file-other-window)))

(defun rr/alchemist-toggle-file-and-tests ()
  (interactive)
  (if (rr/alchemist--is-test-file-p)
      (rr/alchemist--project-open-file-for-current-tests 'find-file)
    (rr/alchemist--project-open-tests-for-current-file 'find-file)))

(defun rr/alchemist--is-test-file-p ()
  (string-match "_test\.exs$" (buffer-file-name)))

(defun rr/alchemist--project-open-file-for-current-tests (toggler)
  (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
         (filename (replace-regexp-in-string "^test/" "lib/" filename))
         (filename (replace-regexp-in-string "_test\.exs$" "\.ex" filename))
         (filename (format "%s/%s" (alchemist-project-root) filename)))
    (funcall toggler filename)))

(defun rr/alchemist--project-open-tests-for-current-file (toggler)
  (interactive)
  (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
         (filename (replace-regexp-in-string "^lib/" "test/" filename))
         (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
         (filename (format "%s/%s" (alchemist-project-root) filename)))
    (funcall toggler filename)))

;; pretty symbols
(setq pretty-symbol-patterns
      (append pretty-symbol-patterns
	      `((?→ lambda "->" (elixir-mode))
		(?𝆑 lambda "\\<fn\\>" (elixir-mode))
		;; (?` lambda "\\<quote\\>" (elixir-mode))
		;; (?, lambda "\\<unquote\\>" (elixir-mode))
		)))

(define-bindings alchemist-mode-map
  '(("C-c , y" . rr/alchemist-toggle-file-and-tests-other-window)
    ("C-c , t" . rr/alchemist-toggle-file-and-tests)
    ("C-c , a" . alchemist-mix-test)
    ("C-c , s" . alchemist-mix-test-at-point)
    ("C-c , v" . alchemist-mix-test-this-buffer)))

(provide 'init-elixir)
;;; init-elixir.el ends here
