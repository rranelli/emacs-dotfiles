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
          (lambda () (setq-local default-directory (alchemist-project-root))))
(add-hook 'elixir-mode-hook
          (lambda () (setq-local default-directory (alchemist-project-root))))
(delete 'company-dabbrev company-backends) ;; fix issue with iex

;; pretty symbols
(setq pretty-symbol-patterns
      (append pretty-symbol-patterns
	      `((?⟶ lambda "->" (elixir-mode))
                (?⟵ lambda "<-" (elixir-mode))
		(?λ  lambda "\\<fn" (elixir-mode)))))

;; Flycheck!
;; (require 'flycheck-mix)
;; (flycheck-mix-setup)

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

(rr/toggle-env "MIX_TEST_SKIP_DB_SETUP")
(rr/toggle-env "MIX_ELIXIRC_NO_WALL")
(rr/toggle-env "lukla_elastic_search_enabled")

(defun rr/elixir-indent-buffer-no-docs (&rest start)
  "Indent buffer from START onward, but skip @doc and @moduledoc strings."
  (interactive)
  (save-excursion
    (let ((begin (or (car start)
                     (goto-char (point-min))))
          (end   (or (re-search-forward (rx (or "@doc" "@moduledoc")) nil t)
                     (point-max))))

      (indent-region begin end)

      (unless (= end (point-max))
        (sp-forward-sexp)
        (next-line)
        (rr/elixir-indent-buffer-no-docs (point))))))

;;
;;; bindings
;;
(rr/define-bindings elixir-mode-map
                    '(("M-q" . elixir-mode-fill-doc-string)
                      ("C-x C-f" . (lambda ()
                                     (interactive)
                                     (-> (buffer-file-name)
                                         (file-name-directory)
                                         (helm-find-files-1))))
                      ("C-c i" . rr/elixir-indent-buffer-no-docs)))

(rr/define-bindings alchemist-mode-map
                    '(("C-c , t" . alchemist-project-toggle-file-and-tests)
                      ("C-c , y" . alchemist-project-toggle-file-and-tests-other-window)
                      ("C-c , a" . alchemist-mix-test)
                      ("C-c , s" . alchemist-mix-test-at-point)
                      ("C-c , v" . alchemist-project-run-tests-for-current-file)
                      ("C-c , r" . alchemist-mix-rerun-last-test)
                      ("C-c , c" . alchemist-mix-compile)
                      ("C-c , S" . rr/iex-pry)
                      ("C-c r p" . rr/elixir-to-pipe)
                      ("C-c ?" . alchemist-help-search-at-point)))

(define-key alchemist-test-report-mode-map (kbd "T")
  '(lambda () (interactive) (toggle-truncate-lines)))
(define-key alchemist-test-report-mode-map (kbd "g") 'alchemist-mix-rerun-last-test)
(define-key elixir-mode-map (kbd "C-c C-s") 'inferior-elixir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO FIXME: crazy hot fix of alchemist-project-root                          ;;
(defun alchemist-project-root (&optional dir)                                   ;;
  (let ((start-dir (or dir (expand-file-name default-directory))))              ;;
    (or                                                                         ;;
     (locate-dominating-file start-dir alchemist-project-mix-project-indicator) ;;
     (locate-dominating-file start-dir alchemist-project-hex-pkg-indicator))))  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-elixir)
;;; init-elixir.el ends here
