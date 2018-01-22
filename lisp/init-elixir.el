;;; init-elixir.el -- Configures utilities and nice-to-have features for Elixir development.
;;; Commentary:
;;; Code:
(use-package elixir-mode
  :mode "\\.exs?"

  :custom
  (rr/elixir-symbols '(;; Syntax
                       ("==" .       #x2a75)
                       ("!=" .       #x2260)
                       ("->" .       #x27f6)
                       ("<-" .       #x27f5)
                       ("<=" .       #x2a7d)
                       (">=" .       #x2a7e)
                       ("::" .       #x2e2c)
                       ("|>" .       #x2b9a)
                       ("not" .      #x2757)
                       ("in" .       #x2208)
                       ("not in" .   #x2209)
                       ("fn" .       #x1d6cc)
                       ("for" .      #x2200)
                       ("raise" .    #x1f4a3)
                       ("when" . #x2235)
                       ("end" . #x26ac)
                       ;; definitions
                       ("def" .      #x2131)
                       ("defp" .     #x1d4ab)
                       ("defmodule" . #x1d4dc)
                       ("self" .     #x3f0)
                       ("alias" .     #x2abc)
                       ("import" .   #x2abb)
                       ;; Base Types
                       ("true" .     #x1d54b)
                       ("false" .    #x1d53d)
                       ("nil" .     #x2205)
                       ;; types
                       ("any" .      #x2754)))

  :bind
  (:map elixir-mode-map
        ("C-c C-s" . inferior-elixir)
        ("M-q" . elixir-mode-fill-doc-string)
        ("C-x C-f" . (lambda ()
                       (interactive)
                       (-> (buffer-file-name)
                           (file-name-directory)
                           (helm-find-files-1))))
        ("C-c i" . rr/mix-format))

  :hook
  (elixir-mode . prettify-symbols-mode)
  (elixir-mode . rr/set-prettify-elixir-symbols)

  :config
  (defun rr/set-prettify-elixir-symbols ()
    (setq prettify-symbols-alist rr/elixir-symbols))

  :config
  (defun rr/set-mix-env ()
    (interactive)
    (setenv "MIX_ENV" (read-string "MIX_ENV= " "dev")))
  (defun rr/mix-format ()
    (interactive)
    (save-buffer)
    (shell-command (format "mix format %s" (buffer-file-name)))
    (revert-buffer t t)))

(use-package alchemist
  :custom
  (alchemist-test-status-modeline nil)

  :hook
  (elixir-mode . alchemist-mode)

  :bind
  (:map alchemist-mode-map
        ("C-c , t" . alchemist-project-toggle-file-and-tests)
        ("C-c , y" . alchemist-project-toggle-file-and-tests-other-window)
        ("C-c , a" . alchemist-mix-test)
        ("C-c , s" . alchemist-mix-test-at-point)
        ("C-c , v" . alchemist-project-run-tests-for-current-file)
        ("C-c , r" . alchemist-mix-rerun-last-test)
        ("C-c , c" . alchemist-mix-compile)
        ("C-c , S" . rr/iex-pry)
        ("C-c r p" . rr/elixir-to-pipe)
        ("C-c C-d" . alchemist-help-search-at-point))
  (:map alchemist-test-report-mode-map
        ("T" . toggle-truncate-lines)
        ("g" . alchemist-mix-rerun-last-test))

  :config
  (add-hook 'alchemist-test-report-mode-hook
            (lambda () (setq-local default-directory (alchemist-project-root))))
  (add-hook 'elixir-mode-hook
            (lambda () (setq-local default-directory (alchemist-project-root))))
  (add-hook 'elixir-mde-hook
            (lambda () (delete 'company-dabbrev company-backends))))

(use-package flycheck-credo
  :after (flycheck elixir-mode)

  :custom
  (flycheck-elixir-credo-strict t)

  :hook
  (elixir-mode . flycheck-credo-setup))

(use-package flycheck-mix
  :after (flycheck elixir-mode)

  :hook
  (elixir-mode . flycheck-mix-setup))

;; (use-package ob-elixir)

(provide 'init-elixir)
;;; init-elixir.el ends here
