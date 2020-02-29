;;; init-elixir.el -- Configures utilities and nice-to-have features for Elixir development.
;;; Commentary:
;;; Code:
(use-package erlang
  :mode "\\.erl$")

(use-package lsp-mode
  :custom
  (lsp-restart 'ignore)
  (lsp-auto-guess-root t)
  (lsp-response-timeout 5)
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :custom
  (lsp-ui-flycheck-enable t)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-file-watch-ignored (-concat '("\\.asdf" "[/\\\\]\\.elixir_ls$" "[/\\\\]deps$" "[/\\\\]_build$") lsp-file-watch-ignored))
  (lsp-ui-doc-delay 0)
  (lsp-ui-doc-include-signature t))

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-elixir
  :after (lsp-mode))

(use-package elixir-mode
  :mode ("\\.exs?" "mix.lock")

  :after (lsp-mode)

  :commands (rr/disable-elixir-pretty-symbols)

  :custom
  (rr/elixir-symbols '(;; Syntax
                       ("*" . ?⚹)
                       ("==" . #x2a75)
                       ("!=" . #x2260)
                       ("->" . #x27f6)
                       ("<-" . #x27f5)
                       ("<=" . #x2a7d)
                       (">=" . #x2a7e)
                       ("::" . #x2e2c)
                       ("<>" . (?≺ (Br Bl -24 0) ?≻))
                       ("<<" . (?≺ (Br Bl -45 0) ?≺ (Br Bl -100 0) ?\s))
                       (">>" . (?\s (Br Bl -100 0) ?≻ (Br Bl -45 0) ?≻))
                       ("++" . (?+ (Br Bl -35 0) ?+))
                       ("--" . (?- (Br Bl -33 0) ?-))
                       ("|>" . #x2b9a)
                       ;; ("not" . #x2757)
                       ;; ("in" . #x2208)
                       ;; ("not in" . #x2209)
                       ("fn" . #x1d6cc)
                       ;; ("for" . ?∀)
                       ;; ("raise" . ?🔥)
                       ;; ("when" . #x2235)
                       ;; ("do" . (?\s (Bl Bl 35 25) ?〯))
                       ;; ("end" . ?·)

                       ;; messages
                       ;; ("self" . (?𝔰 (Br . Bl)
                       ;;               ?𝔢 (Br . Bl)
                       ;;               ?𝔩 (Br . Bl)
                       ;;               ?𝔣))
                       ;; ("send" . (?𝔰 (Br . Bl)
                       ;;               ?𝔢 (Br . Bl)
                       ;;               ?𝔫 (Br . Bl)
                       ;;               ?𝔡))
                       ;; ("send" . ?⟼)
                       ;; ("receive" . (?𝔯 (Br . Bl)
                       ;;                  ?𝔢 (Br . Bl)
                       ;;                  ?𝔠 (Br . Bl)
                       ;;                  ?𝔢 (Br . Bl)
                       ;;                  ?𝔦 (Br . Bl)
                       ;;                  ?𝔳 (Br . Bl)
                       ;;                  ?𝔢))
                       ;; ("receive" . ?⟻)
                       ;; ("pid" . (?𝔭 (Br . Bl)
                       ;;              ?𝔦 (Br . Bl)
                       ;;              ?𝔡))
                       ;; ("after" . (?𝔞 (Br . Bl)
                       ;;                ?𝔣 (Br . Bl)
                       ;;                ?𝔱 (Br . Bl)
                       ;;                ?𝔢 (Br . Bl)
                       ;;                ?𝔯))

                       ;; ;; Defs
                       ;; ("def" . ?ℱ)
                       ;; ("defp" . (?ℱ (Br Bl 50 0) ?➖))
                       ;; ("defmodule" . ?ℳ)
                       ;; ("defprotocol" . ?𝒫)
                       ;; ("defimpl" . ?𝒥)
                       ;; ("defexception" . ?ℰ)
                       ;; ("defstruct" . ?𝑺)
                       ;; ("defmacro" . ?𝛴)
                       ;; ("defmacrop" . (?𝛴 (Br Bl 50 0) ?➖))

                       ;; ;; modules
                       ;; ("alias" . ?α)
                       ;; ("import" . ?𝜾)
                       ;; ("use" . ?μ)
                       ;; ("require" . ?ρ)

                       ;; Base Types
                       ;; ("true" . #x1d54b)
                       ;; ("false" . #x1d53d)
                       ;; ("nil" . #x2205)

                       ;; types
                       ;; ("any" . #x2754)
                       ;; ("integer" . #x2124)
                       ;; ("float" . #x211d)
                       ))
  :bind
  (:map elixir-mode-map
        ("C-c C-s" . inferior-elixir)
        ("C-c C-d" . lsp-ui-doc-show))

  :hook
  (elixir-mode . lsp)
  (elixir-mode . prettify-symbols-mode)
  (elixir-mode . rr/set-prettify-elixir-symbols)
  (elixir-mode . rr/register-elixir-ls-custom-settings)

  :config
  (set-face-attribute 'elixir-atom-face nil :foreground "dark cyan")

  (defcustom lsp-elixir-ls-language-server-enable-dialyzer "false"
    "Dialyzer analysis enabled"
    :group 'elixir-ls
    :risky t)

  (defun rr/enable-elixir-pretty-symbols ()
    (interactive)
    (add-hook 'elixir-mode-hook 'prettify-symbols-mode)
    (add-hook 'elixir-mode-hook 'rr/set-prettify-elixir-symbols)
    (revert-buffer))

  (defun rr/disable-elixir-pretty-symbols ()
    (interactive)
    (remove-hook 'elixir-mode-hook 'prettify-symbols-mode)
    (remove-hook 'elixir-mode-hook 'rr/set-prettify-elixir-symbols)
    (revert-buffer))

  (defun rr/set-prettify-elixir-symbols ()
    (setq prettify-symbols-alist rr/elixir-symbols))

  (defun rr/register-elixir-ls-custom-settings ()
    (lsp-register-custom-settings '(("dialyzerEnabled" lsp-elixir-ls-language-server-enable-dialyzer)))))

(use-package exunit
  :after (elixir-mode)

  :commands (rr/mix-format)

  :bind
  (:map elixir-mode-map
        ("C-c , a" . exunit-verify-all)
        ("C-c , A" . exunit-verify-all-in-umbrella)
        ("C-c , s" . exunit-verify-single)
        ("C-c , v" . exunit-verify)
        ("C-c , r" . exunit-rerun))
  (:map elixir-mode-map
        ("C-c i" . rr/mix-format))

  :config
  (defun rr/mix-format ()
    (interactive)
    (save-buffer)
    (shell-command (format "cd %s && mix format %s"
                           (or
                            (ignore-errors (exunit-umbrella-project-root))
                            (exunit-project-root))
                           (buffer-file-name)))
    (revert-buffer t t)))

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

(use-package ob-elixir)

(provide 'init-elixir)
;;; init-elixir.el ends here
