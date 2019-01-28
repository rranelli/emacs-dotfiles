;;; init-elixir.el -- Configures utilities and nice-to-have features for Elixir development.
;;; Commentary:
;;; Code:
(use-package erlang
  :mode "\\.erl$")

(use-package elixir-mode
  :mode "\\.exs?"

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
                       ("not" . #x2757)
                       ("in" . #x2208)
                       ("not in" . #x2209)
                       ("fn" . #x1d6cc)
                       ("for" . ?∀)
                       ("raise" . ?🔥)
                       ("when" . #x2235)
                       ("do" . (?\s (Bl Bl 35 25) ?〯))
                       ("end" . ?·)

                       ;; messages
                       ("self" . (?𝔰 (Br . Bl)
                                     ?𝔢 (Br . Bl)
                                     ?𝔩 (Br . Bl)
                                     ?𝔣))
                       ("send" . (?𝔰 (Br . Bl)
                                     ?𝔢 (Br . Bl)
                                     ?𝔫 (Br . Bl)
                                     ?𝔡))
                       ;; ("send" . ?⟼)
                       ("receive" . (?𝔯 (Br . Bl)
                                        ?𝔢 (Br . Bl)
                                        ?𝔠 (Br . Bl)
                                        ?𝔢 (Br . Bl)
                                        ?𝔦 (Br . Bl)
                                        ?𝔳 (Br . Bl)
                                        ?𝔢))
                       ;; ("receive" . ?⟻)
                       ("pid" . (?𝔭 (Br . Bl)
                                    ?𝔦 (Br . Bl)
                                    ?𝔡))
                       ("after" . (?𝔞 (Br . Bl)
                                      ?𝔣 (Br . Bl)
                                      ?𝔱 (Br . Bl)
                                      ?𝔢 (Br . Bl)
                                      ?𝔯))

                       ;; Defs
                       ("def" . ?ℱ)
                       ("defp" . (?ℱ (Br Bl 50 0) ?➖))
                       ("defmodule" . ?ℳ)
                       ("defprotocol" . ?𝒫)
                       ("defimpl" . ?𝒥)
                       ("defexception" . ?ℰ)
                       ("defstruct" . ?𝑺)
                       ("defmacro" . ?𝛴)
                       ("defmacrop" . (?𝛴 (Br Bl 50 0) ?➖))

                       ;; modules
                       ("alias" . ?α)
                       ("import" . ?𝜾)
                       ("use" . ?μ)
                       ("require" . ?ρ)

                       ;; Base Types
                       ("true" . #x1d54b)
                       ("false" . #x1d53d)
                       ("nil" . #x2205)

                       ;; types
                       ("any" . #x2754)
                       ("integer" . #x2124)
                       ("float" . #x211d)))

  :bind
  (:map elixir-mode-map
        ("C-c C-s" . inferior-elixir)
        ("C-c i" . rr/mix-format))

  :hook
  ;; (elixir-mode . prettify-symbols-mode)
  (elixir-mode . rr/set-prettify-elixir-symbols)

  :config
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
  (:map alchemist-test-mode-map
        ("C-c , a" . nil)
        ("C-c , s" . nil)
        ("C-c , v" . nil)
        ("C-c , e" . nil))
  (:map alchemist-mode-map
        ("C-c , t" . alchemist-project-toggle-file-and-tests)
        ("C-c , y" . alchemist-project-toggle-file-and-tests-other-window)
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
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (setq alchemist-goto-elixir-source-dir (concat "/home/milhouse/.asdf/installs/elixir/" (shell-command-to-string "echo -n $(asdf current elixir | cut -d ' ' -f1)")))
              (setq alchemist-goto-erlang-source-dir (concat "/home/milhouse/.asdf/installs/erlang/"))))
  (add-hook 'elixir-mode-hook
            (lambda () (delete 'company-dabbrev company-backends)))

  (defadvice alchemist-project-root (around seancribbs/alchemist-project-root activate)
    (let ((alchemist-project-mix-project-indicator ".git"))
      ad-do-it))
  (defun seancribbs/activate-alchemist-root-advice ()
    "Activates advice to override alchemist's root-finding logic"
    (ad-activate 'alchemist-project-root))
  (defun rr/toggle-dialplan-update-fixture ()
    (interactive)
    (setenv "DIALPLAN_UPDATE_REGRESSION_FIXTURES"
            (if (equalp "true" (getenv "DIALPLAN_UPDATE_REGRESSION_FIXTURES"))
                "false"
              "true")))

  (add-to-list 'elixir-mode-hook 'seancribbs/activate-alchemist-root-advice))

(use-package exunit
  :after (alchemist)

  :bind
  (:map elixir-mode-map
        ("C-c , a" . exunit-verify-all)
        ("C-c , s" . exunit-verify-single)
        ("C-c , v" . exunit-verify)
        ("C-c , r" . exunit-rerun)))

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
