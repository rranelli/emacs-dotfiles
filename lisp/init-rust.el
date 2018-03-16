;;; init-rust.el -- Configuration for working with rust code.
;;; Commentary:
;;; Code:
(use-package rust-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.toml" . conf-unix-mode))

  :bind
  (:map rust-mode-map
        ("TAB" . racer-complete-or-indent)
        ("M-." . racer-find-definition)))

(use-package racer
  :after rust-mode

  :custom
  (racer-rust-src-path "~/code/rust/src/")
  (racer-cmd "~/code/racer/target/release/racer")

  :hook
  (rust-mode . racer-activate))

(use-package flycheck-rust
  :after (flycheck rust-mode)

  :hook
  (flycheck-mode . flycheck-rust-setup))

(provide 'init-rust)
;;; init-rust.el ends here
