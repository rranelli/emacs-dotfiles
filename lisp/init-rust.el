;;; init-rust.el -- Configuration for working with rust code.
;;; Commentary:
;;; Code:
(require 'rust-mode)

(add-to-list 'auto-mode-alist '("\\.toml" . conf-unix-mode))

;;
;;; Racer configuration
;;
(require 'racer)

(setq racer-rust-src-path "~/code/rust/src/")
(setq racer-cmd "~/code/racer/target/release/racer")
(add-hook 'rust-mode-hook #'racer-activate)

;;
;;; Keybindings
;;
(define-key rust-mode-map (kbd "TAB") #'racer-complete-or-indent)
(define-key rust-mode-map (kbd "M-.") #'racer-find-definition)

;;
;;; Hooks
;;
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
;;; init-rust.el ends here
