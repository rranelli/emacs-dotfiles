;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; kill sexp
(define-key sp-keymap (kbd "C-k") 'sp-kill-hybrid-sexp)

;;
;;; navigation
;;
(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;;
;;; barf and slurp
;;
(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;;
;;; splicing
;;
(define-key sp-keymap (kbd "M-s") 'sp-splice-sexp)
(define-key sp-keymap (kbd "M-r") 'sp-splice-sexp-killing-around)

;;
;;; pairs
;;
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "/" "/" "/")
  (sp-local-tag "_" "_" "_")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-with-modes '(org-mode)
  (sp-local-pair "/" "/")
  (sp-local-pair "_" "_")
  (sp-local-pair "*" "*")
  (sp-local-pair "=" "="))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-pair "$" "$")
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

(provide 'init-smartparens)
;;; init-smartparens.el ends here
