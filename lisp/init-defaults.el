;;; package -- Summary
;;; Commentary:
;;; Code:

(diminish 'abbrev-mode)

;; -- Mode preferences --
(winner-mode 1)
(ido-mode t)
(menu-bar-mode -1)
(blink-cursor-mode t)
(delete-selection-mode t)
(smartscan-mode t)
(global-flycheck-mode t)
(wrap-region-global-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq pretty-symbol-categories '(lambda relational))

;; -- Variables --
(setq
 ;; please, share the clipboard
 x-select-enable-clipboard t
 save-abbrevs nil
 ;; no backups
 make-backup-files nil
 auto-save-default nil
 backup-inhibited t
 suggest-key-bindings t
 column-number-mode t
 show-trailing-whitespace t
 ;; ignore case completion on emacs lisp and find files
 eshell-cmpl-ignore-case t
 pcomplete-ignore-case t
 ;; highlight ag matches
 ag-highlight-search t
 fill-column 80
 ;; more itens to recentf
 recentf-max-saved-items 250
 ;; more memory. it's the distant future
 gc-cons-threshold 20000000
 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil)

(setq-default
 display-buffer-reuse-frames t
 abbrev-mode t
 ;; no more two spaces to end sentences. Jeez.
 sentence-end-double-space nil)

;; -- Hooks --
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'pretty-symbols-mode)

;; nice paren-style highlight, but with buffer local configuration ;)
(defun expression-style-show-paren ()
  "make show-paren expression only for lisp modes"
  (make-variable-buffer-local 'show-paren-style)
  (setq show-paren-style 'expression))
(add-hook 'emacs-lisp-mode-hook 'expression-style-show-paren)

;; make cursor type a bar
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

(defadvice shell (after do-not-query-shell-exit
                        first (&optional buffer)
                        activate)
  "Do not query exit confirmation for shell process buffer."
  (interactive)
  (set-process-query-on-exit-flag (get-process "shell") nil))

;; -- Abbrev --
(define-abbrev-table 'global-abbrev-table
  '(
    ;; math/unicode symbols
    ("8bes" "bundle exec rspec")
    ("8be" "bundle exec")
    ("8rdbm" "bundle exec rake db:migrate db:rollback && bundle exec rake db:migrate")
    ("8bejs" "bundle exec jekyll serve --watch")
    ))

;; -- some automodes --
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(provide 'init-defaults)
;;; init-utils.el ends here
