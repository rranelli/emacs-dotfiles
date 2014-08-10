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
(ido-vertical-mode)

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
 ;; set initial mode
 initial-major-mode 'emacs-lisp-mode
 initial-scratch-message ";;This be scratch Buffer."
 ;; underline at next line
 x-underline-at-descent-line t
 suggest-key-bindings t
 column-number-mode t
 show-trailing-whitespace t
 ;; ignore case completion on emacs lisp and find files
 eshell-cmpl-ignore-case t
 pcomplete-ignore-case t
 ;; more itens to recentf
 recentf-max-saved-items 250
 ;; more memory. it's the distant future
 gc-cons-threshold 20000000
 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil)

(setq-default
 display-buffer-reuse-frames t
 abbrev-mode t
 fill-column 100
 ;; no more two spaces to end sentences. Jeez.
 sentence-end-double-space nil)

;; -- Hooks --
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'pretty-symbols-mode)

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
;;; init-defaults.el ends here
