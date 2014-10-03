;;; package -- Summary
;;; Commentary:
;;; Code:

;; -- Mode preferences --
(winner-mode 1)
(blink-cursor-mode t)
(delete-selection-mode t)
(show-paren-mode 1)
(smartscan-mode t)
(global-flycheck-mode t)
(global-undo-tree-mode)
(ido-mode t)
(ido-vertical-mode)
(global-auto-revert-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(smex-initialize)

;; add pretty symbols for lambdas and relationals
(setq pretty-symbol-categories '(lambda nil))
(add-to-list 'pretty-symbol-patterns
	     `(?λ lambda "\\<fn\\>" (clojure-mode)))

;; -- Wrap Region --
(wrap-region-global-mode)

(wrap-region-add-wrapper "`" "`")
(wrap-region-add-wrapper "=" "=")
(wrap-region-add-wrapper "*" "*")
(wrap-region-add-wrapper "/" "/")
(wrap-region-add-wrapper "_" "_")

;; -- Variables --
(setq
 ;; please, share the clipboard
 x-select-enable-clipboard t
 save-abbrevs nil
 uniquify-buffer-name-style 'forward
 ;; no backups
 make-backup-files nil
 auto-save-default nil
 backup-inhibited t
 ;; set initial mode
 initial-major-mode 'emacs-lisp-mode
 initial-scratch-message ";;This be scratch Buffer.\n"
 ;; underline at next line
 x-underline-at-descent-line t
 suggest-key-bindings t
 column-number-mode t
 show-trailing-whitespace t
 indent-tabs-mode nil
 ;; ignore case completion on emacs lisp and find files
 eshell-cmpl-ignore-case t
 pcomplete-ignore-case t
 ;; more itens to recentf
 recentf-max-saved-items 250
 ;; more memory. it's the distant future
 gc-cons-threshold 20000000
 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil
 visible-bell t
 save-place t
 whitespace-style '(face trailing lines-tail tabs)
 whitespace-line-column 80
 ;; no more two spaces to end sentences. Jeez.
 sentence-end-double-space nil)

(setq
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-auto-merge-work-directories-length nil
 ido-create-new-buffer 'always
 ido-use-filename-at-point 'guess
 ido-use-virtual-buffers t
 ido-handle-duplicate-virtual-buffers 2
 ido-max-prospects 10)

(setq-default
 display-buffer-reuse-frames t
 abbrev-mode t
 fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; -- Hooks --
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'pretty-symbols-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'custom-add-watchwords)

(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)

;; -- Abbrev --
(define-abbrev-table 'global-abbrev-table
  '(("8bes" "bundle exec rspec")
    ("8be" "bundle exec")
    ("8rdbm" "bundle exec rake db:migrate db:rollback && bundle exec rake db:migrate")
    ("8bejs" "bundle exec jekyll serve --watch")))

;; -- more fontlock --
(defun custom-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
	  1 font-lock-warning-face t))))

;; -- some automodes --
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; diminish
(diminish 'undo-tree-mode)
(diminish 'abbrev-mode)

(provide 'init-defaults)
;;; init-defaults.el ends here
