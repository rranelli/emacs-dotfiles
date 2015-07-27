;;; init-defaults.el -- Sets default global configurations and general Emacs behavior.
;;; Commentary:
;;; Code:

;; undisable upcase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; -- Mode preferences --
(winner-mode 1)
(blink-cursor-mode t)
(delete-selection-mode t)
(show-paren-mode 1)
(smartscan-mode t)
(global-undo-tree-mode)
(ido-mode t)
(ido-vertical-mode)
(global-auto-revert-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(smex-initialize)

;; safe variables
(add-to-list 'safe-local-variable-values '(encoding . utf-8))

;; add pretty symbols for lambdas and relationals
(setq pretty-symbol-categories '(lambda))

;; -- Variables --
(setq
 ;; please, share the clipboard
 x-select-enable-clipboard t
 uniquify-buffer-name-style 'forward
 ;; improve rendering performance
 redisplay-dont-pause t
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
 sentence-end-double-space nil
 ;; use firefox for browsing. Thanks.
 browse-url-default-browser 'browse-url-firefox
 )

;; more depth and sizes
(setq max-specpdl-size 10000
      max-lisp-eval-depth 20000)

;; -- tramp stuff --
(setq
 recentf-keep '(file-remote-p file-readable-p)
 recentf-auto-cleanup 'never
 tramp-persistency-file-name nil
 tramp-default-method "ssh")

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
 indent-tabs-mode nil
 save-place t
 fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)

;; -- more fontlock --
(defun custom-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|HACK\\|NOCOMMIT\\)"
	1 font-lock-warning-face t))))

;; -- Hooks --
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'pretty-symbols-mode)
(add-hook 'prog-mode-hook 'custom-add-watchwords)

(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)

(add-hook 'restclient-mode-hook 'custom-add-watchwords)

;; -- some automodes --
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.vbs$" . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.asp$" . visual-basic-mode))

(global-flycheck-mode t)

(provide 'init-defaults)
;;; init-defaults.el ends here
