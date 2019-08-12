;;; init-defaults.el -- Sets default global configurations and general Emacs behavior.
;;; Commentary:
;;; Code:
(require 'iso-transl)

;; undisable upcase
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; -- Mode preferences --
(--each '(winner-mode
          blink-cursor-mode
          delete-selection-mode
          show-paren-mode
          global-undo-tree-mode
          ido-mode
          ido-vertical-mode
          volatile-highlights-mode
          global-wakatime-mode)
  (funcall it 1))

(--each '(global-auto-revert-mode
          menu-bar-mode
          tool-bar-mode
          electric-indent-mode
          scroll-bar-mode)
  (funcall it -1))

;; diminish stuff
(--each '(smartparens-mode
          projectile-mode
          yas-minor-mode
          company-mode
          undo-tree-mode
          eldoc-mode
          volatile-highlights-mode
          pretty-symbols-mode
          alchemist-mode)
  (funcall 'diminish it))

;; safe variables
(add-to-list 'safe-local-variable-values '(encoding . utf-8))

;; smooth scroling!
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position nil)

;; -- Variables --
(setq select-enable-clipboard t ;; please, share the clipboard
      uniquify-buffer-name-style 'forward
      ;; maybe this will fix the #filename auto-saved files
      auto-save-visited-file-name t
      ;; no backups & locks
      create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      backup-inhibited t
      ;; case insensitive sort lines
      sort-fold-case t
      ;; js mode offset
      js-indent-level 2
      ;; set initial mode
      initial-major-mode 'emacs-lisp-mode
      initial-scratch-message ";;This be scratch Buffer.\n"
      ;; underline at next line
      x-underline-at-descent-line t
      suggest-key-bindings t
      column-number-mode t
      show-trailing-whitespace t
      ;; ignore case completion on emacs lisp and find files
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
      ;; make mans open in the same window
      Man-notify-method 'pushy
      ;; make tab complete and indent
      tab-always-indent 'complete
      wakatime-api-key (shell-command-to-string "mimipass get wakatime/api-key"))

;; more depth and sizes
(setq max-specpdl-size 10000
      max-lisp-eval-depth 20000)

;; -- tramp stuff --
(setq recentf-keep '(file-remote-p file-readable-p)
      recentf-auto-cleanup 'never
      tramp-persistency-file-name nil
      tramp-default-method "ssh")

;; access remote machines with sudo via `C-x C-f /sudo:root@corp002.u:/etc/'
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '("\\.u\\'" "\\`root\\'" "/ssh:%h:"))

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-vertical-define-keys 'C-n-and-C-p-only)

(setq-default display-buffer-reuse-frames t
              indent-tabs-mode nil
              sh-basic-offset 2
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
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'custom-add-watchwords)

(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)
(add-hook 'html-mode-hook #'(lambda () (auto-fill-mode -1)))

;; -- some automodes --
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(provide 'init-defaults)
;;; init-defaults.el ends here
