;;; package -- Summary
;;; Commentary:
;;; Code:

;; -- Utility packages --
(eval-when-compile (require 'cl))
(require 'ido)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'linum)
(require 'whitespace)
(require 'dired-x)
(require 'dirtree)
(require 'iso-transl) ;; makes dead-keys work

;; -- Mode preferences --
(winner-mode t)
(ido-mode t)
(menu-bar-mode -1)
(blink-cursor-mode t)
(delete-selection-mode t)
(smartscan-mode t)
(global-flycheck-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; -- Variables --
(setq
 column-number-mode t
 save-abbrevs nil
 show-trailing-whitespace t
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 pcomplete-ignore-case t
 eshell-cmpl-ignore-case t
 suggest-key-bindings t
 ag-highlight-search t
 x-select-enable-clipboard t
 )

(setq-default
 display-buffer-reuse-frames t ;; reuses frames
 abbrev-mode t
 )

;; -- Hooks --
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'linum-mode)

;; -- Abbrev --
(define-abbrev-table 'global-abbrev-table
  '(
    ;; math/unicode symbols
    ("8bes" "bundle exec rspec")
    ("8be" "bundle exec")
    ("8gpl" "git pull")
    ("8rdbm" "bundle exec rake db:migrate db:rollback db:migrate")
    ))

;; -- some automodes --
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(provide 'init-utils)
;;; init-utils.el ends here
