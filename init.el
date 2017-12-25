;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq use-package-always-ensure t)
(eval-when-compile
  (package-install 'use-package)
  (require 'use-package))

(require 'init-magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; legacy initialization                                          ;;
(setq rr/initialization-errors nil)                               ;;
(require 'init-bootstrap)                                         ;;
(rr/safe-load-init-files)                                         ;;
(message (if rr/initialization-errors                             ;;
             (mapconcat #'identity rr/initialization-errors "\n") ;;
           "All is sane, and init.el got to its end"))            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-commit-arguments
   (quote
    ("--no-verify" "--signoff" "--gpg-sign=8AB21633BDD12B22")))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only) t)
 '(magit-last-seen-setup-instructions "1.4.0" t)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(magit-push-always-verify nil t)
 '(magit-revision-show-gravatars (quote ("^Author:     " . "^Commit:     ")))
 '(magit-status-buffer-switch-function (quote switch-to-buffer) t)
 '(package-selected-packages
   (quote
    (magit zenburn-theme yard-mode yaml-mode web-mode wakatime-mode volatile-highlights use-package unicode-fonts undo-tree tern telephone-line synosaurus solarized-theme smex smartparens rust-mode ruby-refactor rubocop rspec-mode robe rhtml-mode restclient realgud rainbow-mode python-mode pretty-symbols org-journal omnisharp ob-elixir nginx-mode neotree maven-test-mode markdown-mode+ magit-gh-pulls langtool javadoc-lookup indium iedit ido-vertical-mode hl-anything highlight-symbol helm-projectile helm-emmet helm-bundle-show haskell-mode gruvbox-theme graphviz-dot-mode git-timemachine gist fuzzy flycheck-rust flycheck-mix expand-region es-mode erlang erc-view-log erc-image erc-hl-nicks ensime elpy dockerfile-mode diminish dash-functional company-c-headers company-ansible color-theme-sanityinc-tomorrow cider better-registers back-button avy auto-package-update anzu ansible-doc ansible alchemist ag)))
 '(safe-local-variable-values
   (quote
    ((ag-ignore-list "priv/static/**" "vendor/**" "node_modules/**")
     (encoding . utf-8))))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
