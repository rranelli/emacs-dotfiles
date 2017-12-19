;;; Package --- Summary
;;; Commentary:
;;; Code:

;; (package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq rr/initialization-errors nil)

(require 'init-bootstrap)
(rr/safe-load-init-files)

;; Finish!
(message "======================================")
(message (if rr/initialization-errors
             (mapconcat #'identity rr/initialization-errors "\n")
           "All is sane, and init.el got to its end"))
(message "======================================")
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-commit-arguments
   (quote
    ("--no-verify" "--signoff" "--gpg-sign=8AB21633BDD12B22")))
 '(package-selected-packages
   (quote
    (zenburn-theme yard-mode yaml-mode web-mode volatile-highlights undo-tree tern telephone-line synosaurus solarized-theme smex smartparens rust-mode ruby-refactor rubocop rspec-mode robe rhtml-mode restclient realgud rainbow-mode python-mode pretty-symbols omnisharp org-journal ob-elixir nginx-mode neotree maven-test-mode markdown-mode+ magit-gh-pulls magit langtool javadoc-lookup inf-ruby indium iedit ido-vertical-mode hl-anything highlight-symbol helm-projectile helm-emmet helm-bundle-show helm haskell-mode gruvbox-theme graphviz-dot-mode git-timemachine gist fuzzy flycheck-rust flycheck-mix flycheck expand-region es-mode erlang erc-view-log erc-image erc-hl-nicks ensime emmet-mode elpy dockerfile-mode dash-functional diminish csharp-mode company-c-headers company-ansible color-theme-sanityinc-tomorrow cider better-registers back-button avy auto-package-update anzu ansible-doc ansible alchemist ag)))
 '(safe-local-variable-values
   (quote
    ((ag-ignore-list "priv/static/**" "vendor/**" "node_modules/**")
     (encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
