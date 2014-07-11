;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages
  '(
    ;; general configuration
    magit
    git-timemachine
    flycheck
    starter-kit
    starter-kit-bindings
    starter-kit-lisp
    graphviz-dot-mode
    ;; general editing tools
    ag
    smartscan
    helm
    helm-ls-git
    yasnippet
    smooth-scrolling
    dirtree
    auto-complete
    ace-jump-mode
    pretty-symbols
    ;; themes
    solarized-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    ;; ruby packages
    starter-kit-ruby
    inf-ruby
    rinari
    robe
    rspec-mode
    rhtml-mode
    ;; other packages
    haskell-mode
    sml-mode
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

(provide 'init-packages)
;;; init-packages.el ends here
