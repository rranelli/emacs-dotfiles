;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(;; general configuration
    org
    magit
    git-timemachine
    flycheck
    starter-kit
    graphviz-dot-mode
    ido-vertical-mode
    fuzzy
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
    anzu
    diminish
    wrap-region
    markdown-mode+
    yaml-mode
    ;; themes
    solarized-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    gruvbox-theme
    ;; ruby packages
    inf-ruby
    rinari
    robe
    rspec-mode
    rhtml-mode
    ruby-refactor
    ruby-block
    ;; other packages
    haskell-mode
    sml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -- vendor packages --
(require 'better-registers)
(require 'uniquify)
(require 'linum)
(require 'dirtree)
(require 'whitespace)

;; -- Utility packages --
(require 'cl)
(require 'ido)
(require 'ffap)
(require 'recentf)
(require 'ansi-color)
(require 'dired-x)
(require 'paredit)
(require 'iso-transl) ;; makes dead-keys work

(provide 'init-packages)
;;; init-packages.el ends here
