;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)

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
    auto-complete
    ace-jump-mode
    pretty-symbols
    anzu
    diminish
    wrap-region
    markdown-mode+
    undo-tree
    yaml-mode
    neotree
    ;; themes
    solarized-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    gruvbox-theme
    zenburn-theme
    ;; ruby packages
    inf-ruby
    rinari
    robe
    rspec-mode
    rhtml-mode
    ruby-refactor
    ruby-block
    rubocop
    ;; other packages
    haskell-mode
    sml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -- vendor packages --
(defvar vendor-libs
  '(cl
    better-registers
    uniquify
    linum
    whitespace
    ffap
    recentf
    ansi-color
    dired-x
    paredit
    iso-transl
    yaml-mode))

(dolist (lib vendor-libs)
  (require lib))

(provide 'init-packages)
;;; init-packages.el ends here
