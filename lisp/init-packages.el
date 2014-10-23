;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("e6h" . "http://www.e6h.org/packages/"))

(package-refresh-contents)

(defvar my-packages
  '(;; general configuration
    magit
    git-timemachine
    flycheck
    graphviz-dot-mode
    ido-vertical-mode
    fuzzy
    s
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
    expand-region
    better-registers
    smex
    paredit
    idle-highlight-mode
    find-file-in-project
    bash-completion
    popup
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
    rubocop
    ;; other packages
    haskell-mode
    sml-mode
    wanderlust
    javadoc-lookup
    cider)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -- vendor packages --
(defvar libs-to-require
  '(cl
    better-registers
    uniquify
    linum
    paredit
    smex
    whitespace
    ffap
    find-file-in-project
    recentf
    saveplace
    ansi-color
    dired-x
    visual-basic-mode
    iso-transl
    dash
    flyspell
    markdown-mode
    ox-gfm
    s
    sgml-mode
    yaml-mode))

(dolist (lib libs-to-require)
  (require lib))

(defun rr-update-packages ()
  "Update installed Emacs packages."
  (interactive)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute t)
  (kill-buffer))

(provide 'init-packages)
;;; init-packages.el ends here
