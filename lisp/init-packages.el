;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq save-abbrevs nil)

(defvar my-packages
  '(
    ag
    alchemist
    anzu
    auto-package-update
    back-button
    better-registers
    cider
    color-theme-sanityinc-tomorrow
    company
    company-c-headers
    dash
    dash-functional
    dockerfile-mode
    elixir-mode
    ensime
    erc-hl-nicks
    erc-image
    erc-view-log
    erlang
    expand-region
    flycheck
    flycheck-rust
    fuzzy
    gist
    git-timemachine
    graphviz-dot-mode
    gruvbox-theme
    haskell-mode
    helm
    helm-bundle-show
    helm-projectile
    highlight-symbol
    hl-anything
    ido-vertical-mode
    iedit
    inf-ruby
    javadoc-lookup
    js2-mode
    langtool
    magit
    markdown-mode+
    maven-test-mode
    neotree
    nginx-mode
    popup
    pretty-symbols
    projectile
    racer
    rainbow-mode
    realgud
    request
    restclient
    rhtml-mode
    rinari
    robe
    rspec-mode
    rubocop
    ruby-refactor
    rust-mode
    s
    scala-mode2
    smartparens
    smex
    sml-mode
    smooth-scrolling
    solarized-theme
    synosaurus
    telephone-line
    undo-tree
    volatile-highlights
    w3m
    wanderlust
    writegood-mode
    yaml-mode
    yard-mode
    yasnippet
    zenburn-theme
    )
  "A list of packages to ensure are installed at launch.")

;; -- vendor packages --
(defvar libs-to-require
  '(
    ansi-color
    cl
    dash
    iedit
    dash-functional
    dired-x
    dpkg-dev-el
    ffap
    flyspell
    hl-anything
    iso-transl
    linum
    markdown-mode
    nxml-mode
    ox-gfm
    recentf
    s
    saveplace
    sgml-mode
    sh-script
    smex
    uniquify
    visual-basic-mode
    volatile-highlights
    yaml-mode
    ))

;; package loading
(setq packaged-contents-refreshed-p nil)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case ex
	(package-install p)
      ('error (if packaged-contents-refreshed-p
		  (error ex)
		(package-refresh-contents)
		(setq packaged-contents-refreshed-p t)
		(package-install p))))))

;; vendor loading
(dolist (lib libs-to-require)
  (require lib))

;; Automagically updating packages
(require 'auto-package-update)
(auto-package-update-maybe)
(auto-package-update-at-time "05:00")

(provide 'init-packages)
;;; init-packages.el ends here
