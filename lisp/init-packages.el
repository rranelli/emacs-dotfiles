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
    ace-jump-mode
    ag
    alchemist
    anzu
    auto-package-update
    bash-completion
    better-registers
    cider
    color-theme-sanityinc-tomorrow
    company
    diminish
    dockerfile-mode
    elixir-mode
    expand-region
    find-file-in-project
    flycheck
    fuzzy
    git-timemachine
    graphviz-dot-mode
    gruvbox-theme
    haskell-mode
    helm
    helm-projectile
    hl-anything
    ido-vertical-mode
    inf-ruby
    javadoc-lookup
    langtool
    magit
    markdown-mode+
    maven-test-mode
    neotree
    nginx-mode
    popup
    pretty-symbols
    projectile
    rainbow-mode
    restclient
    rhtml-mode
    rinari
    robe
    rspec-mode
    rubocop
    ruby-refactor
    s
    smartparens
    smartscan
    smex
    sml-mode
    smooth-scrolling
    solarized-theme
    synosaurus
    undo-tree
    w3m
    wanderlust
    writegood-mode
    yaml-mode
    yasnippet
    zenburn-theme
    )
  "A list of packages to ensure are installed at launch.")

;; -- vendor packages --
(defvar libs-to-require
  '(
    ansi-color
    better-registers
    cl
    dash
    dired-x
    dpkg-dev-el
    ffap
    find-file-in-project
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
    yaml-mode
    yard-mode
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

(provide 'init-packages)
;;; init-packages.el ends here
