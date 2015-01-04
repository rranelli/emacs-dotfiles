;;; package -- Summary
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
    anzu
    auto-complete
    auto-package-update
    bash-completion
    better-registers
    cider
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow
    diminish
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
    idle-highlight-mode
    ido-vertical-mode
    inf-ruby
    javadoc-lookup
    magit
    markdown-mode+
    maven-test-mode
    neotree
    nginx-mode
    paredit
    popup
    pretty-symbols
    projectile
    restclient
    rhtml-mode
    rinari
    robe
    rspec-mode
    rubocop
    ruby-refactor
    s
    smartscan
    smex
    sml-mode
    smooth-scrolling
    solarized-theme
    undo-tree
    w3m
    wanderlust
    wrap-region
    yaml-mode
    yasnippet
    zenburn-theme
    )
  "A list of packages to ensure are installed at launch.")

;; -- vendor packages --
(defvar libs-to-require
  '(cl
    better-registers
    uniquify
    linum
    paredit
    smex
    whitespace
    wrap-region
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
    hl-anything
    sh-script
    sgml-mode
    nxml-mode
    yaml-mode))

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
