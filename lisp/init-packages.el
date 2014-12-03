;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)
(require 'auto-package-update)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("e6h" . "http://www.e6h.org/packages/"))

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
    projectile
    helm-projectile
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
    hl-anything
    smex
    paredit
    idle-highlight-mode
    find-file-in-project
    bash-completion
    popup
    nginx-mode
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
    w3m
    javadoc-lookup
    maven-test-mode
    restclient
    cider)
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

;; Automagically updateing packages
(rr-update-packages-if-needed)

(provide 'init-packages)
;;; init-packages.el ends here
