;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:
(use-package auto-package-update
  :config
  (auto-package-update-at-time "05:00"))

(defvar my-packages
  '(ag
    alchemist
    ansible
    ansible-doc
    anzu
    auto-package-update
    avy
    back-button
    better-registers
    cider
    color-theme-sanityinc-tomorrow
    company
    company-ansible
    company-c-headers
    csharp-mode
    dash
    diminish
    dash-functional
    ;; flycheck-credo
    ;; racer
    dockerfile-mode
    elixir-mode
    emmet-mode
    ensime
    erc-hl-nicks
    erc-image
    erc-view-log
    erlang
    es-mode
    expand-region
    flycheck
    flycheck-mix
    flycheck-rust
    fuzzy
    gist
    graphviz-dot-mode
    gruvbox-theme
    haskell-mode
    helm
    helm-bundle-show
    helm-emmet
    helm-projectile
    highlight-symbol
    hl-anything
    ido-vertical-mode
    iedit
    indium
    inf-ruby
    javadoc-lookup
    js2-mode
    langtool
    markdown-mode+
    maven-test-mode
    neotree
    nginx-mode
    ob-elixir
    omnisharp
    org-journal
    popup
    pretty-symbols
    projectile
    rainbow-mode
    realgud
    request
    restclient
    rhtml-mode
    robe
    rspec-mode
    rubocop
    ruby-refactor
    rust-mode
    s
    scala-mode
    smartparens
    smex
    solarized-theme
    synosaurus
    telephone-line
    unicode-fonts
    tern
    undo-tree
    volatile-highlights
    web-mode
    yaml-mode
    yard-mode
    yasnippet
    wakatime-mode
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

;; -- vendor packages --
(defvar libs-to-require
  '(ansi-color
    cl
    dash
    dash-functional
    dired-x
    dpkg-dev-el
    ffap
    flyspell
    hl-anything
    iedit
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
    volatile-highlights
    yaml-mode
    web-mode))

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

(provide 'init-packages)
;;; init-packages.el ends here
