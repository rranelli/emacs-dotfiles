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
    solarized-theme
    smartscan
    helm
    helm-ls-git
    yasnippet
    smooth-scrolling
    dirtree
    graphviz-dot-mode
    auto-complete
    ace-jump-mode
    ag
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

(provide 'init-packages)
;;; init-packages.el ends here
