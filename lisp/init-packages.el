;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:
(use-package auto-package-update
  :config
  (auto-package-update-at-time "05:00"))

(use-package ag)
(use-package avy)
(use-package company)
(use-package dash)
(use-package dash-functional)
(use-package diminish)
(use-package dired-x :ensure nil)
(use-package dockerfile-mode
  :mode ("^Dockerfile$"
         "\\.dockerfile$"))
(use-package es-mode :defer t)
(use-package expand-region)
(use-package gist)
(use-package graphviz-dot-mode)
(use-package highlight-symbol)
;; (use-package hl-anything)
(use-package ido-vertical-mode)
(use-package iedit)
(use-package markdown-mode
  :mode ("\\.md$" "\\.markdown$")
  :config
  (rr/expose-default-bindings markdown-mode-map))
(use-package neotree)
(use-package nginx-mode :defer t)
(use-package pretty-symbols)
(use-package projectile)
(use-package rainbow-mode)
(use-package s)
(use-package smartparens)
(use-package unicode-fonts)
(use-package undo-tree)
(use-package volatile-highlights)
(use-package yaml-mode)
(use-package yasnippet)
(use-package wakatime-mode :diminish t)

(provide 'init-packages)
;;; init-packages.el ends here
