;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:
(use-package auto-package-update
  :config
  (auto-package-update-at-time "05:00"))

(use-package ag)
(use-package alchemist)
(use-package anzu)
(use-package auto-package-update)
(use-package avy)
(use-package back-button)
(use-package better-registers)
(use-package cider)
(use-package company)
(use-package company-c-headers)
(use-package csharp-mode)
(use-package dash)
(use-package diminish)
(use-package dash-functional)
(use-package dockerfile-mode)
(use-package elixir-mode)
(use-package emmet-mode)
(use-package ensime)
(use-package erc-hl-nicks)
(use-package erc-image)
(use-package erc-view-log)
(use-package erlang)
(use-package es-mode)
(use-package expand-region)
(use-package flycheck)
(use-package flycheck-mix)
(use-package flycheck-rust)
(use-package fuzzy)
(use-package gist)
(use-package graphviz-dot-mode)
(use-package haskell-mode)
(use-package helm)
(use-package helm-bundle-show)
(use-package helm-emmet)
(use-package helm-projectile)
(use-package highlight-symbol)
(use-package hl-anything)
(use-package ido-vertical-mode)
(use-package iedit)
(use-package indium)
(use-package inf-ruby)
(use-package javadoc-lookup)
(use-package js2-mode)
(use-package langtool)
(use-package markdown-mode+)
(use-package maven-test-mode)
(use-package neotree)
(use-package nginx-mode)
(use-package ob-elixir)
(use-package omnisharp)
(use-package org-journal)
(use-package popup)
(use-package pretty-symbols)
(use-package projectile)
(use-package rainbow-mode)
(use-package realgud)
(use-package request)
(use-package restclient)
(use-package rhtml-mode)
(use-package robe)
(use-package rspec-mode)
(use-package rubocop)
(use-package ruby-refactor)
(use-package rust-mode)
(use-package s)
(use-package scala-mode)
(use-package smartparens)
(use-package smex)
(use-package synosaurus)
(use-package telephone-line)
(use-package unicode-fonts)
(use-package tern)
(use-package undo-tree)
(use-package volatile-highlights)
(use-package web-mode)
(use-package yaml-mode)
(use-package yard-mode)
(use-package yasnippet)
(use-package wakatime-mode)

(use-package color-theme-sanityinc-tomorrow
  :defer t)
(use-package solarized-theme
  :defer t)
(use-package zenburn-theme
  :defer t)
(use-package gruvbox-theme
  :defer t)

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
    uniquify
    volatile-highlights
    web-mode))

;; vendor loading
(dolist (lib libs-to-require)
  (require lib))

(provide 'init-packages)
;;; init-packages.el ends here
