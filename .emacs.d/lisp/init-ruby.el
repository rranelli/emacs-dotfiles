;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'rinari-autoloads)
(require 'rhtml-mode)
(require 'ruby-electric)
(require 'robe)
(require 'ac-robe)

(add-hook 'rhtml-mode-hook 'rinari-launch)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\GuardFile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))

(autoload 'inf-ruby-keys "inf-ruby")

;; don't indent parenthesis in a weird way
(setq ruby-deep-indent-paren-style 'space)

;; do not add encoding automagically
(setq ruby-insert-encoding-magic-comment nil)

;; Fix annoying sole close paren. Thanks to Mr. DGutov
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  "Indent sole parenthesis in loca's way."
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; hook auxiliary modes to ruby mode
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'rinari-launch)
(add-hook 'ruby-mode-hook 'rspec-mode)

(add-hook 'robe-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-robe)))

(add-hook 'ruby-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))

;; -- rinari configuration --
(setq rinari-tags-file-name "TAGS")

;; -- keybindings --
(add-hook 'rspec-mode-hook (lambda () (define-key rspec-mode-map (kbd "C-c , y") 'rspec-find-spec-or-target-other-window)))

(provide 'init-ruby)
;;; init-ruby.el ends here
