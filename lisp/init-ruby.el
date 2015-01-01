;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'ruby-electric)
(require 'inf-ruby)
(require 'rspec-mode)
(require 'ac-robe)
(require 'robe)
(require 'rubocop)

(diminish 'ruby-electric-mode)
(diminish 'rubocop-mode)
(diminish 'auto-fill-function)

;; auto modes
(dolist (fp '("\\.rb$"
	      "\\.ru$"
	      "\\.jbuilder$"
	      "\\.gemspec$"
	      "\\GuardFile$"
	      "\\Rakefile$"
	      "\\Vagrantfile$"
	      "\\Gemfile$"
	      "\\Godfile$"
	      "\\Godfile$"
	      "\\.god$"))
  (add-to-list 'auto-mode-alist `(,fp . ruby-mode)))

;; hook auxiliary modes to ruby mode
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(add-hook 'ruby-mode-hook 'rubocop-mode)

(add-hook 'robe-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-robe)))
(add-hook 'ruby-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))

;; fix for rspec and pry
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setenv "PAGER" (executable-find "cat"))

(setq rspec-use-rake-when-possible nil)

;; -- GODAMMIT RUBY INDENTATION!!! --
;; don't indent parenthesis in a weird way
(setq ruby-align-chained-calls nil)
(setq ruby-align-to-stmt-keywords nil)
(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
(setq ruby-use-smie nil)

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
;; ------------------------------------

;; do not add encoding automagically
(setq ruby-insert-encoding-magic-comment nil)

;; Interactive ruby development
(defun ruby-send-buffer ()
  "Send whole buffer to inferior process."
  (interactive)
  (ruby-send-region (point-min) (point-max)))

;; ruby-electric playing nice with wrap region
(defadvice ruby-electric-quote (around first ()
                                       activate)
  "Make electric quote play nice with wrap region."
  (if (use-region-p)
      (wrap-region-trigger arg (string last-command-event))
    ad-do-it))

(defadvice ruby-electric-curlies (around first ()
                                         activate)
  "Make electric quote play nice with wrap region."
  (if (use-region-p)
      (wrap-region-trigger arg (string last-command-event))
    ad-do-it))

(defadvice ruby-electric-matching-char (around first ()
                                               activate)
  "Make electric quote play nice with wrap region."
  (if (use-region-p)
      (wrap-region-trigger arg (string last-command-event))
    ad-do-it))

;; -- Rspec stuff --
(defadvice rspec-compile
    (before rspec-save-before-compile (A-FILE-OR-DIR &optional opts) activate)
  "Save current buffer before running spec.  This remove the annoying save confirmation."
  (save-some-buffers (lambda () (string-match "\\.rb" (buffer-name  (current-buffer))))))

(defun rspec-spec-or-target-other-window-no-change-window ()
  "Just like rspec-find-spec-or-target-other-window but does not change the current window."
  (interactive)
  (rspec-find-spec-or-target-other-window)
  (other-window 1))

;; -- misc --
(defun rr-convert-to-ruby-1.9-hash-syntax ()
  (interactive)
  (save-excursion
    (re-search-backward ":")
    (delete-char 1)
    (forward-sexp)
    (insert ":")
    (re-search-forward "\s?=>")
    (replace-regexp "\s?=>" "" nil (line-beginning-position) (point))))

;; -- keybindings --
(dolist (map '(rspec-mode-keymap rspec-verifiable-mode-keymap))
  (define-bindings map
    '(("y" . rspec-spec-or-target-other-window-no-change-window)
      ("u" . rspec-find-spec-or-target-other-window)
      ("e" . rspec-find-spec-or-target-find-example-other-window)
      ("w" . rspec-toggle-spec-and-target-find-example))))

(define-bindings ruby-mode-map
  '(("C-c r b" . ruby-send-buffer)
    ("C-c r r" . ruby-send-region)
    ("C-c r v" . ruby-refactor-extract-local-variable)
    ("C-c r m" . ruby-refactor-extract-to-method)
    ("C-c r l" . ruby-refactor-extract-to-let)
    ("C-c r h" . rr-convert-to-ruby-1.9-hash-syntax)))

(provide 'init-ruby)
;;; init-ruby.el ends here
