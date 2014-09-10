;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'ruby-electric)
(require 'inf-ruby)
(require 'rspec-mode)
(require 'ac-robe)
(require 'robe)

(diminish 'ruby-electric-mode)
(diminish 'auto-fill-function)

;; auto modes
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\GuardFile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Godfile$" . ruby-mode))

;; hook auxiliary modes to ruby mode
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)

(add-hook 'robe-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-robe)))
(add-hook 'ruby-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))

;; fix for rspec and pry
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setenv "PAGER" (executable-find "cat"))

;; don't indent parenthesis in a weird way
(setq ruby-align-chained-calls nil)
(setq ruby-align-to-stmt-keywords t)
(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
;; this should be fixed, and smie would go t...
(setq ruby-use-smie nil)

;; do not add encoding automagically
(setq ruby-insert-encoding-magic-comment nil)

;; inf ruby stuff
(defun ruby-send-buffer ()
  "Send whole buffer to inferior process."
  (interactive)
  (ruby-send-region (point-min) (point-max)))

;; Fix annoying sole close paren. Thanks to Mr. DGutov. Not needed in emacs 24.4
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

;; -- keybindings --
(define-key rspec-mode-verifiable-keymap (kbd "y") 'rspec-spec-or-target-other-window-no-change-window)
(define-key rspec-mode-verifiable-keymap (kbd "u") 'rspec-find-spec-or-target-other-window)
(define-key rspec-mode-verifiable-keymap (kbd "e") 'rspec-find-spec-or-target-find-example-other-window)
(define-key rspec-mode-verifiable-keymap (kbd "w") 'rspec-toggle-spec-and-target-find-example)

(defvar ruby-mode-custom-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'ruby-send-buffer)
    (define-key map (kbd "r") 'ruby-send-region)
    (define-key map (kbd "v") 'ruby-refactor-extract-local-variable)
    (define-key map (kbd "m") 'ruby-refactor-extract-to-method)
    (define-key map (kbd "l") 'ruby-refactor-extract-to-let)
    map))
(define-key ruby-mode-map (kbd "C-c r") ruby-mode-custom-map)

(provide 'init-ruby)
;;; init-ruby.el ends here
