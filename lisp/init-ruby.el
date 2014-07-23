;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'ruby-electric)
(require 'inf-ruby)
(require 'rspec-mode)
(require 'ac-robe)
(require 'robe)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\GuardFile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))

;; don't indent parenthesis in a weird way
(setq ruby-deep-indent-paren-style nil)

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

(defadvice rspec-compile
  (before rspec-save-before-compile (A-FILE-OR-DIR &optional opts) activate)
  "Save current buffer before running spec.  This remove the annoying save confirmation."
  (save-some-buffers (lambda () (string-match "\\.rb" (buffer-name  (current-buffer))))))

(defun ruby-send-buffer ()
  "Send whole buffer to inferior process."
  (interactive)
  (ruby-send-region (point-min) (point-max)))

;; hook auxiliary modes to ruby mode
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)

(add-hook 'robe-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-robe)))

(add-hook 'ruby-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-yasnippet)))

;; -- rinari configuration --
(setq rinari-tags-file-name "TAGS")

;; -- keybindings --
(define-key rspec-mode-map (kbd "C-c , y") 'rspec-find-spec-or-target-other-window)

(define-key ruby-mode-map (kbd "C-c s b") 'ruby-send-buffer)
(define-key ruby-mode-map (kbd "C-c s r") 'ruby-send-region)

(define-key ruby-mode-map (kbd "C-c s v") 'ruby-refactor-extract-local-variable)
(define-key ruby-mode-map (kbd "C-c s m") 'ruby-refactor-extract-to-method)
(define-key ruby-mode-map (kbd "C-c s l") 'ruby-refactor-extract-to-let)

(provide 'init-ruby)
;;; init-ruby.el ends here
