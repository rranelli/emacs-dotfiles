;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'ruby-electric)
(require 'inf-ruby)
(require 'rspec-mode)
(require 'ac-robe)
(require 'robe)
(require 'ruby-block)

(diminish 'ruby-electric-mode)
(diminish 'auto-fill-function)
;;(add-hook 'ruby-mode-hook (lambda () (diminish 'ruby-block-mode)))
(diminish 'ruby-block-mode)

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

;; toggle ruby-block highlight to both keyword and line
(ruby-block-mode t)
(setq ruby-block-highlight-toggle 'overlay)
(setq ruby-block-highlight-face 'show-paren-match)

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

(defun rspec-spec-or-target-other-window-no-change-window ()
  "Just like rspec-find-spec-or-target-other-window but does not change the current window."
  (interactive)
  (rspec-find-spec-or-target-other-window)
  (other-window 1))

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
(define-key rspec-mode-map (kbd "C-c , y") 'rspec-spec-or-target-other-window-no-change-window)
(define-key rspec-mode-map (kbd "C-c , u") 'rspec-find-spec-or-target-other-window)

(define-key ruby-mode-map (kbd "C-c r b") 'ruby-send-buffer)
(define-key ruby-mode-map (kbd "C-c r r") 'ruby-send-region)

(define-key ruby-mode-map (kbd "C-c r v") 'ruby-refactor-extract-local-variable)
(define-key ruby-mode-map (kbd "C-c r m") 'ruby-refactor-extract-to-method)
(define-key ruby-mode-map (kbd "C-c r l") 'ruby-refactor-extract-to-let)

(provide 'init-ruby)
;;; init-ruby.el ends here
