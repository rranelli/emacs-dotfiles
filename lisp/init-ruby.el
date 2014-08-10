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

;; -- keybindings --
(define-key rspec-mode-verifiable-keymap (kbd "y") 'rspec-spec-or-target-other-window-no-change-window)
(define-key rspec-mode-verifiable-keymap (kbd "u") 'rspec-find-spec-or-target-other-window)
(define-key rspec-mode-verifiable-keymap (kbd "e") 'rspec-find-spec-or-target-find-example-other-window)
(define-key rspec-mode-verifiable-keymap (kbd "w") 'rspec-toggle-spec-and-target-find-example)

(define-key ruby-mode-map (kbd "C-c r b") 'ruby-send-buffer)
(define-key ruby-mode-map (kbd "C-c r r") 'ruby-send-region)

(define-key ruby-mode-map (kbd "C-c r v") 'ruby-refactor-extract-local-variable)
(define-key ruby-mode-map (kbd "C-c r m") 'ruby-refactor-extract-to-method)
(define-key ruby-mode-map (kbd "C-c r l") 'ruby-refactor-extract-to-let)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following stuff will probably be merged into rspec-mode
(defun rspec--toggle-spec-and-target-find-method (toggle-function)
  (defun get-method-name ()
    (save-excursion
      (end-of-line)
      (search-backward-regexp "\\(?:describe ['\"][#\\.]\\(.*\\)['\"] do\\|def \\(?:self\\)?\\(.*\\)[ (\\$]\\)")
      (if (match-string 1)
          (match-string 1)
        (match-string 2))))

  (condition-case ex
      (let ((target-regexp (if (rspec-buffer-is-spec-p)
                               (format "def \\(self\\)?\\.?%s" (get-method-name))
                             (format "describe ['\"]#?%s['\"]" (get-method-name)))))
        (funcall toggle-function)
        (if (string-match-p target-regexp (buffer-string))
            (progn
              (beginning-of-buffer)
              (search-forward-regexp target-regexp))))
    ('error (message "No method/spec definition before point"))))

(defun rspec-toggle-spec-and-target-find-example ()
  "Just like rspec-toggle-spec-and-target but tries to toggle between
the specific example and method given the current point."
  (interactive)
  (rspec--toggle-spec-and-target-find-method 'rspec-toggle-spec-and-target))

(defun rspec-find-spec-or-target-find-example-other-window ()
  "Finds in the other window the spec or the target file, and tries
  to find the corresponding example or method given the current
  point."
  (interactive)
  (rspec--toggle-spec-and-target-find-method 'rspec-find-spec-or-target-other-window))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-ruby)
;;; init-ruby.el ends here
