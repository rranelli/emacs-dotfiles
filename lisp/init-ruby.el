;;; init-ruby.el -- Configures nice-to-have features for Ruby development.
;;; Commentary:
;;; Code:
(use-package ruby-mode
  :ensure nil
  :mode ("\\.rb$"
         "\\.ru$"
         "\\.rake"
         "\\.jbuilder$"
         "\\.gemspec$"
         "\\GuardFile$"
         "\\Rakefile$"
         "\\Vagrantfile$"
         "\\Gemfile$"
         "\\Godfile$"
         "\\.god$")

  :custom
  (ruby-align-chained-calls nil)
  (ruby-align-to-stmt-keywords nil)
  (ruby-deep-indent-paren nil)
  (ruby-deep-indent-paren-style nil)
  (ruby-use-smie t)
  (ruby-insert-encoding-magic-comment nil)

  :bind
  (:map ruby-mode-map
        ("C-c r b" . ruby-send-buffer)
        ("C-c r r" . ruby-send-region)
        ("C-c r v" . ruby-refactor-extract-local-variable)
        ("C-c r m" . ruby-refactor-extract-to-method)
        ("C-c r h" . rr/convert-to-ruby-1.9-hash-syntax)
        ("C-c r s" . rr/split-module-nesting)
        ("C-c r l" . rr/wrap-in-stabby-lambda)
        ("#" . rr/ruby-interpolate))

  :config
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

  (defun rr/convert-to-ruby-1.9-hash-syntax ()
    (interactive)
    (save-excursion
      (re-search-backward ":")
      (delete-char 1)
      (forward-sexp)
      (insert ":")
      (re-search-forward "\s?=>")
      (replace-regexp "\s?=>" "" nil (line-beginning-position) (point))))

  (defun rr/split-module-nesting ()
    (interactive)
    (save-excursion
      (when (re-search-forward "\\(class\\|module\\|describe\\).*::" nil t)
        (backward-delete-char 2)
        (set-mark (point))
        (backward-sexp)
        (kill-region (point) (mark))
        (beginning-of-buffer)
        (insert "module ")
        (yank)
        (insert "\n")
        (end-of-buffer)
        (insert "end\n")
        (indent-region (point-min) (point-max)))))

  (defun rr/wrap-in-stabby-lambda (begin end)
    (interactive "r")
    (goto-char end)
    (insert " }")
    (goto-char begin)
    (insert "-> { "))

  (defun rr/ruby-interpolate ()
    "In a double quoted string, interpolate."
    (interactive)
    (insert "#")
    (when (and
           (looking-back "\".*")
           (looking-at ".*\""))
      (insert "{}")
      (backward-char 1)))

  (defun rr/initialize-instance-vars (text)
    (interactive)
    (->> text
         (rr/extract-arg-names-from-declaration)
         (-map (lambda (x) (format "@%s = %s" x x)))
         (s-join "\n")))

  (defun rr/initialize-readers (text)
    (interactive)
    (let ((args (->> text
                     (rr/extract-arg-names-from-declaration)
                     (-map (lambda (x) (format ":%s" x)))
                     (s-join ", "))))
      (unless (string-empty-p args)
        (s-concat "attr_reader " args))))

  (defun rr/extract-arg-names-from-declaration (text)
    (->> (s-split "\\(=[^,]?+\\|[, *]\\)" text t)
         (-map 's-trim)))

  (defun rr/pry-byebug-jump-to-source ()
    "Jumps to source location given debugger output"
    (interactive)
    (delete-other-windows)
    (when (save-excursion (search-backward-regexp "From: \\(.*\.rb\\) @ line \\([0-9]+\\)")))
    (let ((file (match-string 1))
          (line (string-to-int (match-string 2))))
      (find-file-other-window file)
      (goto-line line)))

  (defun rr/insert-requires ()
    "Require all internal classes/modules of the current file."
    (interactive)
    (let* ((fbase (file-name-base (file-name-sans-extension (buffer-file-name))))
           (fdir (file-name-directory (buffer-file-name)))
           (dirz (expand-file-name fbase fdir))
           (files (directory-files dirz nil ".rb$"))
           (require-text (->> files
                              (-map (lambda (name) (replace-regexp-in-string ".rb$" "" name)))
                              (-map (lambda (name) (format "require_relative '%s/%s'" fbase name))))))
      (insert (s-join "\n" require-text)))))

(use-package rspec-mode
  :custom
  (rspec-use-rake-when-possible nil)
  (rspec-use-spring-when-possible nil)
  (rspec-primary-source-dirs '("app"))

  :hook ruby-mode

  :bind
  (:map rspec-verifiable-mode-keymap
        ("y" . rspec-spec-or-target-other-window-no-change-window)
        ("u" . rspec-find-spec-or-target-other-window)
        ("e" . rspec-find-spec-or-target-find-example-other-window)
        ("w" . rspec-toggle-spec-and-target-find-example))
  (:map rspec-mode-keymap
        ("y" . rspec-spec-or-target-other-window-no-change-window)
        ("u" . rspec-find-spec-or-target-other-window)
        ("e" . rspec-find-spec-or-target-find-example-other-window)
        ("w" . rspec-toggle-spec-and-target-find-example))
  (:map compilation-mode-map
        ("e" . rr/pry-in-rspec-compilation)
        ("v" . rr/find-file-at-point-with-vlc)
        ("o" . compilation-display-error))

  :config
  (defadvice rspec-compile
      (before rspec-save-before-compile (A-FILE-OR-DIR &optional opts) activate)
    "Save current buffer before running spec.  This remove the annoying save confirmation."
    (save-some-buffers (lambda () (string-match "\\.rb" (buffer-name  (current-buffer))))))

  (defun rspec-spec-or-target-other-window-no-change-window ()
    "Just like rspec-find-spec-or-target-other-window but does not change the current window."
    (interactive)
    (rspec-find-spec-or-target-other-window)
    (other-window 1))

  (defun rr/pry-in-rspec-compilation ()
    "Fast usage of pry inside compilation mode buffersa"
    (interactive)
    (end-of-buffer)
    (inf-ruby-switch-from-compilation)))

(use-package inf-ruby
  :hook
  (ruby-mode . inf-ruby-switch-setup)

  :bind
  (:map inf-ruby-minor-mode-map
        ("C-M-x" . ruby-send-block)
        ("C-c C-c" . inf-ruby-console-auto))
  (:map inf-ruby-mode-map
        ("C-c C-c" . rr/pry-byebug-jump-to-source))

  :config
  (setenv "PAGER" (executable-find "cat"))
  (defun ruby-send-buffer ()
    "Send whole buffer to inferior process."
    (interactive)
    (ruby-send-region (point-min) (point-max))))

(use-package rubocop
  :hook
  (ruby-mode . rubocop-mode))

(use-package yard-mode
  :hook ruby-mode)

(use-package robe
  :hook
  (ruby-mode . robe-mode)

  :config
  (require 'company-robe))

(use-package rhtml-mode
  :mode "\\.html.erb$")

(use-package ruby-refactor
  :hook (ruby-mode . ruby-refactor-mode))

(provide 'init-ruby)
;;; init-ruby.el ends here
