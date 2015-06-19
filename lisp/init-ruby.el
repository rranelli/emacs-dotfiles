;;; init-ruby.el -- Configures nice-to-have features for Ruby development.
;;; Commentary:
;;; Code:
(require 'inf-ruby)
(require 'rspec-mode)
(require 'company-robe)
(require 'robe)
(require 'rubocop)

;; auto modes
(dolist (fp '("\\.rb$"
	      "\\.ru$"
              "\\.rake"
	      "\\.jbuilder$"
	      "\\.gemspec$"
	      "\\GuardFile$"
	      "\\Rakefile$"
	      "\\Vagrantfile$"
	      "\\Gemfile$"
	      "\\Godfile$"
	      "\\.god$"))
  (add-to-list 'auto-mode-alist `(,fp . ruby-mode)))

;; pretty stabby lambda
(setq pretty-symbol-patterns
      (append pretty-symbol-patterns
	      `((?λ lambda "-> " (ruby-mode)))))

;; hook auxiliary modes to ruby mode
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(add-hook 'ruby-mode-hook 'rubocop-mode)
(add-hook 'ruby-mode-hook 'yard-mode)

;; fix for rspec and pry
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setenv "PAGER" (executable-find "cat"))

(setq rspec-use-rake-when-possible nil)
(setq rspec-use-spring-when-possible nil)

;; -- GODAMMIT RUBY INDENTATION!!! --
;; don't indent parenthesis in a weird way
(setq ruby-align-chained-calls nil
      ruby-align-to-stmt-keywords nil
      ruby-deep-indent-paren nil
      ruby-deep-indent-paren-style nil
      ruby-use-smie nil)

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

(defun rr/pry-in-rspec-compilation ()
  "Fast usage of pry inside compilation mode buffersa"
  (interactive)
  (end-of-buffer)
  (inf-ruby-switch-from-compilation))

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
    (insert (s-join "\n" require-text))))

(defun rr/toggle-vcr-off ()
  "Toggles VCR_OFF environment variable between 'true' and nil"
  (interactive)
  (-> (getenv "VCR_OFF")
      (equal "true")
      (unless "true")
      (->> (setenv "VCR_OFF")
           (format "VCR_OFF set to %s"))
      (message)))

;; -- keybindings --
(dolist (map '(rspec-mode-keymap rspec-verifiable-mode-keymap))
  (rr/define-bindings map
                      '(("y" . rspec-spec-or-target-other-window-no-change-window)
                        ("u" . rspec-find-spec-or-target-other-window)
                        ("e" . rspec-find-spec-or-target-find-example-other-window)
                        ("w" . rspec-toggle-spec-and-target-find-example))))

(rr/define-bindings ruby-mode-map
                    '(("C-c r b" . ruby-send-buffer)
                      ("C-c r r" . ruby-send-region)
                      ("C-c r v" . ruby-refactor-extract-local-variable)
                      ("C-c r m" . ruby-refactor-extract-to-method)
                      ("C-c r h" . rr/convert-to-ruby-1.9-hash-syntax)
                      ("C-c r s" . rr/split-module-nesting)
                      ("C-c r l" . rr/wrap-in-stabby-lambda)))

(rr/define-bindings rspec-compilation-mode-map
                    '(("e" . rr/pry-in-rspec-compilation)))

(provide 'init-ruby)
;;; init-ruby.el ends here
