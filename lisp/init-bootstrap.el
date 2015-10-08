;;; init-bootstrap.el --- Configures the bootstrapping of the Emacs configuration.
;;; Commentary:
;;; Code:
(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (vendor-dir (expand-file-name "vendor" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir)
  (add-to-list 'load-path vendor-dir))

(defvar init-files
  '(init-packages
    init-custom-defuns
    init-edit-defuns
    init-keybindings
    init-defaults
    init-company
    init-helm
    init-project-utils
    init-path
    init-magit
    init-yas
    init-octave
    init-haskell
    init-ruby
    init-java
    init-lisp
    init-rust
    init-clojure
    init-scala
    init-elixir
    init-isearch
    init-writing
    init-smartparens
    init-org
    init-shell
    init-mail
    init-erc
    init-appearance
    init-registers))

(defun safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (require feature)
    ('error (add-to-list 'rr/initialization-errors
			 (format "[ERROR LOADING \"%s\"]: %s" (symbol-name feature) ex)))))

(defun rr/safe-load-init-files ()
  (dolist (file init-files)
    (safe-require file)))

(defun rr/unsafe-load-init-files ()
  (dolist (file init-files)
    (require file)))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
