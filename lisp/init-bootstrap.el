;;; init-bootstrap.el --- Configures the bootstrapping of the Emacs configuration.
;;; Commentary:
;;; Code:
(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (vendor-dir (expand-file-name "vendor" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir)
  (add-to-list 'load-path vendor-dir))

;; (require 'cl-lib)
;; (defmacro defmodule (module-name &rest args)
;;   (let ((packages (plist-get args :packages))
;;         (config (plist-get args :config)))
;;     `(progn ,@config
;;             (provide ',module-name))))

(defvar init-files
  '(init-packages
    init-custom-defuns
    init-edit-defuns
    init-keybindings
    init-defaults
    init-company
    init-helm
    init-isearch
    init-project-utils
    init-path
    init-magit
    init-smartparens
    init-yas
    init-shell
    init-haskell
    init-ruby
    init-js
    init-lisp
    init-python
    init-clojure
    init-csharp
    init-elixir
    init-web
    init-ansible
    init-writing
    ;; init-octave
    ;; init-java
    ;; init-rust
    ;; init-c
    init-scala
    ;; init-mail
    init-org
    init-erc
    init-appearance
    init-registers))

(defun safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (progn
        (message (format "loading %s"
                         (symbol-name feature)))
        (require feature))
    ('error (add-to-list 'rr/initialization-errors
			 (format "[ERROR LOADING \"%s\"]: %s"
                                 (symbol-name feature) ex)))))

(defun rr/safe-load-init-files ()
  (dolist (file init-files)
    (safe-require file)))

(defun rr/unsafe-load-init-files ()
  (dolist (file init-files)
    (require file)))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
