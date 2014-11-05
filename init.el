;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'cl)

;; --- bootstrapping ---
(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (vendor-dir (expand-file-name "vendor" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir)
  (add-to-list 'load-path vendor-dir))

(defun safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (require feature)
    ('error (message (format "[ERROR LOADING \"%s\"]: %s" (symbol-name feature) ex)))))

(defvar init-files
  '(init-packages
    init-custom-defuns
    init-edit-defuns
    init-keybindings
    init-defaults
    init-helm
    init-project-utils
    init-path
    init-appearance
    init-magit
    init-ac
    init-yas
    init-haskell
    init-ruby
    init-java
    init-lisp
    init-clojure
    init-isearch
    init-writing
    init-org
    init-mail
    init-shell))

(defun rr-load-init-files ()
  (dolist (file init-files)
    (safe-require file)))

(rr-load-init-files)

;; -- custom --
(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; -- auto start server --
(require 'server)
(unless (server-running-p)
  (server-start))

;; Finish!
(message
 "======================================
All is sane, and init.el got to its end
========================================")
;;; init.el ends here
