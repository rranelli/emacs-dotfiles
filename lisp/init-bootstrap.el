;;; Package --- Summary
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

(defun safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (require feature)
    ('error (message (format "[ERROR LOADING \"%s\"]: %s" (symbol-name feature) ex)))))

(defun rr-safe-load-init-files ()
  (dolist (file init-files)
    (safe-require file)))

(defun rr-unsafe-load-init-files ()
  (dolist (file init-files)
    (require file)))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
