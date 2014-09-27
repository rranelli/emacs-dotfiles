;;; Package --- Summary
;;; Commentary:
;;; Code:
(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(require 'cl)

;; --- bootstrapping ---
(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (vendor-dir (expand-file-name "vendor" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir)
  (add-to-list 'load-path vendor-dir))

(defvar init-files
  '(init-packages
    init-custom-defuns
    init-keybindings
    init-defaults
    init-path
    init-appearance
    init-magit
    init-ac
    init-yas
    init-ruby
    init-lisp
    init-isearch
    init-project-utils
    init-writing
    init-helm
    init-org
    init-shell))

(dolist (file init-files)
  (require file))

;; -- custom --
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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
(put 'set-goal-column 'disabled nil)
