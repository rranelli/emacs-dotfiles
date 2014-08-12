;;; Package --- Summary
;;; Commentary:
;;; Code:
(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(defconst *spell-check-support-enabled* nil)

;; --- bootstrapping ---
(require 'cl)
(require 'init-packages)
(require 'init-custom-defuns)
(require 'init-defaults)
(require 'init-path)

;; Init everything else
(require 'init-appearance)
(require 'init-magit)
(require 'init-ac)
(require 'init-yas)
(require 'init-ruby)
(require 'init-lisp)
(require 'init-isearch)
(require 'init-project-utils)
(require 'init-writting)
(require 'init-helm)
(require 'init-org)
(require 'init-neotree)
(require 'init-keybindings)

;; -- custom --
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; -- auto start server --
(require 'server)
(unless (server-running-p)
    (server-start))

;; Finish!
(message  "All is sane, and init.el got to its end")
;;; init.el ends here
