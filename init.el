;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package init-git :ensure nil)
(use-package init-python :ensure nil)
(use-package init-ansible :ensure nil)
(use-package init-flycheck :ensure nil)
(use-package init-packages :ensure nil)
(use-package init-elixir :ensure nil)
(use-package init-custom-defuns :ensure nil)
(use-package init-edit-defuns :ensure nil)
(use-package init-keybindings :ensure nil)
(use-package init-defaults :ensure nil)
(use-package init-company :ensure nil)
(use-package init-ivy :ensure nil)
(use-package init-project-utils :ensure nil)
(use-package init-path :ensure nil)
(use-package init-smartparens :ensure nil)
(use-package init-yas :ensure nil)
(use-package init-shell :ensure nil)
(use-package init-ruby :ensure nil)
;; (use-package init-js :ensure nil)
(use-package init-lisp :ensure nil)
(use-package init-java :ensure nil)
(use-package init-org :ensure nil)
(use-package init-appearance :ensure nil)
;; (use-package init-registers :ensure nil)
(use-package init-c :ensure nil)
;; (use-package init-scala :ensure nil)
;; (use-package init-rust :ensure nil)
(use-package init-restclient :ensure nil)
;; init-haskell
;; init-clojure
;; init-csharp
;; init-web
;; (use-package init-writing :ensure nil)
;; init-octave
;; init-mail
;; init-erc

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))
