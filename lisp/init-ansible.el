;;; init-ansible.el -- Configures utilities for dealing with ansible.
;;; Commentary:
;;; Code:
(require 'ansible)
(require 'ansible-doc)

(add-to-list 'auto-mode-alist (cons "\\.vault$" 'yaml-mode))
(setq ansible::vault-password-file "/home/renan/.emacs.d/.ansible-vault")

(defun rr/set-ansible-vault-mimipass-pwd ()
  "Choose which mimipass password to be used for ansible vault."
  (interactive)
  (rr/write-string (format "#!/bin/bash\nmimipass get %s"
                           (rr/helm-mimipass))
                   ansible::vault-password-file)
  (chmod ansible::vault-password-file
         (string-to-number "700" 8)))

;;; Hooks
(add-hook 'yaml-mode-hook (-partial 'ansible 1))
(add-hook 'yaml-mode-hook 'ansible-doc-mode)
(add-hook 'yaml-mode-hook (-partial 'auto-fill-mode -1))
(add-hook 'yaml-mode-hook 'flyspell-mode-off)

(add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)

;;; Keybindings
(define-key ansible-doc-module-mode-map (kbd "C-x C-s") 'ignore)
(define-key ansible::key-map (kbd "C-c v") 'rr/set-ansible-vault-mimipass-pwd)
(define-key ansible::key-map (kbd "C-c C-d") 'ansible-doc)

(provide 'init-ansible)
;;; init-ansible.el ends here
