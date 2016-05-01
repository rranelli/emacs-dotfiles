;;; init-ansible.el -- Configures utilities for dealing with ansible.
;;; Commentary:
;;; Code:
(require 'ansible)
(require 'ansible-doc)

(add-to-list 'auto-mode-alist (cons "\\.vault$" 'yaml-mode))
(setq ansible::vault-password-file "/home/renan/.emacs.d/.ansible-vault")

(defun rr/set-ansible-vault-mimipass-pwd ()
  ""
  (interactive)
  (rr/write-string (format "#!/bin/bash\nmimipass get %s"
                           (read-string "Which mimipass password? "
                                        "xerpa/ansible-vault"))
                   ansible::vault-password-file))

;;; Hooks
(add-hook 'yaml-mode-hook (-partial 'ansible 1))
(add-hook 'yaml-mode-hook 'ansible-doc-mode)
(add-hook 'yaml-mode-hook (-partial 'auto-fill-mode -1))
(add-hook 'yaml-mode-hook 'flyspell-mode-off)

(add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)

;;; Keybindings
(define-key ansible-doc-module-mode-map (kbd "C-x C-s") 'ignore)
(define-key ansible::key-map (kbd "C-c v") 'rr/set-ansible-vault-mimipass-pwd)

(provide 'init-ansible)
;;; init-ansible.el ends here
