;;; init-ansible.el -- Configures utilities for dealing with ansible.
;;; Commentary:
;;; Code:
(require 'ansible)
(require 'ansible-doc)

(setq ansible::vault-password-file "/home/renan/code/emacs-dotfiles/ansible-vault")

(defun rr/set-ansible-vault-mimipass-pwd ()
  (interactive)
  (rr/write-string (format "#!/bin/bash\nmimipass get %s"
                           (read-string "Which mimipass password? "
                                        "xerpa/ansible-vault"))
                   ansible::vault-password-file))

(add-hook 'yaml-mode-hook (-partial 'ansible 1))
(add-hook 'yaml-mode-hook 'ansible-doc-mode)
(add-hook 'yaml-mode-hook (-partial 'auto-fill-mode -1))

(define-key ansible-doc-module-mode-map (kbd "C-x C-s") 'ignore)
(define-key ansible::key-map (kbd "C-c v") 'rr/set-ansible-vault-mimipass-pwd)

(provide 'init-ansible)
;;; init-ansible.el ends here
