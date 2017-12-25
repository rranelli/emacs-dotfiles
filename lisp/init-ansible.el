;;; init-ansible.el -- Configures utilities for dealing with ansible.
;;; Commentary:
;;; Code:
(use-package yaml-mode
  :mode "\\.vault"

  :config
  (add-hook 'yaml-mode-hook (lambda () (auto-fill-mode -1)))
  (add-hook 'yaml-mode-hook 'flyspell-mode-off))

(use-package ansible
  :after yaml-mode

  :custom
  (ansible::vault-password-file "/home/renan/.emacs.d/.ansible-vault")

  :hook (yaml-mode . ansible)

  :bind
  (:map ansible::key-map
        ("C-c v" . rr/set-ansible-vault-mimipass-pwd)
        ("C-c C-d" . ansible-doc))

  :config
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)
  (defun rr/set-ansible-vault-mimipass-pwd ()
    "Choose which mimipass password to be used for ansible vault."
    (interactive)
    (rr/write-string (format "#!/bin/bash\nmimipass get %s"
                             (rr/helm-mimipass))
                     ansible::vault-password-file)
    (chmod ansible::vault-password-file
           (string-to-number "700" 8))))

(use-package company-ansible
  :hook ansible-mode)

(use-package ansible-doc
  :hook ansible
  :bind
  (:map ansible-doc-module-mode-map
        ("C-x C-s" . ignore)))

(provide 'init-ansible)
;;; init-ansible.el ends here
