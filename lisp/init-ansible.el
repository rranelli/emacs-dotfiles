;;; init-ansible.el -- Configures utilities for dealing with ansible.
;;; Commentary:
;;; Code:
(use-package yaml-mode
  :mode "\\.vault")

(use-package ansible
  :after yaml-mode
  :mode "\\.yml"

  :custom
  (ansible-vault-password-file "~/bin/ansible-vault-pwd")

  :hook (yaml-mode . ansible)

  :bind
  (:map ansible-key-map
        ("C-c v" . rr/set-ansible-vault-mimipass-pwd)
        ("C-c C-d" . ansible-doc))

  :config
  (add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)
  (defun rr/write-string (string file)
    (with-temp-buffer
      (insert string)
      (write-region (point-min) (point-max) file)))

  (defun rr/set-ansible-vault-mimipass-pwd ()
    "Choose which mimipass password to be used for ansible vault."
    (interactive)
    (rr/write-string (format "#!/bin/bash\nmimipass get %s"
                             (rr/helm-mimipass))
                     ansible-vault-password-file)
    (chmod ansible-vault-password-file
           (string-to-number "700" 8))))

(use-package k8s-mode
  :ensure t
  :mode "\\.yaml"
  :hook (k8s-mode . yas-minor-mode))

(use-package company-ansible
  :hook ansible-mode)

(use-package ansible-doc
  :hook ansible
  :bind
  (:map ansible-doc-module-mode-map
        ("C-x C-s" . ignore)))

(provide 'init-ansible)
;;; init-ansible.el ends here
