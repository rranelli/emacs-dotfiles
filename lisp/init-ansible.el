;;; init-ansible.el -- Configures utilities for dealing with ansible.
;;; Commentary:
;;; Code:
(require 'ansible)
(require 'ansible-doc)

(setq ansible::vault-password-file "/home/renan/.emacs.d/.ansible-vault")

(defun rr/decrypt-if-vault ()
  ""
  (let ((vault-file? (string-match-p "\$ANSIBLE_VAULT;[0-9]+\.[0-9]+"
                                     (buffer-substring-no-properties (point-min)
                                                                     (point-max)))))
    (when vault-file?
      (condition-case ex
          (progn
            (ansible::decrypt-buffer)
            (add-hook 'before-save-hook 'ansible::encrypt-buffer nil t)
            (add-hook 'after-save-hook  'ansible::decrypt-buffer nil t))
        ('error
         (message "Could not decrypt file. Use `C-c v' to choose another password"))))))

(add-to-list 'auto-mode-alist (cons "\\.vault$" 'yaml-mode))
(add-hook 'ansible-hook 'rr/decrypt-if-vault)

(defun rr/set-ansible-vault-mimipass-pwd ()
  ""
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
