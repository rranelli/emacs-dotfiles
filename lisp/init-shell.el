;;; init-shell.el -- Configures features that enhances one's work with terminals inside Emacs.
;;; Commentary:
;;; Code:

;; new shells
(defun rr/new-shell (arg shell-fn shell)
  "Create an eshell with given name.
If ARG is present, open a new eshell regardless."
  (interactive "P")
  (let* ((custom-name (if arg (format "[%s]" (read-string "Shell name: ")) ""))
	 (shell-name (format "%s: %s %s" (symbol-name shell) (rr/shell-project-name) custom-name))
	 (shell-exists-p (bufferp (get-buffer shell-name))))

    (if (not shell-exists-p)
        (progn (funcall shell-fn)
               (rename-buffer shell-name))
      (switch-to-buffer shell-name))

    (when (not shell-exists-p)
      (goto-char (point-max))
      (insert (format "cd %s # [Enter] cds to root" (rr/shell-wd))))))

(defun rr/new-eshell (arg)
  (interactive "P")
  (rr/new-shell arg 'eshell 'eshell))

(defun rr/new-ansi-term (arg)
  (interactive "P")
  (rr/new-shell arg '(lambda () (ansi-term "/bin/bash")) 'ansi-term))

(defun rr/shell-project-name ()
  "Return project name from directory."
  (->> (if (projectile-project-p) (projectile-project-root) default-directory)
       (directory-file-name)
       (file-name-base)))

(defun rr/shell-wd ()
  "Return what should be the shell working directory."
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

;; eshell compatibility
(defun rr/run-command-in-bash (&rest cmd)
  "Run CMD as if you were in a bash shell instead of Eshell."
  (insert (format "bash -c 'source ~/.bashrc; cd %s; %s'"
                  (eshell/pwd)
                  (s-join " " (car cmd))))
  (eshell-send-input))

;; -- keybindings --

;; term
(add-hook 'term-mode-hook
          (lambda ()
            (rr/expose-bindings term-raw-map
                                (->> rr/default-bindings-to-expose
                                     (remove "C-h")
                                     (remove "M-h")))))

;; eshell
(global-set-key (kbd "C-x RET") 'rr/new-eshell)
(global-set-key (kbd "C-x M-RET") 'rr/new-ansi-term)

(rr/expose-default-bindings-with-hook eshell-mode)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (rr/define-bindings eshell-mode-map
                                  '(( "C-x C-s" 'ignore)
                                    ("M-r" 'helm-eshell-history)))))

(provide 'init-shell)
;;; init-shell.el ends here
